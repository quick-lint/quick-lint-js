// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <atomic>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <map>
#include <mongoose.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/debug/debug-server-fs.h>
#include <quick-lint-js/debug/debug-server.h>
#include <quick-lint-js/debug/find-debug-server.h>
#include <quick-lint-js/debug/mongoose.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/binary-writer.h>
#include <quick-lint-js/util/instance-tracker.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/synchronized.h>
#include <string>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
class Trace_Flusher_WebSocket_Backend final : public Trace_Flusher_Backend {
 public:
  explicit Trace_Flusher_WebSocket_Backend(::mg_connection *connection,
                                           Debug_Server *server)
      : connection_(connection), server_(server) {}

  void trace_thread_begin(Trace_Flusher_Thread_Index) override {}

  void trace_thread_end(Trace_Flusher_Thread_Index) override {}

  void trace_thread_write_data(Trace_Flusher_Thread_Index thread_index,
                               Span<const std::byte> data) override {
    Lock_Ptr thread_queues = this->thread_queues_.lock();

    Async_Byte_Queue &queue = (*thread_queues)[thread_index];
    queue.append_copy(data.data(), narrow_cast<std::size_t>(data.size()));
    queue.commit();
    server_->wake_up_server_thread();
  }

  // Called on the server thread.
  void flush_if_needed() {
    Lock_Ptr thread_queues = this->thread_queues_.lock();

    for (auto &[thread_index, queue] : *thread_queues) {
      std::size_t total_message_size = 0;

      {
        std::uint8_t header[sizeof(std::uint64_t)];
        Binary_Writer writer(header);
        writer.u64_le(thread_index);
        int ok = ::mg_send(this->connection_, header, sizeof(header));
        QLJS_ASSERT(ok);
        total_message_size += sizeof(header);
      }

      queue.take_committed(
          [&](Span<const std::byte> data) {
            // FIXME(strager): ::mg_send fails if size is 0. We shouldn't need
            // this size check, but async_byte_queue gives us empty chunks for
            // some reason. We should make async_byte_queue not give us empty
            // chunks.
            if (!data.empty()) {
              int ok = ::mg_send(this->connection_, data.data(),
                                 narrow_cast<std::size_t>(data.size()));
              QLJS_ASSERT(ok);
              total_message_size += narrow_cast<std::size_t>(data.size());
            }
          },
          [] {});

      ::mg_ws_wrap(this->connection_, total_message_size, WEBSOCKET_OP_BINARY);
    }
  }

 private:
  ::mg_connection *const connection_;
  Debug_Server *const server_;

  Synchronized<Hash_Map<Trace_Flusher_Thread_Index, Async_Byte_Queue>>
      thread_queues_;

  friend class Debug_Server;
};

std::shared_ptr<Debug_Server> Debug_Server::create() {
  std::shared_ptr<Debug_Server> instance =
      std::make_shared<Debug_Server>(Create_Tag());
  Instance_Tracker<Debug_Server>::track(instance);
  return instance;
}

std::vector<std::shared_ptr<Debug_Server>> Debug_Server::instances() {
  return Instance_Tracker<Debug_Server>::instances();
}

Debug_Server::Debug_Server(Create_Tag) {}

Debug_Server::~Debug_Server() {
  if (this->server_thread_.joinable()) {
    this->stop_server_thread();
    this->server_thread_.join();
  }

  for (auto &backend : this->tracer_backends_) {
    Trace_Flusher::instance()->disable_backend(backend.get());
  }
}

void Debug_Server::set_listen_address(std::string_view address) {
  QLJS_ASSERT(!this->server_thread_.joinable());

  this->state_.lock()->requested_listen_address = address;
}

void Debug_Server::start_server_thread() {
  QLJS_ASSERT(!this->server_thread_.joinable());

  {
    Lock_Ptr<Shared_State> state = this->state_.lock();
    state->initialized = false;
    state->init_error.clear();
  }

  this->server_thread_ = Thread([this] { this->run_on_current_thread(); });
}

void Debug_Server::stop_server_thread() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->stop_server_thread_ = true;
  this->wake_up_server_thread(state);

  this->did_wait_for_server_start_ = false;
}

Result<void, Debug_Server_IO_Error> Debug_Server::wait_for_server_start() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->initialized_.wait(
      state, [&] { return state->initialized || !state->init_error.empty(); });

  if (!state->init_error.empty()) {
    return failed_result(Debug_Server_IO_Error{
        .error_message = state->init_error,
    });
  }

  this->did_wait_for_server_start_ = true;
  return {};
}

std::string Debug_Server::url() { return this->url("/"sv); }

std::string Debug_Server::url(std::string_view path) {
  QLJS_ASSERT(this->did_wait_for_server_start_);

  std::string result;
  result.reserve(path.size() + 100);
  result += "http://"sv;
  this->get_host_and_port(result);
  result += path;
  return result;
}

std::string Debug_Server::websocket_url(std::string_view path) {
  QLJS_ASSERT(this->did_wait_for_server_start_);

  std::string result;
  result.reserve(path.size() + 100);
  result += "ws://"sv;
  this->get_host_and_port(result);
  result += path;
  return result;
}

std::uint16_t Debug_Server::tcp_port_number() {
  QLJS_ASSERT(this->did_wait_for_server_start_);
  return this->state_.lock()->port_number();
}

void Debug_Server::debug_probe_publish_lsp_documents() {
  this->need_publish_lsp_documents_ = true;
  this->wake_up_server_thread();
}

void Debug_Server::debug_probe_publish_vector_profile() {
  this->need_publish_vector_profile_ = true;
  this->wake_up_server_thread();
}

void Debug_Server::wake_up_server_thread() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->wake_up_server_thread(state);
}

void Debug_Server::wake_up_server_thread(Lock_Ptr<Shared_State> &state) {
  if (state->initialized) {
    char wakeup_signal[] = {0};
    long rc = ::send(state->wakeup_pipe, wakeup_signal, sizeof(wakeup_signal),
                     /*flags=*/0);
    QLJS_ALWAYS_ASSERT(rc == 1);
  }
}

void Debug_Server::get_host_and_port(std::string &out) {
  ::mg_addr address = this->state_.lock()->actual_listen_address;
  ::mg_xprintf(
      [](char c, void *user_data) -> void {
        static_cast<std::string *>(user_data)->push_back(c);
      },
      &out, "%M", ::mg_print_ip_port, &address);
}

void Debug_Server::run_on_current_thread() {
  Trace_Flusher::instance()->register_current_thread();

  Mongoose_Mgr mgr;

  {
    Lock_Ptr<Shared_State> state = this->state_.lock();

    std::string connect_logs;
    mongoose_begin_capturing_logs_on_current_thread(&connect_logs);
    ::mg_connection *server_connection = ::mg_http_listen(
        mgr.get(), state->requested_listen_address.c_str(),
        mongoose_callback<&Debug_Server::http_server_callback>(), this);
    mongoose_stop_capturing_logs_on_current_thread();
    if (!server_connection) {
      if (connect_logs.empty()) {
        state->init_error = "unknown error in mg_http_listen";
      } else {
        state->init_error = std::move(connect_logs);
      }
      this->initialized_.notify_all();

      Trace_Flusher::instance()->unregister_current_thread();
      return;
    }

    QLJS_ASSERT(!state->initialized);

    // server_connection->loc is initialized synchronously, so we should be able
    // to use it now.
    state->actual_listen_address = server_connection->loc;

    register_current_thread_as_debug_server_thread(state->port_number());

    state->wakeup_pipe = ::mg_mkpipe(
        mgr.get(), mongoose_callback<&Debug_Server::wakeup_pipe_callback>(),
        this,
        /*udp=*/false);
    QLJS_ALWAYS_ASSERT(state->wakeup_pipe != -1);

    state->initialized = true;
    this->initialized_.notify_all();
  }

  while (!this->stop_server_thread_.load()) {
    ::mg_mgr_poll(mgr.get(), /*timeout_ms=*/-1);
  }

  Trace_Flusher::instance()->unregister_current_thread();
}

void Debug_Server::begin_closing_all_connections(::mg_mgr *mgr) {
  ::mg_connection *c = mgr->conns;
  while (c) {
    c->is_closing = true;
    c = c->next;
  }
}

void Debug_Server::http_server_callback(::mg_connection *c, int ev,
                                        void *ev_data) {
  switch (ev) {
  case ::MG_EV_HTTP_MSG: {
    ::mg_http_message *hm = static_cast<::mg_http_message *>(ev_data);
    if (::mg_http_match_uri(hm, "/api/trace")) {
      ::mg_ws_upgrade(c, hm, nullptr);
    } else {
      std::string public_directory = get_debug_server_public_directory();
      ::mg_http_serve_opts options = {
          .root_dir = public_directory.c_str(),
          .ssi_pattern = nullptr,
          .extra_headers = nullptr,
          .mime_types = "mjs=text/javascript",
          .page404 = nullptr,
          .fs = nullptr,
      };
      ::mg_http_serve_dir(c, hm, &options);
    }
    break;
  }

  case ::MG_EV_WS_OPEN: {
    this->tracer_backends_.emplace_back(
        std::make_unique<Trace_Flusher_WebSocket_Backend>(c, this));
    Trace_Flusher_WebSocket_Backend *backend =
        this->tracer_backends_.back().get();
    Trace_Flusher::instance()->enable_backend(backend);

    // Publish initial state to the new client. (As a side effect, this also
    // publishes to other connected clients, but that's okay.)
    this->debug_probe_publish_lsp_documents();
    this->debug_probe_publish_vector_profile();
    break;
  }

  case ::MG_EV_CLOSE: {
    auto backend_it = std::find_if(
        this->tracer_backends_.begin(), this->tracer_backends_.end(),
        [&](auto &backend) { return backend->connection_ == c; });
    if (backend_it != this->tracer_backends_.end()) {
      Trace_Flusher::instance()->disable_backend(backend_it->get());
      this->tracer_backends_.erase(backend_it);
    }
    break;
  }

  default:
    break;
  }
}

void Debug_Server::wakeup_pipe_callback(::mg_connection *c, int ev, void *) {
  switch (ev) {
  case ::MG_EV_READ:
    // wake_up_server_thread was called.
    if (this->stop_server_thread_.load()) {
      this->begin_closing_all_connections(c->mgr);
    }

#if QLJS_FEATURE_VECTOR_PROFILING
    if (this->need_publish_vector_profile_.load()) {
      this->need_publish_vector_profile_.store(false);

      this->max_size_histogram_.add_entries(
          Vector_Instrumentation::instance.take_entries());

      Trace_Writer *tw =
          Trace_Flusher::instance()->trace_writer_for_current_thread();
      if (tw != nullptr) {
        Monotonic_Allocator memory("Debug_Server publish vector profile");
        auto histogram = this->max_size_histogram_.histogram(&memory);

        tw->write_event_vector_max_size_histogram_by_owner(
            Trace_Event_Header{
                .timestamp = 0,  // TODO(strager)
            },
            Trace_Event_Vector_Max_Size_Histogram_By_Owner{
                .entries = histogram,
            });
        tw->commit();
      }
      Trace_Flusher::instance()->flush_sync();
    }
#endif

    this->publish_lsp_documents_if_needed();

    for (auto &backend : this->tracer_backends_) {
      backend->flush_if_needed();
    }
    break;

  default:
    break;
  }
}

void Debug_Server::publish_lsp_documents_if_needed() {
  if (!this->need_publish_lsp_documents_.load()) {
    return;
  }
  this->need_publish_lsp_documents_.store(false);

  Synchronized<LSP_Documents> *documents_raw = get_lsp_server_documents();
  if (documents_raw == nullptr) {
    return;
  }

  Trace_Writer *tw =
      Trace_Flusher::instance()->trace_writer_for_current_thread();
  if (tw == nullptr) {
    return;
  }

  std::vector<Trace_LSP_Document_State> document_states;
  {
    Lock_Ptr<LSP_Documents> documents = documents_raw->lock();
    document_states.reserve(documents->documents.size());
    for (auto &[uri, doc] : documents->documents) {
      document_states.push_back(Trace_LSP_Document_State{
          .type = doc->trace_type(),
          .uri = uri,
          .text = doc->doc.string().string_view(),
          .language_id = to_string8_view(doc->language_id),
      });
    }

    tw->write_event_lsp_documents(
        Trace_Event_Header{
            .timestamp = 0,  // TODO(strager)
        },
        Trace_Event_LSP_Documents{
            .documents = Span<const Trace_LSP_Document_State>(document_states),
        });
  }
  tw->commit();
  Trace_Flusher::instance()->flush_sync();
}

std::uint16_t Debug_Server::Shared_State::port_number() const {
  return ::mg_ntohs(this->actual_listen_address.port);
}
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
