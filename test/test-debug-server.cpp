// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <cctype>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <mongoose.h>
#include <optional>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/debug/debug-server.h>
#include <quick-lint-js/debug/find-debug-server.h>
#include <quick-lint-js/debug/mongoose.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/binary-reader.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/version.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct HTTP_Response {
  std::optional<int> status;
  std::string data;
  std::vector<std::pair<std::string, std::string>> headers;

  std::string_view get_last_header_value_or_empty(
      std::string_view header_name) const;

  static HTTP_Response failed([[maybe_unused]] std::string_view error_message) {
    return HTTP_Response();
  }

  // Returns false if there was a failure when making the request.
  /*implicit*/ operator bool() const { return this->status.has_value(); }
};

HTTP_Response http_fetch(const char *url);
HTTP_Response http_fetch(const std::string &url);

class HTTP_WebSocket_Client;

class HTTP_WebSocket_Client_Delegate {
 public:
  virtual ~HTTP_WebSocket_Client_Delegate() = default;

  virtual void on_message_binary(HTTP_WebSocket_Client *, const void *message,
                                 std::size_t message_size) = 0;
};

class HTTP_WebSocket_Client {
 public:
  static void connect_and_run(const char *url,
                              HTTP_WebSocket_Client_Delegate *);

  void stop();

 private:
  void callback(::mg_connection *c, int ev, void *ev_data);

  explicit HTTP_WebSocket_Client(HTTP_WebSocket_Client_Delegate *);

  bool done_ = false;
  HTTP_WebSocket_Client_Delegate *delegate_;
  Mongoose_Mgr mgr_;

  ::mg_connection *current_connection_;
};

using Trace_Event_Callback = bool(Debug_Server &,
                                  const Parsed_Trace_Event &event,
                                  std::uint64_t thread_index);

// Creates a debug_server, connects to the trace WebSocket, and calls
// on_trace_event when messages are received.
//
// test_web_socket keeps running until on_trace_event returns false or until a
// timeout elapses, whichever comes first.
void test_web_socket(Function_Ref<Trace_Event_Callback> on_trace_event);

class Test_Debug_Server : public ::testing::Test {
 public:
  ~Test_Debug_Server() { set_lsp_server_documents(nullptr); }
};

TEST_F(Test_Debug_Server, start_thread_then_immediately_stop) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  server->stop_server_thread();
}

TEST_F(Test_Debug_Server, serves_html_at_index) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  auto wait_result = server->wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  HTTP_Response response = http_fetch(server->url("/"));
  ASSERT_TRUE(response);
  EXPECT_EQ(response.status, 200);  // OK
  EXPECT_THAT(response.data, ::testing::StartsWith("<!DOCTYPE html>"));
  EXPECT_THAT(response.get_last_header_value_or_empty("content-type"),
              ::testing::AnyOf(::testing::StrEq("text/html"),
                               ::testing::StartsWith("text/html;")));
}

TEST_F(Test_Debug_Server, serves_javascript) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  auto wait_result = server->wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  HTTP_Response response = http_fetch(server->url("/index.mjs"));
  ASSERT_TRUE(response);
  EXPECT_EQ(response.status, 200);  // OK
  EXPECT_THAT(response.data, ::testing::StartsWith("//"));
  EXPECT_THAT(response.get_last_header_value_or_empty("content-type"),
              ::testing::AnyOf(::testing::StrEq("text/javascript"),
                               ::testing::StartsWith("text/javascript;")));
}

TEST_F(Test_Debug_Server, serves_not_found_page) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  auto wait_result = server->wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  HTTP_Response response = http_fetch(server->url("/route-does-not-exist"));
  ASSERT_TRUE(response);
  EXPECT_EQ(response.status, 404);  // Not Found
}

TEST_F(Test_Debug_Server, two_servers_listening_on_same_port_fails) {
  std::shared_ptr<Debug_Server> server_a = Debug_Server::create();
  server_a->start_server_thread();
  auto a_wait_result = server_a->wait_for_server_start();
  ASSERT_TRUE(a_wait_result.ok()) << a_wait_result.error_to_string();

  std::string server_a_address = server_a->url("");
  SCOPED_TRACE("server_a address: " + server_a_address);

  std::shared_ptr<Debug_Server> server_b = Debug_Server::create();
  server_b->set_listen_address(server_a_address);
  server_b->start_server_thread();
  auto b_wait_result = server_b->wait_for_server_start();
  EXPECT_FALSE(b_wait_result.ok());
}

#if QLJS_FEATURE_VECTOR_PROFILING
TEST_F(Test_Debug_Server,
       web_socket_publishes_vector_profile_stats_on_connect) {
  Vector_Instrumentation::instance.clear();
  {
    Instrumented_Vector<std::vector<int>> v("debug server test vector", {});
    ASSERT_EQ(v.size(), 0);
  }

  bool received_vector_max_size_histogram_by_owner_event = true;
  auto on_trace_event =
      [&](Debug_Server &, const Parsed_Trace_Event &event,
          [[maybe_unused]] std::uint64_t thread_index) -> bool {
    switch (event.type) {
    case Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event: {
      // This should eventually be called.
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> entries =
          event.vector_max_size_histogram_by_owner_event.entries;
      EXPECT_EQ(entries.size(), 1);
      if (entries.size() > 0) {
        EXPECT_EQ(entries[0].owner, u8"debug server test vector"_sv);
        EXPECT_THAT(entries[0].max_size_entries,
                    ::testing::ElementsAreArray({
                        Trace_Vector_Max_Size_Histogram_Entry{.max_size = 0,
                                                              .count = 1},
                    }));
      }

      received_vector_max_size_histogram_by_owner_event = true;
      return false;
    }

    case Parsed_Trace_Event_Type::error_invalid_magic:
    case Parsed_Trace_Event_Type::error_invalid_uuid:
    case Parsed_Trace_Event_Type::error_unsupported_compression_mode:
    case Parsed_Trace_Event_Type::error_unsupported_lsp_document_type:
      ADD_FAILURE();
      return false;

    case Parsed_Trace_Event_Type::init_event:
    case Parsed_Trace_Event_Type::lsp_client_to_server_message_event:
    case Parsed_Trace_Event_Type::lsp_documents_event:
    case Parsed_Trace_Event_Type::packet_header:
    case Parsed_Trace_Event_Type::process_id_event:
    case Parsed_Trace_Event_Type::vscode_document_changed_event:
    case Parsed_Trace_Event_Type::vscode_document_closed_event:
    case Parsed_Trace_Event_Type::vscode_document_opened_event:
    case Parsed_Trace_Event_Type::vscode_document_sync_event:
      return true;
    }
    QLJS_UNREACHABLE();
  };
  test_web_socket(on_trace_event);
  EXPECT_TRUE(received_vector_max_size_histogram_by_owner_event);
}

TEST_F(Test_Debug_Server, vector_profile_probe_publishes_stats) {
  Vector_Instrumentation::instance.clear();

  bool received_vector_max_size_histogram_by_owner_event = false;
  auto on_trace_event =
      [&](Debug_Server &server, const Parsed_Trace_Event &event,
          [[maybe_unused]] std::uint64_t thread_index) -> bool {
    switch (event.type) {
    case Parsed_Trace_Event_Type::packet_header: {
      // This should eventually be called.
      Instrumented_Vector<std::vector<int>> v1("debug server test vector", {});
      v1.push_back(100);
      v1.push_back(200);
      EXPECT_EQ(v1.size(), 2);

      Instrumented_Vector<std::vector<int>> v2("debug server test vector", {});
      v2.push_back(100);
      v2.push_back(200);
      v2.push_back(300);
      v2.push_back(400);
      EXPECT_EQ(v2.size(), 4);

      server.debug_probe_publish_vector_profile();
      return true;
    }

    case Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event: {
      // This should eventually be called.
      if (event.vector_max_size_histogram_by_owner_event.entries.empty()) {
        // We will receive an initial vector_max_size_histogram_by_owner
        // event. Ignore it.
        return true;
      }

      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> entries =
          event.vector_max_size_histogram_by_owner_event.entries;
      EXPECT_EQ(entries.size(), 1);
      if (entries.size() > 0) {
        EXPECT_EQ(entries[0].owner, u8"debug server test vector"_sv);
        EXPECT_THAT(entries[0].max_size_entries,
                    ::testing::ElementsAreArray({
                        Trace_Vector_Max_Size_Histogram_Entry{.max_size = 2,
                                                              .count = 1},
                        Trace_Vector_Max_Size_Histogram_Entry{.max_size = 4,
                                                              .count = 1},
                    }));
      }

      received_vector_max_size_histogram_by_owner_event = true;
      return false;
    }

    case Parsed_Trace_Event_Type::error_invalid_magic:
    case Parsed_Trace_Event_Type::error_invalid_uuid:
    case Parsed_Trace_Event_Type::error_unsupported_compression_mode:
    case Parsed_Trace_Event_Type::error_unsupported_lsp_document_type:
      ADD_FAILURE();
      return false;

    case Parsed_Trace_Event_Type::init_event:
    case Parsed_Trace_Event_Type::lsp_client_to_server_message_event:
    case Parsed_Trace_Event_Type::lsp_documents_event:
    case Parsed_Trace_Event_Type::process_id_event:
    case Parsed_Trace_Event_Type::vscode_document_changed_event:
    case Parsed_Trace_Event_Type::vscode_document_closed_event:
    case Parsed_Trace_Event_Type::vscode_document_opened_event:
    case Parsed_Trace_Event_Type::vscode_document_sync_event:
      return true;
    }
    QLJS_UNREACHABLE();
  };
  test_web_socket(on_trace_event);
  EXPECT_TRUE(received_vector_max_size_histogram_by_owner_event);
}
#endif

TEST_F(Test_Debug_Server, trace_websocket_sends_trace_data) {
  Trace_Flusher &tracer = *Trace_Flusher::instance();

  Mutex test_mutex;
  Condition_Variable cond;
  bool registered_other_thread = false;
  bool finished_test = false;

  // Register two threads. The WebSocket server should send data for each thread
  // in separate messages.
  tracer.register_current_thread();
  Thread other_thread([&]() {
    tracer.register_current_thread();

    {
      std::unique_lock<Mutex> lock(test_mutex);
      registered_other_thread = true;
      cond.notify_all();
      cond.wait(lock, [&] { return finished_test; });
    }

    tracer.unregister_current_thread();
  });

  {
    std::unique_lock<Mutex> lock(test_mutex);
    cond.wait(lock, [&] { return registered_other_thread; });
  }

  struct Thread_State {
    bool got_packet_header = false;
    bool got_init_event = false;
  };
  std::unordered_map<Trace_Flusher_Thread_Index, Thread_State> thread_states;

  auto on_trace_event = [&](Debug_Server &, const Parsed_Trace_Event &event,
                            std::uint64_t thread_index) -> bool {
    Thread_State &state = thread_states[thread_index];
    switch (event.type) {
    case Parsed_Trace_Event_Type::packet_header:
      EXPECT_FALSE(state.got_packet_header);
      state.got_packet_header = true;
      break;

    case Parsed_Trace_Event_Type::init_event:
      EXPECT_FALSE(state.got_init_event);
      state.got_init_event = true;
      EXPECT_EQ(event.init_event.version, QUICK_LINT_JS_VERSION_STRING_U8_SV);
      break;

    case Parsed_Trace_Event_Type::error_invalid_magic:
    case Parsed_Trace_Event_Type::error_invalid_uuid:
    case Parsed_Trace_Event_Type::error_unsupported_compression_mode:
    case Parsed_Trace_Event_Type::error_unsupported_lsp_document_type:
      ADD_FAILURE();
      return false;

    case Parsed_Trace_Event_Type::lsp_client_to_server_message_event:
    case Parsed_Trace_Event_Type::lsp_documents_event:
    case Parsed_Trace_Event_Type::process_id_event:
    case Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event:
    case Parsed_Trace_Event_Type::vscode_document_changed_event:
    case Parsed_Trace_Event_Type::vscode_document_closed_event:
    case Parsed_Trace_Event_Type::vscode_document_opened_event:
    case Parsed_Trace_Event_Type::vscode_document_sync_event:
      return true;
    }

    // We expect messages for only three threads: main, other, and debug server.
    if (thread_states.size() < 3) {
      // Wait for messages for new threads.
      return true;
    }
    for (auto &[_thread_index, s] : thread_states) {
      if (!s.got_packet_header || !s.got_init_event) {
        // Wait for more messages for this thread.
        return true;
      }
    }
    // We are done.
    return false;
  };
  test_web_socket(on_trace_event);

  EXPECT_EQ(thread_states.size(), 3)
      << "expected three streams: main thread, other thread, debug server "
         "thread";
  for (auto &[thread_index, state] : thread_states) {
    SCOPED_TRACE(thread_index);
    EXPECT_TRUE(state.got_packet_header);
    EXPECT_TRUE(state.got_init_event);
  }

  {
    std::lock_guard<Mutex> lock(test_mutex);
    finished_test = true;
    cond.notify_all();
  }
  other_thread.join();

  tracer.unregister_current_thread();
}

TEST_F(Test_Debug_Server, web_socket_publishes_lsp_documents_on_connect) {
  Synchronized<LSP_Documents> documents;
  std::unique_ptr<LSP_Documents::Lintable_Document> example_doc =
      std::make_unique<LSP_Documents::Lintable_Document>();
  example_doc->doc.set_text(u8"hello"_sv);
  example_doc->language_id = "yabbascrip";
  example_doc->version_json = u8"123"_sv;
  documents.lock()->documents[u8"file:///example.js"] = std::move(example_doc);
  set_lsp_server_documents(&documents);

  bool received_lsp_documents_event = false;
  auto on_trace_event =
      [&](Debug_Server &, const Parsed_Trace_Event &event,
          [[maybe_unused]] std::uint64_t thread_index) -> bool {
    switch (event.type) {
    case Parsed_Trace_Event_Type::lsp_documents_event: {
      // This should eventually be called.
      Span<const Trace_LSP_Document_State> parsed_documents =
          event.lsp_documents_event.documents;
      EXPECT_EQ(parsed_documents.size(), 1);
      if (parsed_documents.size() > 0) {
        EXPECT_EQ(parsed_documents[0].uri, u8"file:///example.js"_sv);
        EXPECT_EQ(parsed_documents[0].text, u8"hello"_sv);
        EXPECT_EQ(parsed_documents[0].language_id, u8"yabbascrip"_sv);
      }

      received_lsp_documents_event = true;
      return false;
    }

    case Parsed_Trace_Event_Type::error_invalid_magic:
    case Parsed_Trace_Event_Type::error_invalid_uuid:
    case Parsed_Trace_Event_Type::error_unsupported_compression_mode:
    case Parsed_Trace_Event_Type::error_unsupported_lsp_document_type:
      ADD_FAILURE();
      return false;

    case Parsed_Trace_Event_Type::init_event:
    case Parsed_Trace_Event_Type::lsp_client_to_server_message_event:
    case Parsed_Trace_Event_Type::packet_header:
    case Parsed_Trace_Event_Type::process_id_event:
    case Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event:
    case Parsed_Trace_Event_Type::vscode_document_changed_event:
    case Parsed_Trace_Event_Type::vscode_document_closed_event:
    case Parsed_Trace_Event_Type::vscode_document_opened_event:
    case Parsed_Trace_Event_Type::vscode_document_sync_event:
      return true;
    }
    QLJS_UNREACHABLE();
  };
  test_web_socket(on_trace_event);
  EXPECT_TRUE(received_lsp_documents_event);
}

TEST_F(Test_Debug_Server, lsp_documents_probe_publishes_state) {
  Synchronized<LSP_Documents> documents;
  set_lsp_server_documents(&documents);

  bool received_lsp_documents_event = false;
  auto on_trace_event =
      [&](Debug_Server &server, const Parsed_Trace_Event &event,
          [[maybe_unused]] std::uint64_t thread_index) -> bool {
    switch (event.type) {
    case Parsed_Trace_Event_Type::packet_header: {
      // This should eventually be called.
      std::unique_ptr<LSP_Documents::Lintable_Document> example_doc =
          std::make_unique<LSP_Documents::Lintable_Document>();
      example_doc->doc.set_text(u8"hello"_sv);
      example_doc->language_id = "yabbascrip";
      example_doc->version_json = u8"123"_sv;
      documents.lock()->documents[u8"file:///example.js"] =
          std::move(example_doc);

      server.debug_probe_publish_lsp_documents();
      return true;
    }

    case Parsed_Trace_Event_Type::lsp_documents_event: {
      // This should eventually be called.
      if (event.lsp_documents_event.documents.empty()) {
        // We will receive an initial lsp_documents event. Ignore it.
        return true;
      }

      Span<const Trace_LSP_Document_State> parsed_documents =
          event.lsp_documents_event.documents;
      EXPECT_EQ(parsed_documents.size(), 1);
      if (parsed_documents.size() > 0) {
        EXPECT_EQ(parsed_documents[0].uri, u8"file:///example.js"_sv);
        EXPECT_EQ(parsed_documents[0].text, u8"hello"_sv);
        EXPECT_EQ(parsed_documents[0].language_id, u8"yabbascrip"_sv);
      }

      received_lsp_documents_event = true;
      return false;
    }

    case Parsed_Trace_Event_Type::error_invalid_magic:
    case Parsed_Trace_Event_Type::error_invalid_uuid:
    case Parsed_Trace_Event_Type::error_unsupported_compression_mode:
    case Parsed_Trace_Event_Type::error_unsupported_lsp_document_type:
      ADD_FAILURE();
      return false;

    case Parsed_Trace_Event_Type::init_event:
    case Parsed_Trace_Event_Type::lsp_client_to_server_message_event:
    case Parsed_Trace_Event_Type::process_id_event:
    case Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event:
    case Parsed_Trace_Event_Type::vscode_document_changed_event:
    case Parsed_Trace_Event_Type::vscode_document_closed_event:
    case Parsed_Trace_Event_Type::vscode_document_opened_event:
    case Parsed_Trace_Event_Type::vscode_document_sync_event:
      return true;
    }
    QLJS_UNREACHABLE();
  };
  test_web_socket(on_trace_event);
  EXPECT_TRUE(received_lsp_documents_event);
}

TEST_F(Test_Debug_Server, instances_shows_new_instance) {
  std::shared_ptr<Debug_Server> s = Debug_Server::create();
  EXPECT_THAT(Debug_Server::instances(), ::testing::Contains(s));
}

TEST_F(Test_Debug_Server, destroying_debug_server_removes_from_instances) {
  std::shared_ptr<Debug_Server> s = Debug_Server::create();
  Debug_Server *s_raw = s.get();
  s.reset();

  for (std::shared_ptr<Debug_Server> &instance : Debug_Server::instances()) {
    EXPECT_NE(instance.get(), s_raw);
  }
}

TEST_F(Test_Debug_Server, find_debug_servers_finds_running_instance_SLOW) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  auto wait_result = server->wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();
  std::uint16_t server_port = server->tcp_port_number();

  std::vector<Found_Debug_Server> servers = find_debug_servers();
  auto found_server_it =
      find_first_if(servers, [&](const Found_Debug_Server &s) -> bool {
        return s.port_number == server_port;
      });
  ASSERT_NE(found_server_it, servers.end()) << "should find our server";
  ASSERT_EQ(found_server_it->port_number, server_port);

  EXPECT_EQ(found_server_it->process_id, get_current_process_id());
}

bool strings_equal_case_insensitive(std::string_view a, std::string_view b) {
  return ranges_equal(
      a, b, [](char x, char y) { return std::tolower(x) == std::tolower(y); });
}

std::string_view HTTP_Response::get_last_header_value_or_empty(
    std::string_view header_name) const {
  for (const auto &[name, value] : this->headers) {
    if (strings_equal_case_insensitive(name, header_name)) {
      return value;
    }
  }
  return ""sv;
}

HTTP_Response http_fetch(const char *url) {
  Mongoose_Mgr mgr;

  struct State {
    const char *request_url;
    HTTP_Response response;
    bool done = false;
    std::string protocol_error;

    void callback(::mg_connection *c, int ev, void *ev_data) {
      switch (ev) {
      case ::MG_EV_CONNECT: {
        ::mg_str host = ::mg_url_host(this->request_url);
        ::mg_printf(c,
                    "GET %s HTTP/1.1\r\n"
                    "Host: %.*s\r\n"
                    "\r\n",
                    ::mg_url_uri(this->request_url), narrow_cast<int>(host.len),
                    host.ptr);
        break;
      }

      case ::MG_EV_HTTP_MSG: {
        ::mg_http_message *hm = static_cast<::mg_http_message *>(ev_data);
        this->response.data.assign(hm->body.ptr, hm->body.len);
        this->response.status = ::mg_http_status(hm);

        for (int i = 0; i < MG_MAX_HTTP_HEADERS; ++i) {
          const ::mg_http_header &h = hm->headers[i];
          if (h.name.len == 0) {
            break;
          }
          this->response.headers.emplace_back(
              std::string(h.name.ptr, h.name.len),
              std::string(h.value.ptr, h.value.len));
        }

        this->done = true;
        c->is_closing = true;
        break;
      }

      case ::MG_EV_ERROR:
        this->protocol_error = static_cast<const char *>(ev_data);
        this->done = true;
        c->is_closing = true;
        break;

      default:
        break;
      }
    }
  };

  State s;
  s.request_url = url;
  ::mg_connection *connection = ::mg_http_connect(
      mgr.get(), s.request_url, mongoose_callback<&State::callback>(), &s);
  if (!connection) {
    ADD_FAILURE() << "mg_http_connect failed";
    return HTTP_Response::failed("mg_http_connect failed");
  }

  while (!s.done) {
    int timeout_ms = 1000;
    ::mg_mgr_poll(mgr.get(), timeout_ms);
  }

  if (!s.protocol_error.empty()) {
    ADD_FAILURE() << "protocol error: " << s.protocol_error;
    return HTTP_Response::failed(s.protocol_error);
  }

  return std::move(s.response);
}

HTTP_Response http_fetch(const std::string &url) {
  return http_fetch(url.c_str());
}

void HTTP_WebSocket_Client::connect_and_run(
    const char *url, HTTP_WebSocket_Client_Delegate *delegate) {
  HTTP_WebSocket_Client client(delegate);
  ::mg_connection *connection = ::mg_ws_connect(
      client.mgr_.get(), url,
      mongoose_callback<&HTTP_WebSocket_Client::callback>(), &client, nullptr);
  if (!connection) {
    ADD_FAILURE() << "mg_ws_connect failed";
    return;
  }

  auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1);
  while (!client.done_) {
    if (std::chrono::steady_clock::now() >= deadline) {
      ADD_FAILURE() << "timed out";
      return;
    }
    int timeout_ms = 100;
    ::mg_mgr_poll(client.mgr_.get(), timeout_ms);
  }
}

void HTTP_WebSocket_Client::stop() {
  this->done_ = true;
  this->current_connection_->is_closing = true;
}

void HTTP_WebSocket_Client::callback(::mg_connection *c, int ev,
                                     void *ev_data) {
  this->current_connection_ = c;

  switch (ev) {
  case ::MG_EV_WS_MSG: {
    ::mg_ws_message *wm = static_cast<::mg_ws_message *>(ev_data);
    int websocket_operation = wm->flags & 0xf;
    switch (websocket_operation) {
    case WEBSOCKET_OP_BINARY:
      this->delegate_->on_message_binary(this, wm->data.ptr, wm->data.len);
      break;

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
    break;
  }

  case ::MG_EV_ERROR:
    ADD_FAILURE() << "protocol error: " << static_cast<const char *>(ev_data);
    this->stop();
    break;

  default:
    break;
  }

  this->current_connection_ = nullptr;
}

HTTP_WebSocket_Client::HTTP_WebSocket_Client(
    HTTP_WebSocket_Client_Delegate *delegate)
    : delegate_(delegate) {}

void test_web_socket(Function_Ref<Trace_Event_Callback> on_trace_event) {
  std::shared_ptr<Debug_Server> server = Debug_Server::create();
  server->start_server_thread();
  auto wait_result = server->wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  class Test_Delegate : public HTTP_WebSocket_Client_Delegate {
   public:
    explicit Test_Delegate(Debug_Server *server,
                           Function_Ref<Trace_Event_Callback> on_trace_event)
        : server(server), on_trace_event(on_trace_event) {}

    void on_message_binary(HTTP_WebSocket_Client *client, const void *message,
                           std::size_t message_size) override {
      if (message_size < 8) {
        ADD_FAILURE() << "unexpected tiny message (size=" << message_size
                      << ")";
        client->stop();
        return;
      }

      Checked_Binary_Reader reader(
          reinterpret_cast<const std::uint8_t *>(message), message_size,
          []() { QLJS_ALWAYS_ASSERT(false && "unexpected end of file"); });
      std::uint64_t thread_index = reader.u64_le();

      Trace_Reader &r = this->get_trace_reader(thread_index);
      r.append_bytes(reader.cursor(), reader.remaining());

      for (const Parsed_Trace_Event &event : r.pull_new_events()) {
        bool keep_going =
            this->on_trace_event(*this->server, event, thread_index);
        if (!keep_going) {
          client->stop();
          break;
        }
      }
    }

    Trace_Reader &get_trace_reader(Trace_Flusher_Thread_Index thread_index) {
      auto [it, _inserted] = this->trace_readers.try_emplace(thread_index);
      return it->second;
    }

    Debug_Server *server;
    Function_Ref<Trace_Event_Callback> on_trace_event;
    std::map<Trace_Flusher_Thread_Index, Trace_Reader> trace_readers;
  };
  Test_Delegate delegate(server.get(), on_trace_event);
  HTTP_WebSocket_Client::connect_and_run(
      server->websocket_url("/api/trace").c_str(), &delegate);
}
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
