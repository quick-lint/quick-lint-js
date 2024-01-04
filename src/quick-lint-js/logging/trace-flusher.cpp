// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <algorithm>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread-name.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/vector-erase.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/version.h>
#include <string>
#include <vector>

namespace quick_lint_js {
thread_local std::atomic<Trace_Writer*> Trace_Flusher::thread_stream_writer_;

struct Trace_Flusher::Registered_Thread {
  explicit Registered_Thread(Trace_Flusher* flusher, std::uint64_t thread_id,
                             Trace_Flusher_Thread_Index thread_index,
                             std::atomic<Trace_Writer*>* thread_writer)
      : flusher(flusher),
        thread_id(thread_id),
        thread_index(thread_index),
        thread_writer(thread_writer) {}

  Async_Byte_Queue stream_queue;
  Trace_Writer stream_writer = Trace_Writer(&this->stream_queue);

  Trace_Flusher* const flusher;
  std::uint64_t const thread_id;
  Trace_Flusher_Thread_Index const thread_index;
  // Points to the thread-local thread_stream_writer_ object.
  std::atomic<Trace_Writer*>* const thread_writer;
};

Trace_Flusher::Trace_Flusher() = default;

Trace_Flusher::~Trace_Flusher() {
  Shared_State* state = this->state_.get_without_lock_unsafe();
  QLJS_ASSERT(state->backends.empty());

  this->stop_flushing_thread();
}

Trace_Flusher* Trace_Flusher::instance() {
  static Trace_Flusher tracer;
  return &tracer;
}

void Trace_Flusher::enable_backend(Trace_Flusher_Backend* backend) {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->enable_backend(state, backend);
}

void Trace_Flusher::enable_backend(Lock_Ptr<Shared_State>& state,
                                   Trace_Flusher_Backend* backend) {
  QLJS_ASSERT(backend);
  // A single backend cannot be enabled twice.
  QLJS_ASSERT(std::find(state->backends.begin(), state->backends.end(),
                        backend) == state->backends.end());

  state->backends.push_back(backend);

  for (auto& t : state->registered_threads) {
    this->enable_thread_writer(state, *t, backend);
  }
}

void Trace_Flusher::disable_backend(Trace_Flusher_Backend* backend) {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->disable_backend(state, backend);
}

void Trace_Flusher::disable_backend(Lock_Ptr<Shared_State>& state,
                                    Trace_Flusher_Backend* backend) {
  QLJS_ASSERT(backend);
  auto backend_it =
      std::find(state->backends.begin(), state->backends.end(), backend);
  QLJS_ASSERT(backend_it != state->backends.end());

  for (auto& t : state->registered_threads) {
    backend->trace_thread_end(t->thread_index);
  }

  state->backends.erase(backend_it);

  if (state->backends.empty()) {
    for (auto& t : state->registered_threads) {
      t->thread_writer->store(nullptr);
    }
  }
}

void Trace_Flusher::disable_all_backends() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  while (!state->backends.empty()) {
    this->disable_backend(state, state->backends.back());
  }
}

bool Trace_Flusher::is_enabled() { return this->state_.lock()->is_enabled(); }

bool Trace_Flusher::Shared_State::is_enabled() {
  return !this->backends.empty();
}

Trace_Flusher_Thread_Index Trace_Flusher::register_current_thread() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  QLJS_ASSERT(this->thread_stream_writer_.load() == nullptr);

  state->registered_threads.push_back(std::make_unique<Registered_Thread>(
      this, get_current_thread_id(), state->next_thread_index++,
      &this->thread_stream_writer_));
  Registered_Thread* t = state->registered_threads.back().get();

  for (Trace_Flusher_Backend* backend : state->backends) {
    this->enable_thread_writer(state, *t, backend);
  }

  return t->thread_index;
}

void Trace_Flusher::unregister_current_thread() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  auto registered_thread_it = find_unique_existing_if(
      state->registered_threads,
      [](auto& t) { return t->thread_writer == &thread_stream_writer_; });
  Registered_Thread& t = **registered_thread_it;
  if (state->is_enabled()) {
    this->flush_one_thread_sync(state, t);
  }
  for (Trace_Flusher_Backend* backend : state->backends) {
    backend->trace_thread_end(t.thread_index);
  }
  state->registered_threads.erase(registered_thread_it);
  this->thread_stream_writer_.store(nullptr);
}

void Trace_Flusher::unregister_all_threads() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  for (auto& t : state->registered_threads) {
    t->thread_writer->store(nullptr);
    if (state->is_enabled()) {
      this->flush_one_thread_sync(state, *t);
    }
    for (Trace_Flusher_Backend* backend : state->backends) {
      backend->trace_thread_end(t->thread_index);
    }
  }
  state->registered_threads.clear();
}

Trace_Writer* Trace_Flusher::trace_writer_for_current_thread() {
  return this->thread_stream_writer_.load();
}

void Trace_Flusher::flush_sync() {
  Lock_Ptr<Shared_State> state = this->state_.lock();
  this->flush_sync(state);
}

void Trace_Flusher::flush_sync(Lock_Ptr<Shared_State>& state) {
  for (auto& t : state->registered_threads) {
    this->flush_one_thread_sync(state, *t);
  }
}

void Trace_Flusher::flush_async() { this->flush_requested_cond_.notify_one(); }

void Trace_Flusher::start_flushing_thread() {
  QLJS_ASSERT(!this->flushing_thread_.joinable());
  this->flushing_thread_.start([this]() {
    set_current_thread_name(u8"quick-lint-js Trace_Flusher",
                            /*short_name=*/u8"qljs-tracing");
    Lock_Ptr<Shared_State> state = this->state_.lock();
    while (!state->stop_flushing_thread) {
      this->flush_sync(state);
      this->flush_requested_cond_.wait(state);
    }
  });
}

void Trace_Flusher::stop_flushing_thread() {
  if (this->flushing_thread_.joinable()) {
    {
      Lock_Ptr<Shared_State> state = this->state_.lock();
      state->stop_flushing_thread = true;
      this->flush_requested_cond_.notify_all();
    }
    this->flushing_thread_.join();
    {
      Lock_Ptr<Shared_State> state = this->state_.lock();
      state->stop_flushing_thread = false;
    }
  }
}

void Trace_Flusher::flush_one_thread_sync(Lock_Ptr<Shared_State>& state,
                                          Registered_Thread& t) {
  // TODO(strager): Use writev if supported.
  t.stream_queue.take_committed(
      [&](Span<const std::byte> data) {
        for (Trace_Flusher_Backend* backend : state->backends) {
          backend->trace_thread_write_data(t.thread_index, data);
        }
      },
      [] {});
}

void Trace_Flusher::enable_thread_writer(Lock_Ptr<Shared_State>& state,
                                         Registered_Thread& t,
                                         Trace_Flusher_Backend* backend) {
  backend->trace_thread_begin(t.thread_index);
  this->write_thread_header_to_backend(state, t, backend);

  t.thread_writer->store(&t.stream_writer);
}

void Trace_Flusher::write_thread_header_to_backend(
    Lock_Ptr<Shared_State>&, Registered_Thread& t,
    Trace_Flusher_Backend* backend) {
  // NOTE(strager): We use a temporary async_byte_queue instead of reusing
  // t.stream_queue so we can write to *just* this backend and not involve any
  // other backends.
  Async_Byte_Queue temp_queue;
  Trace_Writer writer(&temp_queue);
  writer.write_header(Trace_Context{
      .thread_id = t.thread_id,
  });
  writer.write_event(Trace_Event_Header{.timestamp = 0},  // TODO(strager)
                     Trace_Event_Init{
                         .version = QUICK_LINT_JS_VERSION_STRING_U8_SV,
                     });
  writer.write_event(Trace_Event_Header{.timestamp = 0},  // TODO(strager)
                     Trace_Event_Process_ID{
                         .process_id = get_current_process_id(),
                     });
  temp_queue.commit();
  temp_queue.take_committed(
      [&](Span<const std::byte> data) {
        backend->trace_thread_write_data(t.thread_index, data);
      },
      [] {});
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
