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
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/vector-erase.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/version.h>
#include <string>
#include <vector>

namespace quick_lint_js {
thread_local std::atomic<trace_writer*> trace_flusher::thread_stream_writer_;

struct trace_flusher::registered_thread {
  explicit registered_thread(trace_flusher* flusher, std::uint64_t thread_id,
                             trace_flusher_thread_index thread_index,
                             std::atomic<trace_writer*>* thread_writer)
      : flusher(flusher),
        thread_id(thread_id),
        thread_index(thread_index),
        thread_writer(thread_writer) {}

  async_byte_queue stream_queue;
  trace_writer stream_writer = trace_writer(&this->stream_queue);

  trace_flusher* const flusher;
  std::uint64_t const thread_id;
  trace_flusher_thread_index const thread_index;
  // Points to the thread-local thread_stream_writer_ object.
  std::atomic<trace_writer*>* const thread_writer;
};

trace_flusher::trace_flusher() = default;

trace_flusher::~trace_flusher() {
  QLJS_ASSERT(this->backends_.empty());

  this->stop_flushing_thread();
}

trace_flusher* trace_flusher::instance() {
  static trace_flusher tracer;
  return &tracer;
}

void trace_flusher::enable_backend(trace_flusher_backend* backend) {
  std::unique_lock<mutex> lock(this->mutex_);
  this->enable_backend(lock, backend);
}

void trace_flusher::enable_backend(std::unique_lock<mutex>& lock,
                                   trace_flusher_backend* backend) {
  QLJS_ASSERT(backend);
  // A single backend cannot be enabled twice.
  QLJS_ASSERT(std::find(this->backends_.begin(), this->backends_.end(),
                        backend) == this->backends_.end());

  this->backends_.push_back(backend);

  for (auto& t : this->registered_threads_) {
    this->enable_thread_writer(lock, *t, backend);
  }
}

void trace_flusher::disable_backend(trace_flusher_backend* backend) {
  std::unique_lock<mutex> lock(this->mutex_);
  this->disable_backend(lock, backend);
}

void trace_flusher::disable_backend(std::unique_lock<mutex>&,
                                    trace_flusher_backend* backend) {
  QLJS_ASSERT(backend);
  auto backend_it =
      std::find(this->backends_.begin(), this->backends_.end(), backend);
  QLJS_ASSERT(backend_it != this->backends_.end());

  for (auto& t : this->registered_threads_) {
    backend->trace_thread_end(t->thread_index);
  }

  this->backends_.erase(backend_it);

  if (this->backends_.empty()) {
    for (auto& t : this->registered_threads_) {
      t->thread_writer->store(nullptr);
    }
  }
}

void trace_flusher::disable_all_backends() {
  std::unique_lock<mutex> lock(this->mutex_);
  while (!this->backends_.empty()) {
    this->disable_backend(lock, this->backends_.back());
  }
}

bool trace_flusher::is_enabled() const {
  std::unique_lock<mutex> lock(this->mutex_);
  return this->is_enabled(lock);
}

bool trace_flusher::is_enabled(std::unique_lock<mutex>&) const {
  return !this->backends_.empty();
}

trace_flusher_thread_index trace_flusher::register_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  QLJS_ASSERT(this->thread_stream_writer_.load() == nullptr);

  this->registered_threads_.push_back(std::make_unique<registered_thread>(
      this, get_current_thread_id(), this->next_thread_index_++,
      &this->thread_stream_writer_));
  registered_thread* t = this->registered_threads_.back().get();

  for (trace_flusher_backend* backend : this->backends_) {
    this->enable_thread_writer(lock, *t, backend);
  }

  return t->thread_index;
}

void trace_flusher::unregister_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  auto registered_thread_it = find_unique_existing_if(
      this->registered_threads_,
      [](auto& t) { return t->thread_writer == &thread_stream_writer_; });
  registered_thread& t = **registered_thread_it;
  if (this->is_enabled(lock)) {
    this->flush_one_thread_sync(lock, t);
  }
  for (trace_flusher_backend* backend : this->backends_) {
    backend->trace_thread_end(t.thread_index);
  }
  this->registered_threads_.erase(registered_thread_it);
  this->thread_stream_writer_.store(nullptr);
}

void trace_flusher::unregister_all_threads() {
  std::unique_lock<mutex> lock(this->mutex_);
  for (auto& t : this->registered_threads_) {
    t->thread_writer->store(nullptr);
    if (this->is_enabled(lock)) {
      this->flush_one_thread_sync(lock, *t);
    }
    for (trace_flusher_backend* backend : this->backends_) {
      backend->trace_thread_end(t->thread_index);
    }
  }
  this->registered_threads_.clear();
}

trace_writer* trace_flusher::trace_writer_for_current_thread() {
  return this->thread_stream_writer_.load();
}

void trace_flusher::flush_sync() {
  std::unique_lock<mutex> lock(this->mutex_);
  this->flush_sync(lock);
}

void trace_flusher::flush_sync(std::unique_lock<mutex>& lock) {
  for (auto& t : this->registered_threads_) {
    this->flush_one_thread_sync(lock, *t);
  }
}

void trace_flusher::flush_async() { this->flush_requested_cond_.notify_one(); }

void trace_flusher::start_flushing_thread() {
  QLJS_ASSERT(!this->flushing_thread_.joinable());
  this->flushing_thread_.start([this]() {
    std::unique_lock<mutex> lock(this->mutex_);
    while (!this->stop_flushing_thread_) {
      this->flush_sync(lock);
      this->flush_requested_cond_.wait(lock);
    }
  });
}

void trace_flusher::stop_flushing_thread() {
  if (this->flushing_thread_.joinable()) {
    {
      std::unique_lock<mutex> lock(this->mutex_);
      this->stop_flushing_thread_ = true;
      this->flush_requested_cond_.notify_all();
    }
    this->flushing_thread_.join();
    {
      std::unique_lock<mutex> lock(this->mutex_);
      this->stop_flushing_thread_ = false;
    }
  }
}

void trace_flusher::flush_one_thread_sync(std::unique_lock<mutex>&,
                                          registered_thread& t) {
  // TODO(strager): Use writev if supported.
  t.stream_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        for (trace_flusher_backend* backend : this->backends_) {
          backend->trace_thread_write_data(t.thread_index, data, size);
        }
      },
      [] {});
}

void trace_flusher::enable_thread_writer(std::unique_lock<mutex>& lock,
                                         registered_thread& t,
                                         trace_flusher_backend* backend) {
  backend->trace_thread_begin(t.thread_index);
  this->write_thread_header_to_backend(lock, t, backend);

  t.thread_writer->store(&t.stream_writer);
}

void trace_flusher::write_thread_header_to_backend(
    std::unique_lock<mutex>&, registered_thread& t,
    trace_flusher_backend* backend) {
  // NOTE(strager): We use a temporary async_byte_queue instead of reusing
  // t.stream_queue so we can write to *just* this backend and not involve any
  // other backends.
  async_byte_queue temp_queue;
  trace_writer writer(&temp_queue);
  writer.write_header(trace_context{
      .thread_id = t.thread_id,
  });
  writer.write_event_init(trace_event_init{
      .timestamp = 0,  // TODO(strager)
      .version = QUICK_LINT_JS_VERSION_STRING_U8,
  });
  writer.write_event_process_id(trace_event_process_id{
      .timestamp = 0,  // TODO(strager)
      .process_id = get_current_process_id(),
  });
  temp_queue.commit();
  temp_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        backend->trace_thread_write_data(t.thread_index, data, size);
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
