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
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/version.h>
#include <string>
#include <vector>

namespace quick_lint_js {
thread_local std::atomic<trace_writer*> trace_flusher::thread_stream_writer_;

struct trace_flusher::registered_thread {
  explicit registered_thread(trace_flusher* flusher,
                             std::atomic<trace_writer*>* thread_writer)
      : flusher(flusher), thread_writer(thread_writer) {}

  ~registered_thread() {
    if (this->backend) {
      this->backend->trace_thread_end(this->backend_thread_data);
    }
  }

  // Protected by trace_flusher::mutex_:
  trace_flusher_backend* backend = nullptr;
  trace_flusher_backend_thread_data backend_thread_data;

  async_byte_queue stream_queue;
  trace_writer stream_writer = trace_writer(&this->stream_queue);
  trace_flusher* flusher;

  // Points to the thread-local thread_stream_writer_ object.
  std::atomic<trace_writer*>* const thread_writer;
};

trace_flusher::trace_flusher() = default;

trace_flusher::~trace_flusher() {
  // HACK(strager): Each thread should have unregistered itself already.
  // However, in tests, we're lazy about unregistering. Forcefully unregister
  // all threads so our tests don't interfere with each other.
  this->disable();

  if (this->flushing_thread_.joinable()) {
    this->stop_flushing_thread();
    this->flushing_thread_.join();
  }
}

void trace_flusher::disable() {
  std::unique_lock<mutex> lock(this->mutex_);
  for (auto& t : this->registered_threads_) {
    t->thread_writer->store(nullptr);
    t->backend = nullptr;
  }
  for (trace_flusher_backend* backend : this->backends_) {
    // FIXME(strager): We should call trace_disabled here, but our tests are
    // sloppy and have already destructed the backend by now.
    // this->backend_->trace_disabled();
    static_cast<void>(backend);
  }
  this->backends_.clear();
}

void trace_flusher::enable_backend(trace_flusher_backend* backend) {
  std::unique_lock<mutex> lock(this->mutex_);
  this->enable_backend(lock, backend);
}

void trace_flusher::enable_backend(std::unique_lock<mutex>& lock,
                                   trace_flusher_backend* backend) {
  QLJS_ASSERT(this->backends_.empty());

  this->backends_.push_back(backend);
  backend->trace_enabled();

  this->next_stream_index_ = 1;
  for (auto& t : this->registered_threads_) {
    if (t->backend) {
      t->backend->trace_thread_end(t->backend_thread_data);
    }
    this->enable_thread_writer(lock, *t, backend);
  }
}

bool trace_flusher::is_enabled() const {
  std::unique_lock<mutex> lock(this->mutex_);
  return this->is_enabled(lock);
}

bool trace_flusher::is_enabled(std::unique_lock<mutex>&) const {
  return !this->backends_.empty();
}

void trace_flusher::register_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  QLJS_ASSERT(this->thread_stream_writer_.load() == nullptr);

  this->registered_threads_.push_back(
      std::make_unique<registered_thread>(this, &this->thread_stream_writer_));
  registered_thread* t = this->registered_threads_.back().get();

  if (!this->backends_.empty()) {
    this->enable_thread_writer(lock, *t, this->backends_[0]);
  }
}

void trace_flusher::unregister_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  auto registered_thread_it = find_unique_existing_if(
      this->registered_threads_,
      [](auto& t) { return t->thread_writer == &thread_stream_writer_; });
  if (this->is_enabled(lock)) {
    this->flush_one_thread_sync(lock, **registered_thread_it);
  }
  this->registered_threads_.erase(registered_thread_it);
  this->thread_stream_writer_.store(nullptr);
}

trace_writer* trace_flusher::trace_writer_for_current_thread() {
  return this->thread_stream_writer_.load();
}

void trace_flusher::flush_sync() {
  std::unique_lock<mutex> lock(this->mutex_);
  for (auto& t : this->registered_threads_) {
    this->flush_one_thread_sync(lock, *t);
  }
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
  std::unique_lock<mutex> lock(this->mutex_);
  this->stop_flushing_thread_ = true;
  this->flush_requested_cond_.notify_all();
}

void trace_flusher::flush_one_thread_sync(std::unique_lock<mutex>&,
                                          registered_thread& t) {
  if (!t.backend) {
    // flush was called, but tracing was not enabled for this thread.
    // TODO(strager): We shouldn't buffer if logging is enabled but the file
    // failed to open.
    return;
  }
  // TODO(strager): Use writev if supported.
  t.stream_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        t.backend->trace_thread_write_data(data, size, t.backend_thread_data);
      },
      [] {});
}

void trace_flusher::enable_thread_writer(std::unique_lock<mutex>& lock,
                                         registered_thread& t,
                                         trace_flusher_backend* backend) {
  std::uint64_t stream_index = this->next_stream_index_++;
  backend->trace_thread_begin(stream_index, t.backend_thread_data);
  t.backend = backend;

  t.stream_writer.write_header(trace_context{
      .thread_id = get_current_thread_id(),
  });
  t.stream_writer.write_event_init(trace_event_init{
      .timestamp = 0,  // TODO(strager)
      .version = QUICK_LINT_JS_VERSION_STRING_U8,
  });
  t.stream_queue.commit();
  this->flush_one_thread_sync(lock, t);

  t.thread_writer->store(&t.stream_writer);
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
