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
#include <quick-lint-js/async-byte-queue.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/log.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/result.h>
#include <quick-lint-js/thread.h>
#include <quick-lint-js/trace-flusher.h>
#include <quick-lint-js/trace-metadata.h>
#include <quick-lint-js/trace-writer.h>
#include <quick-lint-js/version.h>
#include <string>
#include <vector>

namespace quick_lint_js {
thread_local std::atomic<trace_writer*> trace_flusher::thread_stream_writer_;

struct trace_flusher::registered_thread {
  explicit registered_thread(std::atomic<trace_writer*>* thread_writer)
      : thread_writer(thread_writer) {}

  // Protected by mutex_:
  platform_file file;

  async_byte_queue stream_queue;
  trace_writer stream_writer = trace_writer(&this->stream_queue);

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

result<void, write_file_io_error> trace_flusher::enable_for_directory(
    const std::string& trace_directory) {
  std::unique_lock<mutex> lock(this->mutex_);

  this->trace_directory_ = trace_directory;
  auto write_result =
      write_file(this->trace_directory_ + "/metadata", trace_metadata);
  if (!write_result.ok()) {
    return write_result.propagate();
  }

  this->next_stream_index_ = 1;
  for (auto& t : this->registered_threads_) {
    if (t->file.valid()) {
      t->file.close();
    }
    this->create_stream_file_and_enable_thread_writer(lock, *t);
  }

  return {};
}

void trace_flusher::disable() {
  std::unique_lock<mutex> lock(this->mutex_);
  for (auto& t : this->registered_threads_) {
    t->thread_writer->store(nullptr);
  }
}

bool trace_flusher::is_enabled() const {
  std::unique_lock<mutex> lock(this->mutex_);
  return this->is_enabled(lock);
}

bool trace_flusher::is_enabled(std::unique_lock<mutex>&) const {
  return !this->trace_directory_.empty();
}

void trace_flusher::register_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  QLJS_ASSERT(this->thread_stream_writer_.load() == nullptr);

  this->registered_threads_.push_back(
      std::make_unique<registered_thread>(&this->thread_stream_writer_));
  registered_thread* t = this->registered_threads_.back().get();

  if (this->is_enabled(lock)) {
    this->create_stream_file_and_enable_thread_writer(lock, *t);
  }
}

void trace_flusher::unregister_current_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  auto registered_thread_it = std::find_if(
      this->registered_threads_.begin(), this->registered_threads_.end(),
      [](auto& t) { return t->thread_writer == &thread_stream_writer_; });
  QLJS_ASSERT(registered_thread_it != this->registered_threads_.end());
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
  if (!t.file.valid()) {
    // No file is open. Buffer the data in memory.
    // TODO(strager): We shouldn't buffer if logging is enabled but the file
    // failed to open.
    return;
  }
  // TODO(strager): Use writev if supported.
  t.stream_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        auto write_result = t.file.write_full(data, size);
        if (!write_result.ok()) {
          QLJS_DEBUG_LOG("warning: failed to append to trace stream file: %s\n",
                         write_result.error_to_string().c_str());
          // TODO(strager): Disable further writes to prevent file corruption
          // and noisy logs.
        }
      },
      [] {});
}

void trace_flusher::create_stream_file_and_enable_thread_writer(
    std::unique_lock<mutex>& lock, registered_thread& t) {
  QLJS_ASSERT(!t.file.valid());

  std::uint64_t stream_index = this->next_stream_index_++;
  std::string stream_path =
      this->trace_directory_ + "/thread" + std::to_string(stream_index);
  auto file = open_file_for_writing(stream_path.c_str());
  if (!file.ok()) {
    QLJS_DEBUG_LOG("warning: failed to create trace stream file %s: %s\n",
                   stream_path.c_str(), file.error_to_string().c_str());
    return;  // Give up. Do not enable the thread's writer.
  }
  t.file = std::move(*file);

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
