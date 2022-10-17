// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <algorithm>
#include <array>
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
                             std::uint64_t thread_index,
                             std::atomic<trace_writer*>* thread_writer)
      : flusher(flusher),
        thread_id(thread_id),
        thread_index(thread_index),
        thread_writer(thread_writer) {}

  ~registered_thread() {
    for (backend_state& backend : this->backends) {
      if (backend.backend) {
        backend.backend->trace_thread_end(backend.thread_data);
      }
    }
  }

  struct backend_state {
    trace_flusher_backend* backend = nullptr;

    // This is initialized by backend->trace_thread_begin.
    trace_flusher_backend_thread_data thread_data;
  };

  trace_flusher_backend_thread_data& add_backend(
      std::unique_lock<mutex>&, trace_flusher_backend* backend) {
    for (backend_state& s : this->backends) {
      if (!s.backend) {
        s.backend = backend;
        return s.thread_data;
      }
    }
    QLJS_ALWAYS_ASSERT(false && "backends array is full");
  }

  // Protected by trace_flusher::mutex_:
  std::array<backend_state, 2> backends;

  async_byte_queue stream_queue;
  trace_writer stream_writer = trace_writer(&this->stream_queue);

  trace_flusher* const flusher;
  std::uint64_t const thread_id;
  std::uint64_t const thread_index;
  // Points to the thread-local thread_stream_writer_ object.
  std::atomic<trace_writer*>* const thread_writer;
};

trace_flusher::trace_flusher() = default;

trace_flusher::~trace_flusher() {
  // HACK(strager): Each thread should have unregistered itself already.
  // However, in tests, we're lazy about unregistering. Forcefully unregister
  // all threads so our tests don't interfere with each other.
  {
    std::unique_lock<mutex> lock(this->mutex_);
    while (!this->backends_.empty()) {
      this->disable_backend(lock, this->backends_.front());
    }
  }

  if (this->flushing_thread_.joinable()) {
    this->stop_flushing_thread();
    this->flushing_thread_.join();
  }
}

void trace_flusher::enable_backend(trace_flusher_backend* backend) {
  std::unique_lock<mutex> lock(this->mutex_);
  this->enable_backend(lock, backend);
}

void trace_flusher::enable_backend(std::unique_lock<mutex>& lock,
                                   trace_flusher_backend* backend) {
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
  for (auto& t : this->registered_threads_) {
    for (registered_thread::backend_state& s : t->backends) {
      if (s.backend == backend) {
        s.backend = nullptr;
      }
    }
  }

  erase(this->backends_, backend);

  if (this->backends_.empty()) {
    for (auto& t : this->registered_threads_) {
      t->thread_writer->store(nullptr);
    }
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

  this->registered_threads_.push_back(std::make_unique<registered_thread>(
      this, get_current_thread_id(), this->next_thread_index_++,
      &this->thread_stream_writer_));
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
  // TODO(strager): Use writev if supported.
  t.stream_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        for (registered_thread::backend_state& backend : t.backends) {
          if (backend.backend) {
            backend.backend->trace_thread_write_data(data, size,
                                                     backend.thread_data);
          }
        }
      },
      [] {});
}

void trace_flusher::enable_thread_writer(std::unique_lock<mutex>& lock,
                                         registered_thread& t,
                                         trace_flusher_backend* backend) {
  trace_flusher_backend_thread_data& thread_data = t.add_backend(lock, backend);
  backend->trace_thread_begin(t.thread_index, thread_data);
  this->write_thread_header_to_backend(lock, t, backend, thread_data);

  t.thread_writer->store(&t.stream_writer);
}

void trace_flusher::write_thread_header_to_backend(
    std::unique_lock<mutex>&, registered_thread& t,
    trace_flusher_backend* backend,
    trace_flusher_backend_thread_data& thread_data) {
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
  temp_queue.commit();
  temp_queue.take_committed(
      [&](const std::byte* data, std::size_t size) {
        backend->trace_thread_write_data(data, size, thread_data);
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
