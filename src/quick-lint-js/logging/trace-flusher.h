// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOGGING_TRACE_FLUSHER_H
#define QUICK_LINT_JS_LOGGING_TRACE_FLUSHER_H

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <atomic>
#include <cstdint>
#include <memory>
#include <optional>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/thread.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class trace_writer;

using trace_flusher_thread_index = std::uint64_t;

// These member functions are called with a lock held. Do not interact with
// trace_flusher in any implementations of these functions.
class trace_flusher_backend {
 public:
  explicit trace_flusher_backend() = default;

  trace_flusher_backend(trace_flusher_backend&&) = default;
  trace_flusher_backend& operator=(trace_flusher_backend&&) = default;

  virtual ~trace_flusher_backend() = default;

  // For a single trace_flusher, each call to trace_thread_begin is made with a
  // unique value for thread_index.
  //
  // Called from any thread.
  virtual void trace_thread_begin(trace_flusher_thread_index thread_index) = 0;

  // For a single trace_flusher, trace_thread_end is called exactly 0 or 1
  // times. It is called 0 times if trace_thread_begin was never called, and it
  // is called 1 times if trace_thread_end was ever called.
  //
  // thread_index was previously given to trace_thread_begin.
  //
  // Called from any thread.
  virtual void trace_thread_end(trace_flusher_thread_index thread_index) = 0;

  // trace_thread_write_data can be called zero or more times.
  //
  // thread_index was previously given to trace_thread_begin (but not
  // trace_thread_end).
  //
  // Called from any thread.
  virtual void trace_thread_write_data(trace_flusher_thread_index thread_index,
                                       const std::byte* data,
                                       std::size_t size) = 0;
};

class trace_flusher_directory_backend final : public trace_flusher_backend {
 public:
  const std::string& trace_directory() const { return this->trace_directory_; }

  void trace_thread_begin(trace_flusher_thread_index thread_index) override;
  void trace_thread_end(trace_flusher_thread_index thread_index) override;
  void trace_thread_write_data(trace_flusher_thread_index thread_index,
                               const std::byte* data,
                               std::size_t size) override;

  // Creates a 'metadata' file in the given directory.
  //
  // If the directory does not exist, or if creating the 'metadata' file fails,
  // an error is returned.
  static result<trace_flusher_directory_backend, write_file_io_error>
  init_directory(const std::string& trace_directory);

  // Creates the given directory if it doesn't exist, then creates a
  // subdirectory with a timestamped name, then calls init_directory.
  //
  // If there was an error creating the directory, logs a message and returns
  // nullopt.
  //
  // Thread-safe.
  static std::optional<trace_flusher_directory_backend> create_child_directory(
      const std::string& directory);

 private:
  explicit trace_flusher_directory_backend(const std::string& trace_directory);

  std::string trace_directory_;
  hash_map<trace_flusher_thread_index, platform_file> thread_files_;
};

// A trace_flusher gives trace_writer instances and writes traces to files.
//
// See docs/TRACING.md for details on the file format.
//
// Typical use:
//
// 1. Get a trace_flusher using trace_flusher::instance.
// 2. Enable tracing with enable_backend. (This can be done at any time.)
// 3. On threads which want to write data, call register_current_thread.
// 4. Periodically, call trace_writer_for_current_thread()->write_[event].
// 5. After events have been written, call
//    trace_writer_for_current_thread()->commit.
//
// Unless otherwise written, all public trace_flusher member functions are
// thread-safe. They can be called from any thread without synchronization.
class trace_flusher {
 private:
  // trace_flusher is a Singleton.
  /*implicit*/ trace_flusher();

 public:
  trace_flusher(const trace_flusher&) = delete;
  trace_flusher& operator=(const trace_flusher&) = delete;

  ~trace_flusher();

  static trace_flusher* instance();

  void enable_backend(trace_flusher_backend*);

  // disable_backend is synchronizing. After disable_backend returns, it is
  // guaranteed that no methods on the backend are being called on other
  // threads.
  void disable_backend(trace_flusher_backend*);

  // For testing only:
  void disable_all_backends();
  void unregister_all_threads();

  bool is_enabled() const;

  trace_flusher_thread_index register_current_thread();
  void unregister_current_thread();
  trace_writer* trace_writer_for_current_thread();

  void flush_sync();
  void flush_async();

  // start_flushing_thread can only be called once until a call to
  // stop_flushing_thread.
  //
  // start_flushing_thread is not thread-safe. Calls must be synchronized with
  // stop_flushing_thread.
  void start_flushing_thread();

  // stop_flushing_thread is idempotent; it can be called whether or not
  // start_flushing_thread has been called, and it can be called multiple times.
  //
  // stop_flushing_thread is not thread-safe. Calls must be synchronized with
  // start_flushing_thread and concurrent calls to stop_flushing_thread.
  void stop_flushing_thread();

 private:
  struct registered_thread;

  void flush_sync(std::unique_lock<mutex>&);

  void enable_backend(std::unique_lock<mutex>&, trace_flusher_backend*);
  void disable_backend(std::unique_lock<mutex>&, trace_flusher_backend*);

  bool is_enabled(std::unique_lock<mutex>&) const;

  void flush_one_thread_sync(std::unique_lock<mutex>&, registered_thread&);

  void enable_thread_writer(std::unique_lock<mutex>&, registered_thread&,
                            trace_flusher_backend*);

  void write_thread_header_to_backend(std::unique_lock<mutex>&,
                                      registered_thread&,
                                      trace_flusher_backend*);

  // If tracing is enabled, this points to a registered_thread::stream_writer
  // from this->registered_threads_.
  //
  // If tracing is disabled, this points to nullptr.
  static thread_local std::atomic<trace_writer*> thread_stream_writer_;

  // Protected by mutex_:
  std::vector<trace_flusher_backend*> backends_;
  std::vector<std::unique_ptr<registered_thread> > registered_threads_;
  trace_flusher_thread_index next_thread_index_ = 1;
  bool stop_flushing_thread_ = false;

  mutable mutex mutex_;
  condition_variable flush_requested_cond_;

  thread flushing_thread_;
};
}

#endif

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
