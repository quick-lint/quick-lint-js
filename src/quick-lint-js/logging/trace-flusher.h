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
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/thread.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class trace_writer;

union trace_flusher_backend_thread_data {
  trace_flusher_backend_thread_data() {}
  trace_flusher_backend_thread_data(const trace_flusher_backend_thread_data&) =
      delete;
  trace_flusher_backend_thread_data& operator=(
      const trace_flusher_backend_thread_data&) = delete;
  ~trace_flusher_backend_thread_data() {}

  platform_file file;
  std::uint64_t u64;
};

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
  // Must initialize thread_data.
  //
  // Called from any thread.
  virtual void trace_thread_begin(
      std::uint64_t thread_index,
      trace_flusher_backend_thread_data& thread_data) = 0;

  // For a single trace_flusher, trace_thread_end is called exactly 0 or 1
  // times. It is called 0 times if trace_thread_begin was never called, and it
  // is called 1 times if trace_thread_end was ever called.
  //
  // trace_thread_end must uninitialize thread_data.
  //
  // thread_data was previously given to trace_thread_begin.
  //
  // Called from any thread.
  virtual void trace_thread_end(
      trace_flusher_backend_thread_data& thread_data) = 0;

  // trace_thread_write_data can be called zero or more times.
  //
  // thread_data was previously given to trace_thread_begin (but not
  // trace_thread_end).
  //
  // Called from any thread.
  virtual void trace_thread_write_data(
      const std::byte* data, std::size_t size,
      trace_flusher_backend_thread_data& thread_data) = 0;
};

class trace_flusher_directory_backend final : public trace_flusher_backend {
 public:
  const std::string& trace_directory() const { return this->trace_directory_; }

  void trace_thread_begin(
      std::uint64_t thread_index,
      trace_flusher_backend_thread_data& thread_data) override;
  void trace_thread_end(
      trace_flusher_backend_thread_data& thread_data) override;
  void trace_thread_write_data(
      const std::byte* data, std::size_t size,
      trace_flusher_backend_thread_data& thread_data) override;

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
};

// A trace_flusher gives trace_writer instances and writes traces to files.
//
// See docs/TRACING.md for details on the file format.
//
// Typical use:
//
// 1. Create a trace_flusher.
// 2. Enable tracing with enable_backend. (This can be done at any time.)
// 3. On threads which want to write data, call register_current_thread.
// 4. Periodically, call trace_writer_for_current_thread()->write_[event].
// 5. After events have been written, call
//    trace_writer_for_current_thread()->commit.
//
// All public trace_flusher member functions are thread-safe. They can be called
// from any thread without synchronization.
class trace_flusher {
 public:
  /*implicit*/ trace_flusher();

  trace_flusher(const trace_flusher&) = delete;
  trace_flusher& operator=(const trace_flusher&) = delete;

  ~trace_flusher();

  void enable_backend(trace_flusher_backend*);
  void disable_backend(trace_flusher_backend*);

  // For testing only:
  void disable_all_backends();

  bool is_enabled() const;

  void register_current_thread();
  void unregister_current_thread();
  trace_writer* trace_writer_for_current_thread();

  void flush_sync();
  void flush_async();

  // start_flushing_thread can be called at most once.
  void start_flushing_thread();
  void stop_flushing_thread();

 private:
  struct registered_thread;

  void flush_sync(std::unique_lock<mutex>&);

  void enable_backend(std::unique_lock<mutex>&, trace_flusher_backend*);
  void disable_backend(std::unique_lock<mutex>&, trace_flusher_backend*);

  bool is_enabled(std::unique_lock<mutex>&) const;

  void flush_one_thread_sync(std::unique_lock<mutex>&, registered_thread&);

  void enable_thread_writer(std::unique_lock<mutex>&, registered_thread&,
                            std::size_t backend_index);

  void write_thread_header_to_backend(std::unique_lock<mutex>&,
                                      registered_thread&,
                                      trace_flusher_backend*,
                                      trace_flusher_backend_thread_data&);

  void compact_backends(std::unique_lock<mutex>&);

  template <class Func>
  void for_each_backend(std::unique_lock<mutex>&, Func&&);

  // If tracing is enabled, this points to a registered_thread::stream_writer
  // from this->registered_threads_.
  //
  // If tracing is disabled, this points to nullptr.
  static thread_local std::atomic<trace_writer*> thread_stream_writer_;

  // Protected by mutex_:
  std::vector<std::unique_ptr<registered_thread> > registered_threads_;
  std::uint64_t next_thread_index_ = 1;
  bool stop_flushing_thread_ = false;

  // backends_[i] corresponds to registered_threads_[n]->backends[i]. Therefore,
  // if a backend is deleted, we store a null pointer (tombstone) in
  // backends_[i] to avoid moving registered_threads_[n]->backends items around.
  //
  // Protected by mutex_.
  std::vector<trace_flusher_backend*> backends_;

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
