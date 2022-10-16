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
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/thread.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class trace_writer;

// A trace_flusher gives trace_writer instances and writes traces to files.
//
// See docs/TRACING.md for details on the file format.
//
// Typical use:
//
// 1. Create a trace_flusher.
// 2. Enable tracing with enable_for_directory. (This can be done at any time.)
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

  // At most one directory can be enabled at a time.
  result<void, write_file_io_error> enable_for_directory(
      const std::string& trace_directory);

  // Like enable_for_directory, except:
  // * creates the given directory if it doesn't exist
  // * creates a subdirectory with a timestamped name
  // * on error, logs a message
  // * on success, logs a message
  void create_and_enable_in_child_directory(const std::string& directory);

  void disable();

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

  bool is_enabled(std::unique_lock<mutex>&) const;

  void flush_one_thread_sync(std::unique_lock<mutex>&, registered_thread&);

  void create_stream_file_and_enable_thread_writer(std::unique_lock<mutex>&,
                                                   registered_thread&);

  // If tracing is enabled, this points to a registered_thread::stream_writer
  // from this->registered_threads_.
  //
  // If tracing is disabled, this points to nullptr.
  static thread_local std::atomic<trace_writer*> thread_stream_writer_;

  // Protected by mutex_:
  std::string trace_directory_;
  std::vector<std::unique_ptr<registered_thread> > registered_threads_;
  std::uint64_t next_stream_index_ = 1;
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
