// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRACE_FLUSHER_H
#define QUICK_LINT_JS_TRACE_FLUSHER_H

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <atomic>
#include <cstdint>
#include <memory>
#include <quick-lint-js/file.h>
#include <quick-lint-js/result.h>
#include <quick-lint-js/thread.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class trace_writer;

class trace_flusher {
 public:
  /*implicit*/ trace_flusher();

  trace_flusher(const trace_flusher&) = delete;
  trace_flusher& operator=(const trace_flusher&) = delete;

  ~trace_flusher();

  result<void, write_file_io_error> enable_for_directory(
      const std::string& trace_directory);
  void disable();

  // Like enable_for_directory, except:
  // * creates the given directory if it doesn't exist
  // * creates a subdirectory with a timestamped name
  // * on error, logs a message
  // * on success, logs a message
  void create_and_enable_in_child_directory(const std::string& directory);

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
