// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PIPE_WRITER_H
#define QUICK_LINT_JS_PIPE_WRITER_H

#if defined(__EMSCRIPTEN__)
// No pipe_writer on the web.
#else

#include <condition_variable>
#include <mutex>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <thread>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if defined(_WIN32)
#define QLJS_PIPE_WRITER_SEPARATE_THREAD 1
#else
#define QLJS_PIPE_WRITER_SEPARATE_THREAD 0
#endif

namespace quick_lint_js {
#if QLJS_PIPE_WRITER_SEPARATE_THREAD
class background_thread_pipe_writer {
 public:
  // Precondition: pipe is blocking
  explicit background_thread_pipe_writer(platform_file_ref pipe);

  background_thread_pipe_writer(const background_thread_pipe_writer &) = delete;
  background_thread_pipe_writer &operator=(
      const background_thread_pipe_writer &) = delete;

  ~background_thread_pipe_writer();

  // write is non-blocking. It will defer the work to a separate thread.
  void write(byte_buffer &&);

  // Block waiting for previous calls to send_message to fully complete. After
  // flush returns, background_thread_pipe_writer won't write data to the pipe
  // until send_message is called again.
  //
  // For testing purposes only.
  void flush();

 private:
  void write_all_now_blocking(byte_buffer_iovec &);

  void run_flushing_thread();

  platform_file_ref pipe_;

  std::thread flushing_thread_;
  std::mutex mutex_;
  std::condition_variable data_is_pending_;
  std::condition_variable data_is_flushed_;

  // Protected by mutex_:
  byte_buffer_iovec pending_;
  bool writing_ = false;
  bool stop_ = false;
};
#endif

#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
class non_blocking_pipe_writer {
 public:
  // Precondition: pipe is non-blocking
  explicit non_blocking_pipe_writer(platform_file_ref pipe);

  non_blocking_pipe_writer(const non_blocking_pipe_writer &) = delete;
  non_blocking_pipe_writer &operator=(const non_blocking_pipe_writer &) =
      delete;

  // write is non-blocking. It writes as much data as possible immediately. Call
  // on_poll_event to write remaining data.
  void write(byte_buffer &&);

  // Block waiting for previous calls to send_message to fully complete. After
  // flush returns, non_blocking_pipe_writer won't write data to the pipe until
  // send_message is called again.
  //
  // For testing purposes only.
  void flush();

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
  std::optional<posix_fd_file_ref> get_event_fd() noexcept;
#endif

#if QLJS_HAVE_KQUEUE
  void on_poll_event(const struct ::kevent &);
#endif
#if QLJS_HAVE_POLL
  void on_poll_event(const ::pollfd &);
#endif

 private:
  void write_as_much_as_possible_now_non_blocking(byte_buffer_iovec &);

  platform_file_ref pipe_;
  byte_buffer_iovec pending_;
};
#endif

#if QLJS_PIPE_WRITER_SEPARATE_THREAD
using pipe_writer = background_thread_pipe_writer;
#else
using pipe_writer = non_blocking_pipe_writer;
#endif
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
