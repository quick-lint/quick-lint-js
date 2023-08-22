// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No pipe_writer on the web.
#else

#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>

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
class Background_Thread_Pipe_Writer {
 public:
  // Precondition: pipe is blocking
  explicit Background_Thread_Pipe_Writer(Platform_File_Ref pipe);

  Background_Thread_Pipe_Writer(const Background_Thread_Pipe_Writer &) = delete;
  Background_Thread_Pipe_Writer &operator=(
      const Background_Thread_Pipe_Writer &) = delete;

  ~Background_Thread_Pipe_Writer();

  // write is non-blocking. It will defer the work to a separate thread.
  void write(Byte_Buffer &&);

  // Block waiting for previous calls to send_message to fully complete. After
  // flush returns, Background_Thread_Pipe_Writer won't write data to the pipe
  // until send_message is called again.
  //
  // For testing purposes only.
  void flush();

 private:
  void write_all_now_blocking(Byte_Buffer_IOVec &);

  void run_flushing_thread();

  Platform_File_Ref pipe_;

  Thread flushing_thread_;
  Mutex mutex_;
  Condition_Variable data_is_pending_;
  Condition_Variable data_is_flushed_;

  // Protected by mutex_:
  Byte_Buffer_IOVec pending_;
  bool writing_ = false;
  bool stop_ = false;
};
#endif

#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
class Non_Blocking_Pipe_Writer {
 public:
  // Precondition: pipe is non-blocking
  explicit Non_Blocking_Pipe_Writer(Platform_File_Ref pipe);

  Non_Blocking_Pipe_Writer(const Non_Blocking_Pipe_Writer &) = delete;
  Non_Blocking_Pipe_Writer &operator=(const Non_Blocking_Pipe_Writer &) =
      delete;

  // write is non-blocking. It writes as much data as possible immediately. Call
  // on_poll_event to write remaining data.
  void write(Byte_Buffer &&);

  // Block waiting for previous calls to send_message to fully complete. After
  // flush returns, non_blocking_pipe_writer won't write data to the pipe until
  // send_message is called again.
  //
  // For testing purposes only.
  void flush();

  // Returns true if there is data which has not been written to the pipe yet.
  bool has_pending_data() const;

  Platform_File_Ref get_pipe_fd();

#if QLJS_HAVE_KQUEUE
  void on_poll_event(const struct ::kevent &);
#endif
#if QLJS_HAVE_POLL
  void on_poll_event(const ::pollfd &);
#endif

  void on_pipe_write_ready();
  void on_pipe_write_end();

 private:
  void write_as_much_as_possible_now_non_blocking(Byte_Buffer_IOVec &);

  Platform_File_Ref pipe_;
  Byte_Buffer_IOVec pending_;
};
#endif

#if QLJS_PIPE_WRITER_SEPARATE_THREAD
using Pipe_Writer = Background_Thread_Pipe_Writer;
#else
using Pipe_Writer = Non_Blocking_Pipe_Writer;
#endif
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
