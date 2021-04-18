// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_PIPE_WRITER_H
#define QUICK_LINT_JS_LSP_PIPE_WRITER_H

#include <condition_variable>
#include <mutex>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <thread>

namespace quick_lint_js {
class byte_buffer;

// An lsp_pipe_writer sends server->client Language Server Protocol messages via
// a pipe or socket.
//
// lsp_pipe_writer satisfies lsp_endpoint_remote.
//
// lsp_pipe_writer is not thread-safe.
class lsp_pipe_writer {
 public:
  // Precondition: pipe is non-blocking
  explicit lsp_pipe_writer(platform_file_ref pipe);

  lsp_pipe_writer(const lsp_pipe_writer &) = delete;
  lsp_pipe_writer &operator=(const lsp_pipe_writer &) = delete;

  ~lsp_pipe_writer();

  // send_message is non-blocking. It might defer the work of sending to a
  // separate thread.
  void send_message(const byte_buffer &);

  // Block waiting for previous calls to send_message to fully complete. After
  // flush returns, lsp_pipe_writer won't write data to the pipe until
  // send_message is called again.
  void flush();

 private:
  template <class T>
  void write_integer(T);

  void write(string8_view);

  string8_view write_as_much_as_possible_now(string8_view);

  void start_flushing_thread_if_needed();
  void run_flushing_thread();

  platform_file_ref pipe_;

  std::thread flushing_thread_;
  std::mutex mutex_;
  std::condition_variable data_is_pending_;
  std::condition_variable data_is_flushed_;

  // Protected by mutex_:
  byte_buffer pending_;
  bool stop_ = false;
};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
