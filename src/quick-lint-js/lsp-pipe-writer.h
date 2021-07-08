// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_PIPE_WRITER_H
#define QUICK_LINT_JS_LSP_PIPE_WRITER_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/pipe-writer.h>

namespace quick_lint_js {
class byte_buffer;

// An lsp_pipe_writer sends server->client Language Server Protocol messages via
// a pipe or socket.
//
// lsp_pipe_writer satisfies lsp_endpoint_remote.
//
// lsp_pipe_writer is not thread-safe.
class lsp_pipe_writer : private pipe_writer {
 public:
  explicit lsp_pipe_writer(platform_file_ref pipe);

  void send_message(byte_buffer &&);

  using pipe_writer::flush;
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD && QLJS_HAVE_POLL
  using pipe_writer::get_event_fd;
  using pipe_writer::on_poll_event;
#endif
};
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
