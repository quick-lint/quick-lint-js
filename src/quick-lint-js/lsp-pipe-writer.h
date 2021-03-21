// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_PIPE_WRITER_H
#define QUICK_LINT_JS_LSP_PIPE_WRITER_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>

namespace quick_lint_js {
class byte_buffer;

// An lsp_pipe_writer sends server->client Language Server Protocol messages via
// a pipe or socket.
//
// lsp_pipe_writer satisfies lsp_endpoint_remote.
class lsp_pipe_writer {
 public:
  explicit lsp_pipe_writer(platform_file_ref pipe);

  void send_message(const byte_buffer&);

 private:
  template <class T>
  void write_integer(T);

  void write(string8_view);

  platform_file_ref pipe_;
};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
