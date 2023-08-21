// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/pipe-writer.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>

namespace quick_lint_js {
class Byte_Buffer;

// An lsp_pipe_writer sends server->client Language Server Protocol messages via
// a pipe or socket.
//
// lsp_pipe_writer is not thread-safe.
class LSP_Pipe_Writer : public LSP_Endpoint_Remote, private Pipe_Writer {
 public:
  explicit LSP_Pipe_Writer(Platform_File_Ref pipe);

  void send_message(Byte_Buffer &&) override;

  using Pipe_Writer::flush;
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD && QLJS_HAVE_POLL
  using Pipe_Writer::get_event_fd;
  using Pipe_Writer::get_pipe_fd;
  using Pipe_Writer::on_pipe_write_end;
  using Pipe_Writer::on_pipe_write_ready;
  using Pipe_Writer::on_poll_event;
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
