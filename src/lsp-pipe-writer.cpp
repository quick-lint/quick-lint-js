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

#include <array>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
lsp_pipe_writer::lsp_pipe_writer(platform_file_ref pipe) : pipe_(pipe) {}

void lsp_pipe_writer::send_message(string8_view message) {
  this->write(u8"Content-Length: ");
  this->write_integer(message.size());
  this->write(u8"\r\n\r\n");
  this->write(message);
}

template <class T>
void lsp_pipe_writer::write_integer(T value) {
  std::array<char8, integer_string_length<T>> buffer;
  char8* end = quick_lint_js::write_integer(value, buffer.data());
  this->write(string8_view(buffer.data(),
                           narrow_cast<std::size_t>(end - buffer.data())));
}

void lsp_pipe_writer::write(string8_view message) {
  while (!message.empty()) {
    std::optional<int> bytes_written =
        this->pipe_.write(message.data(), narrow_cast<int>(message.size()));
    if (!bytes_written.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
    message = message.substr(narrow_cast<std::size_t>(*bytes_written));
  }
}
}
