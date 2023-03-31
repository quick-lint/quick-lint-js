// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <array>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/lsp/lsp-pipe-writer.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
namespace {
constexpr string8_view header_prefix = u8"Content-Length: "_sv;
constexpr string8_view header_suffix = u8"\r\n\r\n"_sv;
constexpr std::size_t max_header_size = header_prefix.size() +
                                        integer_string_length<std::size_t> +
                                        header_suffix.size();

char8* make_header(std::size_t message_size, char8* out) {
  out = std::copy(header_prefix.begin(), header_prefix.end(), out);
  out = write_integer(message_size, out);
  out = std::copy(header_suffix.begin(), header_suffix.end(), out);
  return out;
}
}

lsp_pipe_writer::lsp_pipe_writer(platform_file_ref pipe) : pipe_writer(pipe) {}

void lsp_pipe_writer::send_message(byte_buffer&& message) {
  std::array<char8, max_header_size> header;
  char8* header_end = make_header(message.size(), header.data());
  message.prepend_copy(make_string_view(header.data(), header_end));
  this->write(std::move(message));
}
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
