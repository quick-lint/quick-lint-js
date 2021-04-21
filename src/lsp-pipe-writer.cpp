// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/narrow-cast.h>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#include <unistd.h>
#endif

namespace quick_lint_js {
namespace {
constexpr string8_view header_prefix = u8"Content-Length: ";
constexpr string8_view header_suffix = u8"\r\n\r\n";
using header_buffer = std::array<char8, header_prefix.size() +
                                            integer_string_length<std::size_t> +
                                            header_suffix.size()>;

string8_view make_header(std::size_t message_size, header_buffer* out) {
  char8* begin = out->data();
  char8* end = begin;
  end = std::copy(header_prefix.begin(), header_prefix.end(), end);
  end = quick_lint_js::write_integer(message_size, end);
  end = std::copy(header_suffix.begin(), header_suffix.end(), end);
  return string8_view(begin, narrow_cast<std::size_t>(end - begin));
}
}

lsp_pipe_writer::lsp_pipe_writer(platform_file_ref pipe) : pipe_(pipe) {}

void lsp_pipe_writer::send_message(byte_buffer&& message) {
  header_buffer header;
  string8_view header_span = make_header(message.size(), &header);
  message.prepend_copy(header_span);

#if QLJS_HAVE_WRITEV
  this->write(std::move(message).to_iovec());
#else
  // TODO(strager): Avoid std::basic_string's bloat. (SSO will never kick in.)
  string8 message_string;
  message_string.resize(message.size());
  message.copy_to(message_string.data());
  this->write(message_string);
#endif
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

#if QLJS_HAVE_WRITEV
void lsp_pipe_writer::write(byte_buffer_iovec&& message) {
  while (message.iovec_count() != 0) {
    ::ssize_t bytes_written =
        ::writev(this->pipe_.get(), message.iovec(), message.iovec_count());
    if (bytes_written < 0) {
      QLJS_UNIMPLEMENTED();
    }
    QLJS_ASSERT(bytes_written != 0);
    message.remove_front(narrow_cast<std::size_t>(bytes_written));
  }
}
#endif
}

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
