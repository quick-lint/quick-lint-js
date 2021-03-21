// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
const char8* lsp_message_parser_base::find_content_begin(
    const char8* headers_begin) {
  const char8* buffer_end = &this->buffer_.data()[this->buffer_.size()];
  const char8 headers_terminator[] = {u8'\r', u8'\n', u8'\r', u8'\n'};
  const char8* found_headers_terminator =
      std::search(headers_begin, buffer_end, std::begin(headers_terminator),
                  std::end(headers_terminator));
  if (found_headers_terminator == buffer_end) {
    return nullptr;
  } else {
    return found_headers_terminator + std::size(headers_terminator);
  }
}

lsp_message_parser_base::parsed_message_headers
lsp_message_parser_base::parse_message_headers(const char8* headers_begin) {
  // TODO(strager): Use std::optional<std::size_t> instead.
  std::optional<int> content_length;
  for (const char8* c = headers_begin;;) {
    parsed_header header = parse_header(c);
    c = header.next;

    if (header_is(header.name, u8"content-length")) {
      const char8* header_value_end = &header.value.data()[header.value.size()];
      content_length.emplace();
      from_chars_result result = from_chars(
          reinterpret_cast<const char*>(header.value.data()),
          reinterpret_cast<const char*>(header_value_end), *content_length);
      if (reinterpret_cast<const char8*>(result.ptr) != header_value_end ||
          result.ec != std::errc{}) {
        QLJS_UNIMPLEMENTED();
      }
      // We found the content-type header. No need to look at other headers;
      // we'd ignore them anyway.
      break;
    } else {
      // Ignore unknown headers (including Content-Type).
    }
  }

  if (!content_length.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  return parsed_message_headers{.content_length =
                                    narrow_cast<std::size_t>(*content_length)};
}

lsp_message_parser_base::parsed_header lsp_message_parser_base::parse_header(
    const char8* c) {
  // tchar: https://tools.ietf.org/html/rfc7230#section-3.2
  // ALPHA and DIGIT: https://tools.ietf.org/html/rfc5234#appendix-B.1
  static constexpr const char8 tchar[] =
      u8"!#$%&'*+-.^_`|~"
      u8"0123456789"
      u8"abcdefghijklmnopqrstuvwxyz"
      u8"ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  // Part of HTTP-message: https://tools.ietf.org/html/rfc7230#section-3
  static constexpr const char8 crlf[] = u8"\r\n";

  // OWS: https://tools.ietf.org/html/rfc7230#section-3.2.3
  static constexpr const char8 ows[] = u8" \t";

  string8_view header_name(c, strspn(c, tchar));
  c += header_name.size();
  if (header_name.empty()) {
    QLJS_UNIMPLEMENTED();
  }

  if (*c != u8':') {
    QLJS_UNIMPLEMENTED();
  }
  c += 1;

  c += strspn(c, ows);
  const char8* header_value_begin = c;
  const char8* header_terminator_begin = strstr(c, crlf);
  QLJS_ASSERT(header_terminator_begin);
  const char8* header_value_end = header_terminator_begin;
  // TODO(strager): Trim trailing whitespace.

  return parsed_header{
      .name = header_name,
      .value = string8_view(
          header_value_begin,
          narrow_cast<std::size_t>(header_value_end - header_value_begin)),
      .next = header_terminator_begin + strlen(crlf),
  };
}

bool lsp_message_parser_base::header_is(string8_view header_name,
                                        string8_view expected_header_name) {
  static auto tolower = [](char8 c) -> char8 {
    if (u8'A' <= c && c <= u8'Z') {
      return narrow_cast<char8>(c - u8'A' + u8'a');
    } else {
      return c;
    }
  };
  return std::equal(header_name.begin(), header_name.end(),
                    expected_header_name.begin(), expected_header_name.end(),
                    [](char8 x, char8 y) {
                      QLJS_ASSERT(y == tolower(y));
                      return tolower(x) == y;
                    });
}
}

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
