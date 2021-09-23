// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

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
lsp_message_parser_base::parse_message_headers(string8_view headers) {
  const char8* buffer_end = headers.data() + headers.size();
  std::optional<std::size_t> content_length;
  for (const char8* c = headers.data();;) {
    parsed_header header =
        parse_header(string8_view(c, narrow_cast<std::size_t>(buffer_end - c)));
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
  return parsed_message_headers{.content_length = *content_length};
}

lsp_message_parser_base::parsed_header lsp_message_parser_base::parse_header(
    string8_view data) {
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

  std::size_t header_name_end_index = data.find_first_not_of(tchar);
  QLJS_ASSERT(header_name_end_index != data.npos);  // We expect at least \r\n.
  string8_view header_name = data.substr(0, header_name_end_index);
  data = data.substr(header_name_end_index);
  if (header_name.empty()) {
    QLJS_UNIMPLEMENTED();
  }

  QLJS_ASSERT(!data.empty());
  if (data[0] != u8':') {
    QLJS_UNIMPLEMENTED();
  }
  data = data.substr(1);

  std::size_t header_value_index = data.find_first_not_of(ows);
  QLJS_ASSERT(header_value_index != data.npos);  // We expect at least \r\n.
  data = data.substr(header_value_index);
  const char8* header_value_begin = data.data();
  auto header_terminator_begin =
      std::search(data.begin(), data.end(), crlf, crlf + strlen(crlf));
  QLJS_ASSERT(header_terminator_begin != data.end());
  const char8* header_value_end = &*header_terminator_begin;
  // TODO(strager): Trim trailing whitespace.

  return parsed_header{
      .name = header_name,
      .value = string8_view(
          header_value_begin,
          narrow_cast<std::size_t>(header_value_end - header_value_begin)),
      .next = header_value_end + strlen(crlf),
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
