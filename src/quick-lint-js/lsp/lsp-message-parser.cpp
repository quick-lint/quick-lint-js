// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/narrow-cast.h>
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
  std::optional<std::size_t> content_length;
  while (!headers.empty()) {
    parsed_header header = parse_header(headers);
    headers = header.remaining;

    if (header_is(header.name, u8"content-length"_sv)) {
      content_length.emplace();
      if (parse_integer_exact(header.value, *content_length) ==
          parse_integer_exact_error::ok) {
        // We found the content-type header. No need to look at other headers;
        // we'd ignore them anyway.
        break;
      } else {
        // Invalid header value. Ignore this header.
      }
    } else {
      // Ignore unknown headers (including Content-Type).
    }
  }

  return parsed_message_headers{.content_length = content_length};
}

lsp_message_parser_base::parsed_header lsp_message_parser_base::parse_header(
    string8_view data) {
  // tchar: https://tools.ietf.org/html/rfc7230#section-3.2
  // ALPHA and DIGIT: https://tools.ietf.org/html/rfc5234#appendix-B.1
  static constexpr string8_view tchar =
      u8"!#$%&'*+-.^_`|~"
      u8"0123456789"
      u8"abcdefghijklmnopqrstuvwxyz"
      u8"ABCDEFGHIJKLMNOPQRSTUVWXYZ"_sv;

  // Part of HTTP-message: https://tools.ietf.org/html/rfc7230#section-3
  static constexpr const char8 crlf[] = u8"\r\n";

  // OWS: https://tools.ietf.org/html/rfc7230#section-3.2.3
  static constexpr string8_view ows = u8" \t"_sv;

  auto find_line_terminator = [](string8_view haystack) -> const char8* {
    auto terminator_begin = std::search(haystack.begin(), haystack.end(), crlf,
                                        crlf + strlen(crlf));
    QLJS_ASSERT(terminator_begin != haystack.end());
    return &*terminator_begin;
  };

  auto error_skip_line = [&]() -> parsed_header {
    const char8* line_terminator = find_line_terminator(data);
    return parsed_header{
        .name = {},
        .value = {},
        .remaining = data.substr(narrow_cast<std::size_t>(
            (line_terminator + strlen(crlf)) - data.data())),
    };
  };

  std::size_t header_name_end_index = data.find_first_not_of(tchar);
  QLJS_ASSERT(header_name_end_index != data.npos);  // We expect at least \r\n.
  string8_view header_name = data.substr(0, header_name_end_index);
  data = data.substr(header_name_end_index);
  if (header_name.empty()) {
    return error_skip_line();
  }

  QLJS_ASSERT(!data.empty());
  if (data[0] != u8':') {
    return error_skip_line();
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
  string8_view header_value =
      make_string_view(header_value_begin, header_value_end);
  // TODO(strager): Trim trailing whitespace.
  data = data.substr(header_value.size() + strlen(crlf));

  return parsed_header{
      .name = header_name,
      .value = header_value,
      .remaining = data,
  };
}

bool lsp_message_parser_base::header_is(string8_view header_name,
                                        string8_view expected_header_name) {
  return ranges_equal(header_name, expected_header_name, [](char8 x, char8 y) {
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
