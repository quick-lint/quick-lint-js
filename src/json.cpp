// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/unreachable.h>
#include <string>
#include <string_view>
#include <type_traits>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class Char, class Predicate>
typename std::basic_string_view<Char>::size_type find_first_if(
    std::basic_string_view<Char> string, Predicate &&predicate) {
  using string_view_type = std::basic_string_view<Char>;
  using size_type = typename string_view_type::size_type;
  for (size_type i = 0; i < string.size(); ++i) {
    if (predicate(string[i])) {
      return i;
    }
  }
  return string_view_type::npos;
}

template <class Char, class WriteFunc>
void write_json_escaped_string_impl(WriteFunc &&write_string,
                                    std::basic_string_view<Char> string) {
  for (;;) {
    auto special_character_index =
        find_first_if<Char>(string, [](Char c) -> bool {
          return (0x00 <= c && c < 0x20) || c == u8'\\' || c == u8'"';
        });
    if (special_character_index == string.npos) {
      break;
    }
    write_string(string.substr(0, special_character_index));
    Char special_character = string[special_character_index];
    switch (special_character) {
      // clang-format off
    case u8'\\': write_string(u8"\\\\"sv); break;
    case u8'"':  write_string(u8"\\\""sv); break;
    case u8'\b': write_string(u8"\\b"sv);  break;
    case u8'\f': write_string(u8"\\f"sv);  break;
    case u8'\n': write_string(u8"\\n"sv);  break;
    case u8'\r': write_string(u8"\\r"sv);  break;
    case u8'\t': write_string(u8"\\t"sv);  break;
      // clang-format on
    default: {
      QLJS_ASSERT(special_character >= u8'\x00');
      QLJS_ASSERT(special_character < u8'\x20');
      char8 buffer[6] = u8"\\u00";
      buffer[4] = narrow_cast<char8>(
          u8'0' + ((narrow_cast<int>(special_character) & 0xf0) >> 4));
      buffer[5] =
          u8"0123456789abcdef"[narrow_cast<int>(special_character) & 0x0f];
      write_string(string8_view(buffer, std::size(buffer)));
      break;
    }
    }
    string = string.substr(special_character_index + 1);
  }
  write_string(string);
}
}

void write_json_escaped_string(byte_buffer &output, string8_view string) {
  write_json_escaped_string_impl(
      [&](const string8_view &s) { output.append_copy(s); }, string);
}

void write_json_escaped_string(output_stream &output, string8_view string) {
  write_json_escaped_string_impl(
      [&](const string8_view &s) { output.append_copy(s); }, string);
}

string8 to_json_escaped_string_with_quotes(string8_view string) {
  string8 output = u8"\"";
  write_json_escaped_string_impl(
      [&](const string8_view &s) { output.append(s); }, string);
  output.push_back(u8'"');
  return output;
}
}

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
