// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <iosfwd>
#include <ostream>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/unreachable.h>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class Char, class WriteFunc>
void write_json_escaped_string_impl(WriteFunc &&write_string,
                                    std::basic_string_view<Char> string) {
  for (;;) {
    auto special_character_index =
        string.find_first_of(reinterpret_cast<const Char *>(u8"\\\"\n"));
    if (special_character_index == string.npos) {
      break;
    }
    write_string(string.substr(0, special_character_index));
    Char special_character = string[special_character_index];
    switch (special_character) {
    case u8'\\':
      write_string(u8"\\\\"sv);
      break;
    case u8'"':
      write_string(u8"\\\""sv);
      break;
    case u8'\n':
      write_string(u8"\\n"sv);
      break;
    default:
      QLJS_UNREACHABLE();
    }
    string = string.substr(special_character_index + 1);
  }
  write_string(string);
}
}

template <class Char>
void write_json_escaped_string(std::ostream &output,
                               std::basic_string_view<Char> string) {
  write_json_escaped_string_impl(
      [&](const auto &s) {
        if constexpr (std::is_same_v<std::decay_t<decltype(s)>,
                                     std::string_view>) {
          output << s;
        } else {
          output << out_string8(s);
        }
      },
      string);
}

template void write_json_escaped_string<char>(std::ostream &,
                                              std::basic_string_view<char>);
#if QLJS_HAVE_CHAR8_T
template void write_json_escaped_string<char8_t>(
    std::ostream &, std::basic_string_view<char8_t>);
#endif

void write_json_escaped_string(byte_buffer &output, string8_view string) {
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
