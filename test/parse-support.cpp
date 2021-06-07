// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
template void parser::parse_and_visit_module<spy_visitor>(spy_visitor &v);
template bool parser::parse_and_visit_statement<spy_visitor>(
    spy_visitor &v, bool allow_declarations);

string8 escape_first_character_in_keyword(string8_view keyword) {
  constexpr char8 alphabet[] = u8"0123456789abcdef";
  string8 result;
  std::size_t expected_size = keyword.size() + 6 - 1;
  result.reserve(expected_size);
  result += u8"\\u{";
  result += alphabet[(keyword[0] >> 4) & 0xf];
  result += alphabet[(keyword[0] >> 0) & 0xf];
  result += u8'}';
  result += keyword.substr(1);
  QLJS_ASSERT(result.size() == expected_size);
  return result;
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
