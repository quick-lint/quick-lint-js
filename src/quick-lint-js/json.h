// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_JSON_H
#define QUICK_LINT_JS_JSON_H

#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <string>

namespace quick_lint_js {
class byte_buffer;

template <class Char>
void write_json_escaped_string(std::ostream &, std::basic_string_view<Char>);

extern template void write_json_escaped_string<char>(
    std::ostream &, std::basic_string_view<char>);
#if QLJS_HAVE_CHAR8_T
extern template void write_json_escaped_string<char8_t>(
    std::ostream &, std::basic_string_view<char8_t>);
#endif

void write_json_escaped_string(byte_buffer &, string8_view);

string8 to_json_escaped_string_with_quotes(string8_view);
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
