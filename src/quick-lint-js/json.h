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

#ifndef QUICK_LINT_JS_JSON_H
#define QUICK_LINT_JS_JSON_H

#include <iosfwd>
#include <json/value.h>
#include <quick-lint-js/have.h>
#include <string>

namespace quick_lint_js {
template <class Char>
void write_json_escaped_string(std::ostream &, std::basic_string_view<Char>);

extern template void write_json_escaped_string<char>(
    std::ostream &, std::basic_string_view<char>);
#if QLJS_HAVE_CHAR8_T
extern template void write_json_escaped_string<char8_t>(
    std::ostream &, std::basic_string_view<char8_t>);
#endif

bool parse_json(string8_view json, ::Json::Value *result,
                ::Json::String *errors);
}

#endif
