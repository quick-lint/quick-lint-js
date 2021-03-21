// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
template void parser::parse_and_visit_module<spy_visitor>(spy_visitor &v);
template bool parser::parse_and_visit_statement<spy_visitor>(spy_visitor &v);
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
