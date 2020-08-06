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

#include <ostream>
#include <quick-lint-js/spy-visitor.h>

namespace quick_lint_js {
void PrintTo(const spy_visitor::visited_variable_assignment &x,
             std::ostream *out) {
  *out << x.name;
}

void PrintTo(const spy_visitor::visited_variable_declaration &x,
             std::ostream *out) {
#define QLJS_CASE(k)     \
  case variable_kind::k: \
    *out << #k;          \
    break;
  switch (x.kind) {
    QLJS_CASE(_catch)
    QLJS_CASE(_class)
    QLJS_CASE(_const)
    QLJS_CASE(_function)
    QLJS_CASE(_import)
    QLJS_CASE(_let)
    QLJS_CASE(_parameter)
    QLJS_CASE(_var)
  }
#undef QLJS_CASE
  *out << ' ' << x.name;
}

void PrintTo(const spy_visitor::visited_variable_use &x, std::ostream *out) {
  *out << x.name;
}
}  // namespace quick_lint_js
