// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/language.h>

namespace quick_lint_js {
std::ostream& operator<<(std::ostream& out, variable_kind kind) {
#define QLJS_CASE(k)     \
  case variable_kind::k: \
    out << #k;           \
    return out;
  switch (kind) {
    QLJS_CASE(_catch)
    QLJS_CASE(_class)
    QLJS_CASE(_const)
    QLJS_CASE(_function)
    QLJS_CASE(_import)
    QLJS_CASE(_interface)
    QLJS_CASE(_let)
    QLJS_CASE(_parameter)
    QLJS_CASE(_var)
  }
#undef QLJS_CASE
  out << "???";
  return out;
}

std::ostream& operator<<(std::ostream& out, function_attributes attributes) {
#define QLJS_CASE(a)           \
  case function_attributes::a: \
    out << #a;                 \
    return out;
  switch (attributes) {
    QLJS_CASE(async)
    QLJS_CASE(async_generator)
    QLJS_CASE(generator)
    QLJS_CASE(normal)
  }
#undef QLJS_CASE
  out << "???";
  return out;
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
