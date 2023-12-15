// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
std::ostream& operator<<(std::ostream& out, Statement_Kind kind) {
#define QLJS_CASE(k)      \
  case Statement_Kind::k: \
    out << #k;            \
    return out;
  switch (kind) {
    QLJS_CASE(do_while_loop)
    QLJS_CASE(for_loop)
    QLJS_CASE(if_statement)
    QLJS_CASE(while_loop)
    QLJS_CASE(with_statement)
    QLJS_CASE(labelled_statement)
    QLJS_CASE(class_implements_clause)
    QLJS_CASE(class_extends_clause)
    QLJS_CASE(interface_extends_clause)
    QLJS_CASE(typeof_type)
  }
#undef QLJS_CASE
  out << "???";
  return out;
}

std::ostream& operator<<(std::ostream& out, Enum_Kind kind) {
#define QLJS_CASE(k) \
  case Enum_Kind::k: \
    out << #k;       \
    return out;
  switch (kind) {
    QLJS_CASE(declare_const_enum)
    QLJS_CASE(const_enum)
    QLJS_CASE(declare_enum)
    QLJS_CASE(normal)
  }
#undef QLJS_CASE
  out << "???";
  return out;
}

std::ostream& operator<<(std::ostream& out, Variable_Kind kind) {
  out << out_string8(to_string(kind));
  return out;
}

std::ostream& operator<<(std::ostream& out, Function_Attributes attributes) {
#define QLJS_CASE(a)           \
  case Function_Attributes::a: \
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
