// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>

namespace quick_lint_js {
string8_view translated_headlinese_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATE("'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATE("'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATE("'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATE("'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATE("'with' statement");
  }
  QLJS_UNREACHABLE();
}

string8_view translated_singular_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATE("a 'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATE("a 'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATE("an 'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATE("a 'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATE("a 'with' statement");
  }
  QLJS_UNREACHABLE();
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
