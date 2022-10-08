// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/diagnostic-formatter.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
string8_view headlinese_enum_kind(enum_kind ek) noexcept {
  switch (ek) {
  case enum_kind::const_enum:
    return u8"const enum"sv;
  case enum_kind::declare_const_enum:
    return u8"declare const enum"sv;
  case enum_kind::declare_enum:
    return u8"declare enum"sv;
  case enum_kind::normal:
    return u8"enum"sv;
  }
  QLJS_UNREACHABLE();
}

translatable_message headlinese_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATABLE("'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATABLE("'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATABLE("'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATABLE("'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATABLE("'with' statement");
  case statement_kind::labelled_statement:
    return QLJS_TRANSLATABLE("labelled statement");
  }
  QLJS_UNREACHABLE();
}

translatable_message singular_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATABLE("a 'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATABLE("a 'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATABLE("an 'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATABLE("a 'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATABLE("a 'with' statement");
  case statement_kind::labelled_statement:
    return QLJS_TRANSLATABLE("a labelled statement");
  }
  QLJS_UNREACHABLE();
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
