// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/util/cpp.h>

namespace quick_lint_js {
String8_View to_string(Variable_Kind kind) {
#define QLJS_CASE(k)     \
  case Variable_Kind::k: \
    return QLJS_CPP_QUOTE_U8(k);
  switch (kind) {
    QLJS_CASE(_arrow_parameter)
    QLJS_CASE(_catch)
    QLJS_CASE(_class)
    QLJS_CASE(_const)
    QLJS_CASE(_enum)
    QLJS_CASE(_function)
    QLJS_CASE(_function_parameter)
    QLJS_CASE(_function_type_parameter)
    QLJS_CASE(_generic_parameter)
    QLJS_CASE(_import)
    QLJS_CASE(_import_alias)
    QLJS_CASE(_import_type)
    QLJS_CASE(_index_signature_parameter)
    QLJS_CASE(_infer_type)
    QLJS_CASE(_interface)
    QLJS_CASE(_let)
    QLJS_CASE(_namespace)
    QLJS_CASE(_type_alias)
    QLJS_CASE(_var)
  }
#undef QLJS_CASE
  return u8"???";
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
