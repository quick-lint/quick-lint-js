// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAGNOSTIC_TYPES_H
#define QUICK_LINT_JS_DIAG_DIAGNOSTIC_TYPES_H

#include <iosfwd>
#include <quick-lint-js/diag/diagnostic-metadata-generated.h>
#include <quick-lint-js/diag/diagnostic-types-2.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
// NOTE(strager): Enum members in Diag_Type are Upper_Snake_Case (matching the
// type names) instead of the usual lower_snake_case.
enum class Diag_Type {
#define QLJS_DIAG_TYPE_NAME(name) name,
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
};

std::ostream& operator<<(std::ostream&, Diag_Type);
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
