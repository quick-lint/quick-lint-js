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

// QLJS_DIAG_TYPE should have the following signature:
//
// #define QLJS_DIAG_TYPE(error_name, error_code, severity, struct_body,
// format) ...
//
// * error_name: identifier
// * error_code: string literal
// * severity: Diagnostic_Severity value
// * struct_body: class member list, wrapped in { }
// * format: member function calls
//
// A class named *error_name* is created in the quick_lint_js namespace.
// *struct_body* is the body of the class.
//
// *format* should look like the following:
//
//    MESSAGE(QLJS_TRANSLATABLE("format string"), source_location)
//
// Within *format*:
//
// * MESSAGE's first argument must be QLJS_TRANSLATABLE(...)
// * MESSAGE's second argument must be a member variable of the *error_name*
//   class (i.e. listed in *struct_body*)
// * MESSAGE's second argument must have type *Source_Code_Span*
//
// When removing a diagnostic from this list, add an entry to
// QLJS_X_RESERVED_DIAG_TYPES.
#define QLJS_X_DIAG_TYPES     \
  QLJS_X_DIAG_TYPES_GENERATED \
  /* END */

// QLJS_X_RESERVED_DIAG_TYPES lists reserved error codes. These codes were used
// in the past but no longer mean anything.
//
// When removing a diagnostic from QLJS_X_DIAG_TYPES, add an entry to
// QLJS_X_RESERVED_DIAG_TYPES.
//
// QLJS_DIAG_TYPE should have the following signature:
//
// #define QLJS_DIAG_TYPE(error_name, error_code, severity, struct_body,
// format) ...
//
// * error_name: (unset)
// * error_code: string literal
// * severity: (unset)
// * struct_body: (unset)
// * format: (unset)
#define QLJS_X_RESERVED_DIAG_TYPES \
  QLJS_DIAG_TYPE(, "E0242", , , )  \
  QLJS_DIAG_TYPE(, "E0271", , , )  \
  QLJS_DIAG_TYPE(, "E0279", , , )  \
  QLJS_DIAG_TYPE(, "E0707", , , )  \
  /* END */

namespace quick_lint_js {
// NOTE(strager): Enum members in Diag_Type are Upper_Snake_Case (matching the
// type names) instead of the usual lower_snake_case.
enum class Diag_Type {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) name,
  QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
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
