// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAGNOSTIC_H
#define QUICK_LINT_JS_DIAGNOSTIC_H

#include <cstdint>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/language.h>

namespace quick_lint_js {
enum class diagnostic_severity : std::uint8_t {
  error,
  note,
  warning,
};

enum class diagnostic_arg_type : std::uint8_t {
  char8,
  identifier,
  source_code_span,
  statement_kind,
  variable_kind,
};

struct diagnostic_message_arg_info {
  std::uint8_t offset;
  diagnostic_arg_type type;
};

struct diagnostic_message_info {
  gmo_message format;
  diagnostic_severity severity;
  diagnostic_message_arg_info args[3];
};

struct diagnostic_info {
  char code[5];
  diagnostic_message_info messages[2];
};

extern const diagnostic_info all_diagnostic_infos[];
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
