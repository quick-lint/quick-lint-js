// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
std::ostream& operator<<(std::ostream& out, diag_type type) {
  switch (type) {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  case diag_type::name:                                                \
    return out << #name;
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
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
