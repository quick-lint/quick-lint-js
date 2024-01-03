// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>

namespace quick_lint_js {
#define QLJS_DIAG_TYPE_NAME(name)              \
  void Diag_Reporter::report(name diag) {      \
    this->report_impl(Diag_Type::name, &diag); \
  }
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

void Diag_Reporter::report(const Diag_List& diags) {
  diags.for_each([&](Diag_Type type, void* diag) -> void {
    this->report_impl(type, diag);
  });
}

Null_Diag_Reporter Null_Diag_Reporter::instance;
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
