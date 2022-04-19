// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_REPORTER_H
#define QUICK_LINT_JS_DIAG_REPORTER_H

#include <quick-lint-js/diagnostic-types.h>

namespace quick_lint_js {
class diag_reporter {
 public:
  diag_reporter() noexcept = default;

  diag_reporter(const diag_reporter &) noexcept = default;
  diag_reporter &operator=(const diag_reporter &) noexcept = default;

  diag_reporter(diag_reporter &&) noexcept = default;
  diag_reporter &operator=(diag_reporter &&) noexcept = default;

  virtual ~diag_reporter() = default;

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format) \
  void report(name diag) {                                        \
    this->report_impl(error_type_from_type<name>, &diag);         \
  }
  QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

  virtual void report_impl(diag_type type, void *diag) = 0;
};

class null_diag_reporter : public diag_reporter {
 public:
  static null_diag_reporter instance;

  void report_impl(diag_type, void *) override {}
};
inline null_diag_reporter null_diag_reporter::instance;
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
