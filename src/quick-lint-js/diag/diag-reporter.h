// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAG_REPORTER_H
#define QUICK_LINT_JS_DIAG_DIAG_REPORTER_H

#include <quick-lint-js/diag/diagnostic-types.h>

namespace quick_lint_js {
class Diag_Reporter {
 public:
  Diag_Reporter() noexcept = default;

  Diag_Reporter(const Diag_Reporter &) noexcept = default;
  Diag_Reporter &operator=(const Diag_Reporter &) noexcept = default;

  Diag_Reporter(Diag_Reporter &&) noexcept = default;
  Diag_Reporter &operator=(Diag_Reporter &&) noexcept = default;

  virtual ~Diag_Reporter() = default;

#define QLJS_DIAG_TYPE_NAME(name) void report(name diag);
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

  virtual void report_impl(Diag_Type type, void *diag) = 0;
};

class Null_Diag_Reporter : public Diag_Reporter {
 public:
  static Null_Diag_Reporter instance;

  void report_impl(Diag_Type, void *) override {}
};
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
