// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_REPORTER_H
#define QUICK_LINT_JS_ERROR_REPORTER_H

#include <quick-lint-js/error.h>

namespace quick_lint_js {
class error_reporter {
 public:
  error_reporter() noexcept = default;

  error_reporter(const error_reporter &) noexcept = default;
  error_reporter &operator=(const error_reporter &) noexcept = default;

  error_reporter(error_reporter &&) noexcept = default;
  error_reporter &operator=(error_reporter &&) noexcept = default;

  virtual ~error_reporter() = default;

#define QLJS_ERROR_TYPE(name, code, struct_body, format)   \
  void report(name error) {                                \
    this->report_impl(error_type_from_type<name>, &error); \
  }
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  virtual void report_impl(error_type type, void *error) = 0;
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

  void report_impl(error_type, void *) override {}
};
inline null_error_reporter null_error_reporter::instance;
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
