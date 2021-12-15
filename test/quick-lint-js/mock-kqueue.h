// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_MOCK_KQUEUE_H
#define QUICK_LINT_JS_MOCK_KQUEUE_H

#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/have.h>

namespace quick_lint_js {
#if QLJS_HAVE_KQUEUE
class mock_kqueue_directory_open_error_guard {
 public:
  explicit mock_kqueue_directory_open_error_guard(int error) noexcept
      : old_error_(mock_kqueue_force_directory_open_error) {
    mock_kqueue_force_directory_open_error = error;
  }

  mock_kqueue_directory_open_error_guard(
      const mock_kqueue_directory_open_error_guard&) = delete;
  mock_kqueue_directory_open_error_guard& operator=(
      const mock_kqueue_directory_open_error_guard&) = delete;

  ~mock_kqueue_directory_open_error_guard() {
    mock_kqueue_force_directory_open_error = this->old_error_;
  }

 private:
  int old_error_;
};
#endif
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
