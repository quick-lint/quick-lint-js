// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/port/have.h>

namespace quick_lint_js {
#if QLJS_HAVE_KQUEUE
class Mock_Kqueue_Directory_Open_Error_Guard {
 public:
  explicit Mock_Kqueue_Directory_Open_Error_Guard(int error)
      : old_error_(mock_kqueue_force_directory_open_error) {
    mock_kqueue_force_directory_open_error = error;
  }

  Mock_Kqueue_Directory_Open_Error_Guard(
      const Mock_Kqueue_Directory_Open_Error_Guard&) = delete;
  Mock_Kqueue_Directory_Open_Error_Guard& operator=(
      const Mock_Kqueue_Directory_Open_Error_Guard&) = delete;

  ~Mock_Kqueue_Directory_Open_Error_Guard() {
    mock_kqueue_force_directory_open_error = this->old_error_;
  }

 private:
  int old_error_;
};
#endif
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
