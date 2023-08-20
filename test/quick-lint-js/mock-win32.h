// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
#if defined(_WIN32)
class Mock_Win32_Watch_Error_Guard {
 public:
  explicit Mock_Win32_Watch_Error_Guard(::DWORD* to_mock, ::DWORD error)
      : to_mock_(to_mock), old_error_(*this->to_mock_) {
    *this->to_mock_ = error;
  }

  Mock_Win32_Watch_Error_Guard(const Mock_Win32_Watch_Error_Guard&) = delete;
  Mock_Win32_Watch_Error_Guard& operator=(const Mock_Win32_Watch_Error_Guard&) =
      delete;

  ~Mock_Win32_Watch_Error_Guard() { *this->to_mock_ = this->old_error_; }

 private:
  ::DWORD* to_mock_;
  ::DWORD old_error_;
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
