// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_MOCK_WIN32_H
#define QUICK_LINT_JS_MOCK_WIN32_H

#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/have.h>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/windows.h>
//#include <Windows.h>
#endif

namespace quick_lint_js {
#if defined(_WIN32)
class mock_win32_watch_error_guard {
 public:
  explicit mock_win32_watch_error_guard(::DWORD* to_mock,
                                        ::DWORD error) noexcept
      : to_mock_(to_mock), old_error_(*this->to_mock_) {
    *this->to_mock_ = error;
  }

  mock_win32_watch_error_guard(const mock_win32_watch_error_guard&) = delete;
  mock_win32_watch_error_guard& operator=(const mock_win32_watch_error_guard&) =
      delete;

  ~mock_win32_watch_error_guard() { *this->to_mock_ = this->old_error_; }

 private:
  ::DWORD* to_mock_;
  ::DWORD old_error_;
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
