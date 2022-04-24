// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_WINDOWS_ERROR_H
#define QUICK_LINT_JS_WINDOWS_ERROR_H

#include <quick-lint-js/have.h>

#if QLJS_HAVE_WINDOWS_H

#include <quick-lint-js/windows.h>
#include <string>

namespace quick_lint_js {
// Returns a human-readable string representing the given error.
//
// The given error must be a code from GetLastError(). It cannot be an HRESULT
// or a code from WSAGetLastError().
std::string windows_error_message(DWORD error);

// Uses the error code from GetLastError().
std::string windows_last_error_message();
}

#endif

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
