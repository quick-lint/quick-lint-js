// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TEMPORARY_DIRECTORY_H
#define QUICK_LINT_JS_TEMPORARY_DIRECTORY_H

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <string>

namespace quick_lint_js {
// Crashes on failure.
std::string make_temporary_directory();

// Crashes on failure.
void create_directory(const std::string& path);

// Crashes on failure.
void delete_directory_recursive(const std::string& path);

// Crashes on failure.
std::string get_current_working_directory();

// Crashes on failure.
void set_current_working_directory(const char* path);
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
