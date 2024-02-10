// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <ostream>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/cpp.h>
#include <string>
#include <string_view>

#if defined(_WIN32)
#define QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR "\\"
#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS "\\/"
#else
#define QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR "/"
#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS "/"
#endif

#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS_SV \
  QLJS_CPP_CONCAT(QLJS_ALL_PATH_DIRECTORY_SEPARATORS, sv)

namespace quick_lint_js {
std::string parent_path(std::string&&);

std::string_view path_file_name(std::string_view);

#if defined(_WIN32)
struct Simplified_Path {
  // Null-terminated absolute path.
  wchar_t* full_path;

  // Root portion of the path. Substring of full_path. Does not contain a
  // trailing '\'.
  std::wstring_view root;

  // Relative portion of the path. Substring of full_path. Does not start with a
  // leading '\'.
  std::wstring_view relative;

  friend std::ostream& operator<<(std::ostream&, Simplified_Path);
};

// Simplify (resolve '.' and '..') and make the path absolute (based on the
// current working directories).
//
// This function should not change the path according to ::CreateFileW and other
// Win32 APIs.
//
// * Preserves at most one trailing '\'.
// * Combines redundant '\' characters.
// * Expands relative paths into absolute paths using the process's current
//   working directory and the process's per-drive working directories.
// * Does not resolve symlinks, junctions, shortcuts, etc.
// * Does not check validity of the path.
// * Does not check for existence of directories and files in the path.
// * Does not convert 8.3 names into long names.
//
// Returns pointers into memory allocated by 'allocator'.
Simplified_Path simplify_path_and_make_absolute(Monotonic_Allocator* allocator,
                                                const wchar_t* path);
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
