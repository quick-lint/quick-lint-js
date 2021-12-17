// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <climits>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/string-utilities.h>

namespace quick_lint_js {
// Memory management in C and auto allocating sprintf() - asprintf():
// https://insanecoding.blogspot.com/2014/06/memory-management-in-c-and-auto.html
int asprintf(char **strp, const char *fmt, ...) {
  int ret;
  va_list argptr;
  va_start(argptr, fmt);
  ret = vasprintf(strp, fmt, argptr);
  va_end(argptr);
  return ret;
}

int vasprintf(char **strp, const char *fmt, va_list argptr) {
#ifdef _MSC_VER
  int ret = -1, size = _vscprintf(fmt, argptr);
#else
  int ret = -1, size;

  // Since vsnprintf() being passed the original va_list may modify its
  // contents, a copy is needed because the function is called twice.
  va_list argptr2;
  va_copy(argptr2, argptr);

  size = vsnprintf(nullptr, 0, fmt, argptr2);
#endif
  if ((size >= 0) && (size < INT_MAX)) {
    // +1 for null
    *strp = static_cast<char *>(malloc(static_cast<std::size_t>(size + 1)));
    if (*strp) {
      // +1 for null
      ret = vsnprintf(*strp, static_cast<std::size_t>(size + 1), fmt, argptr);
      if ((ret < 0) || (ret > size)) {
        QLJS_SAFE_FREE(*strp);
        ret = -1;
      }
    }
  } else {
    *strp = nullptr;
  }
#ifndef _MSC_VER
  va_end(argptr2);
#endif
  return ret;
}
}  // namespace quick_lint_js

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
