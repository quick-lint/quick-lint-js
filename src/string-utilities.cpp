// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/string-utilities.h>

// Define the following, and this will work with MSVC or other platforms that
// lack va_copy, but a simple copying of va_list does the trick.
#ifndef va_copy
#define QLJS_ASPRINTF_VA_COPY(dest, src) dest = src
#else
#define QLJS_ASPRINTF_VA_COPY(dest, src) va_copy(dest, src)
#endif

namespace quick_lint_js {
// Memory management in C and auto allocating sprintf() - asprintf():
// https://insanecoding.blogspot.com/2014/06/memory-management-in-c-and-auto.html
int asprintf(char **strp, const char *format, ...) {
  int ret;
  va_list argptr;
  va_start(argptr, format);
  ret = vasprintf(strp, format, argptr);
  va_end(argptr);
  return ret;
}
int vasprintf(char **strp, const char *format, va_list argptr) {
#ifdef _MSC_VER
  int ret = -1, size = _vscprintf(format, argptr);
#else
  int ret = -1, size;

  va_list argptr2;
  // Since vsnprintf() being passed the original va_list may modify its
  // contents, a copy is needed because the function is called twice.
  QLJS_ASPRINTF_VA_COPY(argptr2, argptr);

  size = vsnprintf(0, 0, format, argptr2);
#endif
  if ((size >= 0) && (size < INT_MAX)) {
    *strp = (char *)malloc(size + 1);  // +1 for null
    if (*strp) {
      ret = vsnprintf(*strp, size + 1, format, argptr);  // +1 for null
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

char **astrcat(char **destptr, const char *src) {
  std::size_t size;
  size = std::strlen(*destptr);
  size += std::strlen(src);
  destptr = (char **)std::realloc(destptr, size + 1);  // +1 for null
  std::strcat(*destptr, src);
  return destptr;
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
