// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ASPRINTF_H
#define QUICK_LINT_JS_ASPRINTF_H

#define QLJS_SAFE_FREE(ptr) \
  do {                      \
    free(ptr);              \
    ptr = nullptr;          \
  } while (false)

#include <cstdarg>

namespace quick_lint_js {
// Name:
//   asprintf, vasprintf - print to allocated string
// Description:
//   The functions asprintf() and vasprintf() are analogs of sprintf(3) and
// vsprintf(3), except that they allocate a string large enough to hold the
// output including the terminating null byte, and return a pointer to it via
// the first argument. This pointer should be passed to free(3) to release the
// allocated storage when it is no longer needed.
// More details:
//   https://linux.die.net/man/3/asprintf
int asprintf(char **strp, const char *format, ...);
int vasprintf(char **strp, const char *format, va_list argptr);
}

#endif  // QUICK_LINT_JS_ASPRINTF_H

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
