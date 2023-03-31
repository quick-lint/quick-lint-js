// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VERSION_H
#define QUICK_LINT_JS_VERSION_H

#include <quick-lint-js/util/cpp.h>

// QUICK_LINT_JS_VERSION is normally defined by the build system (CMake).
#if !defined(QUICK_LINT_JS_VERSION)
#define QUICK_LINT_JS_VERSION UNKNOWN
#endif

#define QUICK_LINT_JS_VERSION_STRING QLJS_CPP_QUOTE(QUICK_LINT_JS_VERSION)
#define QUICK_LINT_JS_VERSION_STRING_U8 \
  QLJS_CPP_CONCAT(u8, QUICK_LINT_JS_VERSION_STRING)

#define QUICK_LINT_JS_VERSION_STRING_U8_SV \
  QLJS_CPP_CONCAT(QUICK_LINT_JS_VERSION_STRING_U8, _sv)

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
