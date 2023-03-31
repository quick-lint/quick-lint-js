// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_WARNING_H
#define QUICK_LINT_JS_PORT_WARNING_H

#if defined(__clang__)
#define QLJS_WARNING_PUSH _Pragma("clang diagnostic push")
#define QLJS_WARNING_POP _Pragma("clang diagnostic pop")
#elif defined(__GNUC__)
#define QLJS_WARNING_PUSH _Pragma("GCC diagnostic push")
#define QLJS_WARNING_POP _Pragma("GCC diagnostic pop")
#elif defined(_MSC_VER)
#define QLJS_WARNING_PUSH __pragma(warning(push))
#define QLJS_WARNING_POP __pragma(warning(pop))
#else
#define QLJS_WARNING_PUSH /* empty */
#define QLJS_WARNING_POP  /* empty */
#endif

#if defined(__clang__)
#define QLJS_WARNING_IGNORE_CLANG(warning_name) \
  _Pragma(QLJS_WARNING_PRAGMA_STRING(clang diagnostic ignored, warning_name))
#else
#define QLJS_WARNING_IGNORE_CLANG(warning_name)
#endif

#if defined(__GNUC__) && !defined(__clang__)
#define QLJS_WARNING_IGNORE_GCC(warning_name) \
  _Pragma(QLJS_WARNING_PRAGMA_STRING(GCC diagnostic ignored, warning_name))
#else
#define QLJS_WARNING_IGNORE_GCC(warning_name)
#endif

#if defined(_MSC_VER)
#define QLJS_WARNING_IGNORE_MSVC(warning_number) \
  __pragma(warning(disable : warning_number))
#else
#define QLJS_WARNING_IGNORE_MSVC(warning_number)
#endif

#define QLJS_WARNING_PRAGMA_STRING(x, y) QLJS_WARNING_PRAGMA_STRING_(x y)
#define QLJS_WARNING_PRAGMA_STRING_(x) #x

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
