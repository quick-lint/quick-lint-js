// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_WARNING_H
#define QUICK_LINT_JS_WARNING_H

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

#define QLJS_WARNING_PRAGMA_STRING(x, y) QLJS_WARNING_PRAGMA_STRING_(x y)
#define QLJS_WARNING_PRAGMA_STRING_(x) #x

#endif
