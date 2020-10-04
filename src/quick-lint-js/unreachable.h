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

#ifndef QUICK_LINT_JS_UNREACHABLE_H
#define QUICK_LINT_JS_UNREACHABLE_H

#if defined(__GNUC__) || defined(__clang__)
#define QLJS_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
#define QLJS_UNREACHABLE() __assume(false)
#else
#define QLJS_UNREACHABLE() \
  do {                     \
  } while (false)
#endif

#endif
