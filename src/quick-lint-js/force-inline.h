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

#ifndef QUICK_LINT_JS_FORCE_INLINE_H
#define QUICK_LINT_JS_FORCE_INLINE_H

#if defined(__clang__) || defined(__GNUC__)
#define QLJS_FORCE_INLINE [[gnu::always_inline]]
#elif defined(_MSC_VER)
#define QLJS_FORCE_INLINE __forceinline
#else
#define QLJS_WARNING_PUSH /* empty */
#define QLJS_WARNING_POP  /* empty */
#endif

#endif
