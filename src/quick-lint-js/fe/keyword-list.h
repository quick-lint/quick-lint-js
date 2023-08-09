// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_KEYWORD_LIST_H
#define QUICK_LINT_JS_FE_KEYWORD_LIST_H

// X-macro for JavaScript and TypeScript reserved words, future reserved words,
// conditionally reserved words, and contextual keywords.
//
// QLJS_KEYWORD should have the following signature:
//
// #define QLJS_KEYWORD(keyword) ...
#define QLJS_X_KEYWORDS     \
  QLJS_KEYWORD(abstract)    \
  QLJS_KEYWORD(any)         \
  QLJS_KEYWORD(as)          \
  QLJS_KEYWORD(assert)      \
  QLJS_KEYWORD(asserts)     \
  QLJS_KEYWORD(async)       \
  QLJS_KEYWORD(await)       \
  QLJS_KEYWORD(bigint)      \
  QLJS_KEYWORD(boolean)     \
  QLJS_KEYWORD(break)       \
  QLJS_KEYWORD(case)        \
  QLJS_KEYWORD(catch)       \
  QLJS_KEYWORD(class)       \
  QLJS_KEYWORD(const)       \
  QLJS_KEYWORD(constructor) \
  QLJS_KEYWORD(continue)    \
  QLJS_KEYWORD(debugger)    \
  QLJS_KEYWORD(declare)     \
  QLJS_KEYWORD(default)     \
  QLJS_KEYWORD(delete)      \
  QLJS_KEYWORD(do)          \
  QLJS_KEYWORD(else)        \
  QLJS_KEYWORD(enum)        \
  QLJS_KEYWORD(export)      \
  QLJS_KEYWORD(extends)     \
  QLJS_KEYWORD(false)       \
  QLJS_KEYWORD(finally)     \
  QLJS_KEYWORD(for)         \
  QLJS_KEYWORD(from)        \
  QLJS_KEYWORD(function)    \
  QLJS_KEYWORD(get)         \
  QLJS_KEYWORD(global)      \
  QLJS_KEYWORD(if)          \
  QLJS_KEYWORD(implements)  \
  QLJS_KEYWORD(import)      \
  QLJS_KEYWORD(in)          \
  QLJS_KEYWORD(infer)       \
  QLJS_KEYWORD(instanceof)  \
  QLJS_KEYWORD(interface)   \
  QLJS_KEYWORD(intrinsic)   \
  QLJS_KEYWORD(is)          \
  QLJS_KEYWORD(keyof)       \
  QLJS_KEYWORD(let)         \
  QLJS_KEYWORD(module)      \
  QLJS_KEYWORD(namespace)   \
  QLJS_KEYWORD(never)       \
  QLJS_KEYWORD(new)         \
  QLJS_KEYWORD(null)        \
  QLJS_KEYWORD(number)      \
  QLJS_KEYWORD(object)      \
  QLJS_KEYWORD(of)          \
  QLJS_KEYWORD(out)         \
  QLJS_KEYWORD(override)    \
  QLJS_KEYWORD(package)     \
  QLJS_KEYWORD(private)     \
  QLJS_KEYWORD(protected)   \
  QLJS_KEYWORD(public)      \
  QLJS_KEYWORD(readonly)    \
  QLJS_KEYWORD(require)     \
  QLJS_KEYWORD(return)      \
  QLJS_KEYWORD(satisfies)   \
  QLJS_KEYWORD(set)         \
  QLJS_KEYWORD(static)      \
  QLJS_KEYWORD(string)      \
  QLJS_KEYWORD(super)       \
  QLJS_KEYWORD(switch)      \
  QLJS_KEYWORD(symbol)      \
  QLJS_KEYWORD(this)        \
  QLJS_KEYWORD(throw)       \
  QLJS_KEYWORD(true)        \
  QLJS_KEYWORD(try)         \
  QLJS_KEYWORD(type)        \
  QLJS_KEYWORD(typeof)      \
  QLJS_KEYWORD(undefined)   \
  QLJS_KEYWORD(unique)      \
  QLJS_KEYWORD(unknown)     \
  QLJS_KEYWORD(var)         \
  QLJS_KEYWORD(void)        \
  QLJS_KEYWORD(while)       \
  QLJS_KEYWORD(with)        \
  QLJS_KEYWORD(yield)

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
