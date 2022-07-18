// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_TYPE_TRAITS_H
#define QUICK_LINT_JS_PORT_TYPE_TRAITS_H

#include <quick-lint-js/port/have.h>
#include <type_traits>

namespace quick_lint_js {
template <class T>
struct make_unsigned : public std::make_unsigned<T> {};

#if QLJS_HAVE_CHAR8_T
// HACK(strager): Work around older versions of libc++ not supporting
// std::make_unsigned<char8_t> despite the corresponding versions of Clang
// supporting char8_t.
template <>
struct make_unsigned<char8_t> {
  using type = char8_t;
};
#endif

template <class T>
using make_unsigned_t = typename make_unsigned<T>::type;
}

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
