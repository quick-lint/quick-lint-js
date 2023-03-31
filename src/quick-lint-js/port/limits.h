// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_LIMITS_H
#define QUICK_LINT_JS_PORT_LIMITS_H

#include <limits>
#include <quick-lint-js/port/have.h>

namespace quick_lint_js {
template <class T>
struct numeric_limits : public std::numeric_limits<T> {};

#if QLJS_HAVE_CHAR8_T
// HACK(strager): Work around older versions of libc++ not supporting
// std::numeric_limits<char8_t> despite the corresponding versions of Clang
// supporting char8_t.
template <>
struct numeric_limits<char8_t> {
  static constexpr char8_t lowest() noexcept {
    return static_cast<char8_t>(uchar_limits::lowest());
  }

  static constexpr char8_t(max)() noexcept {
    return static_cast<char8_t>((uchar_limits::max)());
  }

 private:
  using uchar_limits = numeric_limits<unsigned char>;
};
#endif
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
