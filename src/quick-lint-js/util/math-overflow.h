// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_MATH_OVERFLOW_H
#define QUICK_LINT_JS_UTIL_MATH_OVERFLOW_H

#include <limits>
#include <optional>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
template <class T>
std::enable_if_t<std::is_signed_v<T> && std::is_integral_v<T>, std::optional<T>>
checked_add(T x, T y) noexcept {
  // https://wiki.sei.cmu.edu/confluence/display/c/INT32-C.+Ensure+that+operations+on+signed+integers+do+not+result+in+overflow
  constexpr T t_max = (std::numeric_limits<T>::max)();
  constexpr T t_min = std::numeric_limits<T>::lowest();
  if (((y > 0) && (x > (t_max - y))) || ((y < 0) && (x < (t_min - y)))) {
    return std::nullopt;
  }
  return x + y;
}
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
