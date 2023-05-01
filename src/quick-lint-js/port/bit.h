// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_BIT_H
#define QUICK_LINT_JS_PORT_BIT_H

#include <cstdint>
#include <type_traits>

namespace quick_lint_js {
// TODO(strager): Use std::countr_zero if available.
inline int countr_zero(std::uint32_t x) noexcept {
#if defined(__GNUC__)
  if (x == 0) {
    return 32;
  }
  return __builtin_ctz(x);
#else
  std::uint32_t i;
  for (i = 0; i < 32; ++i) {
    if ((x & (std::uint32_t(1) << i)) != 0) {
      break;
    }
  }
  return i;
#endif
}

// TODO(strager): Use std::countr_one if available.
inline int countr_one(std::uint32_t x) noexcept {
#if defined(__GNUC__)
  return countr_zero(~x);
#else
  std::uint32_t i;
  for (i = 0; i < 32; ++i) {
    if ((x & (std::uint32_t(1) << i)) == 0) {
      break;
    }
  }
  return i;
#endif
}

// TODO(strager): Use std::countl_zero if available.
template <class T>
int countl_zero(T x) noexcept {
  constexpr int width = sizeof(T) * 8;
#if defined(__GNUC__)
  if (x == 0) {
    return width;
  }
  if constexpr (std::is_same_v<T, unsigned>) {
    return __builtin_clz(x);
  } else if constexpr (std::is_same_v<T, unsigned long>) {
    return __builtin_clzl(x);
  } else if constexpr (std::is_same_v<T, unsigned long long>) {
    return __builtin_clzll(x);
  }
#else
  int i;
  for (i = 0; i < width; ++i) {
    if ((x & (T(1) << (width - i - 1))) != 0) {
      break;
    }
  }
  return i;
#endif
}

// TODO(strager): Use std::bit_width if available.
inline int bit_width(std::uint32_t x) noexcept { return 32 - countl_zero(x); }

// TODO(strager): Use std::has_single_bit if available.
template <class T>
bool has_single_bit(T x) noexcept {
  if (x == 0) {
    return false;
  }
  if (((x - 1) & x) != 0) {
    return false;
  }
  return true;
}

// TODO(strager): Use std::bit_ceil if available.
template <class T>
T bit_ceil(T x) noexcept {
  constexpr int width = sizeof(T) * 8;
  if (x == 0) {
    return 1;
  }
  if (has_single_bit(x)) {
    return x;
  }
  int shift = width - countl_zero(x);
  return (T(1) << shift);
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
