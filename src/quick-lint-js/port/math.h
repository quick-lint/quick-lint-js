// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_MATH_H
#define QUICK_LINT_JS_PORT_MATH_H

#include <cstdint>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>

#if QLJS_HAVE_INTRIN_H
#include <intrin.h>
#endif

namespace quick_lint_js {
// On some compilers, std::max is not constexpr. Define our own which is
// constexpr.
template <class T, class U>
constexpr auto maximum(T x, U y) noexcept {
  return x < y ? y : x;
}

// Returns (lhs*rhs)>>64.
inline std::uint64_t multiply_u64_get_top_64(std::uint64_t lhs,
                                             std::uint64_t rhs) {
  // Discussion of approaches: https://stackoverflow.com/q/28868367
#if QLJS_HAVE_INT128
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wpedantic")

  using U128 = unsigned __int128;
  return static_cast<std::uint64_t>(
      (static_cast<U128>(lhs) * static_cast<U128>(rhs)) >> 64);

  QLJS_WARNING_POP
#elif QLJS_HAVE_UMULH
  return __umulh(lhs, rhs);
#else
  // Algorithm adapted from Fabian 'ryg' Giesen's public domain work:
  // https://github.com/jkbonfield/rans_static/blob/576e9db9e02659a5797ce5e21dd99f6ce0285727/rans64.h#L51-L64
  auto lo = [](std::uint64_t x) -> std::uint64_t { return x & 0xffff'ffff; };
  auto hi = [](std::uint64_t x) -> std::uint64_t { return x >> 32; };
  std::uint64_t a = hi(lhs);
  std::uint64_t b = lo(lhs);
  std::uint64_t c = hi(rhs);
  std::uint64_t d = lo(rhs);
  std::uint64_t x0 = d * b;
  std::uint64_t x1 = lo(d * a) + lo(c * b) + hi(x0);
  std::uint64_t x2 = hi(d * a) + hi(c * b) + lo(c * a) + hi(x1);
  std::uint64_t x3 = hi(c * a) + hi(x2);
  // x3, lo(x2), lo(x1), lo(x0)
  return (x3 << 32) + lo(x2);
#endif
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
