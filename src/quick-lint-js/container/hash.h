// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_HASH_H
#define QUICK_LINT_JS_CONTAINER_HASH_H

#include <cstddef>
#include <functional>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <string_view>
#include <type_traits>

namespace quick_lint_js {
inline std::size_t mix_hashes(std::size_t a, std::size_t b) {
  // FIXME(strager): This algorithm is probably bad.
  return ((a << 1) | (a >> (sizeof(a) - 1))) ^ b;
}

// A hash functor for containers like hash_map.
//
// Implements the C++ standard Hash requirement.
template <class T, class = void>
struct hasher;

template <class T>
struct hasher<T, std::enable_if_t<std::is_integral_v<T>>> {
  template <class U>
  std::size_t operator()(U data) const noexcept {
    static_assert(std::is_same_v<T, U>,
                  "implicit integer conversions are not allowed");
    return std::hash<T>()(data);
  }
};

template <class T>
struct hasher<T*> {
  std::size_t operator()(T* data) const noexcept {
    return std::hash<T*>()(data);
  }
};

template <>
struct hasher<std::string_view> {
  std::size_t operator()(std::string_view s) const noexcept {
    return std::hash<std::string_view>()(s);
  }
};
template <>
struct hasher<std::string> : hasher<std::string_view> {};

#if QLJS_HAVE_CHAR8_T
template <>
struct hasher<string8_view> {
  std::size_t operator()(string8_view s) const noexcept {
    return std::hash<string8_view>()(s);
  }
};
template <>
struct hasher<string8> : hasher<string8_view> {};
#endif

template <class T1, class T2>
struct hasher<std::pair<T1, T2>> {
  template <class U1, class U2>
  std::size_t operator()(const std::pair<U1, U2>& x) const noexcept {
    return mix_hashes(hasher<U1>()(x.first), hasher<U2>()(x.second));
  }
};
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
