// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

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
struct Hasher;

template <class T>
struct Hasher<T, std::enable_if_t<std::is_integral_v<T>>> {
  template <class U>
  std::size_t operator()(U data) const {
    static_assert(std::is_same_v<T, U>,
                  "implicit integer conversions are not allowed");
    return std::hash<T>()(data);
  }
};

template <class T>
struct Hasher<T*> {
  std::size_t operator()(T* data) const { return std::hash<T*>()(data); }
};

template <>
struct Hasher<std::string_view> {
  std::size_t operator()(std::string_view s) const {
    return std::hash<std::string_view>()(s);
  }
};
template <>
struct Hasher<std::string> : Hasher<std::string_view> {};

#if QLJS_HAVE_CHAR8_T
template <>
struct Hasher<String8_View> {
  std::size_t operator()(String8_View s) const {
    return std::hash<String8_View>()(s);
  }
};
template <>
struct Hasher<String8> : Hasher<String8_View> {};
#endif

template <>
struct Hasher<std::wstring_view> {
  std::size_t operator()(std::wstring_view s) const {
    return std::hash<std::wstring_view>()(s);
  }
};
template <>
struct Hasher<std::wstring> : Hasher<std::wstring_view> {};

template <class T1, class T2>
struct Hasher<std::pair<T1, T2>> {
  template <class U1, class U2>
  std::size_t operator()(const std::pair<U1, U2>& x) const {
    return mix_hashes(Hasher<U1>()(x.first), Hasher<U2>()(x.second));
  }
};
}

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
