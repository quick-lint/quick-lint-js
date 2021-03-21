// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ARRAY_H
#define QUICK_LINT_JS_ARRAY_H

#include <array>
#include <cstddef>
#include <quick-lint-js/warning.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wlarge-by-value-copy")

template <class... Args>
inline constexpr auto make_array(Args&&... items) {
  using item_type = std::common_type_t<Args...>;
  return std::array<item_type, sizeof...(items)>{std::forward<Args>(items)...};
}

template <class T, std::size_t LHSSize, std::size_t RHSSize,
          std::size_t... LHSIndexes, std::size_t... RHSIndexes>
inline constexpr std::array<T, LHSSize + RHSSize> concat_impl(
    const std::array<T, LHSSize>& lhs, std::index_sequence<LHSIndexes...>,
    const std::array<T, RHSSize>& rhs, std::index_sequence<RHSIndexes...>) {
  return std::array<T, LHSSize + RHSSize>{lhs[LHSIndexes]...,
                                          rhs[RHSIndexes]...};
}

template <class T, std::size_t LHSSize, std::size_t RHSSize>
inline constexpr std::array<T, LHSSize + RHSSize> concat(
    const std::array<T, LHSSize>& lhs, const std::array<T, RHSSize>& rhs) {
  return concat_impl<T, LHSSize, RHSSize>(
      lhs, std::make_index_sequence<LHSSize>(), rhs,
      std::make_index_sequence<RHSSize>());
}

QLJS_WARNING_POP
}

#endif

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
