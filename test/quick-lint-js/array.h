// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <array>
#include <cstddef>
#include <quick-lint-js/port/warning.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wlarge-by-value-copy")

template <class... Args>
inline constexpr auto make_array(Args&&... items) {
  using Item_Type = std::common_type_t<Args...>;
  return std::array<Item_Type, sizeof...(items)>{std::forward<Args>(items)...};
}

template <class T, class... Args>
inline constexpr auto make_array_explicit(Args&&... items) {
  return std::array<T, sizeof...(items)>{T(std::forward<Args>(items))...};
}

template <class T, std::size_t lhs_size, std::size_t rhs_size,
          std::size_t... lhs_indexes, std::size_t... rhs_indexes>
inline constexpr std::array<T, lhs_size + rhs_size> concat_impl(
    const std::array<T, lhs_size>& lhs, std::index_sequence<lhs_indexes...>,
    const std::array<T, rhs_size>& rhs, std::index_sequence<rhs_indexes...>) {
  return std::array<T, lhs_size + rhs_size>{lhs[lhs_indexes]...,
                                            rhs[rhs_indexes]...};
}

template <class T, std::size_t lhs_size, std::size_t rhs_size>
inline constexpr std::array<T, lhs_size + rhs_size> concat(
    const std::array<T, lhs_size>& lhs, const std::array<T, rhs_size>& rhs) {
  return concat_impl<T, lhs_size, rhs_size>(
      lhs, std::make_index_sequence<lhs_size>(), rhs,
      std::make_index_sequence<rhs_size>());
}

QLJS_WARNING_POP
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
