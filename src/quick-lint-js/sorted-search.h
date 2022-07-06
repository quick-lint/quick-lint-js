// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <string_view>

namespace quick_lint_js {
// sorted_search is like std::lower_bound, but with a few changes:
//
// * It is constexpr. (The std functions are not constexpr for all compilers we
//   support.)
// * It returns the end iterator if no equal match was found.
// * It only works with std::string_view.
template <class It>
constexpr It sorted_search(It begin, It end, std::string_view needle) {
  std::ptrdiff_t length = end - begin;
  std::ptrdiff_t lo = 0;
  std::ptrdiff_t hi = length - 1;
  while (lo <= hi) {
    std::ptrdiff_t mid = (lo + hi) / 2;
    It mid_it = begin + mid;
    int comparison = mid_it->compare(needle);
    if (comparison < 0) {  // *mid_it < needle
      lo = mid + 1;
    } else if (comparison > 0) {  // *mid_it > needle
      hi = mid - 1;
    } else {  // *mid_it == needle
      return mid_it;
    }
  }
  return end;
}
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
