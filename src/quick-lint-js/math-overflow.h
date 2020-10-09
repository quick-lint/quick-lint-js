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

#include <limits>
#include <optional>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
// Only permit tested specializations, and disallow implicit conversions (e.g.
// long -> int).
template <class T>
std::optional<T> checked_add(T x, T y) noexcept = delete;

template <>
inline std::optional<int> checked_add(int x, int y) noexcept {
  using out = int;
  using wider_int = long long;
  constexpr out out_max = (std::numeric_limits<out>::max)();
  constexpr out out_min = std::numeric_limits<out>::lowest();
  static_assert(std::numeric_limits<wider_int>::lowest() / 2 <= out_min);
  static_assert(out_max <= (std::numeric_limits<wider_int>::max)() / 2);

  wider_int sum = static_cast<wider_int>(x) + static_cast<wider_int>(y);
  if (in_range<out>(sum)) {
    return static_cast<out>(sum);
  } else {
    return std::nullopt;
  }
}
}
