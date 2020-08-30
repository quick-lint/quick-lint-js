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

#ifndef QUICK_LINT_JS_NARROW_CAST_H
#define QUICK_LINT_JS_NARROW_CAST_H

#include <cassert>
#include <limits>
#include <type_traits>

namespace quick_lint_js {
template <class Out, class In>
constexpr bool can_narrow_cast([[maybe_unused]] In x) noexcept {
  using out_limits = std::numeric_limits<Out>;
  using unsigned_in = std::make_unsigned_t<In>;
  using unsigned_out = std::make_unsigned_t<Out>;
  if constexpr (std::is_same_v<In, Out>) {
    return true;
  } else if constexpr (std::is_signed_v<In> && std::is_signed_v<Out>) {
    return out_limits::lowest() <= x && x <= out_limits::max();
  } else if constexpr (std::is_signed_v<In> && !std::is_signed_v<Out>) {
    return 0 <= x && static_cast<unsigned_in>(x) <= out_limits::max();
  } else if constexpr (!std::is_signed_v<In> && std::is_signed_v<Out>) {
    return x <= unsigned_out{out_limits::max()};
  } else if constexpr (!std::is_signed_v<In> && !std::is_signed_v<Out>) {
    return x <= out_limits::max();
  }
}

template <class Out, class In>
constexpr Out narrow_cast(In x) noexcept {
  assert(can_narrow_cast<Out>(x));
  return static_cast<Out>(x);
}
}  // namespace quick_lint_js

#endif
