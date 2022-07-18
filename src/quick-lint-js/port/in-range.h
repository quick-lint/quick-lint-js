// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_IN_RANGE_H
#define QUICK_LINT_JS_PORT_IN_RANGE_H

#include <quick-lint-js/port/limits.h>
#include <quick-lint-js/port/type-traits.h>
#include <type_traits>

namespace quick_lint_js {
// TODO(strager): Use std::in_range if supported.
template <class Out, class In>
constexpr bool in_range([[maybe_unused]] In x) noexcept {
  [[maybe_unused]] constexpr Out min_out = numeric_limits<Out>::lowest();
  [[maybe_unused]] constexpr Out max_out = (numeric_limits<Out>::max)();
  using unsigned_in = make_unsigned_t<In>;
  using unsigned_out = make_unsigned_t<Out>;
  if constexpr (std::is_same_v<In, Out>) {
    return true;
  } else if constexpr (std::is_signed_v<In> == std::is_signed_v<Out>) {
    return min_out <= x && x <= max_out;
  } else if constexpr (std::is_signed_v<In> && !std::is_signed_v<Out>) {
    return 0 <= x && static_cast<unsigned_in>(x) <= max_out;
  } else if constexpr (!std::is_signed_v<In> && std::is_signed_v<Out>) {
    return x <= unsigned_out{max_out};
  }
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
