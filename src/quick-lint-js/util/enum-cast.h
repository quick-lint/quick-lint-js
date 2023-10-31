// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <type_traits>

namespace quick_lint_js {
// Convert an integer to an enum.
//
// Example:
//
//   enum class Color : unsigned char { red, green, blue };
//   unsigned char raw = 1;                   // green
//   Color c = int_to_enum_cast<Color>(raw);  // Color::green
template <class Enum>
constexpr Enum int_to_enum_cast(std::underlying_type_t<Enum> value) {
  return static_cast<Enum>(value);
}

// Convert an enum to its integer value.
//
// Example:
//
//   enum class Color : unsigned char { red, green, blue };
//   unsigned char raw = enum_to_int_cast(Color::green);     // 1
template <class Enum>
constexpr std::underlying_type_t<Enum> enum_to_int_cast(Enum value) {
  return static_cast<std::underlying_type_t<Enum>>(value);
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
