// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/util/cast.h>

namespace quick_lint_js {
// Returns true if 'e' contains all of the bits set in 'flag'.
//
// Equivalent to ((e & flag) == flag).
template <class Enum>
bool enum_has_flags(Enum e, Enum flag) {
  auto flag_int = enum_to_int_cast(flag);
  return (enum_to_int_cast(e) & flag_int) == flag_int;
}

// Returns only the set bits in 'e' described by 'flag'.
//
// Equivalent to (e & flag).
template <class Enum>
Enum enum_select_flags(Enum e, Enum flag) {
  return int_to_enum_cast<Enum>(enum_to_int_cast(e) & enum_to_int_cast(flag));
}

// Add bits set in 'flag' to 'e'.
//
// Equivalent to (e | flag).
template <class Enum>
Enum enum_set_flags(Enum e, Enum flag) {
  return int_to_enum_cast<Enum>(enum_to_int_cast(e) | enum_to_int_cast(flag));
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
