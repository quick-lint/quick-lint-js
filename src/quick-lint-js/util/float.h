// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
template <class T>
struct Max_Decimal_Float_String_Length_Impl;

template <>
struct Max_Decimal_Float_String_Length_Impl<float> {
  // FIXME(strager): This is certainly overkill.
  static constexpr int value = 512;
};
template <>
struct Max_Decimal_Float_String_Length_Impl<double> {
  // FIXME(strager): This is certainly overkill.
  static constexpr int value = 512;
};

template <class T>
inline constexpr int max_decimal_float_string_length =
    Max_Decimal_Float_String_Length_Impl<T>::value;

template <class T>
Char8 *write_decimal_float(T, Char8 *out);

extern template Char8 *write_decimal_float<float>(float, Char8 *out);
extern template Char8 *write_decimal_float<double>(double, Char8 *out);
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
