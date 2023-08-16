// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstdio>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/float.h>

QLJS_WARNING_IGNORE_CLANG("-Wdouble-promotion")

namespace quick_lint_js {
template <class T>
Char8 *write_decimal_float(T value, Char8 *out) {
  std::array<char, max_decimal_float_string_length<T> + 1> temp;
  int written = std::snprintf(temp.data(), temp.size(), "%.17g", double{value});
  QLJS_ALWAYS_ASSERT(written > 0);
  QLJS_ALWAYS_ASSERT(written <= static_cast<int>(temp.size()));
  return std::copy_n(temp.data(), written, out);
}

template Char8 *write_decimal_float<float>(float, Char8 *out);
template Char8 *write_decimal_float<double>(double, Char8 *out);
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
