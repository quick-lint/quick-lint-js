// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_INTEGER_H
#define QUICK_LINT_JS_INTEGER_H

#include <cstddef>
#include <limits>
#include <quick-lint-js/char8.h>
#include <system_error>

namespace quick_lint_js {
struct from_chars_result {
  const char *ptr;
  std::errc ec;
};

from_chars_result from_chars(const char *begin, const char *end, int &value);
from_chars_result from_chars(const char *begin, const char *end,
                             std::size_t &value);
from_chars_result from_chars_hex(const char *begin, const char *end,
                                 char32_t &value);
from_chars_result from_chars_hex(const char *begin, const char *end,
                                 unsigned char &value);

template <class T>
inline constexpr int integer_string_length =
    (std::numeric_limits<T>::digits10 + 1) +
    (std::numeric_limits<T>::is_signed ? 1 : 0);

template <class T>
char8 *write_integer(T, char8 *out);

extern template char8 *write_integer<int>(int, char8 *out);
extern template char8 *write_integer<long>(long, char8 *out);
extern template char8 *write_integer<long long>(long long, char8 *out);
extern template char8 *write_integer<unsigned>(unsigned, char8 *out);
extern template char8 *write_integer<unsigned long>(unsigned long, char8 *out);
extern template char8 *write_integer<unsigned long long>(unsigned long long,
                                                         char8 *out);
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
