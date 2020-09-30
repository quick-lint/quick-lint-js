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

#include <cerrno>
#include <cstdlib>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <string>

#if QLJS_HAVE_CHARCONV_HEADER
#include <charconv>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CHARCONV_HEADER
from_chars_result from_chars(const char *begin, const char *end, int &value) {
  std::from_chars_result result = std::from_chars(begin, end, value);
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 int &value) {
  std::from_chars_result result =
      std::from_chars(begin, end, value, /*base=*/16);
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}
#else
namespace {
bool is_decimal_digit(char c) noexcept { return '0' <= c && c <= '9'; }

bool is_hexadecimal_digit(char c) noexcept {
  return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
         ('A' <= c && c <= 'F');
}
}

from_chars_result from_chars(const char *begin, const char *end, int &value) {
  std::string buffer(begin, end);
  if (!((buffer.size() >= 1 && is_decimal_digit(buffer[0])) ||
        (buffer.size() >= 2 && buffer[0] == '-' &&
         is_decimal_digit(buffer[1])))) {
    return from_chars_result{.ptr = begin, .ec = std::errc::invalid_argument};
  }
  char *endptr;
  errno = 0;
  long long_value = std::strtol(buffer.c_str(), &endptr, /*base=*/10);
  const char *ptr = (endptr - buffer.c_str()) + begin;
  if (errno == ERANGE || !in_range<int>(long_value)) {
    return from_chars_result{.ptr = ptr, .ec = std::errc::result_out_of_range};
  }
  value = static_cast<int>(long_value);
  return from_chars_result{.ptr = ptr, .ec = std::errc{0}};
}

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 int &value) {
  std::string buffer(begin, end);
  if (!((buffer.size() >= 1 && is_hexadecimal_digit(buffer[0])) ||
        (buffer.size() >= 2 && buffer[0] == '-' &&
         is_hexadecimal_digit(buffer[1])))) {
    return from_chars_result{.ptr = begin, .ec = std::errc::invalid_argument};
  }
  if (buffer.size() >= 1 && (buffer[1] == 'x' || buffer[1] == 'X')) {
    // Prevent strtol from parsing '0x' prefixes.
    buffer[1] = '\0';
  }
  char *endptr;
  errno = 0;
  long long_value = std::strtol(buffer.c_str(), &endptr, /*base=*/16);
  const char *ptr = (endptr - buffer.c_str()) + begin;
  if (errno == ERANGE || !in_range<int>(long_value)) {
    return from_chars_result{.ptr = ptr, .ec = std::errc::result_out_of_range};
  }
  value = static_cast<int>(long_value);
  return from_chars_result{.ptr = ptr, .ec = std::errc{0}};
}
#endif
}
