// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/math-overflow.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <type_traits>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

#if QLJS_HAVE_CHARCONV_HEADER
#include <charconv>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CHARCONV_HEADER
from_chars_result from_chars(const char *begin, const char *end, int &value) {
  std::from_chars_result result = std::from_chars(begin, end, value);
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}

from_chars_result from_chars(const char *begin, const char *end,
                             std::size_t &value) {
  std::from_chars_result result = std::from_chars(begin, end, value);
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 char32_t &value) {
  using underlying_type = std::uint_least32_t;
  static_assert(sizeof(char32_t) == sizeof(underlying_type));
  underlying_type parsed_value;
  std::from_chars_result result =
      std::from_chars(begin, end, parsed_value, /*base=*/16);
  if (result.ec == std::errc{}) {
    value = static_cast<char32_t>(parsed_value);
  }
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}

template <class T>
char8 *write_integer(T value, char8 *out) {
  char *buffer = reinterpret_cast<char *>(out);
  std::to_chars_result result =
      std::to_chars(buffer, &buffer[integer_string_length<T>], value);
  QLJS_ASSERT(result.ec == std::errc{});
  return out + (result.ptr - buffer);
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

from_chars_result from_chars(const char *begin, const char *end,
                             std::size_t &value) {
  std::string buffer(begin, end);
  if (!(buffer.size() >= 1 && is_decimal_digit(buffer[0]))) {
    return from_chars_result{.ptr = begin, .ec = std::errc::invalid_argument};
  }
  const char *c = begin;
  auto out_of_range = [&c, end]() -> from_chars_result {
    for (; is_decimal_digit(*c) && c != end; ++c) {
      // Skip digits.
    }
    return from_chars_result{.ptr = c, .ec = std::errc::result_out_of_range};
  };

  std::size_t result = 0;
  for (; is_decimal_digit(*c) && c != end; ++c) {
    std::size_t new_result = result * 10 + (*c - '0');
    if (new_result < result) {
      return out_of_range();
    }
    result = new_result;
  }
  value = result;
  return from_chars_result{.ptr = c, .ec = std::errc{0}};
}

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 char32_t &value) {
  std::string buffer(begin, end);
  if (!(buffer.size() >= 1 && is_hexadecimal_digit(buffer[0]))) {
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
  if (errno == ERANGE || !in_range<char32_t>(long_value)) {
    return from_chars_result{.ptr = ptr, .ec = std::errc::result_out_of_range};
  }
  value = static_cast<char32_t>(long_value);
  return from_chars_result{.ptr = ptr, .ec = std::errc{0}};
}

template <class T>
char8 *write_integer(T value, char8 *out) {
  char *buffer = reinterpret_cast<char *>(out);
  constexpr std::size_t buffer_size = integer_string_length<T>;
  constexpr const char *format =
      std::is_same_v<T, int>
          ? "%d"
          : std::is_same_v<T, long>
                ? "%ld"
                : std::is_same_v<T, long long>
                      ? "%lld"
                      : std::is_same_v<T, unsigned>
                            ? "%u"
                            : std::is_same_v<T, unsigned long>
                                  ? "%lu"
                                  : std::is_same_v<T, unsigned long long>
                                        ? "%llu"
                                        : "";
  static_assert(*format != '\0', "Unsupported integer type");

  int rc = std::snprintf(buffer, buffer_size, format, value);
  QLJS_ASSERT(rc >= 0);
  if (rc == buffer_size) {
    int digit;
    if constexpr (std::is_unsigned_v<T>) {
      digit = value % 10;
    } else {
      digit = std::abs(narrow_cast<int>(value % 10));
    }
    buffer[buffer_size - 1] = narrow_cast<char>(u8'0' + digit);
  }
  return out + rc;
}
#endif

template char8 *write_integer<int>(int, char8 *out);
template char8 *write_integer<long>(long, char8 *out);
template char8 *write_integer<long long>(long long, char8 *out);
template char8 *write_integer<unsigned>(unsigned, char8 *out);
template char8 *write_integer<unsigned long>(unsigned long, char8 *out);
template char8 *write_integer<unsigned long long>(unsigned long long,
                                                  char8 *out);
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
