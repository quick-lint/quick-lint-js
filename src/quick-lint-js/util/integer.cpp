// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cerrno>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <type_traits>

QLJS_WARNING_IGNORE_CLANG("-Wmissing-prototypes")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wmissing-declarations")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

#if QLJS_HAVE_CHARCONV_HEADER
#include <charconv>
#endif

namespace quick_lint_js {
struct from_chars_result {
  const char *ptr;
  std::errc ec;
};

struct from_char8s_result {
  const char8 *ptr;
  std::errc ec;
};

struct from_wchars_result {
  const wchar_t *ptr;
  std::errc ec;
};

#if QLJS_HAVE_CHARCONV_HEADER
template <class T>
from_chars_result from_chars(const char *begin, const char *end, T &value) {
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

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 std::uint8_t &value) {
  std::from_chars_result result =
      std::from_chars(begin, end, value, /*base=*/16);
  return from_chars_result{.ptr = result.ptr, .ec = result.ec};
}

template <class T>
char *write_integer(T value, char *out) {
  std::to_chars_result result =
      std::to_chars(out, &out[integer_string_length<T>], value);
  QLJS_ASSERT(result.ec == std::errc{});
  return result.ptr;
}
#else
namespace {
bool is_decimal_digit(char c) noexcept { return '0' <= c && c <= '9'; }

bool is_hexadecimal_digit(char c) noexcept {
  return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
         ('A' <= c && c <= 'F');
}
}

template <class T>
from_chars_result from_chars(const char *begin, const char *end, T &value) {
  if constexpr (std::is_same_v<T, int>) {
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
      return from_chars_result{.ptr = ptr,
                               .ec = std::errc::result_out_of_range};
    }
    value = static_cast<int>(long_value);
    return from_chars_result{.ptr = ptr, .ec = std::errc{0}};
  } else {
    static_assert(std::is_unsigned_v<T>,
                  "signed from_chars not yet implemented");

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

    T result = 0;
    for (; is_decimal_digit(*c) && c != end; ++c) {
      T new_result = static_cast<T>(result * 10 + static_cast<T>(*c - '0'));
      if (new_result < result) {
        return out_of_range();
      }
      result = new_result;
    }
    value = result;
    return from_chars_result{.ptr = c, .ec = std::errc{0}};
  }
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

from_chars_result from_chars_hex(const char *begin, const char *end,
                                 std::uint8_t &value) {
  char32_t long_value;
  from_chars_result result = from_chars_hex(begin, end, long_value);
  if (result.ec != std::errc()) {
    return result;
  }
  if (!in_range<std::uint8_t>(long_value)) {
    return from_chars_result{.ptr = result.ptr,
                             .ec = std::errc::result_out_of_range};
  }
  value = static_cast<std::uint8_t>(long_value);
  return result;
}

template <class T>
char *write_integer(T value, char *out) {
  constexpr std::size_t buffer_size = integer_string_length<T>;
  constexpr const char *format =
      std::is_same_v<T, int>
          ? "%d"
          : std::is_same_v<T, long>
                ? "%ld"
                : std::is_same_v<T, long long>
                      ? "%lld"
                      : (std::is_same_v<T, unsigned> ||
                         std::is_same_v<T, unsigned short>)
                            ? "%u"
                            : std::is_same_v<T, unsigned long>
                                  ? "%lu"
                                  : std::is_same_v<T, unsigned long long>
                                        ? "%llu"
                                        : "";
  static_assert(*format != '\0', "Unsupported integer type");

  int rc = std::snprintf(out, buffer_size, format, value);
  QLJS_ASSERT(rc >= 0);
  if (rc == buffer_size) {
    int digit;
    if constexpr (std::is_unsigned_v<T>) {
      digit = value % 10;
    } else {
      digit = std::abs(narrow_cast<int>(value % 10));
    }
    out[buffer_size - 1] = narrow_cast<char>('0' + digit);
  }
  return out + rc;
}
#endif

#if QLJS_HAVE_CHAR8_T
template <class T>
char8 *write_integer(T value, char8 *out) {
  char *buffer = reinterpret_cast<char *>(out);
  buffer = write_integer(value, buffer);
  return reinterpret_cast<char8 *>(buffer);
}
#endif

template <class T>
wchar_t *write_integer(T value, wchar_t *out) {
  // TODO(strager): Write the output in-place.
  std::array<char, integer_string_length<T>> buffer;
  char *end = write_integer(value, buffer.data());
  return std::copy(buffer.data(), end, out);
}

from_char8s_result from_char8s(const char8 *begin, const char8 *end,
                               std::size_t &value) {
  from_chars_result result =
      from_chars(reinterpret_cast<const char *>(begin),
                 reinterpret_cast<const char *>(end), value);
  return from_char8s_result{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

template <class T>
from_wchars_result from_chars(const wchar_t *begin, const wchar_t *end,
                              T &value) {
  // TODO(strager): Parse the string without copying.
  std::array<char, integer_string_length<T> + 1> buffer;
  char *out = buffer.data();
  char *out_end =
      out + std::min(narrow_cast<std::ptrdiff_t>(buffer.size()), end - begin);
  for (const wchar_t *in = begin; out != out_end; ++in) {
    if ((L'0' <= *in && *in <= L'9') || *in == L'-') {
      *out++ = static_cast<char>(*in);
    } else {
      *out++ = '_';  // Invalid character.
    }
  }

  from_chars_result result = from_chars(buffer.data(), out_end, value);
  return from_wchars_result{
      .ptr = &begin[result.ptr - buffer.data()],
      .ec = result.ec,
  };
}

from_char8s_result from_char8s_hex(const char8 *begin, const char8 *end,
                                   char32_t &value) {
  from_chars_result result =
      from_chars_hex(reinterpret_cast<const char *>(begin),
                     reinterpret_cast<const char *>(end), value);
  return from_char8s_result{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

from_char8s_result from_char8s_hex(const char8 *begin, const char8 *end,
                                   unsigned char &value) {
  from_chars_result result =
      from_chars_hex(reinterpret_cast<const char *>(begin),
                     reinterpret_cast<const char *>(end), value);
  return from_char8s_result{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

template <class T>
parse_integer_exact_error parse_integer_exact(std::string_view s, T &value) {
  const char *s_end = s.data() + s.size();
  T temp;
  from_chars_result result = from_chars(s.data(), s_end, temp);
  if (result.ec == std::errc::invalid_argument || result.ptr != s_end) {
    return parse_integer_exact_error::invalid;
  } else if (result.ec == std::errc::result_out_of_range) {
    return parse_integer_exact_error::out_of_range;
  } else {
    QLJS_ASSERT(result.ec == std::errc());
    value = temp;
    return parse_integer_exact_error::ok;
  }
}

template parse_integer_exact_error parse_integer_exact(std::string_view,
                                                       int &value);
template parse_integer_exact_error parse_integer_exact(std::string_view,
                                                       unsigned short &value);
template parse_integer_exact_error parse_integer_exact(std::string_view,
                                                       unsigned &value);
template parse_integer_exact_error parse_integer_exact(std::string_view,
                                                       unsigned long &value);
template parse_integer_exact_error parse_integer_exact(
    std::string_view, unsigned long long &value);

template <class T>
parse_integer_exact_error parse_integer_exact(std::wstring_view s, T &value) {
  // TODO(strager): Deduplicate with the std::string_view overload.
  const wchar_t *s_end = s.data() + s.size();
  T temp;
  from_wchars_result result = from_chars(s.data(), s_end, temp);
  if (result.ec == std::errc::invalid_argument || result.ptr != s_end) {
    return parse_integer_exact_error::invalid;
  } else if (result.ec == std::errc::result_out_of_range) {
    return parse_integer_exact_error::out_of_range;
  } else {
    QLJS_ASSERT(result.ec == std::errc());
    value = temp;
    return parse_integer_exact_error::ok;
  }
}

template parse_integer_exact_error parse_integer_exact(std::wstring_view,
                                                       unsigned short &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
parse_integer_exact_error parse_integer_exact(string8_view s, T &value) {
  return parse_integer_exact(to_string_view(s), value);
}

template parse_integer_exact_error parse_integer_exact(string8_view,
                                                       unsigned &value);
template parse_integer_exact_error parse_integer_exact(string8_view,
                                                       unsigned long &value);
template parse_integer_exact_error parse_integer_exact(
    string8_view, unsigned long long &value);
#endif

template <class T>
parse_integer_exact_error parse_integer_exact_hex(std::string_view s,
                                                  T &value) {
  // TODO(strager): Deduplicate with parse_integer_exact.
  const char *s_end = s.data() + s.size();
  T temp;
  from_chars_result result = from_chars_hex(s.data(), s_end, temp);
  if (result.ec == std::errc::invalid_argument || result.ptr != s_end) {
    return parse_integer_exact_error::invalid;
  } else if (result.ec == std::errc::result_out_of_range) {
    return parse_integer_exact_error::out_of_range;
  } else {
    QLJS_ASSERT(result.ec == std::errc());
    value = temp;
    return parse_integer_exact_error::ok;
  }
}

template parse_integer_exact_error parse_integer_exact_hex(
    std::string_view, unsigned char &value);
template parse_integer_exact_error parse_integer_exact_hex(std::string_view,
                                                           char32_t &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
parse_integer_exact_error parse_integer_exact_hex(string8_view s, T &value) {
  return parse_integer_exact_hex(to_string_view(s), value);
}

template parse_integer_exact_error parse_integer_exact_hex(
    string8_view, unsigned char &value);
template parse_integer_exact_error parse_integer_exact_hex(string8_view,
                                                           char32_t &value);
#endif

template char8 *write_integer<unsigned short>(unsigned short, char8 *out);
template char8 *write_integer<int>(int, char8 *out);
template char8 *write_integer<long>(long, char8 *out);
template char8 *write_integer<long long>(long long, char8 *out);
template char8 *write_integer<unsigned>(unsigned, char8 *out);
template char8 *write_integer<unsigned long>(unsigned long, char8 *out);
template char8 *write_integer<unsigned long long>(unsigned long long,
                                                  char8 *out);

#if QLJS_HAVE_CHAR8_T
template char *write_integer<unsigned short>(unsigned short, char *out);
#endif

template wchar_t *write_integer<unsigned short>(unsigned short, wchar_t *out);
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
