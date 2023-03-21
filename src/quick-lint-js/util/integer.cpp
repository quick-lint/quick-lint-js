// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cerrno>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <type_traits>

QLJS_WARNING_IGNORE_CLANG("-Wmissing-prototypes")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wmissing-declarations")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
template <class Char>
struct from_chars_result {
  const Char *ptr;
  std::errc ec;
};

template <class Base, class T>
from_chars_result<char> from_chars_generic(const char *begin, const char *end,
                                           T &value) {
  if (end == begin) {
    return from_chars_result<char>{.ptr = begin,
                                   .ec = std::errc::invalid_argument};
  }

  if constexpr (std::is_same_v<T, int>) {
    using unsigned_t = std::make_unsigned_t<T>;
    static constexpr T result_min = std::numeric_limits<T>::min();
    static constexpr T result_max = std::numeric_limits<T>::max();
    static constexpr unsigned_t abs_result_min =
        static_cast<unsigned_t>(result_min);

    bool is_negative = *begin == '-';
    if (is_negative) {
      unsigned_t unsigned_value;
      from_chars_result<char> result =
          from_chars_generic<Base>(begin + 1, end, unsigned_value);
      if (result.ec != std::errc()) {
        return result;
      }
      if (unsigned_value > abs_result_min) {
        return from_chars_result<char>{.ptr = result.ptr,
                                       .ec = std::errc::result_out_of_range};
      }
      unsigned_value = -unsigned_value;
      std::memcpy(&value, &unsigned_value, sizeof(T));
      return result;
    } else {
      unsigned_t unsigned_value;
      from_chars_result<char> result =
          from_chars_generic<Base>(begin, end, unsigned_value);
      if (result.ec != std::errc()) {
        return result;
      }
      if (unsigned_value > static_cast<unsigned_t>(result_max)) {
        return from_chars_result<char>{.ptr = result.ptr,
                                       .ec = std::errc::result_out_of_range};
      }
      value = static_cast<T>(unsigned_value);
      return result;
    }
  } else {
    static constexpr T result_max = std::numeric_limits<T>::max();

    if (!Base::is_digit(*begin)) {
      return from_chars_result<char>{.ptr = begin,
                                     .ec = std::errc::invalid_argument};
    }
    const char *c = begin;
    auto out_of_range = [&c, end]() -> from_chars_result<char> {
      for (; Base::is_digit(*c) && c != end; ++c) {
        // Skip digits.
      }
      return from_chars_result<char>{.ptr = c,
                                     .ec = std::errc::result_out_of_range};
    };

    T result = 0;
    for (; Base::is_digit(*c) && c != end; ++c) {
      if (result > result_max / Base::radix()) {
        return out_of_range();
      }
      T new_result = static_cast<T>(result * Base::radix() +
                                    static_cast<T>(Base::parse_digit(*c)));
      if (new_result < result) {
        return out_of_range();
      }
      result = new_result;
    }
    value = result;
    return from_chars_result<char>{.ptr = c, .ec = std::errc{0}};
  }
}

template <class T>
from_chars_result<char> from_chars(const char *begin, const char *end,
                                   T &value) {
  struct decimal {
    static bool is_digit(char c) { return '0' <= c && c <= '9'; }
    static int parse_digit(char c) {
      QLJS_ASSERT(is_digit(c));
      return c - '0';
    }
    static constexpr int radix() { return 10; }
  };
  return from_chars_generic<decimal>(begin, end, value);
}

template <class T>
from_chars_result<char> from_chars_hex(const char *begin, const char *end,
                                       T &value) {
  struct hexadecimal {
    static bool is_digit(char c) {
      return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
             ('A' <= c && c <= 'F');
    }
    static int parse_digit(char c) {
      QLJS_ASSERT(is_digit(c));
      if ('0' <= c && c <= '9') return c - '0';
      if ('a' <= c && c <= 'f') return c - 'a' + 10;
      if ('A' <= c && c <= 'F') return c - 'A' + 10;
      QLJS_UNREACHABLE();
    }
    static constexpr int radix() { return 16; }
  };
  return from_chars_generic<hexadecimal>(begin, end, value);
}

namespace {
template <class Char, class T>
Char *write_integer_generic(T value, Char *out, Char zero_digit) {
  if constexpr (std::is_signed_v<T>) {
    std::make_unsigned_t<T> unsigned_value;
    std::memcpy(&unsigned_value, &value, sizeof(T));
    if (value < 0) {
      *out++ = '-';
      unsigned_value = -unsigned_value;
    }
    return write_integer_generic(unsigned_value, out, zero_digit);
  } else {
    if (value == 0) {
      *out++ = '0';
    } else {
      Char *begin = out;
      while (value > 0) {
        T digit = narrow_cast<T>(value % 10);
        value = narrow_cast<T>(value / 10);
        *out++ = narrow_cast<Char>(static_cast<T>(zero_digit) + digit);
      }
      std::reverse(begin, out);
    }
    return out;
  }
}
}

template <class T>
char *write_integer(T value, char *out) {
  return write_integer_generic(value, out, '0');
}

#if QLJS_HAVE_CHAR8_T
template <class T>
char8 *write_integer(T value, char8 *out) {
  return write_integer_generic(value, out, u8'0');
}
#endif

template <class T>
wchar_t *write_integer(T value, wchar_t *out) {
  return write_integer_generic(value, out, L'0');
}

from_chars_result<char8> from_char8s(const char8 *begin, const char8 *end,
                                     std::size_t &value) {
  from_chars_result<char> result =
      from_chars(reinterpret_cast<const char *>(begin),
                 reinterpret_cast<const char *>(end), value);
  return from_chars_result<char8>{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

template <class T>
from_chars_result<wchar_t> from_chars(const wchar_t *begin, const wchar_t *end,
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

  from_chars_result<char> result = from_chars(buffer.data(), out_end, value);
  return from_chars_result<wchar_t>{
      .ptr = &begin[result.ptr - buffer.data()],
      .ec = result.ec,
  };
}

from_chars_result<char8> from_char8s_hex(const char8 *begin, const char8 *end,
                                         char32_t &value) {
  from_chars_result<char> result =
      from_chars_hex(reinterpret_cast<const char *>(begin),
                     reinterpret_cast<const char *>(end), value);
  return from_chars_result<char8>{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

from_chars_result<char8> from_char8s_hex(const char8 *begin, const char8 *end,
                                         unsigned char &value) {
  from_chars_result<char> result =
      from_chars_hex(reinterpret_cast<const char *>(begin),
                     reinterpret_cast<const char *>(end), value);
  return from_chars_result<char8>{
      .ptr = reinterpret_cast<const char8 *>(result.ptr),
      .ec = result.ec,
  };
}

template <class T>
parse_integer_exact_error parse_integer_exact(std::string_view s, T &value) {
  const char *s_end = s.data() + s.size();
  T temp;
  from_chars_result<char> result = from_chars(s.data(), s_end, temp);
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
  from_chars_result<wchar_t> result = from_chars(s.data(), s_end, temp);
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
  from_chars_result<char> result = from_chars_hex(s.data(), s_end, temp);
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
template char *write_integer<unsigned>(unsigned, char *out);
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
