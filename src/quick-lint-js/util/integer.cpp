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
#include <iterator>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/math-overflow.h>
#include <string>
#include <type_traits>

QLJS_WARNING_IGNORE_CLANG("-Wmissing-prototypes")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wmissing-declarations")  // TODO(strager): Fix.
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
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

template <class Char, class Base, class T>
Char *write_integer_fixed_generic(T value, int width, Char *out) {
  if constexpr (std::is_signed_v<T>) {
    std::make_unsigned_t<T> unsigned_value;
    std::memcpy(&unsigned_value, &value, sizeof(T));
    if (value < 0) {
      *out++ = '-';
      unsigned_value = -unsigned_value;
      width -= 1;
    }
    return write_integer_fixed_generic<Char, Base, std::make_unsigned_t<T>>(
        unsigned_value, width, out);
  } else {
    constexpr T radix = static_cast<T>(Base::radix());
    for (int i = 0; i < width; ++i) {
      T digit = narrow_cast<T>(value % radix);
      value = narrow_cast<T>(value / radix);
      out[width - i - 1] = Base::make_digit(digit);
    }
    return out + width;
  }
}
}

template <class T>
char *write_integer(T value, char *out) {
  return write_integer_generic(value, out, '0');
}

#if QLJS_HAVE_CHAR8_T
template <class T>
Char8 *write_integer(T value, Char8 *out) {
  return write_integer_generic(value, out, u8'0');
}
#endif

template <class T>
wchar_t *write_integer(T value, wchar_t *out) {
  return write_integer_generic(value, out, L'0');
}

template <class Char>
struct Decimal {
  static bool is_digit(Char c) { return '0' <= c && c <= '9'; }
  static int parse_digit(Char c) {
    QLJS_ASSERT(is_digit(c));
    return c - '0';
  }
  static constexpr int radix() { return 10; }
};

template <class Char>
struct Hexadecimal {
  static bool is_digit(Char c) {
    return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
           ('A' <= c && c <= 'F');
  }
  static int parse_digit(Char c) {
    QLJS_ASSERT(is_digit(c));
    if ('0' <= c && c <= '9') return c - '0';
    if ('a' <= c && c <= 'f') return c - 'a' + 10;
    if ('A' <= c && c <= 'F') return c - 'A' + 10;
    QLJS_UNREACHABLE();
  }
  template <class T>
  static Char make_digit(T digit) {
    static constexpr Char digits[] = u8"0123456789abcdef";
    QLJS_ASSERT(digit >= 0);
    QLJS_ASSERT(digit < std::size(digits));
    return digits[digit];
  }
  static constexpr int radix() { return 16; }
};

template <class Char, class Base, class T>
Parse_Integer_Exact_Error parse_integer_exact_generic(
    std::basic_string_view<Char> s, T &value) {
  if (s.empty()) {
    return Parse_Integer_Exact_Error::invalid;
  }

  if constexpr (std::is_same_v<T, int>) {
    using Unsigned_T = std::make_unsigned_t<T>;
    static constexpr T result_min = std::numeric_limits<T>::min();
    static constexpr T result_max = std::numeric_limits<T>::max();
    static constexpr Unsigned_T abs_result_min =
        static_cast<Unsigned_T>(result_min);

    bool is_negative = s[0] == '-';
    if (is_negative) {
      Unsigned_T unsigned_value;
      Parse_Integer_Exact_Error parse_error =
          parse_integer_exact_generic<Char, Base>(s.substr(1), unsigned_value);
      if (parse_error != Parse_Integer_Exact_Error::ok) {
        return parse_error;
      }
      if (unsigned_value > abs_result_min) {
        return Parse_Integer_Exact_Error::out_of_range;
      }
      unsigned_value = -unsigned_value;
      std::memcpy(&value, &unsigned_value, sizeof(T));
      return Parse_Integer_Exact_Error::ok;
    } else {
      Unsigned_T unsigned_value;
      Parse_Integer_Exact_Error parse_error =
          parse_integer_exact_generic<Char, Base>(s, unsigned_value);
      if (parse_error != Parse_Integer_Exact_Error::ok) {
        return parse_error;
      }
      if (unsigned_value > static_cast<Unsigned_T>(result_max)) {
        return Parse_Integer_Exact_Error::out_of_range;
      }
      value = static_cast<T>(unsigned_value);
      return Parse_Integer_Exact_Error::ok;
    }
  } else {
    static constexpr T result_max = std::numeric_limits<T>::max();

    T result = 0;
    constexpr T radix = static_cast<T>(Base::radix());
    for (Char c : s) {
      if (!Base::is_digit(c)) {
        return Parse_Integer_Exact_Error::invalid;
      }
      if (result > result_max / radix) {
        return Parse_Integer_Exact_Error::out_of_range;
      }
      T new_result =
          static_cast<T>(result * radix + static_cast<T>(Base::parse_digit(c)));
      if (new_result < result) {
        return Parse_Integer_Exact_Error::out_of_range;
      }
      result = new_result;
    }
    value = result;
    return Parse_Integer_Exact_Error::ok;
  }
}

template <class T>
Parse_Integer_Exact_Error parse_integer_exact(std::string_view s, T &value) {
  return parse_integer_exact_generic<char, Decimal<char>, T>(s, value);
}

template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                       int &value);
template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                       unsigned short &value);
template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                       unsigned &value);
template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                       unsigned long &value);
template Parse_Integer_Exact_Error parse_integer_exact(
    std::string_view, unsigned long long &value);

template <class T>
Parse_Integer_Exact_Error parse_integer_exact(std::wstring_view s, T &value) {
  return parse_integer_exact_generic<wchar_t, Decimal<wchar_t>, T>(s, value);
}

template Parse_Integer_Exact_Error parse_integer_exact(std::wstring_view,
                                                       unsigned short &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
Parse_Integer_Exact_Error parse_integer_exact(String8_View s, T &value) {
  return parse_integer_exact(to_string_view(s), value);
}

template Parse_Integer_Exact_Error parse_integer_exact(String8_View,
                                                       unsigned short &value);
template Parse_Integer_Exact_Error parse_integer_exact(String8_View,
                                                       unsigned &value);
template Parse_Integer_Exact_Error parse_integer_exact(String8_View,
                                                       unsigned long &value);
template Parse_Integer_Exact_Error parse_integer_exact(
    String8_View, unsigned long long &value);
#endif

template <class T>
Parse_Integer_Exact_Error parse_integer_exact_hex(std::string_view s,
                                                  T &value) {
  return parse_integer_exact_generic<char, Hexadecimal<char>, T>(s, value);
}

template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned char &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(std::string_view,
                                                           char32_t &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned long &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned long long &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
Parse_Integer_Exact_Error parse_integer_exact_hex(String8_View s, T &value) {
  return parse_integer_exact_hex(to_string_view(s), value);
}

template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned char &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(String8_View,
                                                           char32_t &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned long &value);
template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned long long &value);
#endif

template Char8 *write_integer<unsigned char>(unsigned char, Char8 *out);
template Char8 *write_integer<unsigned short>(unsigned short, Char8 *out);
template Char8 *write_integer<int>(int, Char8 *out);
template Char8 *write_integer<long>(long, Char8 *out);
template Char8 *write_integer<long long>(long long, Char8 *out);
template Char8 *write_integer<unsigned>(unsigned, Char8 *out);
template Char8 *write_integer<unsigned long>(unsigned long, Char8 *out);
template Char8 *write_integer<unsigned long long>(unsigned long long,
                                                  Char8 *out);

#if QLJS_HAVE_CHAR8_T
template char *write_integer<unsigned char>(unsigned char, char *out);
template char *write_integer<unsigned short>(unsigned short, char *out);
template char *write_integer<unsigned>(unsigned, char *out);
#endif

template wchar_t *write_integer<unsigned short>(unsigned short, wchar_t *out);

template <class T>
Char8 *write_integer_fixed_hexadecimal(T value, int width, Char8 *out) {
  return write_integer_fixed_generic<Char8, Hexadecimal<Char8>, T>(value, width,
                                                                   out);
}

template Char8 *write_integer_fixed_hexadecimal<int>(int, int width,
                                                     Char8 *out);
template Char8 *write_integer_fixed_hexadecimal<unsigned>(unsigned, int width,
                                                          Char8 *out);
template Char8 *write_integer_fixed_hexadecimal<unsigned long>(unsigned long,
                                                               int width,
                                                               Char8 *out);
template Char8 *write_integer_fixed_hexadecimal<unsigned long long>(
    unsigned long long, int width, Char8 *out);
template Char8 *write_integer_fixed_hexadecimal<char32_t>(char32_t, int width,
                                                          Char8 *out);
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
