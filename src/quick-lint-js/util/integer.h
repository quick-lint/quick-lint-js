// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <limits>
#include <quick-lint-js/port/char8.h>
#include <string_view>

namespace quick_lint_js {
enum class Parse_Integer_Exact_Error {
  ok,
  out_of_range,
  invalid,
};

template <class T>
Parse_Integer_Exact_Error parse_integer_exact(std::string_view, T &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                              int &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    std::string_view, unsigned short &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(std::string_view,
                                                              unsigned &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    std::string_view, unsigned long &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    std::string_view, unsigned long long &value);

template <class T>
Parse_Integer_Exact_Error parse_integer_exact(std::wstring_view, T &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    std::wstring_view, unsigned short &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
Parse_Integer_Exact_Error parse_integer_exact(String8_View, T &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    String8_View, unsigned short &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(String8_View,
                                                              unsigned &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    String8_View, unsigned long &value);
extern template Parse_Integer_Exact_Error parse_integer_exact(
    String8_View, unsigned long long &value);
#endif

template <class T>
Parse_Integer_Exact_Error parse_integer_exact_hex(std::string_view, T &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned char &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, char32_t &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned long &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    std::string_view, unsigned long long &value);

#if QLJS_HAVE_CHAR8_T
template <class T>
Parse_Integer_Exact_Error parse_integer_exact_hex(String8_View, T &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned char &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, char32_t &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned long &value);
extern template Parse_Integer_Exact_Error parse_integer_exact_hex(
    String8_View, unsigned long long &value);
#endif

template <class T>
inline constexpr int integer_string_length =
    (std::numeric_limits<T>::digits10 + 1) +
    (std::numeric_limits<T>::is_signed ? 1 : 0);

template <class T>
Char8 *write_integer(T, Char8 *out);

extern template Char8 *write_integer<unsigned char>(unsigned char, Char8 *out);
extern template Char8 *write_integer<unsigned short>(unsigned short,
                                                     Char8 *out);
extern template Char8 *write_integer<int>(int, Char8 *out);
extern template Char8 *write_integer<long>(long, Char8 *out);
extern template Char8 *write_integer<long long>(long long, Char8 *out);
extern template Char8 *write_integer<unsigned>(unsigned, Char8 *out);
extern template Char8 *write_integer<unsigned long>(unsigned long, Char8 *out);
extern template Char8 *write_integer<unsigned long long>(unsigned long long,
                                                         Char8 *out);

#if QLJS_HAVE_CHAR8_T
template <class T>
char *write_integer(T, char *out);

extern template char *write_integer<unsigned char>(unsigned char, char *out);
extern template char *write_integer<unsigned short>(unsigned short, char *out);
extern template char *write_integer<unsigned>(unsigned, char *out);
#endif

template <class T>
wchar_t *write_integer(T, wchar_t *out);

extern template wchar_t *write_integer<unsigned short>(unsigned short,
                                                       wchar_t *out);

template <class T>
Char8 *write_integer_fixed_hexadecimal(T, int width, Char8 *out);

extern template Char8 *write_integer_fixed_hexadecimal<int>(int, int width,
                                                            Char8 *out);
extern template Char8 *write_integer_fixed_hexadecimal<unsigned>(unsigned,
                                                                 int width,
                                                                 Char8 *out);
extern template Char8 *write_integer_fixed_hexadecimal<unsigned long>(
    unsigned long, int width, Char8 *out);
extern template Char8 *write_integer_fixed_hexadecimal<unsigned long long>(
    unsigned long long, int width, Char8 *out);
extern template Char8 *write_integer_fixed_hexadecimal<char32_t>(char32_t,
                                                                 int width,
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
