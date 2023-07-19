// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_CHAR8_H
#define QUICK_LINT_JS_PORT_CHAR8_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/port/have.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
using Char8 = char8_t;
#else
using Char8 = char;
#endif

// Alias std::u8string or std::string.
using String8 = std::basic_string<Char8>;
using String8_View = std::basic_string_view<Char8>;

#if QLJS_HAVE_CHAR8_T
class Streamable_String8_View {
 public:
  friend std::ostream &operator<<(std::ostream &, Streamable_String8_View);

 private:
  explicit Streamable_String8_View(String8_View) noexcept;

  friend Streamable_String8_View out_string8(String8_View) noexcept;

  String8_View sv_;
};

Streamable_String8_View out_string8(String8_View) noexcept;
#else
inline String8_View out_string8(String8_View sv) noexcept { return sv; }
#endif

String8 to_string8(const std::string &);
String8 to_string8(std::string_view);
std::string to_string(const String8_View &);

std::string_view to_string_view(String8_View);
String8_View to_string8_view(std::string_view);

std::size_t strlen(const Char8 *);
const Char8 *strchr(const Char8 *haystack, Char8 needle);
const Char8 *strstr(const Char8 *haystack, const Char8 *needle);
std::size_t strspn(const Char8 *haystack, const Char8 *needles);

// The following functions treat ASCII A-Z as upper, a-z as lower, and all other
// code units as neither upper nor lower.
Char8 toupper(Char8) noexcept;
Char8 tolower(Char8) noexcept;
bool islower(Char8) noexcept;
bool isupper(Char8) noexcept;

bool haslower(String8_View);
bool hasupper(String8_View);

inline constexpr String8_View operator""_sv(const Char8 *string,
                                            std::size_t length) noexcept {
  return String8_View(string, length);
}

#if QLJS_HAVE_CHAR8_T
inline String8_View operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return String8_View(reinterpret_cast<const Char8 *>(string), length);
}
#else
inline String8_View operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return String8_View(string, length);
}
#endif
}

namespace testing::internal {
#if QLJS_HAVE_CHAR8_T
template <class T>
void PrintTo(const T &, std::ostream *);
template <>
void PrintTo(const char8_t &, std::ostream *);
template <>
void PrintTo(const char8_t *const &, std::ostream *);
template <>
void PrintTo(char8_t *const &, std::ostream *);
#endif
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
