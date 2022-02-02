// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CHAR8_H
#define QUICK_LINT_JS_CHAR8_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
using char8 = char8_t;
#else
using char8 = char;
#endif

// Alias std::u8string or std::string.
using string8 = std::basic_string<char8>;
using string8_view = std::basic_string_view<char8>;

#if QLJS_HAVE_CHAR8_T
class streamable_string8_view {
 public:
  friend std::ostream &operator<<(std::ostream &, streamable_string8_view);

 private:
  explicit streamable_string8_view(string8_view) noexcept;

  friend streamable_string8_view out_string8(string8_view) noexcept;

  string8_view sv_;
};

streamable_string8_view out_string8(string8_view) noexcept;
#else
inline string8_view out_string8(string8_view sv) noexcept { return sv; }
#endif

string8 to_string8(const std::string &);
std::string to_string(const string8_view &);

std::string_view to_string_view(string8_view);
string8_view to_string8_view(std::string_view);

std::size_t strlen(const char8 *);
const char8 *strchr(const char8 *haystack, char8 needle);
const char8 *strstr(const char8 *haystack, const char8 *needle);
std::size_t strspn(const char8 *haystack, const char8 *needles);

// The following functions treat ASCII A-Z as upper, a-z as lower, and all other
// code units as neither upper nor lower.
char8 toupper(char8) noexcept;
char8 tolower(char8) noexcept;
bool islower(char8) noexcept;
bool isupper(char8) noexcept;

inline constexpr string8_view operator""_sv(const char8 *string,
                                            std::size_t length) noexcept {
  return string8_view(string, length);
}

#if QLJS_HAVE_CHAR8_T
inline string8_view operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return string8_view(reinterpret_cast<const char8 *>(string), length);
}
#else
inline string8_view operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return string8_view(string, length);
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
