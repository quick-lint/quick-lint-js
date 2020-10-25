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

#include <cstddef>
#include <cstring>
#include <iostream>
#include <quick-lint-js/char8.h>
#include <string_view>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
streamable_string8_view::streamable_string8_view(string8_view sv) noexcept
    : sv_(sv) {}

std::ostream &operator<<(std::ostream &out, streamable_string8_view sv) {
  out << std::string_view(reinterpret_cast<const char *>(sv.sv_.data()),
                          sv.sv_.size());
  return out;
}

streamable_string8_view out_string8(string8_view sv) noexcept {
  return streamable_string8_view(sv);
}
#endif

#if QLJS_HAVE_CHAR8_T
string8 to_string8(const std::string &s) {
  return string8(reinterpret_cast<const char8 *>(s.c_str()), s.size());
}
#else
string8 to_string8(const std::string &s) { return s; }
#endif

#if QLJS_HAVE_CHAR8_T
std::size_t strlen(const char8 *s) {
  return std::strlen(reinterpret_cast<const char *>(s));
}

const char8 *strchr(const char8 *haystack, char8 needle) {
  return reinterpret_cast<const char8 *>(std::strchr(
      reinterpret_cast<const char *>(haystack), static_cast<char>(needle)));
}

const char8 *strstr(const char8 *haystack, const char8 *needle) {
  return reinterpret_cast<const char8 *>(
      std::strstr(reinterpret_cast<const char *>(haystack),
                  reinterpret_cast<const char *>(needle)));
}

std::size_t strspn(const char8 *haystack, const char8 *needles) {
  return std::strspn(reinterpret_cast<const char *>(haystack),
                     reinterpret_cast<const char *>(needles));
}
#else
std::size_t strlen(const char8 *s) { return std::strlen(s); }

const char8 *strchr(const char8 *haystack, char8 needle) {
  return std::strchr(haystack, needle);
}

const char8 *strstr(const char8 *haystack, const char8 *needle) {
  return std::strstr(haystack, needle);
}

std::size_t strspn(const char8 *haystack, const char8 *needles) {
  return std::strspn(haystack, needles);
}
#endif
}

namespace testing::internal {
#if QLJS_HAVE_CHAR8_T
template <>
void PrintTo(const char8_t &c, std::ostream *out) {
  *out << static_cast<char>(c);
}

template <>
void PrintTo(const char8_t *const &s, std::ostream *out) {
  *out << reinterpret_cast<const char *>(s);
}
#endif
}
