// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string_view>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
streamable_string8_view::streamable_string8_view(string8_view sv) noexcept
    : sv_(sv) {}

streamable_string8_view out_string8(string8_view sv) noexcept {
  return streamable_string8_view(sv);
}
#endif

string8 to_string8(const std::string &s) {
  return string8(reinterpret_cast<const char8 *>(s.c_str()), s.size());
}

string8 to_string8(std::string_view s) { return string8(to_string8_view(s)); }

std::string to_string(const string8_view &s) {
  return std::string(reinterpret_cast<const char *>(s.data()), s.size());
}

std::string_view to_string_view(string8_view s) {
  return std::string_view(reinterpret_cast<const char *>(s.data()), s.size());
}

string8_view to_string8_view(std::string_view s) {
  return string8_view(reinterpret_cast<const char8 *>(s.data()), s.size());
}

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

char8 toupper(char8 c) noexcept {
  if (islower(c)) {
    return narrow_cast<char8>(c - (u8'a' - u8'A'));
  } else {
    return c;
  }
}

char8 tolower(char8 c) noexcept {
  if (isupper(c)) {
    return narrow_cast<char8>(c + (u8'a' - u8'A'));
  } else {
    return c;
  }
}

bool islower(char8 c) noexcept { return u8'a' <= c && c <= u8'z'; }

bool isupper(char8 c) noexcept { return u8'A' <= c && c <= u8'Z'; }
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
