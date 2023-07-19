// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
Streamable_String8_View::Streamable_String8_View(String8_View sv) noexcept
    : sv_(sv) {}

Streamable_String8_View out_string8(String8_View sv) noexcept {
  return Streamable_String8_View(sv);
}
#endif

String8 to_string8(const std::string &s) {
  return String8(reinterpret_cast<const Char8 *>(s.c_str()), s.size());
}

String8 to_string8(std::string_view s) { return String8(to_string8_view(s)); }

std::string to_string(const String8_View &s) {
  return std::string(reinterpret_cast<const char *>(s.data()), s.size());
}

std::string_view to_string_view(String8_View s) {
  return std::string_view(reinterpret_cast<const char *>(s.data()), s.size());
}

String8_View to_string8_view(std::string_view s) {
  return String8_View(reinterpret_cast<const Char8 *>(s.data()), s.size());
}

std::size_t strlen(const Char8 *s) {
  return std::strlen(reinterpret_cast<const char *>(s));
}

const Char8 *strchr(const Char8 *haystack, Char8 needle) {
  return reinterpret_cast<const Char8 *>(std::strchr(
      reinterpret_cast<const char *>(haystack), static_cast<char>(needle)));
}

const Char8 *strstr(const Char8 *haystack, const Char8 *needle) {
  return reinterpret_cast<const Char8 *>(
      std::strstr(reinterpret_cast<const char *>(haystack),
                  reinterpret_cast<const char *>(needle)));
}

std::size_t strspn(const Char8 *haystack, const Char8 *needles) {
  return std::strspn(reinterpret_cast<const char *>(haystack),
                     reinterpret_cast<const char *>(needles));
}

Char8 toupper(Char8 c) noexcept {
  if (islower(c)) {
    return narrow_cast<Char8>(c - (u8'a' - u8'A'));
  } else {
    return c;
  }
}

Char8 tolower(Char8 c) noexcept {
  if (isupper(c)) {
    return narrow_cast<Char8>(c + (u8'a' - u8'A'));
  } else {
    return c;
  }
}

bool islower(Char8 c) noexcept { return u8'a' <= c && c <= u8'z'; }

bool isupper(Char8 c) noexcept { return u8'A' <= c && c <= u8'Z'; }

bool haslower(String8_View s) {
  for (auto const &c : s) {
    if (islower(c)) {
      return true;
    }
  }
  return false;
}

bool hasupper(String8_View s) {
  for (auto const &c : s) {
    if (isupper(c)) {
      return true;
    }
  }
  return false;
}
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
