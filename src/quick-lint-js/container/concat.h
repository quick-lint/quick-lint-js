// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_CONCAT_H
#define QUICK_LINT_JS_CONTAINER_CONCAT_H

#include <quick-lint-js/port/char8.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
// Do not call directly.
template <class Char, class... Rest>
std::basic_string<Char> concat_impl(std::basic_string_view<Char> a,
                                    Rest... rest) {
  std::basic_string<Char> result;
  result.reserve((a.size() + ... + rest.size()));
  result.append(a);
  (..., result.append(rest));
  return result;
}

// Concatenates (joins) two or more strings.
//
// Equivalent to (std::string(a) + std::string(b) + ...).
//
// concat is implemented as function overloads, not a template, to allow
// implicit conversions and reduce template bloat.
[[gnu::always_inline]] inline std::string concat(std::string_view a,
                                                 std::string_view b) {
  return concat_impl(a, b);
}

[[gnu::always_inline]] inline std::string concat(std::string_view a,
                                                 std::string_view b,
                                                 std::string_view c) {
  return concat_impl(a, b, c);
}

[[gnu::always_inline]] inline std::string concat(std::string_view a,
                                                 std::string_view b,
                                                 std::string_view c,
                                                 std::string_view d) {
  return concat_impl(a, b, c, d);
}

[[gnu::always_inline]] inline std::string concat(std::string_view a,
                                                 std::string_view b,
                                                 std::string_view c,
                                                 std::string_view d,
                                                 std::string_view e) {
  return concat_impl(a, b, c, d, e);
}

[[gnu::always_inline]] inline std::string concat(
    std::string_view a, std::string_view b, std::string_view c,
    std::string_view d, std::string_view e, std::string_view f) {
  return concat_impl(a, b, c, d, e, f);
}

[[gnu::always_inline]] inline std::string concat(
    std::string_view a, std::string_view b, std::string_view c,
    std::string_view d, std::string_view e, std::string_view f,
    std::string_view g) {
  return concat_impl(a, b, c, d, e, f, g);
}

// Discourage using string literals.
// clang-format off
std::string concat(const char*, std::string_view) = delete;
std::string concat(std::string_view, const char*) = delete;

std::string concat(const char*, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, const char*, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, const char*) = delete;

std::string concat(const char*, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, const char*, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, const char*, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, const char*) = delete;

std::string concat(const char*, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, const char*, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, const char*, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, const char*, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, const char*) = delete;

std::string concat(const char*, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, const char*, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, const char*, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, const char*, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, const char*, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, const char*) = delete;

std::string concat(const char*, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, const char*, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, const char*, std::string_view, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, const char*, std::string_view, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, const char*, std::string_view, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, const char*, std::string_view) = delete;
std::string concat(std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, const char*) = delete;
// clang-format on

#if QLJS_HAVE_CHAR8_T
[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b) {
  return concat_impl(a, b);
}

[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b,
                                             String8_View c) {
  return concat_impl(a, b, c);
}

[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b,
                                             String8_View c, String8_View d) {
  return concat_impl(a, b, c, d);
}

[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b,
                                             String8_View c, String8_View d,
                                             String8_View e) {
  return concat_impl(a, b, c, d, e);
}

[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b,
                                             String8_View c, String8_View d,
                                             String8_View e, String8_View f) {
  return concat_impl(a, b, c, d, e, f);
}

[[gnu::always_inline]] inline String8 concat(String8_View a, String8_View b,
                                             String8_View c, String8_View d,
                                             String8_View e, String8_View f,
                                             String8_View g) {
  return concat_impl(a, b, c, d, e, f, g);
}

// Discourage using string literals.
// clang-format off
String8 concat(const Char8*, String8_View) = delete;
String8 concat(String8_View, const Char8*) = delete;

String8 concat(const Char8*, String8_View, String8_View) = delete;
String8 concat(String8_View, const Char8*, String8_View) = delete;
String8 concat(String8_View, String8_View, const Char8*) = delete;

String8 concat(const Char8*, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, const Char8*, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, const Char8*, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, const Char8*) = delete;

String8 concat(const Char8*, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, const Char8*, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, const Char8*, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, const Char8*, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, const Char8*) = delete;

String8 concat(const Char8*, String8_View, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, const Char8*, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, const Char8*, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, const Char8*, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, const Char8*, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, String8_View, const Char8*) = delete;

String8 concat(const Char8*, String8_View, String8_View, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, const Char8*, String8_View, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, const Char8*, String8_View, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, const Char8*, String8_View, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, const Char8*, String8_View, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, String8_View, const Char8*, String8_View) = delete;
String8 concat(String8_View, String8_View, String8_View, String8_View, String8_View, String8_View, const Char8*) = delete;
// clang-format on
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
