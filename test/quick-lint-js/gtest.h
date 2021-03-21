// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_GTEST_H
#define QUICK_LINT_JS_GTEST_H

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/have.h>
#include <string_view>

namespace testing::internal {
template <>
void PrintTo(const char32_t &, std::ostream *);

#if QLJS_HAVE_CHAR8_T
template <>
void PrintTo(const std::basic_string<char8_t> &, std::ostream *);
template <>
void PrintTo(const std::basic_string_view<char8_t> &, std::ostream *);
#endif

template <>
inline void PrintTo(const char32_t &c, std::ostream *out) {
  PrintTo(static_cast<std::uint_least32_t>(c), out);
}

#if QLJS_HAVE_CHAR8_T
template <>
inline void PrintTo(const std::basic_string<char8_t> &s, std::ostream *out) {
  PrintTo(std::basic_string_view<char8_t>(s), out);
}

template <>
inline void PrintTo(const std::basic_string_view<char8_t> &s,
                    std::ostream *out) {
  PrintTo(std::string_view(reinterpret_cast<const char *>(s.data()), s.size()),
          out);
}
#endif
}

// HACK(strager): Improve formatting of googletest diagnostics.
namespace std {
inline void PrintTo(const std::string_view &s, std::ostream *out) { *out << s; }
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
