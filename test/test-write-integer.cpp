// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gtest/gtest.h>
#include <limits>
#include <quick-lint-js/integer.h>

namespace quick_lint_js {
namespace {
template <class T>
string8 write_integer(T value) {
  std::array<char8, integer_string_length<T>> chars;
  std::fill(chars.begin(), chars.end(), 'x');
  char8* end = quick_lint_js::write_integer(value, chars.data());
  EXPECT_LE(end - chars.data(), chars.size());
  return string8(chars.data(), end);
}

TEST(test_write_integer, common_integers) {
  EXPECT_EQ(write_integer(std::size_t{0}), u8"0");
  EXPECT_EQ(write_integer(std::size_t{1234}), u8"1234");

  EXPECT_EQ(write_integer(int{0}), u8"0");
  EXPECT_EQ(write_integer(int{1234}), u8"1234");
  EXPECT_EQ(write_integer(int{-42}), u8"-42");
}

TEST(test_write_integer, maximum) {
  if constexpr (std::numeric_limits<std::size_t>::max() >= 4294967295ULL) {
    EXPECT_EQ(write_integer(std::size_t{4294967295ULL}), u8"4294967295");
  }
  if constexpr (std::numeric_limits<std::size_t>::max() >=
                18446744073709551615ULL) {
    EXPECT_EQ(write_integer(std::size_t(18446744073709551615ULL)),
              u8"18446744073709551615");
  }

  if constexpr (std::numeric_limits<int>::max() >= 2147483647LL) {
    EXPECT_EQ(write_integer(int(2147483647LL)), u8"2147483647");
  }
}

TEST(test_write_integer, minimum) {
  if constexpr (std::numeric_limits<int>::min() <= -2147483648LL) {
    EXPECT_EQ(write_integer(int(-2147483648LL)), u8"-2147483648");
  }
}
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
