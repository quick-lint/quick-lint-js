// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <limits>
#include <optional>
#include <quick-lint-js/math-overflow.h>
#include <quick-lint-js/narrow-cast.h>

using int_limits = std::numeric_limits<int>;

namespace quick_lint_js {
namespace {
TEST(test_math_checked_add_int, small_in_range) {
  EXPECT_EQ(checked_add(2, 3), 5);
  EXPECT_EQ(checked_add(-2, -3), -5);
  EXPECT_EQ(checked_add(-2, 3), 1);
  EXPECT_EQ(checked_add(2, -3), -1);
}

TEST(test_math_checked_add_int, near_min) {
  int low = int_limits::lowest();
  EXPECT_EQ(checked_add(low, 0), low);
  EXPECT_EQ(checked_add(0, low), low);
  EXPECT_EQ(checked_add(low + 1, -1), low);
  EXPECT_EQ(checked_add(-1, low + 1), low);
  EXPECT_EQ(checked_add(low + 3, -1), low + 2);
  EXPECT_EQ(checked_add(-1, low + 3), low + 2);
  EXPECT_EQ(checked_add(low + 100, -100), low);
  EXPECT_EQ(checked_add(-100, low + 100), low);
}

TEST(test_math_checked_add_int, near_max) {
  int high = (int_limits::max)();
  EXPECT_EQ(checked_add(high, 0), high);
  EXPECT_EQ(checked_add(0, high), high);
  EXPECT_EQ(checked_add(high - 1, 1), high);
  EXPECT_EQ(checked_add(1, high - 1), high);
  EXPECT_EQ(checked_add(high - 3, 1), high - 2);
  EXPECT_EQ(checked_add(1, high - 3), high - 2);
  EXPECT_EQ(checked_add(high - 100, 100), high);
  EXPECT_EQ(checked_add(100, high - 100), high);
}

TEST(test_math_checked_add_int, over_max) {
  int high = (int_limits::max)();
  EXPECT_EQ(checked_add(high, 1), std::nullopt);
  EXPECT_EQ(checked_add(1, high), std::nullopt);
  EXPECT_EQ(checked_add(high, high), std::nullopt);
  EXPECT_EQ(checked_add(high / 2 + 1, high / 2 + 1), std::nullopt);
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
