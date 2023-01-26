// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <limits>
#include <optional>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
namespace {
template <class BoolVector16>
class test_math_checked_add_signed : public ::testing::Test {};
using signed_types = ::testing::Types<int>;
TYPED_TEST_SUITE(test_math_checked_add_signed, signed_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_math_checked_add_signed, small_in_range) {
  using I = TypeParam;
  EXPECT_EQ(checked_add(I{2}, I{3}), I{5});
  EXPECT_EQ(checked_add(I{-2}, I{-3}), I{-5});
  EXPECT_EQ(checked_add(I{-2}, I{3}), I{1});
  EXPECT_EQ(checked_add(I{2}, I{-3}), I{-1});
}

TYPED_TEST(test_math_checked_add_signed, near_min) {
  using I = TypeParam;
  I low = std::numeric_limits<I>::lowest();
  EXPECT_EQ(checked_add(low, I{0}), low);
  EXPECT_EQ(checked_add(I{0}, low), low);
  EXPECT_EQ(checked_add(low + I{1}, I{-1}), low);
  EXPECT_EQ(checked_add(I{-1}, low + I{1}), low);
  EXPECT_EQ(checked_add(low + I{3}, I{-1}), low + I{2});
  EXPECT_EQ(checked_add(I{-1}, low + I{3}), low + I{2});
  EXPECT_EQ(checked_add(low + I{100}, I{-100}), low);
  EXPECT_EQ(checked_add(I{-100}, low + I{100}), low);
}

TYPED_TEST(test_math_checked_add_signed, near_max) {
  using I = TypeParam;
  I high = (std::numeric_limits<I>::max)();
  EXPECT_EQ(checked_add(high, I{0}), high);
  EXPECT_EQ(checked_add(I{0}, high), high);
  EXPECT_EQ(checked_add(high - I{1}, I{1}), high);
  EXPECT_EQ(checked_add(I{1}, high - I{1}), high);
  EXPECT_EQ(checked_add(high - I{3}, I{1}), high - I{2});
  EXPECT_EQ(checked_add(I{1}, high - I{3}), high - I{2});
  EXPECT_EQ(checked_add(high - I{100}, I{100}), high);
  EXPECT_EQ(checked_add(I{100}, high - I{100}), high);
}

TYPED_TEST(test_math_checked_add_signed, over_max) {
  using I = TypeParam;
  I high = (std::numeric_limits<I>::max)();
  EXPECT_EQ(checked_add(high, I{1}), std::nullopt);
  EXPECT_EQ(checked_add(I{1}, high), std::nullopt);
  EXPECT_EQ(checked_add(high, high), std::nullopt);
  EXPECT_EQ(checked_add(high / I{2} + I{1}, high / I{2} + I{1}), std::nullopt);
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
