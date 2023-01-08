// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/dirty-set.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
TEST(test_dirty_set, empty) {
  dirty_set<int> s = {};
  EXPECT_EQ(s.begin(), s.end());
}

TEST(test_dirty_set, one_int) {
  dirty_set<int> s = {42};
  EXPECT_NE(s.begin(), s.end());
  EXPECT_EQ(*s.begin(), 42);
  EXPECT_EQ(std::next(s.begin(), 1), s.end());
  EXPECT_THAT(s, ElementsAreArray({42}));
}

TEST(test_dirty_set, duplicate_ints_are_deduplicated) {
  dirty_set<int> s = {42, 69, 42, 69};
  EXPECT_THAT(s, ElementsAreArray({42, 69}));
}

TEST(test_dirty_set, intersect) {
  dirty_set<int> s1 = {10, 20};
  dirty_set<int> s2 = {20, 30};
  EXPECT_THAT(s1 & s2, ElementsAreArray({20}));
}

TEST(test_dirty_set, union) {
  dirty_set<int> s1 = {10, 20};
  dirty_set<int> s2 = {20, 30};
  EXPECT_THAT(s1 | s2, ElementsAreArray({10, 20, 30}));
}

TEST(test_dirty_set, difference) {
  dirty_set<int> s1 = {10, 20};
  dirty_set<int> s2 = {20, 30};
  EXPECT_THAT(s1 - s2, ElementsAreArray({10}));
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
