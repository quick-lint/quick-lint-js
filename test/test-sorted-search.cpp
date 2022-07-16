// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/sorted-search.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_binary_search, empty_never_finds) {
  std::array<std::string_view, 0> data = {};
  auto result = sorted_search(data.begin(), data.end(), "hi"sv);
  EXPECT_EQ(result, data.end());
}

TEST(test_binary_search, single_item_array_with_match) {
  std::array data = {"hi"sv};
  auto result = sorted_search(data.begin(), data.end(), "hi"sv);
  EXPECT_EQ(result, data.begin());
}

TEST(test_binary_search, single_item_array_with_no_match) {
  std::array data = {"hi"sv};
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "aye"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "yo"sv), data.end());
}

TEST(test_binary_search, two_item_array) {
  std::array data = {"b"sv, "d"sv};
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "a"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "b"sv), data.begin() + 0);
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "c"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "d"sv), data.begin() + 1);
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "e"sv), data.end());
}

TEST(test_binary_search, three_item_array) {
  std::array data = {"b"sv, "d"sv, "f"sv};
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "a"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "b"sv), data.begin() + 0);
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "c"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "d"sv), data.begin() + 1);
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "e"sv), data.end());
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "f"sv), data.begin() + 2);
  EXPECT_EQ(sorted_search(data.begin(), data.end(), "g"sv), data.end());
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
