// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/string-view.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_string_view, ends_with) {
  EXPECT_FALSE(ends_with(""sv, "suffix"sv));
  EXPECT_FALSE(ends_with("test string"sv, "suffix"sv));
  EXPECT_FALSE(ends_with("suffix not present"sv, "suffix"sv));

  EXPECT_TRUE(ends_with("suffix"sv, "suffix"sv));
  EXPECT_TRUE(ends_with("test suffix"sv, "suffix"sv));
}

TEST(test_string_view, ends_with_empty_is_always_true) {
  EXPECT_TRUE(ends_with("test string"sv, ""sv));
  EXPECT_TRUE(ends_with(""sv, ""sv));
}

TEST(test_string_view, remove_suffix_if_present_not_present) {
  EXPECT_EQ(remove_suffix_if_present(""sv, "suffix"sv), ""sv);
  EXPECT_EQ(remove_suffix_if_present("test string"sv, "suffix"sv),
            "test string"sv);
  EXPECT_EQ(remove_suffix_if_present("suffix not present"sv, "suffix"sv),
            "suffix not present"sv);
}

TEST(test_string_view, remove_suffix_if_present_present) {
  EXPECT_EQ(remove_suffix_if_present("test"sv, ""sv), "test"sv);
  EXPECT_EQ(remove_suffix_if_present("suffix"sv, "suffix"sv), ""sv);
  EXPECT_EQ(remove_suffix_if_present("test suffix"sv, "suffix"sv), "test "sv);
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
