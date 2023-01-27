// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/c-string-list.h>
#include <string_view>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_c_string_list, iterate_empty_list) {
  static constexpr char list_data[] = "\0";
  c_string_list_view list(list_data);
  EXPECT_EQ(list.begin(), list.end());
}

TEST(test_c_string_list, iterate_one_string) {
  static constexpr char list_data[] = "hello\0";
  c_string_list_view list(list_data);

  c_string_list_iterator it = list.begin();
  EXPECT_NE(it, list.end());
  EXPECT_EQ(*it, "hello"sv);
  EXPECT_STREQ(it.c_str(), "hello");

  ++it;
  EXPECT_EQ(it, list.end());
}

TEST(test_c_string_list, iterate_many_strings) {
  static constexpr char list_data[] =
      "one\0two\0three\0four\0five\0six\0seven\0eight\0nine\0ten\0";
  c_string_list_view list(list_data);
  EXPECT_THAT(list, ElementsAreArray({
                        "one"sv,
                        "two"sv,
                        "three"sv,
                        "four"sv,
                        "five"sv,
                        "six"sv,
                        "seven"sv,
                        "eight"sv,
                        "nine"sv,
                        "ten"sv,
                    }));
}

TEST(test_c_string_list, strings_from_iteration_are_null_terminated) {
  static constexpr char list_data[] = "first\0second\0third\0";
  c_string_list_view list(list_data);

  c_string_list_iterator it = list.begin();
  EXPECT_EQ(std::strlen(it.c_str()), "first"sv.size());
  EXPECT_EQ(std::strlen((*it).data()), "first"sv.size());

  ++it;
  EXPECT_EQ(std::strlen(it.c_str()), "second"sv.size());
  EXPECT_EQ(std::strlen((*it).data()), "second"sv.size());
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
