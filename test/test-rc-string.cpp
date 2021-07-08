// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/rc-string.h>

namespace quick_lint_js {
namespace {
TEST(test_rc_string, default_is_empty_string) {
  rc_string s;
  EXPECT_STREQ(s.c_str(), "");
}

TEST(test_rc_string, new_from_c_string_copies) {
  const char* original = "hello, world!";
  rc_string copy = rc_string::copy_c_string(original);
  EXPECT_NE(original, copy.c_str());
}

TEST(test_rc_string, adopt_from_c_string_shares) {
  const char* original = "hello, world!";
  rc_string s = rc_string::adopt_c_string(original);
  EXPECT_EQ(s.c_str(), original);
}

TEST(test_rc_string, copying_rc_string_shares_data) {
  rc_string original = rc_string::copy_c_string("hello");
  rc_string copy = original;
  EXPECT_EQ(original.c_str(), copy.c_str());
  EXPECT_STREQ(copy.c_str(), "hello");
}

TEST(test_rc_string, moving_rc_string_shares_data) {
  rc_string original = rc_string::copy_c_string("hello");
  const char* original_c_str = original.c_str();
  rc_string s = std::move(original);
  EXPECT_EQ(s.c_str(), original_c_str);
  EXPECT_STREQ(s.c_str(), "hello");
}

TEST(test_rc_string, moving_rc_string_clears_original) {
  rc_string original = rc_string::copy_c_string("hello");
  [[maybe_unused]] rc_string s = std::move(original);
  EXPECT_STREQ(original.c_str(), "");
}

TEST(test_rc_string, copy_assigning_rc_string_shares_data) {
  rc_string original = rc_string::copy_c_string("hello");
  rc_string copy;
  copy = original;
  EXPECT_EQ(original.c_str(), copy.c_str());
  EXPECT_STREQ(copy.c_str(), "hello");
}

TEST(test_rc_string, move_assigning_rc_string_shares_data) {
  rc_string original = rc_string::copy_c_string("hello");
  const char* original_c_str = original.c_str();
  rc_string s;
  s = std::move(original);
  EXPECT_EQ(s.c_str(), original_c_str);
  EXPECT_STREQ(s.c_str(), "hello");
}

TEST(test_rc_string, move_assigning_rc_string_clears_original) {
  rc_string original = rc_string::copy_c_string("hello");
  rc_string s;
  s = std::move(original);
  EXPECT_STREQ(original.c_str(), "");
}

TEST(test_rc_string, destroying_copy_does_not_change_data) {
  rc_string original = rc_string::copy_c_string("hello");
  const char* original_c_str = original.c_str();
  { [[maybe_unused]] rc_string copy = original; }
  EXPECT_EQ(original.c_str(), original_c_str);
  EXPECT_STREQ(original.c_str(), "hello");
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
