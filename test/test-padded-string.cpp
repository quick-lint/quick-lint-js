// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gtest/gtest.h>
#include <quick-lint-js/padded-string.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
TEST(test_padded_string, empty_string_has_following_null_bytes) {
  std::string s = "";
  padded_string padded(std::move(s));
  const char *data = padded.c_str();
  for (int i = 0; i < padded.padding_size; ++i) {
    EXPECT_EQ(data[i], '\0') << "i=" << i;
  }
}

TEST(test_padded_string, size_excludes_padding_bytes) {
  std::string s = "hello";
  padded_string padded(std::move(s));
  EXPECT_EQ(padded.size(), 5);
}

TEST(test_padded_string, comparing_with_string_view_excludes_padding_bytes) {
  EXPECT_TRUE(padded_string(std::string("hello")) == std::string_view("hello"));
}
}  // namespace quick_lint_js
