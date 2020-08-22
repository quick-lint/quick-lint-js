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

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_location, ranges_on_first_line) {
  const char code[] = "let x = 2;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin_offset(), 4);
  EXPECT_EQ(x_range.begin().line_number, 1);
  EXPECT_EQ(x_range.begin().column_number, 5);

  EXPECT_EQ(x_range.end_offset(), 5);
  EXPECT_EQ(x_range.end().line_number, 1);
  EXPECT_EQ(x_range.end().column_number, 6);
}

TEST(test_location, ranges_on_second_line) {
  const char code[] = "let x = 2;\nlet y = 3;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[15], &code[16]));

  EXPECT_EQ(x_range.begin_offset(), 15);
  EXPECT_EQ(x_range.begin().line_number, 2);
  EXPECT_EQ(x_range.begin().column_number, 5);

  EXPECT_EQ(x_range.end_offset(), 16);
  EXPECT_EQ(x_range.end().line_number, 2);
  EXPECT_EQ(x_range.end().column_number, 6);
}

TEST(test_location, position_backwards) {
  const char code[] = "ab\nc\n\nd\nefg\nh";

  std::vector<source_position> expected_positions;
  {
    locator l(code);
    for (int i = 0; i < narrow_cast<int>(std::strlen(code)); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<source_position> actual_positions;
  {
    locator l(code);
    for (int i = narrow_cast<int>(std::strlen(code)) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}
}  // namespace
}  // namespace quick_lint_js
