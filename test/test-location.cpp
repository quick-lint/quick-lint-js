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

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
namespace {
std::array<const char8*, 5> line_terminators = {
    u8"\n",      //
    u8"\r",      //
    u8"\r\n",    //
    u8"\u2028",  // 0xe2 0x80 0xa8 Line Separator
    u8"\u2029",  // 0xe2 0x80 0xa9 Paragraph Separator
};

TEST(test_location, ranges_on_first_line) {
  padded_string code(u8"let x = 2;");
  locator l(&code);
  source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin_offset(), 4);
  EXPECT_EQ(x_range.begin().line_number, 1);
  EXPECT_EQ(x_range.begin().column_number, 5);

  EXPECT_EQ(x_range.end_offset(), 5);
  EXPECT_EQ(x_range.end().line_number, 1);
  EXPECT_EQ(x_range.end().column_number, 6);
}

TEST(test_location, ranges_on_second_line) {
  for (string8 line_terminator : line_terminators) {
    padded_string code(u8"let x = 2;" + line_terminator + u8"let y = 3;");
    const char8* y = strchr(code.c_str(), u8'y');
    locator l(&code);
    source_range x_range = l.range(source_code_span(y, y + 1));

    EXPECT_EQ(x_range.begin_offset(), y - code.c_str());
    EXPECT_EQ(x_range.begin().line_number, 2);
    EXPECT_EQ(x_range.begin().column_number, 5);

    EXPECT_EQ(x_range.end_offset(), y + 1 - code.c_str());
    EXPECT_EQ(x_range.end().line_number, 2);
    EXPECT_EQ(x_range.end().column_number, 6);
  }
}

TEST(test_location, lf_cr_is_two_line_terminators) {
  padded_string code(u8"let x = 2;\n\rlet y = 3;");
  const char8* y = strchr(code.c_str(), u8'y');
  locator l(&code);
  source_range x_range = l.range(source_code_span(y, y + 1));

  EXPECT_EQ(x_range.begin_offset(), y - code.c_str());
  EXPECT_EQ(x_range.begin().line_number, 3);
  EXPECT_EQ(x_range.begin().column_number, 5);
}

TEST(test_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh");

  std::vector<source_position> expected_positions;
  {
    locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<source_position> actual_positions;
  {
    locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}
}  // namespace
}  // namespace quick_lint_js
