// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_cli_location, ranges_on_first_line) {
  padded_string code(u8"let x = 2;"_sv);
  cli_locator l(&code);
  cli_source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin_offset(), 4);
  EXPECT_EQ(x_range.begin().line_number, 1);
  EXPECT_EQ(x_range.begin().column_number, 5);

  EXPECT_EQ(x_range.end_offset(), 5);
  EXPECT_EQ(x_range.end().line_number, 1);
  EXPECT_EQ(x_range.end().column_number, 6);
}

TEST(test_cli_location, ranges_on_second_line) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"let x = 2;" + string8(line_terminator) +
                       u8"let y = 3;");
    const char8* y = strchr(code.c_str(), u8'y');
    cli_locator l(&code);
    cli_source_range x_range = l.range(source_code_span(y, y + 1));

    EXPECT_EQ(x_range.begin_offset(), y - code.c_str());
    EXPECT_EQ(x_range.begin().line_number, 2);
    EXPECT_EQ(x_range.begin().column_number, 5);

    EXPECT_EQ(x_range.end_offset(), y + 1 - code.c_str());
    EXPECT_EQ(x_range.end().line_number, 2);
    EXPECT_EQ(x_range.end().column_number, 6);
  }
}

TEST(test_cli_location, first_character_on_line_has_column_1) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"function f() {}" + string8(line_terminator) +
                       u8"g();");
    const char8* g = strchr(code.c_str(), u8'g');
    cli_locator l(&code);
    cli_source_position g_position = l.position(g);

    EXPECT_EQ(g_position.line_number, 2);
    EXPECT_EQ(g_position.column_number, 1);
  }
}

TEST(test_cli_location, lf_cr_is_two_line_terminators) {
  padded_string code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const char8* y = strchr(code.c_str(), u8'y');
  cli_locator l(&code);
  cli_source_range y_range = l.range(source_code_span(y, y + 1));

  EXPECT_EQ(y_range.begin_offset(), y - code.c_str());
  EXPECT_EQ(y_range.begin().line_number, 3);
  EXPECT_EQ(y_range.begin().column_number, 5);
}

TEST(test_cli_location, location_after_null_byte) {
  padded_string code(string8(u8"hello\0beautiful\nworld"_sv));
  const char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  cli_locator l(&code);
  cli_source_range r_range = l.range(source_code_span(r, r + 1));

  EXPECT_EQ(r_range.begin_offset(), r - code.c_str());
  EXPECT_EQ(r_range.begin().line_number, 2);
  EXPECT_EQ(r_range.begin().column_number, 3);
}

TEST(test_cli_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<cli_source_position> expected_positions;
  {
    cli_locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<cli_source_position> actual_positions;
  {
    cli_locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(test_cli_location, position_after_multi_byte_character) {
  {
    // U+2603 has three UTF-8 code units: e2 98 83
    padded_string code(u8"\u2603 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    cli_locator l(&code);
    EXPECT_EQ(l.position(x).column_number, 5);
  }

  {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    padded_string code(u8"\U0001f496 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    cli_locator l(&code);
    EXPECT_EQ(l.position(x).column_number, 6);
  }
}

TEST(test_cli_location, position_within_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  padded_string code(u8"\u2603"_sv);
  cli_locator l(&code);
  EXPECT_EQ(l.position(code.c_str() + 1).column_number, 2);
  EXPECT_EQ(l.position(code.c_str() + 2).column_number, 3);
}
}
}

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
