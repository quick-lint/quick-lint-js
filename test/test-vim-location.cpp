// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/vim-location.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_vim_location, ranges_on_first_line) {
  padded_string code(u8"let x = 2;"_sv);
  vim_locator l(&code);
  vim_source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin.lnum, 1);
  EXPECT_EQ(x_range.begin.col, 5);

  EXPECT_EQ(x_range.end.lnum, 1);
  EXPECT_EQ(x_range.end.col, 6);
}

TEST(test_vim_location, ranges_on_second_line) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"let x = 2;" + string8(line_terminator) +
                       u8"let y = 3;");
    const char8* y = strchr(code.c_str(), u8'y');
    vim_locator l(&code);
    vim_source_range x_range = l.range(source_code_span(y, y + 1));

    EXPECT_EQ(x_range.begin.lnum, 2);
    EXPECT_EQ(x_range.begin.col, 5);

    EXPECT_EQ(x_range.end.lnum, 2);
    EXPECT_EQ(x_range.end.col, 6);
  }
}

TEST(test_vim_location, first_character_on_line_has_column_1) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"function f() {}" + string8(line_terminator) +
                       u8"g();");
    const char8* g = strchr(code.c_str(), u8'g');
    vim_locator l(&code);
    vim_source_position g_position = l.position(g);

    EXPECT_EQ(g_position.lnum, 2);
    EXPECT_EQ(g_position.col, 1);
  }
}

// TODO(strager): Is this correct? Should the behavior of \n\r depend on Vim's
// 'fileformat' setting?
TEST(test_vim_location, lf_cr_is_two_line_terminators) {
  padded_string code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const char8* y = strchr(code.c_str(), u8'y');
  vim_locator l(&code);
  vim_source_range y_range = l.range(source_code_span(y, y + 1));

  EXPECT_EQ(y_range.begin.lnum, 3);
  EXPECT_EQ(y_range.begin.col, 5);
}

TEST(test_vim_location, ls_and_ps_are_not_line_terminators) {
  for (string8_view not_line_terminator : ls_and_ps) {
    padded_string code(u8"let x = 2;" + string8(not_line_terminator) +
                       u8"let y = 3;");
    SCOPED_TRACE(code);
    const char8* y = strchr(code.c_str(), u8'y');
    vim_locator l(&code);
    EXPECT_EQ(l.position(y).lnum, 1);
  }
}

TEST(test_vim_location, location_after_null_byte) {
  padded_string code(string8(u8"hello\0beautiful\nworld"_sv));
  const char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  vim_locator l(&code);
  vim_source_range r_range = l.range(source_code_span(r, r + 1));

  EXPECT_EQ(r_range.begin.lnum, 2);
  EXPECT_EQ(r_range.begin.col, 3);
}

TEST(test_vim_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<vim_source_position> expected_positions;
  {
    vim_locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<vim_source_position> actual_positions;
  {
    vim_locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(test_vim_location, position_after_multi_byte_character) {
  {
    // U+2603 has three UTF-8 code units: e2 98 83
    padded_string code(u8"\u2603 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    vim_locator l(&code);
    EXPECT_EQ(l.position(x).col, 5);
  }

  {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    padded_string code(u8"\U0001f496 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    vim_locator l(&code);
    EXPECT_EQ(l.position(x).col, 6);
  }
}

TEST(test_vim_location, position_within_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  padded_string code(u8"\u2603"_sv);
  vim_locator l(&code);
  EXPECT_EQ(l.position(code.c_str() + 1).col, 2);
  EXPECT_EQ(l.position(code.c_str() + 2).col, 3);
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
