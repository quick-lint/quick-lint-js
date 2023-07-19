// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/cli/vim-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(Test_Vim_Location, ranges_on_first_line) {
  Padded_String code(u8"let x = 2;"_sv);
  Vim_Locator l(&code);
  Vim_Source_Range x_range = l.range(Source_Code_Span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin.lnum, 1);
  EXPECT_EQ(x_range.begin.col, 5);

  EXPECT_EQ(x_range.end.lnum, 1);
  EXPECT_EQ(x_range.end.col, 6);
}

TEST(Test_Vim_Location, ranges_on_second_line) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(
        concat(u8"let x = 2;"_sv, line_terminator, u8"let y = 3;"_sv));
    const Char8* y = strchr(code.c_str(), u8'y');
    Vim_Locator l(&code);
    Vim_Source_Range x_range = l.range(Source_Code_Span(y, y + 1));

    EXPECT_EQ(x_range.begin.lnum, 2);
    EXPECT_EQ(x_range.begin.col, 5);

    EXPECT_EQ(x_range.end.lnum, 2);
    EXPECT_EQ(x_range.end.col, 6);
  }
}

TEST(Test_Vim_Location, first_character_on_line_has_column_1) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(
        concat(u8"function f() {}"_sv, line_terminator, u8"g();"_sv));
    const Char8* g = strchr(code.c_str(), u8'g');
    Vim_Locator l(&code);
    Vim_Source_Position g_position = l.position(g);

    EXPECT_EQ(g_position.lnum, 2);
    EXPECT_EQ(g_position.col, 1);
  }
}

// TODO(strager): Is this correct? Should the behavior of \n\r depend on Vim's
// 'fileformat' setting?
TEST(Test_Vim_Location, lf_cr_is_two_line_terminators) {
  Padded_String code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const Char8* y = strchr(code.c_str(), u8'y');
  Vim_Locator l(&code);
  Vim_Source_Range y_range = l.range(Source_Code_Span(y, y + 1));

  EXPECT_EQ(y_range.begin.lnum, 3);
  EXPECT_EQ(y_range.begin.col, 5);
}

TEST(Test_Vim_Location, ls_and_ps_are_not_line_terminators) {
  for (String8_View not_line_terminator : ls_and_ps) {
    Padded_String code(
        concat(u8"let x = 2;"_sv, not_line_terminator, u8"let y = 3;"_sv));
    SCOPED_TRACE(code);
    const Char8* y = strchr(code.c_str(), u8'y');
    Vim_Locator l(&code);
    EXPECT_EQ(l.position(y).lnum, 1);
  }
}

TEST(Test_Vim_Location, location_after_null_byte) {
  Padded_String code(String8(u8"hello\0beautiful\nworld"_sv));
  const Char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  Vim_Locator l(&code);
  Vim_Source_Range r_range = l.range(Source_Code_Span(r, r + 1));

  EXPECT_EQ(r_range.begin.lnum, 2);
  EXPECT_EQ(r_range.begin.col, 3);
}

TEST(Test_Vim_Location, position_backwards) {
  Padded_String code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<Vim_Source_Position> expected_positions;
  {
    Vim_Locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<Vim_Source_Position> actual_positions;
  {
    Vim_Locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  reverse(actual_positions);

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(Test_Vim_Location, position_after_multi_byte_character) {
  {
    // U+2603 has three UTF-8 code units: e2 98 83
    Padded_String code(u8"\u2603 x"_sv);
    const Char8* x = strchr(code.c_str(), u8'x');
    Vim_Locator l(&code);
    EXPECT_EQ(l.position(x).col, 5);
  }

  {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    Padded_String code(u8"\U0001f496 x"_sv);
    const Char8* x = strchr(code.c_str(), u8'x');
    Vim_Locator l(&code);
    EXPECT_EQ(l.position(x).col, 6);
  }
}

TEST(Test_Vim_Location, position_within_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  Padded_String code(u8"\u2603"_sv);
  Vim_Locator l(&code);
  EXPECT_EQ(l.position(code.c_str() + 1).col, 2);
  EXPECT_EQ(l.position(code.c_str() + 2).col, 3);
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
