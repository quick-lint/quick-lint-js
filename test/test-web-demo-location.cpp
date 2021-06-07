// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_web_demo_location, ranges_on_first_line) {
  padded_string code(u8"let x = 2;"_sv);
  web_demo_locator l(&code);
  web_demo_source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin, 4);
  EXPECT_EQ(x_range.end, 5);
}

TEST(test_web_demo_location, ranges_on_second_line) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"let x = 2;" + string8(line_terminator) +
                       u8"let y = 3;");
    const char8* y = strchr(code.c_str(), u8'y');
    web_demo_locator l(&code);
    web_demo_source_range x_range = l.range(source_code_span(y, y + 1));

    EXPECT_EQ(x_range.begin, y - code.c_str());
    EXPECT_EQ(x_range.end, y + 1 - code.c_str());
  }
}

TEST(test_web_demo_location, lf_cr_is_two_line_terminators) {
  padded_string code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const char8* y = strchr(code.c_str(), u8'y');
  web_demo_locator l(&code);
  web_demo_source_range y_range = l.range(source_code_span(y, y + 1));

  EXPECT_EQ(y_range.begin, y - code.c_str());
}

TEST(test_web_demo_location, location_after_null_byte) {
  padded_string code(string8(u8"hello\0beautiful\nworld"_sv));
  const char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  web_demo_locator l(&code);
  web_demo_source_range r_range = l.range(source_code_span(r, r + 1));

  EXPECT_EQ(r_range.begin, r - code.c_str());
}

TEST(test_web_demo_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<web_demo_source_offset> expected_positions;
  {
    web_demo_locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<web_demo_source_offset> actual_positions;
  {
    web_demo_locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(test_web_demo_location, position_after_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  padded_string code(u8"\u2603 x"_sv);
  const char8* x = strchr(code.c_str(), u8'x');
  web_demo_locator l(&code);
  EXPECT_EQ(l.position(x), 2);
}

TEST(test_web_demo_location, position_after_wide_multi_byte_character) {
  // U+1f496 has four UTF-8 code units: f0 9f 92 96
  // U+1f496 has two UTF-16 code units: D83D DC96
  padded_string code(u8"\U0001f496 x"_sv);
  const char8* x = strchr(code.c_str(), u8'x');
  web_demo_locator l(&code);
  EXPECT_EQ(l.position(x), 3);
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
