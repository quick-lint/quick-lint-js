// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/cli/emacs-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(Test_Emacs_Location, ascii_ranges) {
  Padded_String code(u8"let x = 2;"_sv);
  Emacs_Locator l(&code);
  Emacs_Source_Range x_range = l.range(Source_Code_Span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin_offset(), 5);
  EXPECT_EQ(x_range.end_offset(), 6);
}

TEST(Test_Emacs_Location, multi_byte_ranges) {
  Padded_String code(u8"/* \u263a */ let x = 2;"_sv);
  Emacs_Locator l(&code);
  Emacs_Source_Range x_range = l.range(Source_Code_Span(&code[14], &code[15]));

  EXPECT_EQ(x_range.begin_offset(), 13);
  EXPECT_EQ(x_range.end_offset(), 14);
}

TEST(Test_Emacs_Location, location_after_null_byte) {
  Padded_String code(String8(u8"hello\0beautiful\nworld"_sv));
  const Char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  Emacs_Locator l(&code);
  Emacs_Source_Range r_range = l.range(Source_Code_Span(r, r + 1));

  // Emacs point starts at 1
  EXPECT_EQ(r_range.begin_offset(), r - code.c_str() + 1);
}

TEST(Test_Emacs_Location, location_after_null_multi_byte) {
  Padded_String code(String8(u8"hello\u263b\0beautiful\nworld"_sv));
  const Char8* r = &code[21];
  ASSERT_EQ(*r, u8'r');

  Emacs_Locator l(&code);
  Emacs_Source_Range r_range = l.range(Source_Code_Span(r, r + 1));

  EXPECT_EQ(r_range.begin_offset(), 20);
}

TEST(Test_Emacs_Location, position_backwards) {
  Padded_String code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<Emacs_Source_Position> expected_positions;
  {
    Emacs_Locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<Emacs_Source_Position> actual_positions;
  {
    Emacs_Locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  reverse(actual_positions);

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(Test_Emacs_Location, position_after_multi_byte_character) {
  {
    // U+2603 has three UTF-8 code units: e2 98 83
    Padded_String code(u8"\u2603 x"_sv);
    const Char8* x = strchr(code.c_str(), u8'x');
    Emacs_Locator l(&code);
    EXPECT_EQ(l.position(x).offset, 3);
  }

  {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    Padded_String code(u8"\U0001f496 x"_sv);
    const Char8* x = strchr(code.c_str(), u8'x');
    Emacs_Locator l(&code);
    EXPECT_EQ(l.position(x).offset, 3);
  }
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
