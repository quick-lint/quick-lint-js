// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
namespace {

TEST(test_emacs_location, ascii_ranges) {
  padded_string code(u8"let x = 2;"_sv);
  emacs_locator l(&code);
  emacs_source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.begin_offset(), 5);
  EXPECT_EQ(x_range.end_offset(), 6);
}

TEST(test_emacs_location, multi_byte_ranges) {
  padded_string code(u8"/* \u263a */ let x = 2;"_sv);
  emacs_locator l(&code);
  emacs_source_range x_range = l.range(source_code_span(&code[14], &code[15]));

  EXPECT_EQ(x_range.begin_offset(), 13);
  EXPECT_EQ(x_range.end_offset(), 14);
}

TEST(test_emacs_location, location_after_null_byte) {
  padded_string code(string8(u8"hello\0beautiful\nworld"_sv));
  const char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  emacs_locator l(&code);
  emacs_source_range r_range = l.range(source_code_span(r, r + 1));

  // Emacs point starts at 1
  EXPECT_EQ(r_range.begin_offset(), r - code.c_str() + 1);
}

TEST(test_emacs_location, location_after_null_multi_byte) {
  padded_string code(string8(u8"hello\u263b\0beautiful\nworld"_sv));
  const char8* r = &code[21];
  ASSERT_EQ(*r, u8'r');

  emacs_locator l(&code);
  emacs_source_range r_range = l.range(source_code_span(r, r + 1));

  EXPECT_EQ(r_range.begin_offset(), 20);
}

TEST(test_emacs_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<emacs_source_position> expected_positions;
  {
    emacs_locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<emacs_source_position> actual_positions;
  {
    emacs_locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(test_emacs_location, position_after_multi_byte_character) {
  {
    // U+2603 has three UTF-8 code units: e2 98 83
    padded_string code(u8"\u2603 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    emacs_locator l(&code);
    EXPECT_EQ(l.position(x).offset, 3);
  }

  {
    // U+1f496 has four UTF-8 code units: f0 9f 92 96
    padded_string code(u8"\U0001f496 x"_sv);
    const char8* x = strchr(code.c_str(), u8'x');
    emacs_locator l(&code);
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
