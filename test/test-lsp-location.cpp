// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_lsp_location, ranges_on_first_line) {
  padded_string code(u8"let x = 2;"_sv);
  lsp_locator l(&code);
  lsp_range x_range = l.range(source_code_span(&code[4], &code[5]));

  EXPECT_EQ(x_range.start.line, 0);
  EXPECT_EQ(x_range.start.character, 4);

  EXPECT_EQ(x_range.end.line, 0);
  EXPECT_EQ(x_range.end.character, 5);
}

TEST(test_lsp_location, ranges_on_second_line) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"let x = 2;" + string8(line_terminator) +
                       u8"let y = 3;");
    SCOPED_TRACE(code);
    const char8* y = strchr(code.c_str(), u8'y');
    lsp_locator l(&code);
    lsp_range x_range = l.range(source_code_span(y, y + 1));

    EXPECT_EQ(x_range.start.line, 1);
    EXPECT_EQ(x_range.start.character, 4);

    EXPECT_EQ(x_range.end.line, 1);
    EXPECT_EQ(x_range.end.character, 5);
  }
}

TEST(test_lsp_location, first_character_on_line_is_character_0) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"function f() {}" + string8(line_terminator) +
                       u8"g();");
    SCOPED_TRACE(code);
    const char8* g = strchr(code.c_str(), u8'g');
    lsp_locator l(&code);
    lsp_position g_position = l.position(g);

    EXPECT_EQ(g_position.line, 1);
    EXPECT_EQ(g_position.character, 0);
  }
}

TEST(test_lsp_location, ls_and_ps_are_not_treated_as_newline_characters) {
  for (string8_view not_line_terminator : ls_and_ps) {
    padded_string code(u8"one" + string8(not_line_terminator) + u8"two\nTHREE");
    SCOPED_TRACE(code);
    const char8* r = strchr(code.c_str(), u8'R');
    lsp_locator l(&code);
    EXPECT_EQ(l.position(r).line, 1);
  }
}

TEST(test_lsp_location, lf_cr_is_two_line_terminators) {
  padded_string code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const char8* y = strchr(code.c_str(), u8'y');
  lsp_locator l(&code);
  EXPECT_EQ(l.position(y).line, 2);
}

TEST(test_lsp_location, location_after_null_byte) {
  padded_string code(u8"hello\0beautiful\nworld"_sv);
  const char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  lsp_locator l(&code);
  lsp_range r_range = l.range(source_code_span(r, r + 1));

  EXPECT_EQ(r_range.start.line, 1);
  EXPECT_EQ(r_range.start.character, 2);
}

TEST(test_lsp_location, position_backwards) {
  padded_string code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<lsp_position> expected_positions;
  {
    lsp_locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<lsp_position> actual_positions;
  {
    lsp_locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  std::reverse(actual_positions.begin(), actual_positions.end());

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(test_lsp_location, position_after_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  padded_string code(u8"\u2603 x"_sv);
  const char8* x = strchr(code.c_str(), u8'x');
  lsp_locator l(&code);
  EXPECT_EQ(l.position(x).character, 2);
}

TEST(test_lsp_location, position_after_wide_multi_byte_character) {
  // U+1f496 has four UTF-8 code units: f0 9f 92 96
  // U+1f496 has two UTF-16 code units: D83D DC96
  padded_string code(u8"\U0001f496 x"_sv);
  const char8* x = strchr(code.c_str(), u8'x');
  lsp_locator l(&code);
  EXPECT_EQ(l.position(x).character, 3);
}

TEST(test_lsp_location, position_after_incomplete_utf_8_character) {
  padded_string code("x\xf0\x9f\x92y"_s8v);
  const char8* y = strchr(code.c_str(), u8'y');
  lsp_locator l(&code);
  EXPECT_EQ(l.position(y).character, 4);
}

TEST(test_lsp_location, offset_from_first_line_position) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* o = l.from_position(lsp_position{.line = 0, .character = 4});
  EXPECT_EQ(o, &code[4]);
}

TEST(test_lsp_location, offset_from_second_line_position) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* r = l.from_position(lsp_position{.line = 1, .character = 2});
  EXPECT_EQ(r, &code[8]);
}

TEST(test_lsp_location, offset_from_out_of_range_line_is_end_of_file) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* c = l.from_position(lsp_position{.line = 2, .character = 2});
  EXPECT_EQ(c, code.null_terminator());
}

TEST(test_lsp_location, offset_from_beginning_of_line) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* w = l.from_position(lsp_position{.line = 1, .character = 0});
  EXPECT_EQ(w, &code[6]);
}

TEST(test_lsp_location, offset_from_end_of_line) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* terminator =
      l.from_position(lsp_position{.line = 0, .character = 5});
  EXPECT_EQ(terminator, &code[5]);
}

TEST(test_lsp_location, offset_from_empty_line) {
  padded_string code(u8"hello\n\nworld"_sv);
  lsp_locator l(&code);
  for (int character : {0, 1, 2, 3, 4}) {
    SCOPED_TRACE(character);
    const char8* terminator =
        l.from_position(lsp_position{.line = 1, .character = character});
    EXPECT_EQ(terminator, &code[6]);
  }
}

TEST(test_lsp_location, offset_from_last_character_in_line) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"hello" + string8(line_terminator) + u8"world");
    SCOPED_TRACE(code);
    lsp_locator l(&code);
    const char8* o = l.from_position(lsp_position{.line = 0, .character = 4});
    EXPECT_EQ(o, &code[4]);
  }
}

TEST(test_lsp_location,
     offset_from_beyond_end_of_line_refers_to_line_terminator) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"hello" + string8(line_terminator) + u8"world");
    SCOPED_TRACE(code);
    lsp_locator l(&code);
    const char8* terminator =
        l.from_position(lsp_position{.line = 0, .character = 6});
    EXPECT_EQ(terminator, &code[5]);
  }
}

TEST(
    test_lsp_location,
    offset_from_beyond_end_of_line_containing_non_ascii_refers_to_line_terminator) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string code(u8"hello \u2603!" + string8(line_terminator) +
                       u8"world");
    SCOPED_TRACE(code);
    lsp_locator l(&code);
    const char8* terminator =
        l.from_position(lsp_position{.line = 0, .character = 9});
    EXPECT_EQ(terminator, strchr(code.c_str(), u8'!') + 1);
  }
}

TEST(test_lsp_location, offset_from_end_of_last_line) {
  padded_string code(u8"hello"_sv);
  lsp_locator l(&code);
  for (int character : {5, 6, 10}) {
    SCOPED_TRACE(character);
    const char8* terminator =
        l.from_position(lsp_position{.line = 0, .character = character});
    EXPECT_EQ(terminator, &code[5]);
  }
}

TEST(test_lsp_location, offset_from_end_of_last_line_containing_non_ascii) {
  padded_string code(u8"hello \u2603!"_sv);
  lsp_locator l(&code);
  for (int character : {8, 9, 15}) {
    SCOPED_TRACE(character);
    const char8* terminator =
        l.from_position(lsp_position{.line = 0, .character = character});
    EXPECT_EQ(terminator, code.null_terminator());
  }
}

TEST(test_lsp_location, offset_of_inside_cr_lf_gives_beginning_of_cr_lf) {
  padded_string code(u8"hello\r\nworld"_sv);
  lsp_locator l(&code);
  const char8* terminator = l.from_position(lsp_position{
      .line = 0, .character = narrow_cast<int>(strlen(u8"hello\r"))});
  EXPECT_EQ(terminator, &code[narrow_cast<int>(strlen(u8"hello"))]);
}

TEST(test_lsp_location, offset_from_empty_input) {
  padded_string code(u8""_sv);
  lsp_locator l(&code);
  for (int character : {0, 1, 10}) {
    SCOPED_TRACE(character);
    const char8* terminator =
        l.from_position(lsp_position{.line = 0, .character = character});
    EXPECT_EQ(terminator, &code[0]);
  }
}

TEST(test_lsp_location, offset_from_negative_line) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* c = l.from_position(lsp_position{.line = -2, .character = 0});
  EXPECT_EQ(c, nullptr);
}

TEST(test_lsp_location, offset_from_negative_character) {
  padded_string code(u8"hello\nworld"_sv);
  lsp_locator l(&code);
  const char8* c = l.from_position(lsp_position{.line = 1, .character = -2});
  EXPECT_EQ(c, nullptr);
}

TEST(test_lsp_location, offset_after_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  padded_string code(u8"\u2603 x"_sv);
  lsp_locator l(&code);
  const char8* x = l.from_position(lsp_position{.line = 0, .character = 2});
  EXPECT_EQ(x, strchr(code.c_str(), u8'x'));
}

TEST(test_lsp_location, offset_after_wide_multi_byte_character) {
  // U+1f496 has four UTF-8 code units: f0 9f 92 96
  // U+1f496 has two UTF-16 code units: D83D DC96
  padded_string code(u8"\U0001f496 x"_sv);
  lsp_locator l(&code);
  const char8* x = l.from_position(lsp_position{.line = 0, .character = 3});
  EXPECT_EQ(x, strchr(code.c_str(), u8'x'));
}

TEST(test_lsp_location, offset_after_multi_byte_character_on_middle_line) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  padded_string code(u8"A\u2603a\nB\u2603b\nC\u2603c"_sv);
  lsp_locator l(&code);
  const char8* b = l.from_position(lsp_position{.line = 1, .character = 2});
  EXPECT_EQ(b, strchr(code.c_str(), u8'b'));
}

TEST(test_lsp_location, offset_to_position_to_offset) {
  padded_string code(u8"hello\nworld\rthird\r\nfourth\n\n\nlast"_sv);
  lsp_locator l(&code);
  for (int i = 0; i < code.size(); ++i) {
    SCOPED_TRACE(i);
    const char8* character = &code[i];
    if (i > 0 && character[-1] == u8'\r' && character[0] == u8'\n') {
      // LSP doesn't support positions in the middle of line terminators (i.e.
      // after '\r' and before '\n' in "\r\n").
      continue;
    }
    lsp_position position = l.position(character);
    EXPECT_EQ(l.from_position(position), character) << position;
  }
}

void check_positions_against_reference_locator(lsp_locator& locator,
                                               padded_string_view code) {
  lsp_locator reference_locator(code);
  for (int i = 0; i <= code.size(); ++i) {
    const char8* c = &code[i];
    lsp_position actual_position = locator.position(c);
    lsp_position expected_position = reference_locator.position(c);
    EXPECT_EQ(actual_position, expected_position) << i;
  }
}

TEST(test_lsp_location, add_characters_within_line) {
  padded_string original_code(u8"first line\nsecond line\nthird line"_sv);
  padded_string updated_code(u8"first line\nsecond xxx line\nthird line"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 1, .character = 7},
      },
      u8"xxx ", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, remove_characters_within_line) {
  padded_string original_code(
      u8"first line\nsecond little line\nthird line"_sv);
  padded_string updated_code(u8"first line\nsecond line\nthird line"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 1, .character = 7 + 7},
      },
      u8"", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, add_newline_within_line) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string original_code(u8"first line\nsecond line\nlast line"_sv);
    SCOPED_TRACE(original_code);
    padded_string updated_code(u8"first line\nsecond" +
                               string8(line_terminator) + u8" line\nlast line");
    SCOPED_TRACE(updated_code);

    lsp_locator locator(&original_code);
    locator.replace_text(
        lsp_range{
            .start = {.line = 1, .character = 6},
            .end = {.line = 1, .character = 6},
        },
        line_terminator, &updated_code);

    check_positions_against_reference_locator(locator, &updated_code);
  }
}

TEST(test_lsp_location, delete_newline) {
  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    padded_string original_code(u8"first line\nsecond" +
                                string8(line_terminator) + u8"line\nlast line");
    SCOPED_TRACE(original_code);
    padded_string updated_code(u8"first line\nsecondline\nlast line"_sv);
    SCOPED_TRACE(updated_code);

    lsp_locator locator(&original_code);
    locator.replace_text(
        lsp_range{
            .start = {.line = 1, .character = 6},
            .end = {.line = 2, .character = 0},
        },
        u8"", &updated_code);

    check_positions_against_reference_locator(locator, &updated_code);
  }
}

TEST(test_lsp_location, replace_newline_and_text_with_newline) {
  padded_string original_code(
      u8"first line\nsecond line\nthird line\nlast line"_sv);
  padded_string updated_code(u8"first line\nsecond x\ny line\nlast line"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 2, .character = 5},
      },
      u8"x\ny", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, append_line_with_out_of_range_character) {
  padded_string original_code(u8"first line\nsecond line\nthird line"_sv);
  padded_string updated_code(
      u8"first line\nsecond line extended\nthird line"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 100},
          .end = {.line = 1, .character = 200},
      },
      u8" extended", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, replace_line_with_out_of_range_line) {
  padded_string original_code(u8"first line\nsecond line\nthird line"_sv);
  padded_string updated_code(u8"first line\nsecond line\nlast line"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 2, .character = 0},
          .end = {.line = 3, .character = 0},
      },
      u8"last line", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, change_ascii_line_into_non_ascii_line) {
  padded_string original_code(u8"first\nsecond\nthird"_sv);
  padded_string updated_code(u8"first\nse\u00e7ond\nthird"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\u00e7", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, split_ascii_line_into_non_ascii_line_and_ascii_line) {
  padded_string original_code(u8"first\nsecond\nthird"_sv);
  padded_string updated_code(u8"first\nse\u00e7\nond\nthird"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\u00e7\n", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location, split_ascii_line_into_ascii_line_and_non_ascii_line) {
  padded_string original_code(u8"first\nsecond\nthird"_sv);
  padded_string updated_code(u8"first\nse\n\u00e7ond\nthird"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\n\u00e7", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location,
     split_non_ascii_line_into_ascii_line_and_non_ascii_line) {
  padded_string original_code(u8"first\nse\u00e7ond\nthird"_sv);
  padded_string updated_code(u8"first\nse\n\u00e7ond\nthird"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 2},
      },
      u8"\n", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(test_lsp_location,
     split_non_ascii_line_into_non_ascii_line_and_ascii_line) {
  padded_string original_code(u8"first\nse\u00e7ond\nthird"_sv);
  padded_string updated_code(u8"first\nse\u00e7\nond\nthird"_sv);

  lsp_locator locator(&original_code);
  locator.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 3},
          .end = {.line = 1, .character = 3},
      },
      u8"\n", &updated_code);

  check_positions_against_reference_locator(locator, &updated_code);
}

// TODO(strager): How should replace_text behave when given a negative
// character or an out of range line?
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
