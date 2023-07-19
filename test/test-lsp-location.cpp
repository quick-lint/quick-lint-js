// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(Test_LSP_Location, ranges_on_first_line) {
  Padded_String code(u8"let x = 2;"_sv);
  LSP_Locator l(&code);
  LSP_Range x_range = l.range(Source_Code_Span(&code[4], &code[5]));

  EXPECT_EQ(x_range.start.line, 0);
  EXPECT_EQ(x_range.start.character, 4);

  EXPECT_EQ(x_range.end.line, 0);
  EXPECT_EQ(x_range.end.character, 5);
}

TEST(Test_LSP_Location, ranges_on_second_line) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(
        concat(u8"let x = 2;"_sv, line_terminator, u8"let y = 3;"_sv));
    SCOPED_TRACE(code);
    const Char8* y = strchr(code.c_str(), u8'y');
    LSP_Locator l(&code);
    LSP_Range x_range = l.range(Source_Code_Span(y, y + 1));

    EXPECT_EQ(x_range.start.line, 1);
    EXPECT_EQ(x_range.start.character, 4);

    EXPECT_EQ(x_range.end.line, 1);
    EXPECT_EQ(x_range.end.character, 5);
  }
}

TEST(Test_LSP_Location, first_character_on_line_is_character_0) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(
        concat(u8"function f() {}"_sv, line_terminator, u8"g();"_sv));
    SCOPED_TRACE(code);
    const Char8* g = strchr(code.c_str(), u8'g');
    LSP_Locator l(&code);
    LSP_Position g_position = l.position(g);

    EXPECT_EQ(g_position.line, 1);
    EXPECT_EQ(g_position.character, 0);
  }
}

TEST(Test_LSP_Location, ls_and_ps_are_not_treated_as_newline_characters) {
  for (String8_View not_line_terminator : ls_and_ps) {
    Padded_String code(
        concat(u8"one"_sv, not_line_terminator, u8"two\nTHREE"_sv));
    SCOPED_TRACE(code);
    const Char8* r = strchr(code.c_str(), u8'R');
    LSP_Locator l(&code);
    EXPECT_EQ(l.position(r).line, 1);
  }
}

TEST(Test_LSP_Location, lf_cr_is_two_line_terminators) {
  Padded_String code(u8"let x = 2;\n\rlet y = 3;"_sv);
  const Char8* y = strchr(code.c_str(), u8'y');
  LSP_Locator l(&code);
  EXPECT_EQ(l.position(y).line, 2);
}

TEST(Test_LSP_Location, location_after_null_byte) {
  Padded_String code(u8"hello\0beautiful\nworld"_sv);
  const Char8* r = &code[18];
  ASSERT_EQ(*r, u8'r');

  LSP_Locator l(&code);
  LSP_Range r_range = l.range(Source_Code_Span(r, r + 1));

  EXPECT_EQ(r_range.start.line, 1);
  EXPECT_EQ(r_range.start.character, 2);
}

TEST(Test_LSP_Location, position_backwards) {
  Padded_String code(u8"ab\nc\n\nd\nefg\nh"_sv);

  std::vector<LSP_Position> expected_positions;
  {
    LSP_Locator l(&code);
    for (int i = 0; i < narrow_cast<int>(code.size()); ++i) {
      expected_positions.push_back(l.position(&code[i]));
    }
  }

  std::vector<LSP_Position> actual_positions;
  {
    LSP_Locator l(&code);
    for (int i = narrow_cast<int>(code.size()) - 1; i >= 0; --i) {
      actual_positions.push_back(l.position(&code[i]));
    }
  }
  reverse(actual_positions);

  EXPECT_EQ(actual_positions, expected_positions);
}

TEST(Test_LSP_Location, position_after_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  Padded_String code(u8"\u2603 x"_sv);
  const Char8* x = strchr(code.c_str(), u8'x');
  LSP_Locator l(&code);
  EXPECT_EQ(l.position(x).character, 2);
}

TEST(Test_LSP_Location, position_after_wide_multi_byte_character) {
  // U+1f496 has four UTF-8 code units: f0 9f 92 96
  // U+1f496 has two UTF-16 code units: D83D DC96
  Padded_String code(u8"\U0001f496 x"_sv);
  const Char8* x = strchr(code.c_str(), u8'x');
  LSP_Locator l(&code);
  EXPECT_EQ(l.position(x).character, 3);
}

TEST(Test_LSP_Location, position_after_incomplete_utf_8_character) {
  Padded_String code("x\xf0\x9f\x92y"_s8v);
  const Char8* y = strchr(code.c_str(), u8'y');
  LSP_Locator l(&code);
  EXPECT_EQ(l.position(y).character, 4);
}

TEST(Test_LSP_Location, offset_from_first_line_position) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* o = l.from_position(LSP_Position{.line = 0, .character = 4});
  EXPECT_EQ(o, &code[4]);
}

TEST(Test_LSP_Location, offset_from_second_line_position) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* r = l.from_position(LSP_Position{.line = 1, .character = 2});
  EXPECT_EQ(r, &code[8]);
}

TEST(Test_LSP_Location, offset_from_out_of_range_line_is_end_of_file) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* c = l.from_position(LSP_Position{.line = 2, .character = 2});
  EXPECT_EQ(c, code.null_terminator());
}

TEST(Test_LSP_Location, offset_from_beginning_of_line) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* w = l.from_position(LSP_Position{.line = 1, .character = 0});
  EXPECT_EQ(w, &code[6]);
}

TEST(Test_LSP_Location, offset_from_end_of_line) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* terminator =
      l.from_position(LSP_Position{.line = 0, .character = 5});
  EXPECT_EQ(terminator, &code[5]);
}

TEST(Test_LSP_Location, offset_from_empty_line) {
  Padded_String code(u8"hello\n\nworld"_sv);
  LSP_Locator l(&code);
  for (int character : {0, 1, 2, 3, 4}) {
    SCOPED_TRACE(character);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 1, .character = character});
    EXPECT_EQ(terminator, &code[6]);
  }
}

TEST(Test_LSP_Location, offset_from_last_character_in_line) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(concat(u8"hello"_sv, line_terminator, u8"world"_sv));
    SCOPED_TRACE(code);
    LSP_Locator l(&code);
    const Char8* o = l.from_position(LSP_Position{.line = 0, .character = 4});
    EXPECT_EQ(o, &code[4]);
  }
}

TEST(Test_LSP_Location,
     offset_from_beyond_end_of_line_refers_to_line_terminator) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(concat(u8"hello"_sv, line_terminator, u8"world"_sv));
    SCOPED_TRACE(code);
    LSP_Locator l(&code);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 0, .character = 6});
    EXPECT_EQ(terminator, &code[5]);
  }
}

TEST(
    Test_LSP_Location,
    offset_from_beyond_end_of_line_containing_non_ascii_refers_to_line_terminator) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String code(
        concat(u8"hello \u2603!"_sv, line_terminator, u8"world"_sv));
    SCOPED_TRACE(code);
    LSP_Locator l(&code);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 0, .character = 9});
    EXPECT_EQ(terminator, strchr(code.c_str(), u8'!') + 1);
  }
}

TEST(Test_LSP_Location, offset_from_end_of_last_line) {
  Padded_String code(u8"hello"_sv);
  LSP_Locator l(&code);
  for (int character : {5, 6, 10}) {
    SCOPED_TRACE(character);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 0, .character = character});
    EXPECT_EQ(terminator, &code[5]);
  }
}

TEST(Test_LSP_Location, offset_from_end_of_last_line_containing_non_ascii) {
  Padded_String code(u8"hello \u2603!"_sv);
  LSP_Locator l(&code);
  for (int character : {8, 9, 15}) {
    SCOPED_TRACE(character);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 0, .character = character});
    EXPECT_EQ(terminator, code.null_terminator());
  }
}

TEST(Test_LSP_Location, offset_of_inside_cr_lf_gives_beginning_of_cr_lf) {
  Padded_String code(u8"hello\r\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* terminator = l.from_position(LSP_Position{
      .line = 0, .character = narrow_cast<int>(strlen(u8"hello\r"))});
  EXPECT_EQ(terminator, &code[narrow_cast<int>(strlen(u8"hello"))]);
}

TEST(Test_LSP_Location, offset_from_empty_input) {
  Padded_String code(u8""_sv);
  LSP_Locator l(&code);
  for (int character : {0, 1, 10}) {
    SCOPED_TRACE(character);
    const Char8* terminator =
        l.from_position(LSP_Position{.line = 0, .character = character});
    EXPECT_EQ(terminator, &code[0]);
  }
}

TEST(Test_LSP_Location, offset_from_negative_line) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* c = l.from_position(LSP_Position{.line = -2, .character = 0});
  EXPECT_EQ(c, nullptr);
}

TEST(Test_LSP_Location, offset_from_negative_character) {
  Padded_String code(u8"hello\nworld"_sv);
  LSP_Locator l(&code);
  const Char8* c = l.from_position(LSP_Position{.line = 1, .character = -2});
  EXPECT_EQ(c, nullptr);
}

TEST(Test_LSP_Location, offset_after_multi_byte_character) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  Padded_String code(u8"\u2603 x"_sv);
  LSP_Locator l(&code);
  const Char8* x = l.from_position(LSP_Position{.line = 0, .character = 2});
  EXPECT_EQ(x, strchr(code.c_str(), u8'x'));
}

TEST(Test_LSP_Location, offset_after_wide_multi_byte_character) {
  // U+1f496 has four UTF-8 code units: f0 9f 92 96
  // U+1f496 has two UTF-16 code units: D83D DC96
  Padded_String code(u8"\U0001f496 x"_sv);
  LSP_Locator l(&code);
  const Char8* x = l.from_position(LSP_Position{.line = 0, .character = 3});
  EXPECT_EQ(x, strchr(code.c_str(), u8'x'));
}

TEST(Test_LSP_Location, offset_after_multi_byte_character_on_middle_line) {
  // U+2603 has three UTF-8 code units: e2 98 83
  // U+2603 has one UTF-16 code unit: 2603
  Padded_String code(u8"A\u2603a\nB\u2603b\nC\u2603c"_sv);
  LSP_Locator l(&code);
  const Char8* b = l.from_position(LSP_Position{.line = 1, .character = 2});
  EXPECT_EQ(b, strchr(code.c_str(), u8'b'));
}

TEST(Test_LSP_Location, offset_to_position_to_offset) {
  Padded_String code(u8"hello\nworld\rthird\r\nfourth\n\n\nlast"_sv);
  LSP_Locator l(&code);
  for (int i = 0; i < code.size(); ++i) {
    SCOPED_TRACE(i);
    const Char8* character = &code[i];
    if (i > 0 && character[-1] == u8'\r' && character[0] == u8'\n') {
      // LSP doesn't support positions in the middle of line terminators (i.e.
      // after '\r' and before '\n' in "\r\n").
      continue;
    }
    LSP_Position position = l.position(character);
    EXPECT_EQ(l.from_position(position), character) << position;
  }
}

void check_positions_against_reference_locator(LSP_Locator& locator,
                                               Padded_String_View code) {
  LSP_Locator reference_locator(code);
  for (int i = 0; i <= code.size(); ++i) {
    const Char8* c = &code[i];
    LSP_Position actual_position = locator.position(c);
    LSP_Position expected_position = reference_locator.position(c);
    EXPECT_EQ(actual_position, expected_position) << i;
  }
}

TEST(Test_LSP_Location, add_characters_within_line) {
  Padded_String original_code(u8"first line\nsecond line\nthird line"_sv);
  Padded_String updated_code(u8"first line\nsecond xxx line\nthird line"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 1, .character = 7},
      },
      u8"xxx "_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, remove_characters_within_line) {
  Padded_String original_code(
      u8"first line\nsecond little line\nthird line"_sv);
  Padded_String updated_code(u8"first line\nsecond line\nthird line"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 1, .character = 7 + 7},
      },
      u8""_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, add_newline_within_line) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String original_code(u8"first line\nsecond line\nlast line"_sv);
    SCOPED_TRACE(original_code);
    Padded_String updated_code(concat(
        u8"first line\nsecond"_sv, line_terminator, u8" line\nlast line"_sv));
    SCOPED_TRACE(updated_code);

    LSP_Locator locator(&original_code);
    locator.replace_text(
        LSP_Range{
            .start = {.line = 1, .character = 6},
            .end = {.line = 1, .character = 6},
        },
        line_terminator, &updated_code);
    locator.validate_caches_debug();

    check_positions_against_reference_locator(locator, &updated_code);
  }
}

TEST(Test_LSP_Location, delete_newline) {
  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Padded_String original_code(concat(
        u8"first line\nsecond"_sv, line_terminator, u8"line\nlast line"_sv));
    SCOPED_TRACE(original_code);
    Padded_String updated_code(u8"first line\nsecondline\nlast line"_sv);
    SCOPED_TRACE(updated_code);

    LSP_Locator locator(&original_code);
    locator.replace_text(
        LSP_Range{
            .start = {.line = 1, .character = 6},
            .end = {.line = 2, .character = 0},
        },
        u8""_sv, &updated_code);
    locator.validate_caches_debug();

    check_positions_against_reference_locator(locator, &updated_code);
  }
}

TEST(Test_LSP_Location, replace_newline_and_text_with_newline) {
  Padded_String original_code(
      u8"first line\nsecond line\nthird line\nlast line"_sv);
  Padded_String updated_code(u8"first line\nsecond x\ny line\nlast line"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 7},
          .end = {.line = 2, .character = 5},
      },
      u8"x\ny"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, append_line_with_out_of_range_character) {
  Padded_String original_code(u8"first line\nsecond line\nthird line"_sv);
  Padded_String updated_code(
      u8"first line\nsecond line extended\nthird line"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 100},
          .end = {.line = 1, .character = 200},
      },
      u8" extended"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, replace_line_with_out_of_range_line) {
  Padded_String original_code(u8"first line\nsecond line\nthird line"_sv);
  Padded_String updated_code(u8"first line\nsecond line\nlast line"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 2, .character = 0},
          .end = {.line = 3, .character = 0},
      },
      u8"last line"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, change_ascii_line_into_non_ascii_line) {
  Padded_String original_code(u8"first\nsecond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\u00e7ond\nthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\u00e7"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, split_ascii_line_into_non_ascii_line_and_ascii_line) {
  Padded_String original_code(u8"first\nsecond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\u00e7\nond\nthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\u00e7\n"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, split_ascii_line_into_ascii_line_and_non_ascii_line) {
  Padded_String original_code(u8"first\nsecond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\n\u00e7ond\nthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 3},
      },
      u8"\n\u00e7"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location,
     split_non_ascii_line_into_ascii_line_and_non_ascii_line) {
  Padded_String original_code(u8"first\nse\u00e7ond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\n\u00e7ond\nthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 2},
          .end = {.line = 1, .character = 2},
      },
      u8"\n"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location,
     split_non_ascii_line_into_non_ascii_line_and_ascii_line) {
  Padded_String original_code(u8"first\nse\u00e7ond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\u00e7\nond\nthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 3},
          .end = {.line = 1, .character = 3},
      },
      u8"\n"_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, join_non_ascii_and_ascii_line) {
  Padded_String original_code(u8"first\nse\u00e7ond\nthird"_sv);
  Padded_String updated_code(u8"first\nse\u00e7ondthird"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 6},
          .end = {.line = 2, .character = 0},
      },
      u8""_sv, &updated_code);
  locator.validate_caches_debug();

  check_positions_against_reference_locator(locator, &updated_code);
}

TEST(Test_LSP_Location, join_ascii_and_non_ascii_line) {
  Padded_String original_code(u8"first\nsecond\nth\u00edrd"_sv);
  Padded_String updated_code(u8"first\nsecondth\u00edrd"_sv);

  LSP_Locator locator(&original_code);
  locator.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 6},
          .end = {.line = 2, .character = 0},
      },
      u8""_sv, &updated_code);
  locator.validate_caches_debug();

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
