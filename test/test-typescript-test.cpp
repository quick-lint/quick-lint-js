// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/typescript-test.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/ascii.h>
#include <utility>

namespace quick_lint_js {
namespace {
TEST(test_typescript_test, extract_units_without_directives_gives_one_file) {
  padded_string file(u8"hello\nworld\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, u8"hello\nworld\n"_sv);
}

TEST(test_typescript_test, one_filename_directive) {
  padded_string file(
      u8"hello\nworld\n// @filename: banana.ts\nsecond\nfile\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"hello\nworld\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\nfile\n"_sv);
}

TEST(test_typescript_test, filename_directive_at_end_of_line_is_ignored) {
  string8_view file_data =
      u8"hello\nworld // @filename: banana.ts\nsecond\nfile\n"_sv;
  padded_string file(file_data);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, file_data);
}

TEST(test_typescript_test, filename_directive_at_beginning_of_file) {
  padded_string file(u8"// @filename: banana.ts\nfirst\nfile\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
}

TEST(test_typescript_test, blank_lines_are_trimmed_after_filename_directive) {
  padded_string file(
      u8"first\nfile\n// @filename: banana.ts\n\n\n\nsecond\nfile\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\nfile\n"_sv);
}

TEST(test_typescript_test, filename_directive_at_end_of_file) {
  {
    padded_string file(u8"first\nfile\n// @filename: banana.ts"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 1);
    EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  }

  {
    padded_string file(u8"first\nfile\n// @filename: banana.ts\n"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 1);
    EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  }
}

TEST(test_typescript_test, metadata_name_match_is_case_insensitive) {
  {
    padded_string file(u8"first\n// @FILENAME: banana.ts\nsecond\n"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }

  {
    padded_string file(u8"first\n// @FileName: banana.ts\nsecond\n"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }
}

TEST(test_typescript_test, whitespace_is_allowed_around_metadata_name) {
  {
    padded_string file(u8"first\n//\t@filename   : banana.ts\nsecond\n"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }

  {
    padded_string file(u8"first\n//@filename\t: banana.ts\nsecond\n"_sv);
    typescript_test_units units =
        extract_units_from_typescript_test(std::move(file));
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }
}

TEST(test_typescript_test, multiple_units_are_allowed) {
  padded_string file(
      u8"first\n"_sv
      u8"// @filename: 2.ts\nsecond\n"_sv
      u8"// @filename: 3.ts\nthird\n"_sv
      u8"// @filename: 4.ts\nfourth\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 4);
  EXPECT_EQ(units[0].data, u8"first\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\n"_sv);
  EXPECT_EQ(units[2].data, u8"third\n"_sv);
  EXPECT_EQ(units[3].data, u8"fourth\n"_sv);
}

TEST(test_typescript_test, unrelated_metadata_is_included_in_units) {
  padded_string file(
      u8"first\n// @something: xxx\nunit\n"_sv
      u8"// @filename: split.ts\n"_sv
      u8"second\n// @something: xxx\nunit\n"_sv);
  typescript_test_units units =
      extract_units_from_typescript_test(std::move(file));
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"first\n// @something: xxx\nunit\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\n// @something: xxx\nunit\n"_sv);
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
