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
TEST(Test_TypeScript_Test, extract_units_without_directives_gives_one_file) {
  Padded_String file(u8"hello\nworld\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, u8"hello\nworld\n"_sv);
  EXPECT_EQ(units[0].name, u8"test.ts"_sv);
}

TEST(Test_TypeScript_Test, one_filename_directive) {
  Padded_String file(
      u8"hello\nworld\n// @filename: banana.ts\nsecond\nfile\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"testcase.ts"_sv);
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"hello\nworld\n"_sv);
  EXPECT_EQ(units[0].name, u8"testcase.ts"_sv);
  EXPECT_EQ(units[1].data, u8"second\nfile\n"_sv);
  EXPECT_EQ(units[1].name, u8"banana.ts"_sv);
}

TEST(Test_TypeScript_Test, one_filename_directive_with_dos_line_endings) {
  Padded_String file(
      u8"hello\r\nworld\r\n// @filename: banana.ts\r\nsecond\r\nfile\r\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"testcase.ts"_sv);
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"hello\r\nworld\r\n"_sv);
  EXPECT_EQ(units[0].name, u8"testcase.ts"_sv);
  EXPECT_EQ(units[1].data, u8"second\r\nfile\r\n"_sv);
  EXPECT_EQ(units[1].name, u8"banana.ts"_sv);
}

TEST(Test_TypeScript_Test, filename_directive_at_end_of_line_is_ignored) {
  String8_View file_data =
      u8"hello\nworld // @filename: banana.ts\nsecond\nfile\n"_sv;
  Padded_String file(file_data);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, file_data);
}

TEST(Test_TypeScript_Test, filename_directive_at_beginning_of_file) {
  Padded_String file(u8"// @filename: banana.ts\nfirst\nfile\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
  ASSERT_EQ(units.size(), 1);
  EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  EXPECT_EQ(units[0].name, u8"banana.ts"_sv);
}

TEST(Test_TypeScript_Test, blank_lines_are_trimmed_after_filename_directive) {
  Padded_String file(
      u8"first\nfile\n// @filename: banana.ts\n\n\n\nsecond\nfile\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\nfile\n"_sv);
}

TEST(Test_TypeScript_Test, filename_directive_at_end_of_file) {
  {
    Padded_String file(u8"first\nfile\n// @filename: banana.ts"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 1);
    EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  }

  {
    Padded_String file(u8"first\nfile\n// @filename: banana.ts\n"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 1);
    EXPECT_EQ(units[0].data, u8"first\nfile\n"_sv);
  }
}

TEST(Test_TypeScript_Test, metadata_name_match_is_case_insensitive) {
  {
    Padded_String file(u8"first\n// @FILENAME: banana.ts\nsecond\n"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }

  {
    Padded_String file(u8"first\n// @FileName: banana.ts\nsecond\n"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }
}

TEST(Test_TypeScript_Test, whitespace_is_allowed_around_metadata_name) {
  {
    Padded_String file(u8"first\n//\t@filename   : banana.ts\nsecond\n"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }

  {
    Padded_String file(u8"first\n//@filename\t: banana.ts\nsecond\n"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"test.ts"_sv);
    ASSERT_EQ(units.size(), 2);
    EXPECT_EQ(units[0].data, u8"first\n"_sv);
    EXPECT_EQ(units[1].data, u8"second\n"_sv);
  }
}

TEST(Test_TypeScript_Test, multiple_units_are_allowed) {
  Padded_String file(
      u8"first\n"_sv
      u8"// @filename: 2.ts\nsecond\n"_sv
      u8"// @filename: 3.ts\nthird\n"_sv
      u8"// @filename: 4.ts\nfourth\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"1.ts"_sv);
  ASSERT_EQ(units.size(), 4);
  EXPECT_EQ(units[0].data, u8"first\n"_sv);
  EXPECT_EQ(units[0].name, u8"1.ts"_sv);
  EXPECT_EQ(units[1].data, u8"second\n"_sv);
  EXPECT_EQ(units[1].name, u8"2.ts"_sv);
  EXPECT_EQ(units[2].data, u8"third\n"_sv);
  EXPECT_EQ(units[2].name, u8"3.ts"_sv);
  EXPECT_EQ(units[3].data, u8"fourth\n"_sv);
  EXPECT_EQ(units[3].name, u8"4.ts"_sv);
}

TEST(Test_TypeScript_Test,
     line_comments_without_directives_are_included_as_data) {
  Padded_String file(
      u8"// hello\n"_sv
      u8"// @filename: a.ts\n"_sv
      u8"// a body\n"_sv
      u8"// @filename: b.ts\n"_sv
      u8"// b body\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"testcase.ts"_sv);
  ASSERT_EQ(units.size(), 3);
  EXPECT_EQ(units[0].data, u8"// hello\n"_sv);
  EXPECT_EQ(units[1].data, u8"// a body\n"_sv);
  EXPECT_EQ(units[2].data, u8"// b body\n"_sv);
}

TEST(Test_TypeScript_Test, unrelated_metadata_is_included_in_units) {
  Padded_String file(
      u8"first\n// @something: xxx\nunit\n"_sv
      u8"// @filename: split.ts\n"_sv
      u8"second\n// @something: xxx\nunit\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"test.ts");
  ASSERT_EQ(units.size(), 2);
  EXPECT_EQ(units[0].data, u8"first\n// @something: xxx\nunit\n"_sv);
  EXPECT_EQ(units[1].data, u8"second\n// @something: xxx\nunit\n"_sv);
}

TEST(Test_TypeScript_Test, json_file_is_not_linted) {
  {
    Padded_String file(
        u8"TypeScript code\n"_sv
        u8"// @filename: hello.json\n"_sv
        u8"JSON code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.ts");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[1].get_linter_options();
    ASSERT_FALSE(options.has_value());
  }

  {
    Padded_String file(
        u8"JSON code\n"_sv
        u8"// @filename: hello.ts\n"_sv
        u8"TypeScript code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.json");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[0].get_linter_options();
    ASSERT_FALSE(options.has_value());
  }
}

TEST(Test_TypeScript_Test, typescript_file_is_linted) {
  {
    Padded_String file(
        u8"TypeScript code\n"_sv
        u8"// @filename: hello.json\n"_sv
        u8"JSON code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.ts");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[0].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_TRUE(options->typescript);
    EXPECT_FALSE(options->jsx);
  }

  {
    Padded_String file(
        u8"JSON code\n"_sv
        u8"// @filename: hello.ts\n"_sv
        u8"TypeScript code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.json");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[1].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_TRUE(options->typescript);
    EXPECT_FALSE(options->jsx);
  }
}

TEST(Test_TypeScript_Test, typescript_react_file_is_linted) {
  {
    Padded_String file(
        u8"TypeScript code\n"_sv
        u8"// @filename: hello.json\n"_sv
        u8"JSON code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.tsx");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[0].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_TRUE(options->typescript);
    EXPECT_TRUE(options->jsx);
  }

  {
    Padded_String file(
        u8"JSON code\n"_sv
        u8"// @filename: hello.tsx\n"_sv
        u8"TypeScript code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.json");
    ASSERT_EQ(units.size(), 2);
    std::optional<Linter_Options> options = units[1].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_TRUE(options->typescript);
    EXPECT_TRUE(options->jsx);
  }
}

TEST(Test_TypeScript_Test, typescript_definition_file) {
  Padded_String file(
      u8"// @filename: example.d.ts\n"_sv
      u8"export const a;"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"hello.ts");
  ASSERT_EQ(units.size(), 1);
  std::optional<Linter_Options> options = units[0].get_linter_options();
  ASSERT_TRUE(options.has_value());
  EXPECT_TRUE(options->typescript);
  EXPECT_TRUE(options->typescript_definition);
  EXPECT_FALSE(options->jsx);
}

TEST(Test_TypeScript_Test, typescript_definition_file_with_weird_extension) {
  Padded_String file(
      u8"// @filename: example.d.html.ts\n"_sv
      u8"export const a;"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"hello.ts");
  ASSERT_EQ(units.size(), 1);
  std::optional<Linter_Options> options = units[0].get_linter_options();
  ASSERT_TRUE(options.has_value());
  EXPECT_TRUE(options->typescript);
  EXPECT_TRUE(options->typescript_definition);
}

TEST(Test_TypeScript_Test, javascript_file_is_linted) {
  {
    Padded_String file(
        u8"JavaScript code\n"_sv
        u8"// @filename: hello.js\n"_sv
        u8"more JavaScript code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.js");
    ASSERT_EQ(units.size(), 2);

    std::optional<Linter_Options> options = units[0].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_FALSE(options->typescript);
    // FIXME(strager): Should we only set jsx=true if a @jsx directive is
    // present?
    EXPECT_TRUE(options->jsx);

    options = units[1].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_FALSE(options->typescript);
    // FIXME(strager): Should we only set jsx=true if a @jsx directive is
    // present?
    EXPECT_TRUE(options->jsx);
  }
}

TEST(Test_TypeScript_Test, javascript_react_file_is_linted) {
  {
    Padded_String file(
        u8"JavaScript code\n"_sv
        u8"// @filename: hello.jsx\n"_sv
        u8"more JavaScript code"_sv);
    TypeScript_Test_Units units =
        extract_units_from_typescript_test(std::move(file), u8"hello.jsx");
    ASSERT_EQ(units.size(), 2);

    std::optional<Linter_Options> options = units[0].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_FALSE(options->typescript);
    EXPECT_TRUE(options->jsx);

    options = units[1].get_linter_options();
    ASSERT_TRUE(options.has_value());
    EXPECT_FALSE(options->typescript);
    EXPECT_TRUE(options->jsx);
  }
}

TEST(Test_TypeScript_Test, markdown_unit_is_ignored) {
  Padded_String file(
      u8"// @filename: a.ts\n"_sv
      u8"hello();\n"_sv
      u8"// @filename: b.md\n"_sv
      u8"# hello\n"_sv
      u8"// @filename: c.ts\n"_sv
      u8"hello();\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"hello.ts");
  ASSERT_EQ(units.size(), 3);
  EXPECT_TRUE(units[0].get_linter_options().has_value());
  EXPECT_FALSE(units[1].get_linter_options().has_value());
  EXPECT_TRUE(units[2].get_linter_options().has_value());
}

TEST(Test_TypeScript_Test, files_in_node_modules_are_ignored) {
  // compiler/moduleResolutionWithExtensions_unexpected2.ts has garbage files in
  // node_modules. I think this means we should ignore anything in node_modules
  // (unless explicitly imported?).
  Padded_String file(
      u8"// @filename: /node_modules/foo.js\n"_sv
      u8"hello();\n"_sv);
  TypeScript_Test_Units units =
      extract_units_from_typescript_test(std::move(file), u8"hello.js");
  ASSERT_EQ(units.size(), 1);
  EXPECT_FALSE(units[0].get_linter_options().has_value());
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
