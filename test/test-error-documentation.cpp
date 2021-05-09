// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-documentation.h>
#include <vector>

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_error_documentation, error_code_from_file_path) {
  EXPECT_EQ(parse_error_documentation("E123.md", u8"").file_path_error_code(),
            "E123");
  EXPECT_EQ(
      parse_error_documentation("path/to/E666.md", u8"").file_path_error_code(),
      "E666");
  EXPECT_EQ(parse_error_documentation("path\\to\\E666.md", u8"")
                .file_path_error_code(),
            "E666");
}

TEST(test_error_documentation, title) {
  error_documentation doc =
      parse_error_documentation("file.md", u8"# E123: title goes here\n");
  EXPECT_EQ(doc.title_error_code, "E123");
  EXPECT_EQ(doc.title_error_description, u8"title goes here");
}

TEST(test_error_documentation, title_with_html_entity) {
  error_documentation doc =
      parse_error_documentation("file.md", u8"# E123: title &#x67;oes here\n");
  EXPECT_EQ(doc.title_error_code, "E123");
  // TODO(strager): Translate HTML entities.
  EXPECT_EQ(doc.title_error_description, u8"title &#x67;oes here");
}

TEST(test_error_documentation, level_2_heading_is_not_title) {
  error_documentation doc =
      parse_error_documentation("file.md", u8"## E123: title goes here\n");
  EXPECT_EQ(doc.title_error_code, "");
  EXPECT_EQ(doc.title_error_description, u8"");
}

TEST(test_error_documentation, no_code_blocks) {
  error_documentation doc =
      parse_error_documentation("file.md", u8"paragraph goes here\n");
  EXPECT_THAT(doc.code_blocks, IsEmpty());
}

TEST(test_error_documentation, one_indented_code_block) {
  error_documentation doc = parse_error_documentation("file.md",
                                                      u8R"(see this code:

    here is some code
    with multiple lines

    and a blank line
        and extra indentation

wasn't that neat?\n)");
  EXPECT_THAT(
      doc.code_blocks,
      ElementsAre(
          u8"here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n"_sv));
}

TEST(test_error_documentation, one_bracketed_code_block) {
  error_documentation doc = parse_error_documentation("file.md",
                                                      u8R"(see this code:

```
here is some code
with multiple lines

and a blank line
    and extra indentation
```

wasn't that neat?\n)");
  EXPECT_THAT(
      doc.code_blocks,
      ElementsAre(
          u8"here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n"_sv));
}

TEST(test_error_documentation, one_bracketed_code_block_with_language) {
  error_documentation doc = parse_error_documentation("file.md",
                                                      u8R"(see this code:

```testscript
here is some code
with multiple lines

and a blank line
    and extra indentation
```

wasn't that neat?\n)");
  EXPECT_THAT(
      doc.code_blocks,
      ElementsAre(
          u8"here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n"_sv));
}

TEST(test_error_documentation, multiple_code_blocks) {
  error_documentation doc = parse_error_documentation("file.md",
                                                      u8R"(see this code:

    first

```
second
```

    third

wasn't that neat?\n)");
  EXPECT_THAT(doc.code_blocks,
              ElementsAre(u8"first\n"_sv, u8"second\n"_sv, u8"third\n"_sv));
}

TEST(test_error_documentation, html_wraps_byte_order_mark) {
  error_documentation doc =
      parse_error_documentation("file.md", u8"code:\n\n    \ufeff--BOM\n");
  string8 html;
  doc.to_html(&html);
  EXPECT_THAT(to_string(html),
              HasSubstr(to_string(
                  u8"<code><span class='unicode-bom'>\ufeff</span>--BOM")));
}

TEST(test_error_documentation, substitute_template_with_no_substitutions) {
  string8 out = substitute_error_documentation_template(
      u8"hello world"_sv, u8"(error documentation)");
  EXPECT_EQ(out, u8"hello world"_sv);
}

TEST(test_error_documentation,
     substitute_template_with_generated_substitution) {
  string8 out = substitute_error_documentation_template(
      u8"hello ${generated_message} world"_sv, u8"(error documentation)");
  EXPECT_EQ(
      out,
      u8"hello This file was generated using generate-error-docs.cpp. world"_sv);
}

TEST(test_error_documentation,
     substitute_template_with_error_documentation_substitution) {
  string8 out = substitute_error_documentation_template(
      u8"hello ${error_documentation} world"_sv, u8"(error documentation)");
  EXPECT_EQ(out, u8"hello (error documentation) world"_sv);
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
