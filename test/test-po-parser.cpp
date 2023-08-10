// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/port/char8.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_PO_Parser : public ::testing::Test {
 public:
  Span<PO_Entry> parse_po_file(Padded_String_View code) {
    CLI_Locator locator(code);
    return quick_lint_js::parse_po_file(code, __FILE__ "(test)", &locator,
                                        &this->allocator_);
  }

 private:
  Monotonic_Allocator allocator_{"Test_PO_Parser"};
};

TEST_F(Test_PO_Parser, empty_file_has_no_entries) {
  Padded_String code(u8""_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ::testing::IsEmpty());
}

TEST_F(Test_PO_Parser, minimal_entry) {
  Padded_String code(
      u8R"(msgid ""
msgstr "")"_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ElementsAreArray({
                           PO_Entry{
                               .msgid = u8""_sv,
                               .msgstr = u8""_sv,
                           },
                       }));
}

TEST_F(Test_PO_Parser, simple_entry) {
  Padded_String code(
      u8R"(msgid "my msgid"
msgstr "my msgstr")"_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ElementsAreArray({
                           PO_Entry{
                               .msgid = u8"my msgid"_sv,
                               .msgstr = u8"my msgstr"_sv,
                           },
                       }));
}

TEST_F(Test_PO_Parser, multi_line_entry) {
  Padded_String code(
      u8R"(
    msgid "my " "msgid"
    msgstr ""
    "my "
    "msgstr"
  )"_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ElementsAreArray({
                           PO_Entry{
                               .msgid = u8"my msgid"_sv,
                               .msgstr = u8"my msgstr"_sv,
                           },
                       }));
}

TEST_F(Test_PO_Parser, comments_are_ignored) {
  Padded_String code(
      u8R"(
    # hello
    msgid "my msgid"
    # beautiful
    msgstr "my msgstr"
    # world
  )"_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ElementsAreArray({
                           PO_Entry{
                               .msgid = u8"my msgid"_sv,
                               .msgstr = u8"my msgstr"_sv,
                           },
                       }));
}

TEST_F(Test_PO_Parser, flag_comments) {
  Padded_String code(
      u8R"(
    #, fuzzy
    msgid "my msgid"
    msgstr "my msgstr"
  )"_sv);
  Span<PO_Entry> entries = parse_po_file(&code);
  EXPECT_THAT(entries, ElementsAreArray({
                           PO_Entry{
                               .msgid = u8"my msgid"_sv,
                               .msgstr = u8"my msgstr"_sv,
                               .is_fuzzy = true,
                           },
                       }));
}

TEST_F(Test_PO_Parser, string_escape_sequence) {
  {
    Padded_String code(
        u8R"(msgid "newline=\n tab=\t backspace=\b return=\r feed=\f vtab=\v bell=\a backslash=\\ dquote=\""
  msgstr "")"_sv);
    Span<PO_Entry> entries = parse_po_file(&code);
    EXPECT_THAT(
        entries,
        ElementsAreArray({
            PO_Entry{
                .msgid = u8"newline=\n tab=\t backspace=\b return=\r "_sv
                         u8"feed=\f vtab=\v bell=\a backslash=\\ dquote=\""_sv,
                .msgstr = u8""_sv,
            },
        }));
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
