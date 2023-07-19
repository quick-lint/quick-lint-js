// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

namespace quick_lint_js {
namespace {
TEST(Test_LSP_Document_Text, set_text) {
  LSP_Document_Text doc;
  doc.set_text(u8"content goes here"_sv);
  EXPECT_EQ(doc.string(), u8"content goes here"_sv);
}

TEST(Test_LSP_Document_Text, set_text_multiple_times) {
  LSP_Document_Text doc;
  doc.set_text(u8"content goes here"_sv);
  doc.set_text(u8"newer content goes here"_sv);
  EXPECT_EQ(doc.string(), u8"newer content goes here"_sv);
  doc.set_text(u8"finally"_sv);
  EXPECT_EQ(doc.string(), u8"finally"_sv);
}

TEST(Test_LSP_Document_Text,
     set_text_range_single_line_in_middle_of_document_same_length) {
  LSP_Document_Text doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"were"_sv);
  EXPECT_EQ(doc.string(), u8"content were here"_sv);
}

TEST(Test_LSP_Document_Text,
     set_text_range_single_line_in_middle_of_document_smaller_length) {
  LSP_Document_Text doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"was"_sv);
  EXPECT_EQ(doc.string(), u8"content was here"_sv);
}

TEST(Test_LSP_Document_Text,
     set_text_range_single_line_in_middle_of_document_larger_length) {
  LSP_Document_Text doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"might go somewhere"_sv);
  EXPECT_EQ(doc.string(), u8"content might go somewhere here"_sv);
}

TEST(Test_LSP_Document_Text,
     set_text_range_delete_line_excluding_line_terminator) {
  LSP_Document_Text doc;
  doc.set_text(u8"hello\nworld\n"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 1000},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"\nworld\n"_sv);
}

TEST(Test_LSP_Document_Text,
     set_text_range_delete_line_including_line_terminator) {
  LSP_Document_Text doc;
  doc.set_text(u8"hello\nworld\n"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"world\n"_sv);
}

TEST(Test_LSP_Document_Text, replace_text_multiple_times) {
  LSP_Document_Text doc;
  doc.set_text(u8"content\ngoes\nhere"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 7},
          .end = {.line = 1, .character = 3},
      },
      u8"I wa"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 1, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8"somew"_sv);
  doc.replace_text(
      LSP_Range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 7},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"I was\nsomewhere"_sv);
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
