// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/lsp-document.h>
#include <quick-lint-js/lsp-location.h>

namespace quick_lint_js {
namespace {
TEST(test_lsp_document, set_text) {
  lsp_document document;
  document.set_text(u8"content goes here"_sv);
  EXPECT_EQ(document.string(), u8"content goes here"_sv);
}

TEST(test_lsp_document, set_text_multiple_times) {
  lsp_document document;
  document.set_text(u8"content goes here"_sv);
  document.set_text(u8"newer content goes here"_sv);
  EXPECT_EQ(document.string(), u8"newer content goes here"_sv);
  document.set_text(u8"finally"_sv);
  EXPECT_EQ(document.string(), u8"finally"_sv);
}

TEST(test_lsp_document,
     set_text_range_single_line_in_middle_of_document_same_length) {
  lsp_document document;
  document.set_text(u8"content goes here"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"were"_sv);
  EXPECT_EQ(document.string(), u8"content were here"_sv);
}

TEST(test_lsp_document,
     set_text_range_single_line_in_middle_of_document_smaller_length) {
  lsp_document document;
  document.set_text(u8"content goes here"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"was"_sv);
  EXPECT_EQ(document.string(), u8"content was here"_sv);
}

TEST(test_lsp_document,
     set_text_range_single_line_in_middle_of_document_larger_length) {
  lsp_document document;
  document.set_text(u8"content goes here"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"might go somewhere"_sv);
  EXPECT_EQ(document.string(), u8"content might go somewhere here"_sv);
}

TEST(test_lsp_document, set_text_range_delete_line_excluding_line_terminator) {
  lsp_document document;
  document.set_text(u8"hello\nworld\n"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 1000},
      },
      u8""_sv);
  EXPECT_EQ(document.string(), u8"\nworld\n"_sv);
}

TEST(test_lsp_document, set_text_range_delete_line_including_line_terminator) {
  lsp_document document;
  document.set_text(u8"hello\nworld\n"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8""_sv);
  EXPECT_EQ(document.string(), u8"world\n"_sv);
}

TEST(test_lsp_document, replace_text_multiple_times) {
  lsp_document document;
  document.set_text(u8"content\ngoes\nhere"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 7},
          .end = {.line = 1, .character = 3},
      },
      u8"I wa"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8"somew"_sv);
  document.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 7},
      },
      u8""_sv);
  EXPECT_EQ(document.string(), u8"I was\nsomewhere"_sv);
}
}
}
