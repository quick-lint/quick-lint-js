// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/warning.h>
#include <quick-lint-js/web-demo-location.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

namespace quick_lint_js {
namespace {
template <typename Locator>
class test_document : public testing::Test {};

using document_locator_types = ::testing::Types<lsp_locator, web_demo_locator>;
TYPED_TEST_SUITE(test_document, document_locator_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_document, set_text) {
  using Locator = TypeParam;
  document<Locator> doc;
  doc.set_text(u8"content goes here"_sv);
  EXPECT_EQ(doc.string(), u8"content goes here"_sv);
}

TYPED_TEST(test_document, set_text_multiple_times) {
  using Locator = TypeParam;
  document<Locator> doc;
  doc.set_text(u8"content goes here"_sv);
  doc.set_text(u8"newer content goes here"_sv);
  EXPECT_EQ(doc.string(), u8"newer content goes here"_sv);
  doc.set_text(u8"finally"_sv);
  EXPECT_EQ(doc.string(), u8"finally"_sv);
}

TEST(test_document_lsp_locator,
     set_text_range_single_line_in_middle_of_document_same_length) {
  document<lsp_locator> doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"were"_sv);
  EXPECT_EQ(doc.string(), u8"content were here"_sv);
}

TEST(test_document_lsp_locator,
     set_text_range_single_line_in_middle_of_document_smaller_length) {
  document<lsp_locator> doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"was"_sv);
  EXPECT_EQ(doc.string(), u8"content was here"_sv);
}

TEST(test_document_lsp_locator,
     set_text_range_single_line_in_middle_of_document_larger_length) {
  document<lsp_locator> doc;
  doc.set_text(u8"content goes here"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 8},
          .end = {.line = 0, .character = 12},
      },
      u8"might go somewhere"_sv);
  EXPECT_EQ(doc.string(), u8"content might go somewhere here"_sv);
}

TEST(test_document_lsp_locator,
     set_text_range_delete_line_excluding_line_terminator) {
  document<lsp_locator> doc;
  doc.set_text(u8"hello\nworld\n"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 1000},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"\nworld\n"_sv);
}

TEST(test_document_lsp_locator,
     set_text_range_delete_line_including_line_terminator) {
  document<lsp_locator> doc;
  doc.set_text(u8"hello\nworld\n"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"world\n"_sv);
}

TEST(test_document_lsp_locator, replace_text_multiple_times) {
  document<lsp_locator> doc;
  doc.set_text(u8"content\ngoes\nhere"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 7},
          .end = {.line = 1, .character = 3},
      },
      u8"I wa"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 1, .character = 0},
          .end = {.line = 1, .character = 0},
      },
      u8"somew"_sv);
  doc.replace_text(
      lsp_range{
          .start = {.line = 0, .character = 0},
          .end = {.line = 0, .character = 7},
      },
      u8""_sv);
  EXPECT_EQ(doc.string(), u8"I was\nsomewhere"_sv);
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
