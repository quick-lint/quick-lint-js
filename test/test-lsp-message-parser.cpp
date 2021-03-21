// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

// LSP specification:
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/spy-lsp-message-parser.h>
#include <vector>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_lsp_message_parser, small_full_message) {
  spy_lsp_message_parser parser;
  parser.append(u8"Content-Length: 2\r\n\r\nhi");
  EXPECT_THAT(parser.messages(), ElementsAre(u8"hi"));
}

TEST(test_lsp_message_parser, content_type_header_is_ignored) {
  {
    spy_lsp_message_parser parser;
    parser.append(
        u8"Content-Length: 2\r\nContent-Type: application/vscode-jsonrpc; "
        u8"charset=utf-8\r\n\r\nhi");
    EXPECT_THAT(parser.messages(), ElementsAre(u8"hi"));
  }

  {
    spy_lsp_message_parser parser;
    parser.append(
        u8"Content-Type: application/vscode-jsonrpc; "
        u8"charset=utf-8\r\nContent-Length: 2\r\n\r\nhi");
    EXPECT_THAT(parser.messages(), ElementsAre(u8"hi"));
  }
}

TEST(test_lsp_message_parser, content_length_header_is_case_insensitive) {
  spy_lsp_message_parser parser;
  parser.append(u8"cOntEnT-lEnGtH: 5\r\n\r\nhello");
  EXPECT_THAT(parser.messages(), ElementsAre(u8"hello"));
}

TEST(test_lsp_message_parser, content_length_allows_leading_zeros) {
  spy_lsp_message_parser parser;
  parser.append(u8"Content-Length: 0002\r\n\r\nhi");
  EXPECT_THAT(parser.messages(), ElementsAre(u8"hi"));
}

TEST(test_lsp_message_parser, small_message_one_byte_at_a_time) {
  spy_lsp_message_parser parser;
  string8 full_message = u8"Content-Length: 2\r\n\r\nhi";
  for (char8 c : full_message) {
    parser.append(string8(1, c));
  }
  EXPECT_THAT(parser.messages(), ElementsAre(u8"hi"));
}

TEST(test_lsp_message_parser, two_messages) {
  spy_lsp_message_parser parser;
  parser.append(
      u8"Content-Length: 5\r\n\r\nhelloContent-Length: 5\r\n\r\nworld");
  EXPECT_THAT(parser.messages(), ElementsAre(u8"hello", u8"world"));
}

TEST(test_lsp_message_parser, two_messages_chunked) {
  {
    spy_lsp_message_parser parser;
    parser.append(u8"Content-Length: 5\r\n\r\nhelloContent");
    parser.append(u8"-Length: 5\r\n\r\nworld");
    EXPECT_THAT(parser.messages(), ElementsAre(u8"hello", u8"world"));
  }

  {
    spy_lsp_message_parser parser;
    parser.append(u8"Content-Length: 5\r\n\r\nhel");
    parser.append(u8"loContent-Length: 5\r\n\r\nworld");
    EXPECT_THAT(parser.messages(), ElementsAre(u8"hello", u8"world"));
  }
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
