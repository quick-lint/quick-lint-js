// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// LSP specification:
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-lsp-message-parser.h>
#include <vector>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(Test_LSP_Message_Parser, small_full_message) {
  Spy_LSP_Message_Parser parser;
  parser.append(u8"Content-Length: 2\r\n\r\nhi"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hi"}));
}

TEST(Test_LSP_Message_Parser, content_type_header_is_ignored) {
  {
    Spy_LSP_Message_Parser parser;
    parser.append(
        u8"Content-Length: 2\r\nContent-Type: application/vscode-jsonrpc; "
        u8"charset=utf-8\r\n\r\nhi"_sv);
    EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hi"}));
  }

  {
    Spy_LSP_Message_Parser parser;
    parser.append(
        u8"Content-Type: application/vscode-jsonrpc; "
        u8"charset=utf-8\r\nContent-Length: 2\r\n\r\nhi"_sv);
    EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hi"}));
  }
}

TEST(Test_LSP_Message_Parser, content_length_header_is_case_insensitive) {
  Spy_LSP_Message_Parser parser;
  parser.append(u8"cOntEnT-lEnGtH: 5\r\n\r\nhello"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello"}));
}

TEST(Test_LSP_Message_Parser, content_length_allows_leading_zeros) {
  Spy_LSP_Message_Parser parser;
  parser.append(u8"Content-Length: 0002\r\n\r\nhi"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hi"}));
}

TEST(Test_LSP_Message_Parser, small_message_one_byte_at_a_time) {
  Spy_LSP_Message_Parser parser;
  String8 full_message = u8"Content-Length: 2\r\n\r\nhi";
  for (Char8 c : full_message) {
    parser.append(String8(1, c));
  }
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hi"}));
}

TEST(Test_LSP_Message_Parser, two_messages) {
  Spy_LSP_Message_Parser parser;
  parser.append(
      u8"Content-Length: 5\r\n\r\nhelloContent-Length: 5\r\n\r\nworld"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello", u8"world"}));
}

TEST(Test_LSP_Message_Parser,
     missing_content_length_is_treated_as_empty_message) {
  Spy_LSP_Message_Parser parser;
  parser.append(
      u8"not-content-length: 10\r\n\r\nContent-Length: 5\r\n\r\nhello"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello"}));
}

TEST(Test_LSP_Message_Parser, content_length_with_not_number_is_ignored) {
  Spy_LSP_Message_Parser parser;
  parser.append(u8"Content-Length: asdf\r\nContent-Length: 5\r\n\r\nhello"_sv);
  EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello"}));
}

TEST(Test_LSP_Message_Parser, malformed_headers_are_ignored) {
  for (
      String8_View message : {
          // No header name:
          u8"\r\nContent-Length: 5\r\n\r\nhello"_sv,
          // No header value:
          u8"Content-Length\r\nContent-Length: 5\r\n\r\nhello"_sv,
          // Other:
          u8"Content-Length Content-Length: 3\r\nContent-Length: 5\r\n\r\nhello"_sv,
      }) {
    SCOPED_TRACE(out_string8(message));
    Spy_LSP_Message_Parser parser;
    parser.append(message);
    EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello"}));
  }
}

TEST(Test_LSP_Message_Parser, two_messages_chunked) {
  {
    Spy_LSP_Message_Parser parser;
    parser.append(u8"Content-Length: 5\r\n\r\nhelloContent"_sv);
    parser.append(u8"-Length: 5\r\n\r\nworld"_sv);
    EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello", u8"world"}));
  }

  {
    Spy_LSP_Message_Parser parser;
    parser.append(u8"Content-Length: 5\r\n\r\nhel"_sv);
    parser.append(u8"loContent-Length: 5\r\n\r\nworld"_sv);
    EXPECT_THAT(parser.messages(), ElementsAreArray({u8"hello", u8"world"}));
  }
}
}
}

#endif

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
