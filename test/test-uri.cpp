// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/uri.h>
#include <string>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_uri, file_from_lsp_uri_posix) {
  auto parse = parse_file_from_lsp_uri_posix;

  EXPECT_EQ(parse(u8"file:///"sv), "/");
  EXPECT_EQ(parse(u8"file:///x"sv), "/x");
  EXPECT_EQ(parse(u8"file:///dir/subdir/"sv), "/dir/subdir/");
  EXPECT_EQ(parse(u8"file:///dir/subdir/file.js"sv), "/dir/subdir/file.js");

  // With percent-encoded characters:
  EXPECT_EQ(parse(u8"file:///ab%63"sv), "/abc");
  EXPECT_EQ(parse(u8"file:///%61%62%63"sv), "/abc");
  EXPECT_EQ(parse(u8"file:///sla%2Fsh"sv), "/sla/sh")
      << "%2F should confusingly decode to '/'";

  // With optional components:
  EXPECT_EQ(parse(u8"file:///f?query"sv), "/f");
  EXPECT_EQ(parse(u8"file:///f#fragment"sv), "/f");
  EXPECT_EQ(parse(u8"file:///f?query#fragment"sv), "/f");

  // With authority:
  EXPECT_EQ(parse(u8"file://server"sv), "//server");
  EXPECT_EQ(parse(u8"file://server?query"sv), "//server");
  EXPECT_EQ(parse(u8"file://server#fragment"sv), "//server");
  EXPECT_EQ(parse(u8"file://server/x"sv), "//server/x");
}

TEST(test_uri, file_from_lsp_uri_win32) {
  auto parse = parse_file_from_lsp_uri_win32;

  EXPECT_EQ(parse(u8"file:///c:"sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:/"sv), R"(c:\)");
  EXPECT_EQ(parse(u8"file:///c:/dir/subdir/"sv), R"(c:\dir\subdir\)");
  EXPECT_EQ(parse(u8"file:///c:/dir/subdir/file.js"sv),
            R"(c:\dir\subdir\file.js)");
  EXPECT_EQ(parse(u8"file:///X:/"sv), R"(X:\)");

  // POSIX-style paths:
  EXPECT_EQ(parse(u8"file:///c"sv), R"(\c)");
  EXPECT_EQ(parse(u8"file:///c:x"sv), R"(\c:x)");
  EXPECT_EQ(parse(u8"file:///c:x/"sv), R"(\c:x\)");
  EXPECT_EQ(parse(u8"file:///1:/"sv), R"(\1:\)");

  // With percent-encoded characters:
  EXPECT_EQ(parse(u8"file:///%63:/"sv), R"(c:\)");
  EXPECT_EQ(parse(u8"file:///c:/ab%63"sv), R"(c:\abc)");
  EXPECT_EQ(parse(u8"file:///c:/%61%62%63"sv), R"(c:\abc)");
  EXPECT_THAT(parse(u8"file:///c:/sla%2Fsh"sv),
              ::testing::AnyOf(R"(c:\sla\sh)", R"(c:\sla/sh)"))
      << "%2F should confusingly decode to '/' or '\\'";
  EXPECT_EQ(parse(u8"file:///c:/sla%5Csh"sv), R"(c:\sla\sh)")
      << "%5C should decode to '\\'";
  EXPECT_EQ(parse(u8"file:///c:%2Fname"sv), R"(c:\name)");
  EXPECT_EQ(parse(u8"file:///c:%5Cname"sv), R"(c:\name)");

  // With optional components:
  EXPECT_EQ(parse(u8"file:///c:?query"sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:#fragment"sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:?query#fragment"sv), R"(c:)");

  // With authority:
  EXPECT_EQ(parse(u8"file://server"sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server?query"sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server#fragment"sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server/x"sv), R"(\\server\x)");

  // Drive letter imposter in authority:
  EXPECT_EQ(parse(u8"file://c:/x"sv), R"(\\c:\x)");
}

TEST(test_uri, file_from_lsp_uri_invalid_file_uri) {
  for (auto parse :
       {parse_file_from_lsp_uri_posix, parse_file_from_lsp_uri_win32}) {
    EXPECT_EQ(parse(u8"http://example.com/path"sv), "");
    EXPECT_EQ(parse(u8"file:/x/y/z/w"sv), "");
    EXPECT_EQ(parse(u8"file://"sv), "");
    EXPECT_EQ(parse(u8"file:///%6"sv), "");
    EXPECT_EQ(parse(u8"file:///%6q"sv), "");
    EXPECT_EQ(parse(u8"file:///%q6"sv), "");
    EXPECT_EQ(parse(u8"file:///%"sv), "");
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
