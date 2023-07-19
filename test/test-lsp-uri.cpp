// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/lsp/lsp-uri.h>
#include <string>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(Test_LSP_URI, file_from_lsp_uri_posix) {
  auto parse = parse_file_from_lsp_uri_posix;

  EXPECT_EQ(parse(u8"file:///"_sv), "/");
  EXPECT_EQ(parse(u8"file:///x"_sv), "/x");
  EXPECT_EQ(parse(u8"file:///dir/subdir/"_sv), "/dir/subdir/");
  EXPECT_EQ(parse(u8"file:///dir/subdir/file.js"_sv), "/dir/subdir/file.js");

  // With percent-encoded characters:
  EXPECT_EQ(parse(u8"file:///ab%63"_sv), "/abc");
  EXPECT_EQ(parse(u8"file:///%61%62%63"_sv), "/abc");
  EXPECT_EQ(parse(u8"file:///sla%2Fsh"_sv), "/sla/sh")
      << "%2F should confusingly decode to '/'";

  // With optional components:
  EXPECT_EQ(parse(u8"file:///f?query"_sv), "/f");
  EXPECT_EQ(parse(u8"file:///f#fragment"_sv), "/f");
  EXPECT_EQ(parse(u8"file:///f?query#fragment"_sv), "/f");

  // With authority:
  EXPECT_EQ(parse(u8"file://server"_sv), "//server");
  EXPECT_EQ(parse(u8"file://server?query"_sv), "//server");
  EXPECT_EQ(parse(u8"file://server#fragment"_sv), "//server");
  EXPECT_EQ(parse(u8"file://server/x"_sv), "//server/x");
}

TEST(Test_LSP_URI, file_from_lsp_uri_win32) {
  auto parse = parse_file_from_lsp_uri_win32;

  EXPECT_EQ(parse(u8"file:///c:"_sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:/"_sv), R"(c:\)");
  EXPECT_EQ(parse(u8"file:///c:/dir/subdir/"_sv), R"(c:\dir\subdir\)");
  EXPECT_EQ(parse(u8"file:///c:/dir/subdir/file.js"_sv),
            R"(c:\dir\subdir\file.js)");
  EXPECT_EQ(parse(u8"file:///X:/"_sv), R"(X:\)");

  // POSIX-style paths:
  EXPECT_EQ(parse(u8"file:///c"_sv), R"(\c)");
  EXPECT_EQ(parse(u8"file:///c:x"_sv), R"(\c:x)");
  EXPECT_EQ(parse(u8"file:///c:x/"_sv), R"(\c:x\)");
  EXPECT_EQ(parse(u8"file:///1:/"_sv), R"(\1:\)");

  // With percent-encoded characters:
  EXPECT_EQ(parse(u8"file:///%63:/"_sv), R"(c:\)");
  EXPECT_EQ(parse(u8"file:///c:/ab%63"_sv), R"(c:\abc)");
  EXPECT_EQ(parse(u8"file:///c:/%61%62%63"_sv), R"(c:\abc)");
  EXPECT_THAT(parse(u8"file:///c:/sla%2Fsh"_sv),
              ::testing::AnyOf(R"(c:\sla\sh)", R"(c:\sla/sh)"))
      << "%2F should confusingly decode to '/' or '\\'";
  EXPECT_EQ(parse(u8"file:///c:/sla%5Csh"_sv), R"(c:\sla\sh)")
      << "%5C should decode to '\\'";
  EXPECT_EQ(parse(u8"file:///c:%2Fname"_sv), R"(c:\name)");
  EXPECT_EQ(parse(u8"file:///c:%5Cname"_sv), R"(c:\name)");

  // With optional components:
  EXPECT_EQ(parse(u8"file:///c:?query"_sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:#fragment"_sv), R"(c:)");
  EXPECT_EQ(parse(u8"file:///c:?query#fragment"_sv), R"(c:)");

  // With authority:
  EXPECT_EQ(parse(u8"file://server"_sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server?query"_sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server#fragment"_sv), R"(\\server)");
  EXPECT_EQ(parse(u8"file://server/x"_sv), R"(\\server\x)");

  // Drive letter imposter in authority:
  EXPECT_EQ(parse(u8"file://c:/x"_sv), R"(\\c:\x)");
}

TEST(Test_LSP_URI, file_from_lsp_uri_invalid_file_uri) {
  for (auto parse :
       {parse_file_from_lsp_uri_posix, parse_file_from_lsp_uri_win32}) {
    EXPECT_EQ(parse(u8"http://example.com/path"_sv), "");
    EXPECT_EQ(parse(u8"file:/x/y/z/w"_sv), "");
    EXPECT_EQ(parse(u8"file://"_sv), "");
    EXPECT_EQ(parse(u8"file:///%6"_sv), "");
    EXPECT_EQ(parse(u8"file:///%6q"_sv), "");
    EXPECT_EQ(parse(u8"file:///%q6"_sv), "");
    EXPECT_EQ(parse(u8"file:///%"_sv), "");
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
