// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/narrow-cast.h>
#include <sstream>

namespace quick_lint_js {
namespace {
TEST(test_json, escapes_backslashes) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8R"(hello\world)"));
  EXPECT_EQ(json.str(), R"(hello\\world)");
}

TEST(test_json, escapes_double_quotes) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8R"(hello"world)"));
  EXPECT_EQ(json.str(), R"(hello\"world)");
}

TEST(test_json, escapes_newlines) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8"hello\nworld"));
  EXPECT_EQ(json.str(), R"(hello\nworld)");
}

TEST(test_json, escapes_tabs) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8"hello\tworld"));
  EXPECT_EQ(json.str(), R"(hello\tworld)");
}

TEST(test_json, escapes_carriage_returns) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8"hello\rworld"));
  EXPECT_EQ(json.str(), R"(hello\rworld)");
}

TEST(test_json, escapes_backspaces) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8"hello\bworld"));
  EXPECT_EQ(json.str(), R"(hello\bworld)");
}

TEST(test_json, escapes_form_feeds) {
  std::ostringstream json;
  write_json_escaped_string(json, string8_view(u8"hello\fworld"));
  EXPECT_EQ(json.str(), R"(hello\fworld)");
}

TEST(test_json, ascii_characters_are_parsable_by_boost_json) {
  for (int c = 0; c < 128; ++c) {
    string8 string = string8(u8"hello") + narrow_cast<char8>(c) + u8"world";
    SCOPED_TRACE(out_string8(string));

    std::ostringstream json;
    json << '"';
    write_json_escaped_string(json, string8_view(string));
    json << '"';
    SCOPED_TRACE(json.str());

    std::error_code error;
    ::boost::json::value parsed = ::boost::json::parse(json.str(), error);
    EXPECT_FALSE(error);
    EXPECT_EQ(parsed, to_string_view(string));
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
