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
#include <quick-lint-js/json.h>
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

// TODO(strager): Should we escape other characters too? \t? \r?
}
}
