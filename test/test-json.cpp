// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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
