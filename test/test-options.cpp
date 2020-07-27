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

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <string_view>
#include <vector>

using ::testing::IsEmpty;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
options parse_options(std::initializer_list<const char *> arguments) {
  std::vector<char *> argv;
  argv.emplace_back(const_cast<char *>("(program)"));
  for (const char *argument : arguments) {
    argv.emplace_back(const_cast<char *>(argument));
  }
  return quick_lint_js::parse_options(narrow_cast<int>(argv.size()),
                                      argv.data());
}

TEST(test_options, default_options_with_no_files) {
  options o = parse_options({});
  EXPECT_FALSE(o.print_parser_visits);
  EXPECT_THAT(o.files_to_lint, IsEmpty());
}

TEST(test_options, default_options_with_files) {
  options o = parse_options({"foo.js"});
  EXPECT_FALSE(o.print_parser_visits);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0], "foo.js"sv);
}

TEST(test_options, debug_parser_visits) {
  options o = parse_options({"--debug-parser-visits", "foo.js"});
  EXPECT_TRUE(o.print_parser_visits);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0], "foo.js"sv);
}

TEST(test_options, invalid_option) {
  options o = parse_options({"--option-does-not-exist", "foo.js"});
  ASSERT_EQ(o.error_unrecognized_options.size(), 1);
  EXPECT_EQ(o.error_unrecognized_options[0], "--option-does-not-exist"sv);
  EXPECT_THAT(o.files_to_lint, IsEmpty());
}
}  // namespace
}  // namespace quick_lint_js
