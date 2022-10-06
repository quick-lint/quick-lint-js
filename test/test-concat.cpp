// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <string_view>

using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_concat, string_literals) {
  EXPECT_EQ(concat("hello", "world"), "helloworld"s);
  EXPECT_EQ(concat("hello", "beautiful", "world"), "hellobeautifulworld"s);
  EXPECT_EQ(concat("hello", "beautiful", "world", "!"),
            "hellobeautifulworld!"s);

  EXPECT_EQ(concat(u8"hello", u8"world"), u8"helloworld"s);
  EXPECT_EQ(concat(u8"hello", u8"beautiful", u8"world"),
            u8"hellobeautifulworld"s);
  EXPECT_EQ(concat(u8"hello", u8"beautiful", u8"world", u8"!"),
            u8"hellobeautifulworld!"s);
}

TEST(test_concat, string_views) {
  EXPECT_EQ(concat("hello"sv, "world"sv), "helloworld"s);
  EXPECT_EQ(concat("hello"sv, "beautiful"sv, "world"sv),
            "hellobeautifulworld"s);
  EXPECT_EQ(concat("hello"sv, "beautiful"sv, "world"sv, "!"sv),
            "hellobeautifulworld!"s);

  EXPECT_EQ(concat(u8"hello"_sv, u8"world"_sv), u8"helloworld"s);
  EXPECT_EQ(concat(u8"hello"_sv, u8"beautiful"_sv, u8"world"_sv),
            u8"hellobeautifulworld"s);
  EXPECT_EQ(concat(u8"hello"_sv, u8"beautiful"_sv, u8"world"_sv, u8"!"_sv),
            u8"hellobeautifulworld!"s);
}

TEST(test_concat, std_strings) {
  EXPECT_EQ(concat("hello"s, "world"s), "helloworld"s);
  EXPECT_EQ(concat("hello"s, "beautiful"s, "world"s), "hellobeautifulworld"s);
  EXPECT_EQ(concat("hello"s, "beautiful"s, "world"s, "!"s),
            "hellobeautifulworld!"s);

  EXPECT_EQ(concat(u8"hello"s, u8"world"s), u8"helloworld"s);
  EXPECT_EQ(concat(u8"hello"s, u8"beautiful"s, u8"world"s),
            u8"hellobeautifulworld"s);
  EXPECT_EQ(concat(u8"hello"s, u8"beautiful"s, u8"world"s, u8"!"s),
            u8"hellobeautifulworld!"s);
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
