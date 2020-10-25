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
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
namespace {
void expect_null_terminated(const padded_string &);
}

TEST(test_padded_string, default_constructed_string_has_following_null_bytes) {
  padded_string padded;
  EXPECT_EQ(padded.size(), 0);
  expect_null_terminated(padded);
}

TEST(test_padded_string, empty_string_has_following_null_bytes) {
  string8 s = u8"";
  padded_string padded(std::move(s));
  expect_null_terminated(padded);
}

TEST(test_padded_string, size_excludes_padding_bytes) {
  string8 s = u8"hello";
  padded_string padded(std::move(s));
  EXPECT_EQ(padded.size(), 5);
}

TEST(test_padded_string, resize_with_bigger_size_adds_new_characters) {
  padded_string s(u8"hello");

  s.resize(10);

  EXPECT_EQ(s.size(), 10);
  EXPECT_STREQ(reinterpret_cast<const char *>(s.c_str()), "hello");
  expect_null_terminated(s);
}

TEST(test_padded_string, resize_with_smaller_size_removes_characters) {
  padded_string s(u8"helloworld");

  s.resize(5);

  EXPECT_EQ(s.size(), 5);
  EXPECT_STREQ(reinterpret_cast<const char *>(s.c_str()), "hello");
  expect_null_terminated(s);
}

TEST(test_padded_string, comparing_with_string_view_excludes_padding_bytes) {
  EXPECT_TRUE(padded_string(string8(u8"hello")) == string8_view(u8"hello"));
}

TEST(test_padded_string, writing_to_ostream_does_not_include_padding_bytes) {
  padded_string s(u8"hello");
  std::ostringstream stream;
  stream << "BEFORE" << s << "AFTER";
  EXPECT_EQ(stream.str(), "BEFOREhelloAFTER");
}

TEST(test_padded_string, std_string_view_excludes_padding_bytes) {
  padded_string s(string8(u8"hello"));
  EXPECT_TRUE(s.string_view() == string8_view(u8"hello"));
}

namespace {
void expect_null_terminated(const padded_string &s) {
  const char8 *data = s.c_str();
  for (int i = 0; i < s.padding_size; ++i) {
    int index = s.size() + i;
    EXPECT_EQ(data[index], u8'\0') << "index=" << index;
  }
}
}
}
