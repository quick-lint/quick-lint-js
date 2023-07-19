// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <string>
#include <string_view>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
namespace {
void expect_null_terminated(const Padded_String &);
}

TEST(Test_Padded_String, default_constructed_string_has_following_null_bytes) {
  Padded_String padded;
  EXPECT_EQ(padded.size(), 0);
  expect_null_terminated(padded);
}

TEST(Test_Padded_String, empty_string_has_following_null_bytes) {
  String8 s = u8"";
  Padded_String padded(std::move(s));
  expect_null_terminated(padded);
}

TEST(Test_Padded_String, size_excludes_padding_bytes) {
  String8 s = u8"hello";
  Padded_String padded(std::move(s));
  EXPECT_EQ(padded.size(), 5);
}

TEST(Test_Padded_String, resize_with_bigger_size_adds_new_characters) {
  Padded_String s(u8"hello"_sv);

  s.resize(10);

  EXPECT_EQ(s.size(), 10);
  EXPECT_EQ(s.string_view(), u8"hello\0\0\0\0\0"_sv);
  expect_null_terminated(s);
}

TEST(Test_Padded_String, resize_grow_uninitialized_preserves_original_data) {
  Padded_String s(u8"hello"_sv);

  s.resize_grow_uninitialized(10);

  EXPECT_EQ(s.size(), 10);
  EXPECT_EQ(s.string_view().substr(0, 5), u8"hello"_sv);
  expect_null_terminated(s);
  // Don't read indexes 5 through 9. The data is uninitialized and could be
  // anything.
}

TEST(Test_Padded_String, resize_with_smaller_size_removes_characters) {
  Padded_String s(u8"helloworld"_sv);

  s.resize(5);

  EXPECT_EQ(s.size(), 5);
  EXPECT_EQ(s.string_view(), u8"hello"_sv);
  expect_null_terminated(s);
}

TEST(Test_Padded_String, comparing_with_string_view_excludes_padding_bytes) {
  EXPECT_TRUE(Padded_String(String8(u8"hello")) == u8"hello"_sv);
}

TEST(Test_Padded_String, writing_to_ostream_does_not_include_padding_bytes) {
  Padded_String s(u8"hello"_sv);
  std::ostringstream stream;
  stream << "BEFORE" << s << "AFTER";
  EXPECT_EQ(stream.str(), "BEFOREhelloAFTER");
}

TEST(Test_Padded_String, std_string_view_excludes_padding_bytes) {
  Padded_String s(String8(u8"hello"));
  EXPECT_TRUE(s.string_view() == u8"hello"_sv);
}

TEST(Test_Padded_String, shrinking_does_not_reallocate) {
  Padded_String s(u8"helloworld"_sv);
  const Char8 *old_data = s.data();
  s.resize(5);
  EXPECT_EQ(s.data(), old_data);
  s.resize(1);
  EXPECT_EQ(s.data(), old_data);
}

TEST(Test_Padded_String, move_constructing_does_not_invalidate_pointers) {
  Padded_String s1(u8"helloworld"_sv);
  const Char8 *old_s1_data = s1.data();
  Padded_String s2(std::move(s1));
  EXPECT_EQ(s2.data(), old_s1_data) << "moving should not reallocate";
  EXPECT_EQ(s2.string_view(), u8"helloworld"_sv)
      << "moving should not change data";
  expect_null_terminated(s2);
}

TEST(Test_Padded_String,
     move_constructing_empty_string_does_not_invalidate_pointers) {
  Padded_String s1;
  const Char8 *old_s1_data = s1.data();
  Padded_String s2(std::move(s1));
  EXPECT_EQ(s2.data(), old_s1_data) << "moving should not reallocate";
  EXPECT_EQ(s2.string_view(), u8""_sv) << "moving should not change data";
  expect_null_terminated(s2);
}

TEST(Test_Padded_String, move_assigning_copies_pointers) {
  Padded_String s1(u8"helloworld"_sv);
  const Char8 *old_s1_data = s1.data();
  Padded_String s2(u8"other"_sv);
  s2 = std::move(s1);
  EXPECT_EQ(s2.data(), old_s1_data) << "moving should not reallocate";
  EXPECT_EQ(s2.string_view(), u8"helloworld"_sv)
      << "moving should not change data";
  expect_null_terminated(s2);
}

TEST(Test_Padded_String, move_assigning_empty_string_copies_pointers) {
  Padded_String s1;
  const Char8 *old_s1_data = s1.data();
  Padded_String s2(u8"other"_sv);
  s2 = std::move(s1);
  EXPECT_EQ(s2.data(), old_s1_data) << "moving should not reallocate";
  EXPECT_EQ(s2.string_view(), u8""_sv) << "moving should not change data";
  expect_null_terminated(s2);
}

namespace {
void expect_null_terminated(const Padded_String &s) {
  const Char8 *data = s.c_str();
  for (Padded_String_Size i = 0; i < s.padding_size; ++i) {
    Padded_String_Size index = s.size() + i;
    EXPECT_EQ(data[index], u8'\0') << "index=" << index;
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
