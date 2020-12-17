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

#include <array>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/gtest-char8.h>
#include <quick-lint-js/utf-8.h>

#define EXPECT_ENCODE_UTF_8(code_point, expected)        \
  do {                                                   \
    string8 out((expected).size(), u8'\0');              \
    char8* end = encode_utf_8((code_point), out.data()); \
    EXPECT_EQ(end - out.data(), (expected).size());      \
    EXPECT_EQ(out, (expected));                          \
  } while (false)

namespace quick_lint_js {
namespace {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
string8_view operator""_s8v(const char* string, std::size_t length) noexcept {
  return string8_view(reinterpret_cast<const char8*>(string), length);
}
QLJS_WARNING_POP
}

TEST(test_utf_8_encode, ascii) {
  std::array<char8, 1> out;
  char8* end = encode_utf_8(U'x', out.data());
  EXPECT_EQ(end - out.data(), 1);
  EXPECT_EQ(out, make_array(u8'x'));
}

TEST(test_utf_8_encode, one_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0000', "\x00"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u007f', "\x7f"_s8v);
}

TEST(test_utf_8_encode, two_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\u00a2', "\xc2\xa2"_s8v);
}

TEST(test_utf_8_encode, two_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0080', "\xc2\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u07ff', "\xdf\xbf"_s8v);
}

TEST(test_utf_8_encode, three_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\u0939', "\xe0\xa4\xb9"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u20ac', "\xe2\x82\xac"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ud55c', "\xed\x95\x9c"_s8v);
}

TEST(test_utf_8_encode, three_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0800', "\xe0\xa0\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ud7ff', "\xed\x9f\xbf"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ue000', "\xee\x80\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\uffff', "\xef\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, non_standard_surrogate_code_points) {
  EXPECT_ENCODE_UTF_8(U'\xd800', "\xed\xa0\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\xdfff', "\xed\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, four_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\U00010348', "\xf0\x90\x8d\x88"_s8v);
}

TEST(test_utf_8_encode, four_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\U00010000', "\xf0\x90\x80\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\U0010ffff', "\xf4\x8f\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, non_standard_four_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\x001fffff', "\xf7\xbf\xbf\xbf"_s8v);
}
}
