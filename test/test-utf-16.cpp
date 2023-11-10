// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
#if defined(_WIN32)
TEST(Test_UTF16_Windows, MBArgv) {
  std::vector<wchar_t*> argv;
  std::wstring first = std::wstring(L"-h");
  std::wstring second = std::wstring(L"\xd83c\xdf55");
  argv.emplace_back(first.data());
  argv.emplace_back(second.data());
  quick_lint_js::MBArgv m(quick_lint_js::narrow_cast<int>(argv.size()),
                          argv.data());
  EXPECT_STREQ(m.data()[0], "-h");
  EXPECT_STREQ(m.data()[1], "\xf0\x9f\x8d\x95");
}

TEST(Test_UTF16_Windows, mbstring_to_wstring) {
  {
    std::string mbstring = std::string("-h");
    std::optional<std::wstring> wstring =
        quick_lint_js::mbstring_to_wstring(mbstring.c_str());
    EXPECT_STREQ(wstring.value().c_str(), L"-h");
  }
  {
    std::string mbstring = std::string("\xf0\x9f\x8d\x95");
    std::optional<std::wstring> wstring =
        quick_lint_js::mbstring_to_wstring(mbstring.c_str());
    EXPECT_STREQ(wstring.value().c_str(), L"\xd83c\xdf55");
  }
}

TEST(Test_UTF16_Windows, wstring_to_mbstring) {
  EXPECT_EQ(wstring_to_mbstring(L""sv).value(), "");
  EXPECT_EQ(wstring_to_mbstring(L"-h"sv).value(), "-h");
  EXPECT_EQ(wstring_to_mbstring(L"\xd83c\xdf55").value(), "\xf0\x9f\x8d\x95");
}
#endif

TEST(Test_UTF16, utf_16_to_utf_8) {
  EXPECT_EQ(utf_16_to_utf_8(u""sv), u8"");

  // One UTF-8 code unit per code point:
  EXPECT_EQ(utf_16_to_utf_8(u"-h"sv), u8"-h");

  // Two UTF-8 code units per code point:
  EXPECT_EQ(utf_16_to_utf_8(u"\u0080"), u8"\u0080");
  EXPECT_EQ(utf_16_to_utf_8(u"\u0567"), u8"\u0567");
  EXPECT_EQ(utf_16_to_utf_8(u"\u07ff"), u8"\u07ff");

  // Three UTF-8 code units per code point:
  EXPECT_EQ(utf_16_to_utf_8(u"\u0800"), u8"\u0800");
  EXPECT_EQ(utf_16_to_utf_8(u"\uabcd"), u8"\uabcd");
  EXPECT_EQ(utf_16_to_utf_8(u"\uffff"), u8"\uffff");

  // Surrogate pairs (four UTF-8 code units per code point):
  EXPECT_EQ(utf_16_to_utf_8(u"\U00010000"), u8"\U00010000");
  EXPECT_EQ(utf_16_to_utf_8(u"\U000fedcb"), u8"\U000fedcb");
  EXPECT_EQ(utf_16_to_utf_8(u"\U0010ffff"), u8"\U0010ffff");
}

TEST(Test_UTF16, invalid_utf_16_to_utf_8_adds_replacement_character) {
  // U+D800 through U+DFFF are reserved.
  // If naively coded, U+D83C would be 'ed a0 bc' in UTF-8.
  // If naively coded, U+DF55 would be 'ef bf bd' in UTF-8.

  // U+0FEDCB is 'f3 be b7 8b' in UTF-8.

  // Incomplete surrogate pair (high surrogate only):
  EXPECT_EQ(utf_16_to_utf_8(u"\xd83c"), "\xed\xa0\xbc"_s8v);
  EXPECT_EQ(utf_16_to_utf_8(u"\xd83cxyz"), "\xed\xa0\xbcxyz"_s8v);
  EXPECT_EQ(utf_16_to_utf_8(u"\xd83c\U000fedcb"),
            "\xed\xa0\xbc"_s8v
            "\xf3\xbe\xb7\x8b"_s8v);

  // Incomplete surrogate pair (low surrogate only):
  EXPECT_EQ(utf_16_to_utf_8(u"\xdf55"), "\xed\xbd\x95"_s8v);
  EXPECT_EQ(utf_16_to_utf_8(u"\xdf55xyz"), "\xed\xbd\x95xyz"_s8v);
  EXPECT_EQ(utf_16_to_utf_8(u"\xdf55\U000fedcb"),
            "\xed\xbd\x95"_s8v
            "\xf3\xbe\xb7\x8b"_s8v);

  // Reversed surrogate pair:
  EXPECT_EQ(utf_16_to_utf_8(u"\xdf55\xd83c"),
            "\xed\xbd\x95"_s8v
            "\xed\xa0\xbc"_s8v);
}

TEST(Test_Count_UTF8_Code_Units_In_UTF16, empty_string) {
  EXPECT_EQ(count_utf_8_code_units(u""sv), 0);
}

TEST(Test_Count_UTF8_Code_Units_In_UTF16, ascii) {
  EXPECT_EQ(count_utf_8_code_units(u"abc 123"sv), 7);
  EXPECT_EQ(count_utf_8_code_units(u"\u007f"sv), 1);
  EXPECT_EQ(count_utf_8_code_units(u"\u0000"sv), 1);
}

TEST(Test_Count_UTF8_Code_Units_In_UTF16, 2_byte_utf_8) {
  EXPECT_EQ(count_utf_8_code_units(u"\u0080"sv), 2);
  EXPECT_EQ(count_utf_8_code_units(u"\u07ff"sv), 2);
}

TEST(Test_Count_UTF8_Code_Units_In_UTF16, 3_byte_utf_8) {
  EXPECT_EQ(count_utf_8_code_units(u"\u0800"sv), 3);
  EXPECT_EQ(count_utf_8_code_units(u"\uffff"sv), 3);
}

TEST(Test_Count_UTF8_Code_Units_In_UTF16, surrogate_pair) {
  EXPECT_EQ(count_utf_8_code_units(u"\U00010437"sv), 4) << "D801 DC37";
  EXPECT_EQ(count_utf_8_code_units(u"\U00024B62"sv), 4) << "D852 DF62";
}

// TODO(strager): How should count_utf_8_code_units behave with invalid UTF-16
// (e.g. incomplete surrogate pair)?
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
