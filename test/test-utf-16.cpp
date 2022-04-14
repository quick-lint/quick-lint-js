// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-16.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
#if defined(_WIN32)
TEST(test_utf_16_windows, mbargv) {
  std::vector<wchar_t*> argv;
  std::wstring first = std::wstring(L"-h");
  std::wstring second = std::wstring(L"\xd83c\xdf55");
  argv.emplace_back(first.data());
  argv.emplace_back(second.data());
  quick_lint_js::mbargv m(quick_lint_js::narrow_cast<int>(argv.size()),
                          argv.data());
  EXPECT_STREQ(m.data()[0], "-h");
  EXPECT_STREQ(m.data()[1], "\xf0\x9f\x8d\x95");
}

TEST(test_utf_16_windows, mbstring_to_wstring) {
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

TEST(test_utf_16_windows, wstring_to_mbstring) {
  EXPECT_EQ(wstring_to_mbstring(L""sv).value(), "");
  EXPECT_EQ(wstring_to_mbstring(L"-h"sv).value(), "-h");
  EXPECT_EQ(wstring_to_mbstring(L"\xd83c\xdf55").value(), "\xf0\x9f\x8d\x95");
}
#endif

TEST(test_count_utf_8_code_units_in_utf_16, empty_string) {
  EXPECT_EQ(count_utf_8_code_units(u""sv), 0);
}

TEST(test_count_utf_8_code_units_in_utf_16, ascii) {
  EXPECT_EQ(count_utf_8_code_units(u"abc 123"sv), 7);
  EXPECT_EQ(count_utf_8_code_units(u"\u007f"sv), 1);
  EXPECT_EQ(count_utf_8_code_units(u"\u0000"sv), 1);
}

TEST(test_count_utf_8_code_units_in_utf_16, 2_byte_utf_8) {
  EXPECT_EQ(count_utf_8_code_units(u"\u0080"sv), 2);
  EXPECT_EQ(count_utf_8_code_units(u"\u07ff"sv), 2);
}

TEST(test_count_utf_8_code_units_in_utf_16, 3_byte_utf_8) {
  EXPECT_EQ(count_utf_8_code_units(u"\u0800"sv), 3);
  EXPECT_EQ(count_utf_8_code_units(u"\uffff"sv), 3);
}

TEST(test_count_utf_8_code_units_in_utf_16, surrogate_pair) {
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
