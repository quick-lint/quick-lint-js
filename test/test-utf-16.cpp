// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-16.h>

namespace quick_lint_js {
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
#endif
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
