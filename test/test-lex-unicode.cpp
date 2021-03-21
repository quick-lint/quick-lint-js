// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdio>
#include <gtest/gtest.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <sstream>
#include <string>
#include <unicode/uchar.h>
#include <unicode/uversion.h>

namespace quick_lint_js {
namespace {
char32_t max_code_point = U'\U0010ffff';

std::string pretty(char32_t c) {
  std::array<char, 30> buffer;
  int rc = std::snprintf(buffer.data(), buffer.size(), "U+%04llx",
                         narrow_cast<unsigned long long>(c));
  QLJS_ASSERT(rc > 0);
  return std::string(buffer.data(), narrow_cast<std::size_t>(rc));
}

bool icu_data_is_valid() {
  std::uint8_t minimum_unicode_version = 13;

  UVersionInfo version;
  ::u_getUnicodeVersion(version);
  if (version[0] >= minimum_unicode_version) {
    return true;
  }

  static bool did_log_warning = false;
  if (!did_log_warning) {
    std::fprintf(stderr,
                 "warning: The ICU library has data for Unicode version "
                 "%u.%u.%u.%u, which is too old. Upgrade ICU to Unicode "
                 "version %u or newer. Skipping tests...\n",
                 version[0], version[1], version[2], version[3],
                 minimum_unicode_version);
    did_log_warning = true;
  }
  return false;
}

TEST(test_lex_unicode, is_initial_identifier_character) {
  if (!icu_data_is_valid()) {
    GTEST_SKIP();
  }

  for (char32_t c = 0; c <= max_code_point; ++c) {
    bool expected =
        c == U'$' ||  //
        c == U'_' ||  //
        ::u_hasBinaryProperty(narrow_cast<::UChar32>(c), UCHAR_ID_START);
    EXPECT_EQ(lexer::is_initial_identifier_character(c), expected) << pretty(c);
  }
}

TEST(test_lex_unicode, is_identifier_character) {
  if (!icu_data_is_valid()) {
    GTEST_SKIP();
  }

  for (char32_t c = 0; c <= max_code_point; ++c) {
    bool expected =
        c == U'$' ||       //
        c == U'\u200c' ||  //
        c == U'\u200d' ||
        ::u_hasBinaryProperty(narrow_cast<::UChar32>(c), UCHAR_ID_CONTINUE);
    EXPECT_EQ(lexer::is_identifier_character(c), expected) << pretty(c);
  }
}
}
}

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
