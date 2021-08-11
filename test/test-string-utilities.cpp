// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <csignal>
#include <cstdlib>
#include <gtest/gtest-death-test.h>
#include <gtest/gtest.h>
#include <quick-lint-js/string-utilities.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wformat-zero-length")

using ::testing::KilledBySignal;

namespace quick_lint_js {
namespace {
TEST(test_string_utilities_asprintf, pass_null_string_pointer) {
  ASSERT_DEATH({ asprintf(nullptr, ""); }, "");
}

TEST(test_string_utilities_asprintf, pass_normal_string_pointer) {
  char *str;
  int str_size;

  str_size = asprintf(&str, "normal_string_pointer");
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 21);
  EXPECT_STREQ(str, "normal_string_pointer");
  free(str);
}

TEST(test_string_utilities_asprintf, pass_empty_format) {
  char *str;
  int str_size;

  str_size = asprintf(&str, "");
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 0);
  EXPECT_STREQ(str, "");
  free(str);
}

TEST(test_string_utilities_asprintf, pass_ignorable_adicional_arguments) {
  char *str;
  int str_size;

  str_size = asprintf(&str, "", "", 123, "123");
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 0);
  EXPECT_STREQ(str, "");
  free(str);
}

TEST(test_string_utilities_asprintf, pass_multiple_formats) {
  char *str;
  int str_size;

  str_size = asprintf(&str, "Characters: %c %c \n", 'a', 65);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 17);
  EXPECT_STREQ(str, "Characters: a A \n");
  free(str);

  str_size = asprintf(&str, "Decimals: %d %ld\n", 1977, 650000L);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 22);
  EXPECT_STREQ(str, "Decimals: 1977 650000\n");
  free(str);

  str_size = asprintf(&str, "Preceding with blanks: %10d \n", 1977);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 35);
  EXPECT_STREQ(str, "Preceding with blanks:       1977 \n");
  free(str);

  str_size = asprintf(&str, "Preceding with zeros: %010d \n", 1977);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 34);
  EXPECT_STREQ(str, "Preceding with zeros: 0000001977 \n");
  free(str);

  str_size = asprintf(&str, "Some different radices: %d %x %o %#x %#o \n", 100,
                      100, 100, 100, 100);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 46);
  EXPECT_STREQ(str, "Some different radices: 100 64 144 0x64 0144 \n");
  free(str);

  str_size =
      asprintf(&str, "floats: %4.2f %+.0e %E \n", 3.1416, 3.1416, 3.1416);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 34);
  EXPECT_STREQ(str, "floats: 3.14 +3e+00 3.141600E+00 \n");
  free(str);

  str_size = asprintf(&str, "Width trick: %*d \n", 5, 10);
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 20);
  EXPECT_STREQ(str, "Width trick:    10 \n");
  free(str);

  str_size = asprintf(&str, "%s \n", "A string");
  EXPECT_NE(str, nullptr);
  EXPECT_EQ(str_size, 10);
  EXPECT_STREQ(str, "A string \n");
  free(str);
}
}  // namespace
}  // namespace quick_lint_js

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
