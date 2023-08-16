// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdio>
#include <gtest/gtest.h>
#include <limits>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/float.h>

QLJS_WARNING_IGNORE_CLANG("-Wformat-nonliteral")
QLJS_WARNING_IGNORE_GCC("-Wformat-nonliteral")
QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")
QLJS_WARNING_IGNORE_GCC("-Wtype-limits")

namespace quick_lint_js {
namespace {
template <class T>
String8 write_decimal_float(T value) {
  std::array<Char8, max_decimal_float_string_length<T>> chars;
  fill(chars, 'x');
  Char8* end = quick_lint_js::write_decimal_float(value, chars.data());
  EXPECT_LE(end - chars.data(), chars.size());
  return String8(chars.data(), end);
}

template <class T>
class Test_Write_Float : public ::testing::Test {};
using Test_Write_Float_Types = ::testing::Types<float, double>;
TYPED_TEST_SUITE(Test_Write_Float, Test_Write_Float_Types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(Test_Write_Float, common_floats) {
  using T = TypeParam;

  EXPECT_EQ(write_decimal_float(T(0.0)), u8"0"_sv);
  EXPECT_EQ(write_decimal_float(T(1234.0)), u8"1234"_sv);
  EXPECT_EQ(write_decimal_float(T(-42.0)), u8"-42"_sv);
  EXPECT_EQ(write_decimal_float(T(0.5)), u8"0.5"_sv);
}

TYPED_TEST(Test_Write_Float, round_trip_with_std_from_chars) {
  using T = TypeParam;

  std::vector<T> test_cases = {
      T(1) / T(3),
      T((std::numeric_limits<float>::max)()),
      T((std::numeric_limits<float>::min)()),
      T(std::numeric_limits<float>::lowest()),
  };
  if constexpr (sizeof(T) >= sizeof(double)) {
    test_cases.push_back((std::numeric_limits<double>::max)());
    test_cases.push_back((std::numeric_limits<double>::min)());
    test_cases.push_back(std::numeric_limits<double>::lowest());
  }

  for (T test_case : test_cases) {
    SCOPED_TRACE(test_case);

    String8 buffer = write_decimal_float(test_case);
    SCOPED_TRACE(out_string8(buffer));
    const char* buffer_begin = reinterpret_cast<char*>(buffer.data());

    const char* format;
    if (std::is_same_v<T, double>) {
      format = "%lf";
    } else if (std::is_same_v<T, float>) {
      format = "%f";
    } else {
      ADD_FAILURE() << "could not determine scanf format string for type";
    }
    T parsed_value;
    int matches = std::sscanf(buffer_begin, format, &parsed_value);
    ASSERT_EQ(matches, 1) << "number should be parsable by sscanf";

    EXPECT_EQ(parsed_value, test_case);
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
