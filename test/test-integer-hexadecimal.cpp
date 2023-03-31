// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/integer.h>
#include <string_view>
#include <system_error>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")
QLJS_WARNING_IGNORE_GCC("-Wtype-limits")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
using test_parse_integer_exact_hexadecimal_types =
    ::testing::Types<char32_t, std::uint8_t>;

template <class T>
class test_parse_integer_exact_hexadecimal : public ::testing::Test {};
TYPED_TEST_SUITE(test_parse_integer_exact_hexadecimal,
                 test_parse_integer_exact_hexadecimal_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_parse_integer_exact_hexadecimal, common_integers) {
  {
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("0"sv, number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
  }

  {
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("12"sv, number);
    EXPECT_EQ(number, 0x12);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
  }

  {
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("ab"sv, number);
    EXPECT_EQ(number, 0xab);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
  }

  {
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("AB"sv, number);
    EXPECT_EQ(number, 0xab);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
  }
}

TYPED_TEST(test_parse_integer_exact_hexadecimal,
           negative_integers_are_disallowed) {
  if constexpr (std::is_unsigned_v<TypeParam>) {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("-12"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TYPED_TEST(test_parse_integer_exact_hexadecimal, minimum_integer) {
  if constexpr (std::is_unsigned_v<TypeParam>) {
    static_assert(std::numeric_limits<TypeParam>::min() == 0);
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("0"sv, number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
  }
}

TEST(test_parse_integer_exact_hexadecimal_char32_t, maximum_integer) {
  static_assert(std::numeric_limits<char32_t>::max() == 4294967295LL);
  char32_t number;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("ffffffff"sv, number);
  EXPECT_EQ(number, 4294967295LL);
  EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
}

TEST(test_parse_integer_exact_hexadecimal_uint8_t, maximum_integer) {
  static_assert(std::numeric_limits<std::uint8_t>::max() == 255);
  std::uint8_t number;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("ff"sv, number);
  EXPECT_EQ(number, 255);
  EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
}

TEST(test_parse_integer_exact_hexadecimal_char32_t, over_maximum_integer) {
  static_assert(std::numeric_limits<char32_t>::max() < 4294967296LL);
  char32_t number = 42;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("100000000"sv, number);
  EXPECT_EQ(parse_error, parse_integer_exact_error::out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TEST(test_parse_integer_exact_hexadecimal_uint8_t, over_maximum_integer) {
  static_assert(std::numeric_limits<std::uint8_t>::max() < 256);
  std::uint8_t number = 42;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("100"sv, number);
  EXPECT_EQ(parse_error, parse_integer_exact_error::out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_parse_integer_exact_hexadecimal, over_maximum_integer) {
  TypeParam number = 42;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("fffffffffffffffffff"sv, number);
  EXPECT_EQ(parse_error, parse_integer_exact_error::out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_parse_integer_exact_hexadecimal,
           extra_characters_after_are_not_parsed) {
  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("1fgh"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("ab   "sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TYPED_TEST(test_parse_integer_exact_hexadecimal, extra_characters_before) {
  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("  123"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("--123"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("+123"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TYPED_TEST(test_parse_integer_exact_hexadecimal, radix_prefix_is_not_special) {
  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("0x123a"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid)
        << "'x' is not a hex character";
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("0X123a"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::invalid)
        << "'X' is not a hex character";
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number;
    parse_integer_exact_error parse_error =
        parse_integer_exact_hex("077"sv, number);
    EXPECT_EQ(parse_error, parse_integer_exact_error::ok);
    EXPECT_EQ(number, 0x77);
  }
}

TYPED_TEST(test_parse_integer_exact_hexadecimal,
           empty_input_string_is_unrecognized) {
  TypeParam number = 42;
  parse_integer_exact_error parse_error = parse_integer_exact_hex(""sv, number);
  EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_parse_integer_exact_hexadecimal,
           minus_sign_without_digits_is_unrecognized) {
  TypeParam number = 42;
  parse_integer_exact_error parse_error =
      parse_integer_exact_hex("- 1"sv, number);
  EXPECT_EQ(parse_error, parse_integer_exact_error::invalid);
  EXPECT_EQ(number, 42) << "number should be unmodified";
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
