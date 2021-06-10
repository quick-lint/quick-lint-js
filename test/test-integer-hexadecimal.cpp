// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/warning.h>
#include <system_error>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")
QLJS_WARNING_IGNORE_GCC("-Wtype-limits")

namespace quick_lint_js {
namespace {
using test_integer_from_chars_hexadecimal_types =
    ::testing::Types<char32_t, std::uint8_t>;

template <class T>
class test_integer_from_chars_hexadecimal : public ::testing::Test {};
TYPED_TEST_SUITE(test_integer_from_chars_hexadecimal,
                 test_integer_from_chars_hexadecimal_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_integer_from_chars_hexadecimal, common_integers) {
  {
    TypeParam number;
    const char *input = "0";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "12";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x12);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "ab";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0xab);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "AB";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0xab);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TYPED_TEST(test_integer_from_chars_hexadecimal,
           negative_integers_are_disallowed) {
  if constexpr (std::is_unsigned_v<TypeParam>) {
    TypeParam number = 42;
    const char *input = "-12";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc::invalid_argument);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TYPED_TEST(test_integer_from_chars_hexadecimal, minimum_integer) {
  if constexpr (std::is_unsigned_v<TypeParam>) {
    static_assert(std::numeric_limits<TypeParam>::min() == 0);
    TypeParam number;
    const char *input = "0";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TEST(test_integer_from_chars_hexadecimal_char32_t, maximum_integer) {
  static_assert(std::numeric_limits<char32_t>::max() == 4294967295LL);
  char32_t number;
  const char *input = "ffffffff";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(number, 4294967295LL);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc{0});
}

TEST(test_integer_from_chars_hexadecimal_uint8_t, maximum_integer) {
  static_assert(std::numeric_limits<std::uint8_t>::max() == 255);
  std::uint8_t number;
  const char *input = "ff";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(number, 255);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc{0});
}

TEST(test_integer_from_chars_hexadecimal_char32_t, over_maximum_integer) {
  static_assert(std::numeric_limits<char32_t>::max() < 4294967296LL);
  char32_t number = 42;
  const char *input = "100000000";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc::result_out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TEST(test_integer_from_chars_hexadecimal_uint8_t, over_maximum_integer) {
  static_assert(std::numeric_limits<std::uint8_t>::max() < 256);
  std::uint8_t number = 42;
  const char *input = "100";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc::result_out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_integer_from_chars_hexadecimal, over_maximum_integer) {
  TypeParam number = 42;
  const char *input = "fffffffffffffffffff";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc::result_out_of_range);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_integer_from_chars_hexadecimal,
           extra_characters_after_are_not_parsed) {
  {
    TypeParam number;
    const char *input = "1fgh";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x1f);
    EXPECT_EQ(result.ptr, &input[2]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "ab   ";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0xab);
    EXPECT_EQ(result.ptr, &input[2]);
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TYPED_TEST(test_integer_from_chars_hexadecimal, extra_characters_before) {
  {
    TypeParam number = 42;
    const char *input = "  123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    const char *input = "--123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    TypeParam number = 42;
    const char *input = "+123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TYPED_TEST(test_integer_from_chars_hexadecimal, radix_prefix_is_not_special) {
  {
    TypeParam number;
    const char *input = "0x123a";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, &input[1]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "0X123a";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, &input[1]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    TypeParam number;
    const char *input = "077";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x77);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TYPED_TEST(test_integer_from_chars_hexadecimal,
           empty_input_string_is_unrecognized) {
  TypeParam number = 42;
  const char *input = "";
  from_chars_result result = from_chars_hex(input, input, number);
  EXPECT_EQ(result.ptr, input);
  EXPECT_EQ(result.ec, std::errc::invalid_argument);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TYPED_TEST(test_integer_from_chars_hexadecimal,
           minus_sign_without_digits_is_unrecognized) {
  TypeParam number = 42;
  const char *input = "- 1";
  from_chars_result result = from_chars_hex(input, input, number);
  EXPECT_EQ(result.ptr, input);
  EXPECT_EQ(result.ec, std::errc::invalid_argument);
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
