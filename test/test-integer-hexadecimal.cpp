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

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/integer.h>
#include <system_error>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic ignored "-Wtype-limits"
#endif

namespace quick_lint_js {
namespace {
TEST(test_integer_from_chars_hexadecimal, common_integers) {
  {
    int number;
    const char *input = "0";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "1234";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x1234);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "abcd";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0xabcd);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "ABCD";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0xabcd);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "-1234";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, -0x1234);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TEST(test_integer_from_chars_hexadecimal, minimum_integer) {
  static_assert(std::numeric_limits<int>::min() == -2147483648LL);
  int number;
  const char *input = "-80000000";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(number, -2147483648LL);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc{0});
}

TEST(test_integer_from_chars_hexadecimal, maximum_integer) {
  static_assert(std::numeric_limits<int>::max() == 2147483647);
  int number;
  const char *input = "7fffffff";
  from_chars_result result =
      from_chars_hex(input, input + std::strlen(input), number);
  EXPECT_EQ(number, 2147483647);
  EXPECT_EQ(result.ptr, input + std::strlen(input));
  EXPECT_EQ(result.ec, std::errc{0});
}

TEST(test_integer_from_chars_hexadecimal, over_maximum_integer) {
  static_assert(std::numeric_limits<int>::max() < 2147483648LL);

  {
    int number = 42;
    const char *input = "80000000";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc::result_out_of_range);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    int number = 42;
    const char *input = "fffffffffffffffffff";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc::result_out_of_range);
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TEST(test_integer_from_chars_hexadecimal,
     extra_characters_after_are_not_parsed) {
  {
    int number;
    const char *input = "1234efgh";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x1234ef);
    EXPECT_EQ(result.ptr, &input[6]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "123   ";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x123);
    EXPECT_EQ(result.ptr, &input[3]);
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TEST(test_integer_from_chars_hexadecimal, extra_characters_before) {
  {
    int number = 42;
    const char *input = "  123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    int number = 42;
    const char *input = "--123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }

  {
    int number = 42;
    const char *input = "+123";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(result.ptr, input);
    EXPECT_EQ(result.ec, std::errc{std::errc::invalid_argument});
    EXPECT_EQ(number, 42) << "number should be unmodified";
  }
}

TEST(test_integer_from_chars_hexadecimal, radix_prefix_is_not_special) {
  {
    int number;
    const char *input = "0x123a";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, &input[1]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "0X123a";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0);
    EXPECT_EQ(result.ptr, &input[1]);
    EXPECT_EQ(result.ec, std::errc{0});
  }

  {
    int number;
    const char *input = "0777";
    from_chars_result result =
        from_chars_hex(input, input + std::strlen(input), number);
    EXPECT_EQ(number, 0x777);
    EXPECT_EQ(result.ptr, input + std::strlen(input));
    EXPECT_EQ(result.ec, std::errc{0});
  }
}

TEST(test_integer_from_chars_hexadecimal, empty_input_string_is_unrecognized) {
  int number = 42;
  const char *input = "";
  from_chars_result result = from_chars_hex(input, input, number);
  EXPECT_EQ(result.ptr, input);
  EXPECT_EQ(result.ec, std::errc::invalid_argument);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}

TEST(test_integer_from_chars_hexadecimal,
     minus_sign_without_digits_is_unrecognized) {
  int number = 42;
  const char *input = "- 1";
  from_chars_result result = from_chars_hex(input, input, number);
  EXPECT_EQ(result.ptr, input);
  EXPECT_EQ(result.ec, std::errc::invalid_argument);
  EXPECT_EQ(number, 42) << "number should be unmodified";
}
}
}
