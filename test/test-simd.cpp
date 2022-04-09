// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstring>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/simd.h>

#if QLJS_HAVE_ARM_NEON
#include <arm_neon.h>
#endif

#if QLJS_HAVE_X86_SSE2
#include <emmintrin.h>
#endif

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_X86_SSE2
TEST(test_char_vector_16_sse2, repeated) {
  char8 expected[16];
  std::fill(std::begin(expected), std::end(expected), u8'x');
  __m128i actual = char_vector_16_sse2::repeated('x').m128i();
  EXPECT_EQ(std::memcmp(&actual, expected, sizeof(actual)), 0);
}

TEST(test_char_vector_16_sse2, bitwise_or) {
  constexpr std::uint8_t lhs[16] = {
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  constexpr std::uint8_t rhs[16] = {
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
  };
  __m128i actual =
      (char_vector_16_sse2::load(reinterpret_cast<const char8*>(lhs)) |
       char_vector_16_sse2::load(reinterpret_cast<const char8*>(rhs)))
          .m128i();
  constexpr std::uint8_t expected[16] = {
      0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  EXPECT_EQ(std::memcmp(&actual, expected, sizeof(actual)), 0);
}
#endif

#if QLJS_HAVE_ARM_NEON
TEST(test_char_vector_16_neon, repeated) {
  char8 expected[16];
  std::fill(std::begin(expected), std::end(expected), u8'x');
  ::uint8x16_t actual = char_vector_16_neon::repeated('x').uint8x16();
  EXPECT_EQ(std::memcmp(&actual, expected, sizeof(actual)), 0);
}

TEST(test_char_vector_16_neon, bitwise_or) {
  constexpr std::uint8_t lhs[16] = {
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  constexpr std::uint8_t rhs[16] = {
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
  };
  ::uint8x16_t actual =
      (char_vector_16_neon::load(reinterpret_cast<const char8*>(lhs)) |
       char_vector_16_neon::load(reinterpret_cast<const char8*>(rhs)))
          .uint8x16();
  constexpr std::uint8_t expected[16] = {
      0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  EXPECT_EQ(std::memcmp(&actual, expected, sizeof(actual)), 0);
}

TEST(test_bool_vector_16_neon, first_false_of_all_false) {
  ::uint8x16_t bools_data = {
      0, 0, 0, 0, 0, 0, 0, 0,  //
      0, 0, 0, 0, 0, 0, 0, 0,  //
  };
  bool_vector_16_neon bools(bools_data);
  EXPECT_EQ(bools.find_first_false(), 0);
}

TEST(test_bool_vector_16_neon, first_false_of_all_true) {
  constexpr std::uint8_t t = 0xff;
  ::uint8x16_t bools_data = {
      t, t, t, t, t, t, t, t,  //
      t, t, t, t, t, t, t, t,  //
  };
  bool_vector_16_neon bools(bools_data);
  EXPECT_EQ(bools.find_first_false(), 16);
}

TEST(test_bool_vector_16_neon, find_first_false_exhaustive) {
  for (std::uint32_t i = 0; i <= 0xffff; ++i) {
    SCOPED_TRACE(i);
    ::uint8x16_t bools_data;
    int first_false = 16;
    for (int bit = 0; bit < 16; ++bit) {
      bool bit_on = (i >> bit) & 1;
      bools_data[bit] = bit_on ? 0xff : 0x00;
      if (!bit_on) {
        first_false = std::min(first_false, bit);
      }
    }

    bool_vector_16_neon bools(bools_data);
    ASSERT_EQ(bools.find_first_false(), first_false);
  }
}
#endif
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
