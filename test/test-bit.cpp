// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/port/bit.h>

namespace quick_lint_js {
namespace {
TEST(Test_Bit_Countl_Zero, uint32) {
  EXPECT_EQ(countl_zero(std::uint32_t(0)), 32);

  EXPECT_EQ(
      countl_zero(std::uint32_t(0b0000'0000'0001'0101'0101'0101'0101'0101)),
      11);

  for (int shift = 0; shift < 32; ++shift) {
    std::uint32_t x = std::uint32_t(1) << shift;
    EXPECT_EQ(countl_zero(x), 31 - shift)
        << "x = " << x << "; shift = " << shift;

    std::uint32_t y = x | 1;
    EXPECT_EQ(countl_zero(y), 31 - shift)
        << "y = " << x << "; shift = " << shift;
  }
}

TEST(Test_Bit_Countl_Zero, uint64) {
  EXPECT_EQ(countl_zero(std::uint64_t(0)), 64);

  EXPECT_EQ(
      countl_zero(std::uint64_t(0b0000'0000'0001'0101'0101'0101'0101'0101)),
      43);

  for (int shift = 0; shift < 64; ++shift) {
    std::uint64_t x = std::uint64_t(1) << shift;
    EXPECT_EQ(countl_zero(x), 63 - shift)
        << "x = " << x << "; shift = " << shift;

    std::uint64_t y = x | 1;
    EXPECT_EQ(countl_zero(y), 63 - shift)
        << "y = " << x << "; shift = " << shift;
  }
}

TEST(Test_Bit_Width, uint32) {
  EXPECT_EQ(bit_width(std::uint32_t{0b0}), 0);
  EXPECT_EQ(bit_width(std::uint32_t{0b101}), 3);
  EXPECT_EQ(bit_width(std::uint32_t{0xffffffff}), 32);
  for (std::uint32_t i = 1; i < 32; ++i) {
    EXPECT_EQ(bit_width(std::uint32_t{1} << i), i + 1);
  }
  for (std::uint32_t i = 1; i < 32; ++i) {
    EXPECT_EQ(bit_width((std::uint32_t{1} << i) - 1), i);
  }
}

TEST(Test_Has_Single_Bit, uint32) {
  EXPECT_FALSE(has_single_bit(std::uint32_t(0)));
  EXPECT_FALSE(has_single_bit(std::uint32_t(0b11)));
  EXPECT_FALSE(has_single_bit(std::uint32_t(0b110)));
  EXPECT_FALSE(has_single_bit(std::uint32_t(0b101)));

  for (std::uint32_t shift = 0; shift < 32; ++shift) {
    std::uint32_t x = (std::uint32_t(1) << shift);
    EXPECT_TRUE(has_single_bit(x)) << "x = " << x << "; shift = " << shift;
  }
}

TEST(Test_Bit_Ceil_UInt32, zero_maps_to_one) {
  EXPECT_EQ(bit_ceil(std::uint32_t(0)), 1) << "result must be a power of 2";
}

TEST(Test_Bit_Ceil_UInt64, zero_maps_to_one) {
  EXPECT_EQ(bit_ceil(std::uint64_t(0)), 1) << "result must be a power of 2";
}

TEST(Test_Bit_Ceil_UInt32, powers_of_2_remain_unchanged) {
  for (std::uint32_t shift = 0; shift < 32; ++shift) {
    std::uint32_t x = (std::uint32_t(1) << shift);
    EXPECT_EQ(bit_ceil(x), x) << "x = " << x << "; shift = " << shift;
  }
}

TEST(Test_Bit_Ceil_UInt64, powers_of_2_remain_unchanged) {
  for (std::uint64_t shift = 0; shift < 64; ++shift) {
    std::uint64_t x = (std::uint64_t(1) << shift);
    EXPECT_EQ(bit_ceil(x), x) << "x = " << x << "; shift = " << shift;
  }
}

TEST(Test_Bit_Ceil_UInt32, non_powers_of_2_are_rounded_up) {
  EXPECT_EQ(bit_ceil(std::uint32_t(3)), 4);
  EXPECT_EQ(bit_ceil(std::uint32_t(0b101010)), 0b1'000000);
  EXPECT_EQ(bit_ceil(std::uint32_t(0x7fff'ffff)), 0x8000'0000);
}

TEST(Test_Bit_Ceil_UInt64, non_powers_of_2_are_rounded_up) {
  EXPECT_EQ(bit_ceil(std::uint64_t(3)), 4);
  EXPECT_EQ(bit_ceil(std::uint64_t(0b101010)), 0b1'000000);
  EXPECT_EQ(bit_ceil(std::uint64_t(0x7fff'ffff)), 0x8000'0000);
  EXPECT_EQ(bit_ceil(std::uint64_t(0x7fff'ffff'ffff'ffff)),
            0x8000'0000'0000'0000);
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
