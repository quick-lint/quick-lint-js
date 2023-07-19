// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstring>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/simd.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>

QLJS_WARNING_IGNORE_CLANG("-Wconditional-uninitialized")

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_ARM_NEON || QLJS_HAVE_WEB_ASSEMBLY_SIMD128 || QLJS_HAVE_X86_SSE2
template <class CharVector16>
class Test_Char_Vector_16 : public ::testing::Test {};
using Char_Vector_16_Types = ::testing::Types<
#if QLJS_HAVE_ARM_NEON
    Char_Vector_16_NEON
#endif
#if QLJS_HAVE_WEB_ASSEMBLY_SIMD128
        Char_Vector_16_WASM_SIMD128
#endif
#if QLJS_HAVE_X86_SSE2
            Char_Vector_16_SSE2
#endif
    >;
TYPED_TEST_SUITE(Test_Char_Vector_16, Char_Vector_16_Types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(Test_Char_Vector_16, repeated) {
  using Char_Vector_16 = TypeParam;
  Char8 expected[16];
  fill(expected, u8'x');
  Char8 actual[16];
  Char_Vector_16::repeated('x').store(actual);
  EXPECT_EQ(std::memcmp(actual, expected, sizeof(actual)), 0);
}

TYPED_TEST(Test_Char_Vector_16, bitwise_or) {
  using Char_Vector_16 = TypeParam;
  constexpr std::uint8_t lhs[16] = {
      0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  constexpr std::uint8_t rhs[16] = {
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
      0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,  //
  };
  std::uint8_t actual[16];
  (Char_Vector_16::load(reinterpret_cast<const Char8*>(lhs)) |
   Char_Vector_16::load(reinterpret_cast<const Char8*>(rhs)))
      .store(reinterpret_cast<Char8*>(actual));
  constexpr std::uint8_t expected[16] = {
      0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,  //
      0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,  //
  };
  EXPECT_EQ(std::memcmp(actual, expected, sizeof(actual)), 0);
}
#endif

#if QLJS_HAVE_ARM_NEON || QLJS_HAVE_WEB_ASSEMBLY_SIMD128 || QLJS_HAVE_X86_SSE2
template <class BoolVector16>
class Test_Bool_Vector_16 : public ::testing::Test {};
using Bool_Vector_16_Types = ::testing::Types<
#if QLJS_HAVE_ARM_NEON
    Bool_Vector_16_NEON
#endif
#if QLJS_HAVE_WEB_ASSEMBLY_SIMD128
        Bool_Vector_16_WASM_SIMD128
#endif
#if QLJS_HAVE_X86_SSE2
            Bool_Vector_16_SSE2
#endif
    >;
TYPED_TEST_SUITE(Test_Bool_Vector_16, Bool_Vector_16_Types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(Test_Bool_Vector_16, first_false_of_all_false) {
  using Bool_Vector_16 = TypeParam;
  Char8 bools_data[] = {
      0, 0, 0, 0, 0, 0, 0, 0,  //
      0, 0, 0, 0, 0, 0, 0, 0,  //
  };
  Bool_Vector_16 bools = Bool_Vector_16::load_slow(bools_data);
  EXPECT_EQ(bools.find_first_false(), 0);
}

TYPED_TEST(Test_Bool_Vector_16, first_false_of_all_true) {
  using Bool_Vector_16 = TypeParam;
  constexpr Char8 t = static_cast<Char8>(0xff);
  Char8 bools_data[] = {
      t, t, t, t, t, t, t, t,  //
      t, t, t, t, t, t, t, t,  //
  };
  Bool_Vector_16 bools = Bool_Vector_16::load_slow(bools_data);
  EXPECT_EQ(bools.find_first_false(), 16);
}

TYPED_TEST(Test_Bool_Vector_16, find_first_false_exhaustive_SLOW) {
  using Bool_Vector_16 = TypeParam;
  for (std::uint32_t i = 0; i <= 0xffff; ++i) {
    SCOPED_TRACE(i);
    Char8 bools_data[16];
    int first_false = 16;
    for (int bit = 0; bit < 16; ++bit) {
      bool bit_on = (i >> bit) & 1;
      bools_data[bit] = bit_on;
      if (!bit_on) {
        first_false = std::min(first_false, bit);
      }
    }

    Bool_Vector_16 bools = Bool_Vector_16::load_slow(bools_data);
    ASSERT_EQ(bools.find_first_false(), first_false);
  }
}

TYPED_TEST(Test_Bool_Vector_16, mask_all_false) {
  using Bool_Vector_16 = TypeParam;
  Char8 bools_data[] = {
      0, 0, 0, 0, 0, 0, 0, 0,  //
      0, 0, 0, 0, 0, 0, 0, 0,  //
  };
  Bool_Vector_16 bools = Bool_Vector_16::load_slow(bools_data);
  EXPECT_EQ(bools.mask(), 0x0000);
}

TYPED_TEST(Test_Bool_Vector_16, mask_all_true) {
  using Bool_Vector_16 = TypeParam;
  constexpr Char8 t = static_cast<Char8>(0xff);
  Char8 bools_data[] = {
      t, t, t, t, t, t, t, t,  //
      t, t, t, t, t, t, t, t,  //
  };
  Bool_Vector_16 bools = Bool_Vector_16::load_slow(bools_data);
  EXPECT_EQ(bools.mask(), 0xffff);
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
