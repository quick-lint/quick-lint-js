// Copyright (C) 2020  Matthew "strager" Glazar
// Copyright (c) 2014-2020, Arm Limited.
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SIMD_NEON_ARM_H
#define QUICK_LINT_JS_SIMD_NEON_ARM_H

#include <cstdint>
#include <quick-lint-js/attribute.h>
#include <quick-lint-js/bit.h>
#include <quick-lint-js/simd.h>

#if QLJS_HAVE_ARM_NEON
#include <arm_neon.h>
#endif

// Some routines have a different copyright than the rest of quick-lint-js, thus
// are in this separate file.

namespace quick_lint_js {
#if QLJS_HAVE_ARM_NEON_A64
QLJS_FORCE_INLINE inline int bool_vector_16_neon::find_first_false() const
    noexcept {
  // You might expect a magic pattern to look like the following:
  //
  //   { 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x08, [repeat] }
  //
  // However, the above magic pattern requires mixing cells 3 times
  // (16x8 -> 8x16 -> 4x32 -> 2x64). Our magic pattern requires mixing cells
  // only 2 times, but creates an unusual mask (see
  // NOTE[find_first_false NEON mask]).
  ::uint8x16_t magic = {
      0x01, 0x04, 0x10, 0x40, 0x01, 0x04, 0x10, 0x40,  //
      0x01, 0x04, 0x10, 0x40, 0x01, 0x04, 0x10, 0x40,  //
  };

  // It doesn't matter what 'garbage' is. Could be zeros or ones or anything. If
  // we ever extend this algorithm to uint8x32_t inputs, garbage would be the
  // upper 128 bits.
  ::uint8x16_t garbage = this->data_;

  // We invert the input so that we can use countr_zero instead of countr_one.
  // countr_one can't be used because of the zero bits in our mask (see
  // NOTE[find_first_false NEON mask]).
  ::uint8x16_t mixed_0 = ::vbicq_u8(magic, this->data_);

  // Mix bits to create a mask. Note that arithmetic ADD is effectively
  // bitwise OR.
  //
  // mixed_0: { a b c d  e f g h  i j k l  m n o p }
  // mixed_1: { a+b c+d  e+f g+h  i+j k+l  m+n o+p  (64 bits unused...) }
  // mixed_2: { a+b+c+d  e+f+g+h  i+j+k+l  m+n+o+p  (96 bits unused...) }
  ::uint8x16_t mixed_1 = ::vpaddq_u8(mixed_0, garbage);
  ::uint8x16_t mixed_2 = ::vpaddq_u8(mixed_1, mixed_1);
  std::uint32_t mask = vgetq_lane_u32(::vreinterpretq_u32_u8(mixed_2), 0);

  // NOTE[find_first_false NEON mask]: After mixing bits, an ideal mask looks
  // like this:
  //
  //   0b0000000000000000ABCDEFGHIJKLMNOP
  //
  // But our mask looks like this:
  //
  //   0b0A0B0C0D0E0F0G0H0I0J0K0L0M0N0O0P
  //
  // To deal with the extra zeros, we to divide our countr_zero result by 2.
  return countr_zero(mask) / 2;
}
#elif QLJS_HAVE_ARM_NEON
QLJS_FORCE_INLINE inline int bool_vector_16_neon::find_first_false() const
    noexcept {
  // Algorithm derived from sse2neon's _mm_movemask_epi8 function:
  // https://github.com/DLTcollab/sse2neon/blob/814935c9ba06f68e9549272dbf5df0db8dab2a00/sse2neon.h#L4752-L4830
  // clang-format off
  ::uint16x8_t high_bits = ::vreinterpretq_u16_u8 (vshrq_n_u8 (this->data_,           8 - 1));
  ::uint32x4_t paired16  = ::vreinterpretq_u32_u16(vsraq_n_u16(high_bits, high_bits,  8 - 1));
  ::uint64x2_t paired32  = ::vreinterpretq_u64_u32(vsraq_n_u32(paired16,  paired16,  16 - 2));
  ::uint8x16_t paired64  = ::vreinterpretq_u8_u64 (vsraq_n_u64(paired32,  paired32,  32 - 4));
  // clang-format on

  std::uint32_t mask =
      static_cast<std::uint32_t>(vgetq_lane_u8(paired64, 0)) |
      (static_cast<std::uint32_t>(vgetq_lane_u8(paired64, 8)) << 8);
  return countr_one(mask);
}
#endif
}

#endif

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
//
// ---
//
// Portions of this file are
// Copyright (c) 2014-2020, Arm Limited.
// Source:
// https://github.com/ARM-software/optimized-routines/blob/7a9fd1603e1179b044406fb9b6cc5770d736cde7/string/aarch64/memchr.S
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// --
//
// Portions of this file are from sse2neon.
// Source:
// https://github.com/DLTcollab/sse2neon/blob/814935c9ba06f68e9549272dbf5df0db8dab2a00/sse2neon.h
//
// sse2neon is freely redistributable under the MIT License.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
