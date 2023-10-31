// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <cstring>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/bit.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/cast.h>

#if QLJS_HAVE_ARM_NEON
#include <arm_neon.h>
#endif

#if QLJS_HAVE_WEB_ASSEMBLY_SIMD128
#include <wasm_simd128.h>
#endif

#if QLJS_HAVE_X86_SSE2
#include <emmintrin.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WEB_ASSEMBLY_SIMD128
class alignas(::v128_t) Bool_Vector_16_WASM_SIMD128 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Bool_Vector_16_WASM_SIMD128(::v128_t data)
      : data_(data) {}

  // data must point to at least 16 elements.
  QLJS_FORCE_INLINE static Bool_Vector_16_WASM_SIMD128 load_slow(
      const Char8* data) {
    std::uint8_t bytes[16];
    for (int i = 0; i < 16; ++i) {
      bytes[i] = data[i] == 0 ? 0x00 : 0xff;
    }
    return Bool_Vector_16_WASM_SIMD128(::wasm_v128_load(bytes));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_WASM_SIMD128 operator|(
      Bool_Vector_16_WASM_SIMD128 x, Bool_Vector_16_WASM_SIMD128 y) {
    return Bool_Vector_16_WASM_SIMD128(::wasm_v128_or(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_WASM_SIMD128 operator&(
      Bool_Vector_16_WASM_SIMD128 x, Bool_Vector_16_WASM_SIMD128 y) {
    return Bool_Vector_16_WASM_SIMD128(::wasm_v128_and(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE int find_first_false() const {
    return countr_one(this->mask());
  }

  QLJS_FORCE_INLINE std::uint32_t mask() const {
    return ::wasm_i8x16_bitmask(this->data_);
  }

 private:
  ::v128_t data_;
};

class alignas(::v128_t) Char_Vector_16_WASM_SIMD128 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Char_Vector_16_WASM_SIMD128(::v128_t data)
      : data_(data) {}

  // data must point to at least 16 elements.
  QLJS_FORCE_INLINE static Char_Vector_16_WASM_SIMD128 load(const Char8* data) {
    return Char_Vector_16_WASM_SIMD128(::wasm_v128_load(data));
  }

  // out_data must point to at least 16 elements.
  QLJS_FORCE_INLINE void store(Char8* out_data) {
    ::wasm_v128_store(out_data, this->data_);
  }

  QLJS_FORCE_INLINE static Char_Vector_16_WASM_SIMD128 repeated(
      std::uint8_t c) {
    return Char_Vector_16_WASM_SIMD128(::wasm_u8x16_splat(c));
  }

  QLJS_FORCE_INLINE friend Char_Vector_16_WASM_SIMD128 operator|(
      Char_Vector_16_WASM_SIMD128 x, Char_Vector_16_WASM_SIMD128 y) {
    return Char_Vector_16_WASM_SIMD128(::wasm_v128_or(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_WASM_SIMD128 operator==(
      Char_Vector_16_WASM_SIMD128 x, Char_Vector_16_WASM_SIMD128 y) {
    return Bool_Vector_16_WASM_SIMD128(::wasm_i8x16_eq(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_WASM_SIMD128 operator<(
      Char_Vector_16_WASM_SIMD128 x, Char_Vector_16_WASM_SIMD128 y) {
    return Bool_Vector_16_WASM_SIMD128(::wasm_u8x16_lt(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_WASM_SIMD128 operator>(
      Char_Vector_16_WASM_SIMD128 x, Char_Vector_16_WASM_SIMD128 y) {
    return Bool_Vector_16_WASM_SIMD128(::wasm_u8x16_gt(x.data_, y.data_));
  }

 private:
  ::v128_t data_;
};
#endif

#if QLJS_HAVE_X86_SSE2
class alignas(__m128i) Bool_Vector_16_SSE2 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Bool_Vector_16_SSE2(__m128i data) : data_(data) {}

  // data must point to at least 16 elements.
  QLJS_FORCE_INLINE static Bool_Vector_16_SSE2 load_slow(const Char8* data) {
    std::uint8_t bytes[16];
    for (int i = 0; i < 16; ++i) {
      bytes[i] = data[i] == 0 ? 0x00 : 0xff;
    }
    __m128i vector;
    std::memcpy(&vector, bytes, sizeof(vector));
    return Bool_Vector_16_SSE2(vector);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_SSE2 operator|(
      Bool_Vector_16_SSE2 x, Bool_Vector_16_SSE2 y) {
    return Bool_Vector_16_SSE2(_mm_or_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_SSE2 operator&(
      Bool_Vector_16_SSE2 x, Bool_Vector_16_SSE2 y) {
    return Bool_Vector_16_SSE2(_mm_and_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE int find_first_false() const {
    std::uint32_t mask = this->mask();
    if (~mask == 0) {
      // HACK(strager): Coerce GCC into omitting a branch due to an if check in
      // countr_one's implementation.
      QLJS_UNREACHABLE();
    }
    return countr_one(mask);
  }

  QLJS_FORCE_INLINE std::uint32_t mask() const {
    return narrow_cast<std::uint32_t>(_mm_movemask_epi8(this->data_));
  }

 private:
  __m128i data_;
};

class alignas(__m128i) Char_Vector_16_SSE2 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Char_Vector_16_SSE2(__m128i data) : data_(data) {}

  // data must point to at least 16 elements.
  QLJS_FORCE_INLINE static Char_Vector_16_SSE2 load(const Char8* data) {
    __m128i vector;
    std::memcpy(&vector, data, sizeof(vector));
    return Char_Vector_16_SSE2(vector);
  }

  // out_data must point to at least 16 elements.
  QLJS_FORCE_INLINE void store(Char8* out_data) {
    std::memcpy(out_data, &this->data_, sizeof(this->data_));
  }

  QLJS_FORCE_INLINE static Char_Vector_16_SSE2 repeated(std::uint8_t c) {
    return Char_Vector_16_SSE2(_mm_set1_epi8(static_cast<char>(c)));
  }

  QLJS_FORCE_INLINE friend Char_Vector_16_SSE2 operator|(
      Char_Vector_16_SSE2 x, Char_Vector_16_SSE2 y) {
    return Char_Vector_16_SSE2(_mm_or_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_SSE2 operator==(
      Char_Vector_16_SSE2 x, Char_Vector_16_SSE2 y) {
    return Bool_Vector_16_SSE2(_mm_cmpeq_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_SSE2 operator<(
      Char_Vector_16_SSE2 x, Char_Vector_16_SSE2 y) {
    return Bool_Vector_16_SSE2(_mm_cmplt_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_SSE2 operator>(
      Char_Vector_16_SSE2 x, Char_Vector_16_SSE2 y) {
    return Bool_Vector_16_SSE2(_mm_cmpgt_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE __m128i m128i() const { return this->data_; }

 private:
  __m128i data_;
};
#endif

#if QLJS_HAVE_ARM_NEON
class alignas(::uint8x16_t) Bool_Vector_16_NEON {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Bool_Vector_16_NEON(::uint8x16_t data)
      : data_(data) {}

  // data must point to at least 16 elements.
  static Bool_Vector_16_NEON load_slow(const Char8* data);

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator|(
      Bool_Vector_16_NEON x, Bool_Vector_16_NEON y) {
    return Bool_Vector_16_NEON(::vorrq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator&(
      Bool_Vector_16_NEON x, Bool_Vector_16_NEON y) {
    return Bool_Vector_16_NEON(::vandq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator!(
      Bool_Vector_16_NEON x) {
    return Bool_Vector_16_NEON(::vmvnq_u8(x.data_));
  }

  QLJS_FORCE_INLINE int find_first_false() const;

  QLJS_FORCE_INLINE std::uint32_t mask() const;

 private:
  ::uint8x16_t data_;
};

class alignas(::uint8x16_t) Char_Vector_16_NEON {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit Char_Vector_16_NEON(::uint8x16_t data)
      : data_(data) {}

  // data must point to at least 16 elements.
  QLJS_FORCE_INLINE static Char_Vector_16_NEON load(const Char8* data) {
    ::uint8x16_t vector;
    std::memcpy(&vector, data, sizeof(vector));
    return Char_Vector_16_NEON(vector);
  }

  // out_data must point to at least 16 elements.
  QLJS_FORCE_INLINE void store(Char8* out_data) {
    std::memcpy(out_data, &this->data_, sizeof(this->data_));
  }

  QLJS_FORCE_INLINE static Char_Vector_16_NEON repeated(std::uint8_t c) {
    return Char_Vector_16_NEON(::vdupq_n_u8(c));
  }

  QLJS_FORCE_INLINE friend Char_Vector_16_NEON operator|(
      Char_Vector_16_NEON x, Char_Vector_16_NEON y) {
    return Char_Vector_16_NEON(::vorrq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator==(
      Char_Vector_16_NEON x, Char_Vector_16_NEON y) {
    return Bool_Vector_16_NEON(::vceqq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator!=(
      Char_Vector_16_NEON x, Char_Vector_16_NEON y) {
    return !(x == y);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator<(
      Char_Vector_16_NEON x, Char_Vector_16_NEON y) {
    return Bool_Vector_16_NEON(::vcltq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend Bool_Vector_16_NEON operator>(
      Char_Vector_16_NEON x, Char_Vector_16_NEON y) {
    return Bool_Vector_16_NEON(::vcgtq_u8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE ::uint8x16_t uint8x16() const { return this->data_; }

 private:
  ::uint8x16_t data_;
};

inline Bool_Vector_16_NEON Bool_Vector_16_NEON::load_slow(const Char8* data) {
  return Char_Vector_16_NEON::load(data) != Char_Vector_16_NEON::repeated(0);
}
#endif

class Bool_Vector_1 {
 public:
  static constexpr int size = 1;

  QLJS_FORCE_INLINE explicit Bool_Vector_1(bool data) : data_(data) {}

  QLJS_FORCE_INLINE friend Bool_Vector_1 operator|(Bool_Vector_1 x,
                                                   Bool_Vector_1 y) {
    return Bool_Vector_1(x.data_ || y.data_);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_1 operator&(Bool_Vector_1 x,
                                                   Bool_Vector_1 y) {
    return Bool_Vector_1(x.data_ && y.data_);
  }

  QLJS_FORCE_INLINE int find_first_false() const { return this->data_ ? 1 : 0; }

  QLJS_FORCE_INLINE std::uint32_t mask() const { return this->data_ ? 1 : 0; }

 private:
  bool data_;
};

class Char_Vector_1 {
 public:
  static constexpr int size = 1;

  QLJS_FORCE_INLINE explicit Char_Vector_1(std::uint8_t data) : data_(data) {}

  QLJS_FORCE_INLINE static Char_Vector_1 load(const Char8* data) {
    return Char_Vector_1(static_cast<std::uint8_t>(data[0]));
  }

  QLJS_FORCE_INLINE static Char_Vector_1 repeated(std::uint8_t c) {
    return Char_Vector_1(c);
  }

  QLJS_FORCE_INLINE friend Char_Vector_1 operator|(Char_Vector_1 x,
                                                   Char_Vector_1 y) {
    return Char_Vector_1(x.data_ | y.data_);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_1 operator==(Char_Vector_1 x,
                                                    Char_Vector_1 y) {
    return Bool_Vector_1(x.data_ == y.data_);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_1 operator<(Char_Vector_1 x,
                                                   Char_Vector_1 y) {
    return Bool_Vector_1(x.data_ < y.data_);
  }

  QLJS_FORCE_INLINE friend Bool_Vector_1 operator>(Char_Vector_1 x,
                                                   Char_Vector_1 y) {
    return Bool_Vector_1(x.data_ > y.data_);
  }

 private:
  std::uint8_t data_;
};
}

// Some routines have a different copyright, thus are in a separate file.
#include <quick-lint-js/port/simd-neon-arm.h>

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
