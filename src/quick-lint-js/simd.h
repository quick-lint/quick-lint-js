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

#ifndef QUICK_LINT_JS_SIMD_H
#define QUICK_LINT_JS_SIMD_H

#include <quick-lint-js/bit.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/force-inline.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>

#if QLJS_HAVE_X86_SSE2
#include <emmintrin.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_X86_SSE2
class bool_vector_16_sse2 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit bool_vector_16_sse2(__m128i data) noexcept
      : data_(data) {}

  QLJS_FORCE_INLINE friend bool_vector_16_sse2 operator|(
      bool_vector_16_sse2 x, bool_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_or_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend bool_vector_16_sse2 operator&(
      bool_vector_16_sse2 x, bool_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_and_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE int find_first_false() const noexcept {
    std::uint32_t mask = this->mask();
    if (~mask == 0) {
      // HACK(strager): Coerce GCC into omitting a branch due to an if check in
      // countr_one's implementation.
      QLJS_UNREACHABLE();
    }
    return countr_one(mask);
  }

  QLJS_FORCE_INLINE std::uint32_t mask() const noexcept {
    return narrow_cast<std::uint32_t>(_mm_movemask_epi8(this->data_));
  }

 private:
  __m128i data_;
};

class char_vector_16_sse2 {
 public:
  static constexpr int size = 16;

  QLJS_FORCE_INLINE explicit char_vector_16_sse2(__m128i data) noexcept
      : data_(data) {}

  QLJS_FORCE_INLINE static char_vector_16_sse2 load(const char8* data) {
    __m128i vector;
    std::memcpy(&vector, data, sizeof(vector));
    return char_vector_16_sse2(vector);
  }

  QLJS_FORCE_INLINE static char_vector_16_sse2 repeated(std::uint8_t c) {
    return char_vector_16_sse2(_mm_set1_epi8(static_cast<char>(c)));
  }

  QLJS_FORCE_INLINE friend char_vector_16_sse2 operator|(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return char_vector_16_sse2(_mm_or_si128(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend bool_vector_16_sse2 operator==(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmpeq_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend bool_vector_16_sse2 operator<(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmplt_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE friend bool_vector_16_sse2 operator>(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmpgt_epi8(x.data_, y.data_));
  }

  QLJS_FORCE_INLINE __m128i m128i() const noexcept { return this->data_; }

 private:
  __m128i data_;
};
#endif

class bool_vector_1 {
 public:
  static constexpr int size = 1;

  QLJS_FORCE_INLINE explicit bool_vector_1(bool data) noexcept : data_(data) {}

  QLJS_FORCE_INLINE friend bool_vector_1 operator|(bool_vector_1 x,
                                                   bool_vector_1 y) noexcept {
    return bool_vector_1(x.data_ || y.data_);
  }

  QLJS_FORCE_INLINE friend bool_vector_1 operator&(bool_vector_1 x,
                                                   bool_vector_1 y) noexcept {
    return bool_vector_1(x.data_ && y.data_);
  }

  QLJS_FORCE_INLINE int find_first_false() const noexcept {
    return this->data_ ? 1 : 0;
  }

  QLJS_FORCE_INLINE std::uint32_t mask() const noexcept {
    return this->data_ ? 1 : 0;
  }

 private:
  bool data_;
};

class char_vector_1 {
 public:
  static constexpr int size = 1;

  QLJS_FORCE_INLINE explicit char_vector_1(std::uint8_t data) noexcept
      : data_(data) {}

  QLJS_FORCE_INLINE static char_vector_1 load(const char8* data) {
    return char_vector_1(static_cast<std::uint8_t>(data[0]));
  }

  QLJS_FORCE_INLINE static char_vector_1 repeated(std::uint8_t c) {
    return char_vector_1(c);
  }

  QLJS_FORCE_INLINE friend char_vector_1 operator|(char_vector_1 x,
                                                   char_vector_1 y) noexcept {
    return char_vector_1(x.data_ | y.data_);
  }

  QLJS_FORCE_INLINE friend bool_vector_1 operator==(char_vector_1 x,
                                                    char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ == y.data_);
  }

  QLJS_FORCE_INLINE friend bool_vector_1 operator<(char_vector_1 x,
                                                   char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ < y.data_);
  }

  QLJS_FORCE_INLINE friend bool_vector_1 operator>(char_vector_1 x,
                                                   char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ > y.data_);
  }

 private:
  std::uint8_t data_;
};
}

#endif
