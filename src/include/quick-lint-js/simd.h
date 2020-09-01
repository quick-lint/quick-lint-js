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

#include <array>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <iosfwd>
#include <quick-lint-js/bit.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>

#if QLJS_HAVE_X86_SSE2
#include <emmintrin.h>
#endif

#if QLJS_HAVE_X86_SSSE3
#include <tmmintrin.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_X86_SSSE3
class char_filter_vector_16_ssse3;
#endif

#if QLJS_HAVE_X86_SSE2
class bool_vector_16_sse2 {
 public:
  static constexpr int size = 16;

  [[gnu::always_inline]] explicit bool_vector_16_sse2(__m128i data) noexcept
      : data_(data) {}

  static bool_vector_16_sse2 make(const std::array<bool, 16>& data) noexcept {
    auto widen = [](bool b) -> char {
      return static_cast<char>(b ? 0xff : 0x00);
    };
    return bool_vector_16_sse2(_mm_setr_epi8(
        widen(data[0]), widen(data[1]), widen(data[2]), widen(data[3]),
        widen(data[4]), widen(data[5]), widen(data[6]), widen(data[7]),
        widen(data[8]), widen(data[9]), widen(data[10]), widen(data[11]),
        widen(data[12]), widen(data[13]), widen(data[14]), widen(data[15])));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator|(
      bool_vector_16_sse2 x, bool_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_or_si128(x.data_, y.data_));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator&(
      bool_vector_16_sse2 x, bool_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_and_si128(x.data_, y.data_));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator==(
      bool_vector_16_sse2 x, bool_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmpeq_epi8(x.data_, y.data_));
  }

  [[gnu::always_inline]] bool is_all_true() const noexcept {
    return this->mask() == 0xffff;
  }

  [[gnu::always_inline]] int find_first_false() const noexcept {
    std::uint32_t mask = this->mask();
    if (~mask == 0) {
      // HACK(strager): Coerce GCC into omitting a branch due to an if check in
      // countr_one's implementation.
      QLJS_UNREACHABLE();
    }
    return countr_one(mask);
  }

  friend std::ostream& operator<<(std::ostream&, bool_vector_16_sse2);

 //private:
  [[gnu::always_inline]] std::uint32_t mask() const noexcept {
    return narrow_cast<std::uint32_t>(_mm_movemask_epi8(this->data_));
  }

  __m128i data_;
};

class char_vector_16_sse2 {
 public:
  static constexpr int size = 16;

  [[gnu::always_inline]] explicit char_vector_16_sse2(__m128i data) noexcept
      : data_(data) {}

  [[gnu::always_inline]] static char_vector_16_sse2 make(
      const std::array<std::uint8_t, 16>& data) noexcept {
    return load(data.data());
  }

  [[gnu::always_inline]] static char_vector_16_sse2 load(const char* data) {
    __m128i vector;
    std::memcpy(&vector, data, sizeof(vector));
    return char_vector_16_sse2(vector);
  }

  [[gnu::always_inline]] static char_vector_16_sse2 load(
      const std::uint8_t* data) {
    __m128i vector;
    std::memcpy(&vector, data, sizeof(vector));
    return char_vector_16_sse2(vector);
  }

  [[gnu::always_inline]] static char_vector_16_sse2 repeated(std::uint8_t c) {
    return char_vector_16_sse2(_mm_set1_epi8(static_cast<char>(c)));
  }

  [[gnu::always_inline]] friend char_vector_16_sse2 operator|(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return char_vector_16_sse2(_mm_or_si128(x.data_, y.data_));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator==(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmpeq_epi8(x.data_, y.data_));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator<(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmplt_epi8(x.data_, y.data_));
  }

  [[gnu::always_inline]] friend bool_vector_16_sse2 operator>(
      char_vector_16_sse2 x, char_vector_16_sse2 y) noexcept {
    return bool_vector_16_sse2(_mm_cmpgt_epi8(x.data_, y.data_));
  }

  friend std::ostream& operator<<(std::ostream&, char_vector_16_sse2);

 //private:
  __m128i data_;

#if QLJS_HAVE_X86_SSSE3
  friend char_filter_vector_16_ssse3;
#endif
};
#endif

#if QLJS_HAVE_X86_SSSE3
class char_filter_vector_16_ssse3 {
 private:
  static constexpr int table_size = 16;

 public:
  template <std::uint8_t... Whitelist>
  [[gnu::always_inline]] static char_filter_vector_16_ssse3 make() noexcept {
    constexpr std::array<std::uint8_t, table_size> table =
        make_table(std::array{Whitelist...});
    return char_filter_vector_16_ssse3(table);
  }

  [[gnu::always_inline]] bool_vector_16_sse2 filter(
      char_vector_16_sse2 data) const noexcept {
    return char_vector_16_sse2(
               _mm_shuffle_epi8(this->table_.data_, data.data_)) == data;
  }

 private:
  template <std::size_t WhitelistSize>
  static constexpr std::array<std::uint8_t, table_size> make_table(
      std::array<std::uint8_t, WhitelistSize> whitelist) noexcept {
    static_assert(WhitelistSize <= table_size);

    constexpr int mask = 0x0f;

    std::array<bool, table_size> used = {};
    std::array<std::uint8_t, table_size> table = {};

    // Fill all slots with non-matching junk.
    for (std::size_t i = 0; i < table.size(); ++i) {
      used[i] = false;
      table[i] = narrow_cast<std::uint8_t>(i + 1);
      assert((table[i] & mask) != i);
    }

    // Build the table.
    for (std::uint8_t c : whitelist) {
      assert((c & 0x80) == 0 &&
             "whitelisted bytes should not have high bit set");
      std::uint8_t entry = c & mask;
      assert(!used[entry] &&
             "multiple table entries collided; check that the bottom four bits "
             "are unique for each entry in the whitelist");
      table[entry] = c;
      used[entry] = true;
    }

#if defined(NDEBUG) && !NDEBUG
    // Validate the table.
    for (std::size_t i = 0; i < table.size(); ++i) {
      bool is_match = (table[i] & mask) == i;
      assert(is_match == used[i]);
    }
    for (std::uint8_t c : whitelist) {
      assert(table[c & mask] == c);
    }
#endif

    return table;
  }

  [[gnu::always_inline]] explicit char_filter_vector_16_ssse3(
      const std::array<std::uint8_t, table_size>& table) noexcept
      : table_(char_vector_16_sse2::make(table)) {}

  char_vector_16_sse2 table_;
};
#endif

class bool_vector_1 {
 public:
  static constexpr int size = 1;

  [[gnu::always_inline]] explicit bool_vector_1(bool data) noexcept
      : data_(data) {}

  [[gnu::always_inline]] friend bool_vector_1 operator|(
      bool_vector_1 x, bool_vector_1 y) noexcept {
    return bool_vector_1(x.data_ || y.data_);
  }

  [[gnu::always_inline]] friend bool_vector_1 operator&(
      bool_vector_1 x, bool_vector_1 y) noexcept {
    return bool_vector_1(x.data_ && y.data_);
  }

  [[gnu::always_inline]] int find_first_false() const noexcept {
    return this->data_ ? 1 : 0;
  }

 private:
  bool data_;
};

class char_vector_1 {
 public:
  static constexpr int size = 1;

  [[gnu::always_inline]] explicit char_vector_1(char data) noexcept
      : data_(data) {}

  [[gnu::always_inline]] static char_vector_1 load(const char* data) {
    return char_vector_1(data[0]);
  }

  [[gnu::always_inline]] static char_vector_1 repeated(std::uint8_t c) {
    return char_vector_1(static_cast<char>(c));
  }

  [[gnu::always_inline]] friend char_vector_1 operator|(
      char_vector_1 x, char_vector_1 y) noexcept {
    return char_vector_1(x.data_ | y.data_);
  }

  [[gnu::always_inline]] friend bool_vector_1 operator==(
      char_vector_1 x, char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ == y.data_);
  }

  [[gnu::always_inline]] friend bool_vector_1 operator<(
      char_vector_1 x, char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ < y.data_);
  }

  [[gnu::always_inline]] friend bool_vector_1 operator>(
      char_vector_1 x, char_vector_1 y) noexcept {
    return bool_vector_1(x.data_ > y.data_);
  }

 private:
  char data_;
};
}  // namespace quick_lint_js

#endif
