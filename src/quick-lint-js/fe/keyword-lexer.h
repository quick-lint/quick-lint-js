// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_KEYWORD_LEXER_H
#define QUICK_LINT_JS_FE_KEYWORD_LEXER_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/simd.h>

namespace quick_lint_js {
struct Keyword_Lexer {
  using Selection_Type = std::uint32_t;
  using Hash_Type = std::uint32_t;
  using Seed_Type = std::uint64_t;

  static constexpr int padding_size = 17;

  // Minimum and maximum key lengths supported by the hash table.
  //
  // Lookups can safely provide keys bigger or smaller than these limits. These
  // limits only apply to insertions.
  static constexpr std::size_t minimum_key_length = 2;
  static constexpr std::size_t maximum_key_length = 16;

  // Step 1 of the hash function for lexer::identifier_token_type().
  //
  // Precondition: key[0] can be read.
  // Precondition: key[minimum_key_length - 1] can be read.
  // Precondition: key[key_length - 1] can be read.
  // Precondition: key[padding_size - 1] can be read.
  static Selection_Type select(const char* key,
                               std::size_t key_length) noexcept {
    // With our keys, the following statements are true:
    //
    // * minimum_key_length is >= 2
    // * The first two characters are not unique.
    // * The last two characters are not unique.
    // * The first two + last two characters are unique.
    //
    // Selecting four characters is more efficient than looping over the entire
    // key.

    auto c = [&](std::size_t index) -> std::uint32_t {
      return static_cast<std::uint32_t>(static_cast<std::uint8_t>(key[index]));
    };
    auto read_16_bits = [&](std::size_t index) -> std::uint32_t {
      return (c(index) << 0) | (c(index + 1) << 8);
    };

    std::uint32_t lo = read_16_bits(0);

    // If signed_key_length is 0 or 1, then key[key_length - 2] and
    // key[key_length - 1] are out of bounds. Mask to put the indexes back in
    // bounds:
    //
    // * read_16_bits(0xe) -> key[0xe], key[0xf]
    // * read_16_bits(0xf) -> key[0xf], key[0x10]
    static constexpr int index_mask = 15;
    static constexpr int max_read_index = index_mask + 1;
    static_assert(padding_size > max_read_index);
    std::uint32_t hi = read_16_bits((key_length - 2) & index_mask);

    return lo | (hi << 16);
  }

#if QLJS_HAVE_CHAR8_T
  static Selection_Type select(const char8_t* key,
                               std::size_t key_length) noexcept {
    return select(reinterpret_cast<const char*>(key), key_length);
  }
#endif

  // Step 2 of the hash function for lexer::identifier_token_type().
  static Hash_Type mix(Selection_Type selection, Seed_Type seed) {
    // This hash function executes quickly, but might produce a lot of
    // collisions. Collisions are fine, though; collisions just slow down table
    // generation, not run-time.

    // Pierre L’Ecuyer. 1999. Tables of linear congruential generators of
    // different sizes and good lattice structure. Mathematics of Computation of
    // the American Mathematical Society 68, 225 (1999), 249–260.
    //
    // https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/S0025-5718-99-00996-5.pdf
    std::uint64_t magic = 4292484099903637661ULL;

    std::uint64_t x = static_cast<std::uint64_t>(selection) ^ seed;
    return static_cast<std::uint32_t>(multiply_u64_get_top_64(x, magic));
  }

  // Compare two strings, 'a' and 'b', each with size 'size'.
  //
  // This function possibly reads more bytes than 'size'.
  //
  // If (size < minimum_key_length || size > maximum_key_length), then the
  // result of this function is unspecified.
  //
  // Precondition: a[0] through a[max(padding_size, size) - 1] can be read.
  // Precondition: b[0] through b[max(padding_size, size) - 1] can be read.
  static bool key_strings_equal(const char* a, const char* b, std::size_t size);
#if QLJS_HAVE_CHAR8_T
  static bool key_strings_equal(const char8_t* a, const char8_t* b,
                                std::size_t size);
#endif
};
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
