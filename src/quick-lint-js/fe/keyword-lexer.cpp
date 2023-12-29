// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <quick-lint-js/fe/keyword-lexer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/simd.h>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
bool Keyword_Lexer::key_strings_equal(const char* a, const char* b,
                                      std::size_t size) {
  return key_strings_equal(reinterpret_cast<const char8_t*>(a),
                           reinterpret_cast<const char8_t*>(b), size);
}
#endif

bool Keyword_Lexer::key_strings_equal(const Char8* a, const Char8* b,
                                      std::size_t size) {
#if QLJS_HAVE_X86_SSE2
  Char_Vector_16_SSE2 a_unmasked = Char_Vector_16_SSE2::load(a);
  Char_Vector_16_SSE2 b_unmasked = Char_Vector_16_SSE2::load(b);

  // (uint32_t(x) << 32) is UB, so ensure the shift is less than 32.
  std::size_t mask_shift = (size & 31);
  // Calculating the mask this way seems to be much much faster than
  // '(1 << size) - 1'.
  std::uint32_t inv_mask = ~std::uint32_t(0) << mask_shift;
  std::uint32_t mask = ~inv_mask;

  std::uint32_t equal_mask = (a_unmasked == b_unmasked).mask();
  std::uint32_t not_equal_mask = ~equal_mask;
  return (mask & not_equal_mask) == 0;
#else
  // TODO(strager): Optimize ARM NEON.
  // TODO(strager): Optimize WebAssembly SIMD128.
  return std::memcmp(a, b, minimum(maximum_key_length, size)) == 0;
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
