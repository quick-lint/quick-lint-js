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

#include <gtest/gtest.h>
#include <quick-lint-js/simd.h>

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_X86_SSSE3
TEST(test_simd, filter_table_finds_matches) {
  char_filter_vector_16_ssse3 table =
      char_filter_vector_16_ssse3::make<'a', 'b', 'c', 'd'>();
  char_vector_16_sse2 data = char_vector_16_sse2::make(
      {'a', 'A', 'b', 'C', '?', static_cast<std::uint8_t>('a') | 0x80,
       'd' & ~0xf, '_', '_', 'a', 'a', 'a', 'a', 'a', 'a', 'a'});

  bool_vector_16_sse2 matches = table.filter(data);
  bool_vector_16_sse2 expected = bool_vector_16_sse2::make({
      true,   // 'a',
      false,  // 'A',
      true,   // 'b',
      false,  // 'C',
      false,  // '?',
      false,  // static_cast<std::uint8_t>('a')|0x80,
      false,  // 'd'&~0xf,
      false,  // '_',
      false,  // '_',
      true,   // 'a',
      true,   // 'a',
      true,   // 'a',
      true,   // 'a',
      true,   // 'a',
      true,   // 'a',
      true,   // 'a'
  });
  EXPECT_TRUE((matches == expected).is_all_true())
      << "matches=" << matches << " expected=" << expected;
}
#endif
}  // namespace
}  // namespace quick_lint_js
