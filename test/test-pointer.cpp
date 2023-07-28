// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gtest/gtest.h>
#include <quick-lint-js/util/pointer.h>

namespace quick_lint_js {
namespace {
TEST(Test_Pointer, align_up_with_one_byte_alignment_never_changes_pointer) {
  for (std::uintptr_t p :
       {0ULL, 1ULL, 2ULL, 3ULL, 4ULL, 5ULL, 6ULL, 0xffffffffULL}) {
    EXPECT_EQ(align_up(p, 1), p);
  }
}

TEST(Test_Pointer, align_up_to_even_address) {
  EXPECT_EQ(align_up(0, 2), 0);
  EXPECT_EQ(align_up(1, 2), 2);
  EXPECT_EQ(align_up(2, 2), 2);
  EXPECT_EQ(align_up(3, 2), 4);
  EXPECT_EQ(align_up(4, 2), 4);
  EXPECT_EQ(align_up(5, 2), 6);
}

TEST(Test_Pointer, align_up_to_multiple_of_8) {
  EXPECT_EQ(align_up(0, 8), 0);
  EXPECT_EQ(align_up(1, 8), 8);
  EXPECT_EQ(align_up(2, 8), 8);
  EXPECT_EQ(align_up(3, 8), 8);
  EXPECT_EQ(align_up(4, 8), 8);
  EXPECT_EQ(align_up(5, 8), 8);
  EXPECT_EQ(align_up(6, 8), 8);
  EXPECT_EQ(align_up(7, 8), 8);
  EXPECT_EQ(align_up(8, 8), 8);
  EXPECT_EQ(align_up(9, 8), 16);
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
