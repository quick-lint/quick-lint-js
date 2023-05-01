// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/port/math.h>

namespace quick_lint_js {
namespace {
TEST(test_math, multiply_u64_get_top_64) {
  EXPECT_EQ(multiply_u64_get_top_64(0, 0), 0);

  EXPECT_EQ(multiply_u64_get_top_64(0x80000000'00000000ULL, 2), 1);
  EXPECT_EQ(
      multiply_u64_get_top_64(0x80000000'00000000ULL, 0x80000000'00000000ULL),
      0x40000000'00000000ULL);
  EXPECT_EQ(
      multiply_u64_get_top_64(13869076433459953670ULL, 14835062956724989689ULL),
      11153655150192271059ULL);
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
