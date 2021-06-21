// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/bit.h>

namespace quick_lint_js {
namespace {
TEST(test_bit_width, uint32) {
  EXPECT_EQ(bit_width(std::uint32_t{0b0}), 0);
  EXPECT_EQ(bit_width(std::uint32_t{0b101}), 3);
  EXPECT_EQ(bit_width(std::uint32_t{0xffffffff}), 32);
  for (std::uint32_t i = 1; i < 32; ++i) {
    EXPECT_EQ(bit_width(std::uint32_t{1} << i), i + 1);
  }
  for (std::uint32_t i = 1; i < 32; ++i) {
    EXPECT_EQ(bit_width((std::uint32_t{1} << i) - 1), i);
  }
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
