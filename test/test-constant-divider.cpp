// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <limits>
#include <quick-lint-js/constant-divider.h>
#include <quick-lint-js/narrow-cast.h>

using number_type = std::uint32_t;
using number_limits = std::numeric_limits<number_type>;

namespace quick_lint_js {
namespace {
template <class T>
std::vector<T> various_divisors() {
  std::vector<number_type> divisors;
  for (number_type i = 1; i < 1000; ++i) {
    divisors.push_back(i);
  }
  for (number_type i = 0; i < 10; ++i) {
    divisors.push_back(number_limits::max() - i);
  }
  for (number_type i = 2; i < 5; ++i) {
    for (std::intmax_t j = -3; j <= +3; ++j) {
      divisors.push_back(narrow_cast<number_type>(
          std::intmax_t{number_limits::max() / i} + j));
    }
  }
  return divisors;
}

template <class T>
std::vector<T> various_dividends() {
  std::vector<number_type> dividends = various_divisors<T>();
  dividends.push_back(0);
  return dividends;
}

TEST(test_constant_divider, positive_divided_by_positive) {
  for (number_type real_divisor : various_divisors<number_type>()) {
    constant_divider<number_type> test_divider(real_divisor);
    for (number_type dividend : various_dividends<number_type>()) {
      EXPECT_EQ(dividend / test_divider, dividend / real_divisor)
          << dividend << '/' << real_divisor;
    }
  }
}

TEST(test_constant_divider, positive_modulo_positive) {
  for (number_type real_divisor : various_divisors<number_type>()) {
    constant_divider<number_type> test_divider(real_divisor);
    for (number_type dividend : various_dividends<number_type>()) {
      EXPECT_EQ(dividend % test_divider, dividend % real_divisor)
          << dividend << '%' << real_divisor;
    }
  }
}

TEST(test_constant_divider, default_is_1) {
  constant_divider<number_type> test_divider;
  for (number_type dividend : various_dividends<number_type>()) {
    EXPECT_EQ(dividend / test_divider, dividend) << dividend << " / 1";
    EXPECT_EQ(dividend % test_divider, 0) << dividend << " % 1";
  }
}
}  // namespace
}  // namespace quick_lint_js

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
