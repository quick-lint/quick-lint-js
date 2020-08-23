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

#include <cmath>
#include <gtest/gtest.h>
#include <quick-lint-js/constant-divider.h>

using int_limits = std::numeric_limits<int>;

namespace quick_lint_js {
namespace {

TEST(test_constant_divider, positive_divided_by_positive) {
  std::vector<int> divisers;
  for (int i = 1; i < 1000; ++i) {
    divisers.push_back(i);
  }
  for (int i = 0; i < 10; ++i) {
    divisers.push_back(int_limits::max() - i);
  }
  for (int i = 2; i < 5; ++i) {
    for (int j = -3; j <= +3; ++j) {
      divisers.push_back(int_limits::max() / i + j);
    }
  }

  std::vector<int> dividends(divisers);
  dividends.push_back(0);

  for (int real_diviser : divisers) {
    constant_divider test_divider(real_diviser);
    for (int dividend : dividends) {
      EXPECT_EQ(dividend / test_divider, dividend / real_diviser)
          << dividend << '/' << real_diviser;
    }
  }
}
}  // namespace
}  // namespace quick_lint_js
