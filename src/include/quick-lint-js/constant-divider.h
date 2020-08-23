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

#ifndef QUICK_LINT_JS_CONSTANT_DIVIDER_H
#define QUICK_LINT_JS_CONSTANT_DIVIDER_H

#include <cassert>
#include <cmath>

namespace quick_lint_js {
inline int bit_width(int x) noexcept {
  // TODO(strager): Use std::bit_width or compiler intrinsics if available.
  return static_cast<int>(std::floor(std::log2(x)) + 1);
}

// @@@ sauce https://gmplib.org/~tege/divcnst-pldi94.pdf
class constant_divider {
 public:
  explicit constant_divider(int divisor) noexcept {
    int log_2 = bit_width(divisor);
    assert(divisor <= wide{1} << log_2);
    this->multiplier_ =
        ((wide{1} << N) * ((1 << log_2) - divisor) / divisor) + 1;
    this->log_2_ = log_2;
  }

  friend int operator/(int dividend, constant_divider divisor) noexcept {
    wide temp = (divisor.multiplier_ * dividend) >> N;
    return (dividend + temp) >> divisor.log_2_;
  }

 private:
  using wide = long long;

  static constexpr int N = 31;  // @@@ rename

  wide multiplier_;
  int log_2_;

  static_assert(sizeof(long long) >= sizeof(int) * 2);
};
}  // namespace quick_lint_js

#endif
