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
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/bit.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
// constant_divider implements division and modulus where the divisor
// (right-hand side) is a constant.
//
// The implementation is based on
// "Division by Invariant Integers using Multiplication" by Torbjörn Granlund
// and Peter L. Montgomery: https://gmplib.org/~tege/divcnst-pldi94.pdf
template <class Number>
class constant_divider;

template <>
class constant_divider<std::uint32_t> {
 public:
  using number_type = std::uint32_t;

  explicit constant_divider() noexcept
      : multiplier_((wide{1} << max_fraction_bits) + 1),
        log_2_(1),
        original_(1) {}

  explicit constant_divider(number_type divisor) noexcept : original_(divisor) {
    int log_2 = bit_width(divisor);
    wide rounded_up = wide{1} << log_2;
    QLJS_ASSERT(static_cast<wide>(divisor) <= rounded_up);
    this->multiplier_ =
        ((wide{1} << max_fraction_bits) * (rounded_up - divisor) / divisor) + 1;
    this->log_2_ = log_2;
  }

  friend number_type operator/(number_type dividend,
                               constant_divider divisor) noexcept {
    wide temp = (divisor.multiplier_ * dividend) >> max_fraction_bits;
    return narrow_cast<number_type>((wide{dividend} + temp) >> divisor.log_2_);
  }

  friend number_type operator%(number_type dividend,
                               constant_divider divisor) noexcept {
    return dividend - (dividend / divisor) * divisor.original_;
  }

 private:
  using wide = std::uint64_t;

  static constexpr int max_fraction_bits = 32;

  wide multiplier_;
  int log_2_;
  number_type original_;

  static_assert(sizeof(wide) >= sizeof(number_type) * 2);
};
}  // namespace quick_lint_js

#endif
