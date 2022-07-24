// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_BIT_ITERATOR_H
#define QUICK_LINT_JS_UTIL_BIT_ITERATOR_H

#include <climits>
#include <quick-lint-js/port/attribute.h>
#include <type_traits>

namespace quick_lint_js {
// Iterates over set bits in a bit set integer.
//
// TODO(#798): Make this more efficient with the n&(n-1) trick.
template <class T>
class one_bit_iterator {
 public:
  static constexpr int bit_count = sizeof(T) * CHAR_BIT;

  static_assert(std::is_unsigned_v<T>);
  static_assert(std::is_integral_v<T>);

  QLJS_FORCE_INLINE explicit one_bit_iterator(T mask) noexcept : mask_(mask) {
    if (this->mask_ == 0) {
      this->index = bit_count;
    } else {
      this->index_ = countr_zero(this->mask_);
    }
  }

  QLJS_FORCE_INLINE bool done() const noexcept {
    return this->index_ >= bit_count;
  }

  // Precondition: !this->done()
  QLJS_FORCE_INLINE int index() const noexcept { return this->index_; }

  // Precondition: !this->done()
  QLJS_FORCE_INLINE void next() noexcept {
    do {
      this->index_ += 1;
    } while (!this->done() && (this->mask_ & (1ULL << this->index_)) == 0);
  }

 private:
  T mask_;
  int index_;
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
