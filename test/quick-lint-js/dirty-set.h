// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIRTY_SET_H
#define QUICK_LINT_JS_DIRTY_SET_H

#include <set>

namespace quick_lint_js {
// dirty_set is like std::set but with handy operators for quick-and-dirty set
// operations. It is unoptimized and designed for testing purposes only.
template <class T>
class dirty_set {
 private:
  using set_type = std::set<T>;

 public:
  using value_type = T;
  using reference = T&;
  using const_reference = const T&;
  using const_iterator = typename set_type::const_iterator;
  using iterator = const_iterator;
  using difference_type = typename set_type::difference_type;
  using size_type = typename set_type::size_type;

  /*implicit*/ dirty_set() = default;

  /*implicit*/ dirty_set(std::initializer_list<T> items)
      : items_(std::move(items)) {}

  const_iterator begin() const { return this->items_.begin(); }
  const_iterator end() const { return this->items_.end(); }

  friend dirty_set operator&(const dirty_set& lhs, const dirty_set& rhs) {
    dirty_set result;
    for (const T& x : lhs) {
      if (rhs.items_.count(x) > 0) {
        result.items_.insert(x);
      }
    }
    return result;
  }

  friend dirty_set operator|(const dirty_set& lhs, const dirty_set& rhs) {
    dirty_set result = lhs;
    result.items_.insert(rhs.begin(), rhs.end());
    return result;
  }

  friend dirty_set operator-(const dirty_set& lhs, const dirty_set& rhs) {
    dirty_set result = lhs;
    for (const T& x : rhs) {
      result.items_.erase(x);
    }
    return result;
  }

 private:
  std::set<T> items_;
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
