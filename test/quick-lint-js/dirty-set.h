// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIRTY_SET_H
#define QUICK_LINT_JS_DIRTY_SET_H

#include <set>

namespace quick_lint_js {
// Dirty_Set is like std::set but with handy operators for quick-and-dirty set
// operations. It is unoptimized and designed for testing purposes only.
template <class T>
class Dirty_Set {
 private:
  using Set_Type = std::set<T>;

 public:
  using value_type = T;
  using reference = T&;
  using const_reference = const T&;
  using const_iterator = typename Set_Type::const_iterator;
  using iterator = const_iterator;
  using difference_type = typename Set_Type::difference_type;
  using size_type = typename Set_Type::size_type;

  /*implicit*/ Dirty_Set() = default;

  /*implicit*/ Dirty_Set(std::initializer_list<T> items)
      : items_(std::move(items)) {}

  const_iterator begin() const { return this->items_.begin(); }
  const_iterator end() const { return this->items_.end(); }

  friend Dirty_Set operator&(const Dirty_Set& lhs, const Dirty_Set& rhs) {
    Dirty_Set result;
    for (const T& x : lhs) {
      if (rhs.items_.count(x) > 0) {
        result.items_.insert(x);
      }
    }
    return result;
  }

  friend Dirty_Set operator|(const Dirty_Set& lhs, const Dirty_Set& rhs) {
    Dirty_Set result = lhs;
    result.items_.insert(rhs.begin(), rhs.end());
    return result;
  }

  friend Dirty_Set operator-(const Dirty_Set& lhs, const Dirty_Set& rhs) {
    Dirty_Set result = lhs;
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
