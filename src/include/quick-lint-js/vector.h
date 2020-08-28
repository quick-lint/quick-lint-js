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

#ifndef QUICK_LINT_JS_VECTOR_H
#define QUICK_LINT_JS_VECTOR_H

#include <utility>
#include <vector>

namespace quick_lint_js {
template <class T>
class vector {
 public:
  explicit vector(const char *debug_owner [[maybe_unused]]) noexcept {}

  explicit vector(const char *debug_owner [[maybe_unused]], const T *begin,
                  const T *end)
      : data_(begin, end) {}

  [[gnu::always_inline]] T *data() noexcept { return this->data_.data(); }

  [[gnu::always_inline]] std::size_t size() const noexcept {
    return this->data_.size();
  }

  [[gnu::always_inline]] bool empty() const noexcept {
    return this->data_.empty();
  }

  [[gnu::always_inline]] T &front() noexcept { return this->data_.front(); }

  [[gnu::always_inline]] T &back() noexcept { return this->data_.back(); }

  template <class... Args>
  [[gnu::always_inline]] void emplace_back(Args &&... args) {
    this->data_.emplace_back(std::forward<Args>(args)...);
  }

  [[gnu::always_inline]] void clear() { this->data_.clear(); }

 private:
  std::vector<T> data_;
};
}  // namespace quick_lint_js

#endif
