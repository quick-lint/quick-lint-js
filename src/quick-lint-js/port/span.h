// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_SPAN_H
#define QUICK_LINT_JS_PORT_SPAN_H

#include <array>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
using Span_Size = std::ptrdiff_t;

template <class T, Span_Size max_size>
class Fixed_Vector;

// Like std::span.
template <class T>
class Span {
 public:
  using value_type = T;
  using size_type = Span_Size;
  using difference_type = Span_Size;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = T *;

  explicit Span() : data_(nullptr), size_(0) {}

  template <std::size_t n>
  explicit Span(const std::array<std::remove_const_t<T>, n> &data)
      : data_(data.data()), size_(n) {}

  template <std::size_t n>
  explicit Span(T (&data)[n]) : data_(data), size_(n) {}

  explicit Span(const std::vector<std::remove_const_t<T>> &data)
      : data_(data.data()), size_(narrow_cast<Span_Size>(data.size())) {}

  template <Span_Size max_size>
  explicit Span(const Fixed_Vector<std::remove_const_t<T>, max_size> &data)
      : Span(data.data(), data.size()) {}

  // Convert from Span<int> to Span<const int>, for example.
  template <class U,
            class = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
  explicit Span(const Span<U> &data) : Span(data.data(), data.size()) {}

  explicit Span(T *data, size_type size) : data_(data), size_(size) {}

  explicit Span(T *begin, T *end)
      : data_(begin), size_(narrow_cast<Span_Size>(end - begin)) {}

  T &operator[](size_type index) const {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }

  T front() const { return (*this)[0]; }

  T back() const { return (*this)[this->size() - 1]; }

  T *data() const { return this->data_; }

  size_type size() const { return this->size_; }
  size_type byte_size() const { return this->size_ * sizeof(T); }

  T *begin() const { return this->data_; }
  T *end() const { return this->data_ + this->size_; }

  bool empty() const { return this->size() == 0; }

 private:
  T *data_;
  Span_Size size_;
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
