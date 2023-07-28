// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_FIXED_VECTOR_H
#define QUICK_LINT_JS_CONTAINER_FIXED_VECTOR_H

#include <new>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>

namespace quick_lint_js {
using Fixed_Vector_Size = Span_Size;

// Like std::vector<T>, but with a maximum size.
//
// Like std::array<T, max_size>, but with a run-time number of items.
//
// Like boost::container::static_vector<T, max_size>.
template <class T, Fixed_Vector_Size max_size>
class Fixed_Vector {
 public:
  using value_type = T;
  using size_type = Fixed_Vector_Size;
  using difference_type = Fixed_Vector_Size;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = const T *;

  static_assert(is_winkable_v<T>);

  explicit Fixed_Vector() = default;

  Fixed_Vector(const Fixed_Vector &other) { *this = other; }

  Fixed_Vector &operator=(const Fixed_Vector &other) {
    this->clear();
    for (const T &other_item : other) {
      this->push_back(other_item);
    }
    return *this;
  }

  // TODO(strager): Move constructor and assignment operator.

  // NOTE[Fixed_Vector-destructor]: Because T is winkable, the destructor can
  // remain defaulted. Best case, this makes Fixed_Vector<T> trivially
  // destructable.

  bool empty() const { return this->size() == 0; }
  size_type size() const { return this->size_; }
  size_type capacity() const { return max_size; }

  QLJS_FORCE_INLINE T *data() { return this->storage_slots(); }
  QLJS_FORCE_INLINE const T *data() const { return this->storage_slots(); }

  QLJS_FORCE_INLINE const T *begin() const { return &this->storage_slots()[0]; }
  QLJS_FORCE_INLINE const T *end() const {
    return &this->storage_slots()[this->size_];
  }

  QLJS_FORCE_INLINE T *begin() { return &this->storage_slots()[0]; }
  QLJS_FORCE_INLINE T *end() { return &this->storage_slots()[this->size_]; }

  T &operator[](size_type index) {
    QLJS_ASSERT(index < this->size());
    // NOTE[Fixed_Vector-launder]: From my understand of the C++ language,
    // whenever you use placement new (such as in Fixed_Vector::emplace_back),
    // you must use std::launder if you access the item not using the pointer
    // returned by placement new.
    return *std::launder(&this->storage_slots()[index]);
  }

  T &push_back(const T &value) { return this->emplace_back(value); }
  T &push_back(T &&value) { return this->emplace_back(std::move(value)); }

  template <class... Args>
  T &emplace_back(Args &&... args) {
    QLJS_ASSERT(this->size() < max_size);
    T &result = *new (&this->storage_slots()[this->size_])
                    T(std::forward<Args>(args)...);
    this->size_ += 1;
    return result;
  }

  void clear() {
    for (const T &item : *this) {
      item.~T();
    }
    this->size_ = 0;
  }

 private:
  QLJS_FORCE_INLINE T *storage_slots() {
    return reinterpret_cast<T *>(this->storage_);
  }
  QLJS_FORCE_INLINE const T *storage_slots() const {
    return reinterpret_cast<const T *>(this->storage_);
  }

  Fixed_Vector_Size size_ = 0;
  alignas(T) char storage_[sizeof(T) * max_size];
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
