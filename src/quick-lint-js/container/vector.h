// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <type_traits>
#include <utility>

#if QLJS_FEATURE_VECTOR_PROFILING
#include <quick-lint-js/container/vector-profiler.h>
#endif

namespace quick_lint_js {
// Wraps a vector class so it has the same interface as
// Instrumented_Vector<Vector> (but without the instrumentation overhead).
template <class Vector>
class Uninstrumented_Vector : private Vector {
 public:
  using typename Vector::allocator_type;
  using typename Vector::const_iterator;
  using typename Vector::const_pointer;
  using typename Vector::const_reference;
  using typename Vector::difference_type;
  using typename Vector::iterator;
  using typename Vector::pointer;
  using typename Vector::reference;
  using typename Vector::size_type;
  using typename Vector::value_type;

  explicit Uninstrumented_Vector(
      const char *, const typename Vector::allocator_type &allocator)
      : Vector(allocator) {}

  explicit Uninstrumented_Vector(
      const char *, const typename Vector::allocator_type &allocator,
      const typename Vector::value_type *begin,
      const typename Vector::value_type *end)
      : Vector(begin, end, allocator) {}

  Uninstrumented_Vector(const Uninstrumented_Vector &) = delete;
  Uninstrumented_Vector &operator=(const Uninstrumented_Vector &) = delete;

  Uninstrumented_Vector(Uninstrumented_Vector &&other) = default;

  Uninstrumented_Vector(const char *, Uninstrumented_Vector &&other)
      : Uninstrumented_Vector(std::move(other)) {}

  Uninstrumented_Vector &operator=(Uninstrumented_Vector &&other) = default;

  ~Uninstrumented_Vector() = default;

  using Vector::back;
  using Vector::begin;
  using Vector::capacity;
  using Vector::clear;
  using Vector::data;
  using Vector::emplace_back;
  using Vector::empty;
  using Vector::end;
  using Vector::front;
  using Vector::get_allocator;
  using Vector::operator[];
  using Vector::pop_back;
  using Vector::push_back;
  using Vector::reserve;
  using Vector::resize;
  using Vector::size;

  // NOTE(strager): These are non-standard functions.
  using Vector::append;
  using Vector::get_and_release;
  using Vector::operator Span<const value_type>;
  using Vector::operator Span<value_type>;
  using Vector::operator+=;
  using Vector::release;
  using Vector::release_to_string_view;
  using Vector::to_string_view;
};

using Bump_Vector_Size = std::ptrdiff_t;

template <class T, class Bump_Allocator>
class Raw_Bump_Vector {
 public:
  using value_type = T;
  using allocator_type = Bump_Allocator *;
  using size_type = Bump_Vector_Size;
  using difference_type = Bump_Vector_Size;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = const T *;

  static_assert(is_winkable_v<T>);

  explicit Raw_Bump_Vector(Bump_Allocator *allocator) : allocator_(allocator) {}

  Raw_Bump_Vector(const Raw_Bump_Vector &) = delete;
  Raw_Bump_Vector &operator=(const Raw_Bump_Vector &) = delete;

  Raw_Bump_Vector(Raw_Bump_Vector &&other)
      : data_(other.data_),
        data_end_(other.data_end_),
        capacity_end_(other.capacity_end_),
        allocator_(other.allocator_) {
    other.data_ = nullptr;
    other.data_end_ = nullptr;
    other.capacity_end_ = nullptr;
  }

  ~Raw_Bump_Vector() { this->clear(); }

  Bump_Allocator *get_allocator() const { return this->allocator_; }

  bool empty() const { return this->data_ == this->data_end_; }
  size_type size() const {
    return narrow_cast<size_type>(this->data_end_ - this->data_);
  }
  size_type capacity() const {
    return narrow_cast<size_type>(this->capacity_end_ - this->data_);
  }

  QLJS_FORCE_INLINE T *data() { return this->data_; }
  QLJS_FORCE_INLINE const T *data() const { return this->data_; }

  QLJS_FORCE_INLINE const T *begin() const { return this->data_; }
  QLJS_FORCE_INLINE const T *end() const { return this->data_end_; }

  QLJS_FORCE_INLINE T *begin() { return this->data_; }
  QLJS_FORCE_INLINE T *end() { return this->data_end_; }

  T &front() {
    QLJS_ASSERT(!this->empty());
    return this->data_[0];
  }
  T &back() {
    QLJS_ASSERT(!this->empty());
    return this->data_end_[-1];
  }

  const T &front() const {
    QLJS_ASSERT(!this->empty());
    return this->data_[0];
  }
  const T &back() const {
    QLJS_ASSERT(!this->empty());
    return this->data_end_[-1];
  }

  T &operator[](size_type index) {
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }

  void reserve(size_type new_capacity) {
    QLJS_ASSERT(new_capacity > 0);
    if (this->capacity() < new_capacity) {
      this->reserve_grow(new_capacity);
    }
  }

  void reserve_grow(size_type new_capacity) {
    QLJS_ASSERT(new_capacity > this->capacity());
    if (this->data_) {
      bool grew = this->allocator_->try_grow_array_in_place(
          this->data_,
          /*old_size=*/narrow_cast<std::size_t>(this->capacity()),
          /*new_size=*/narrow_cast<std::size_t>(new_capacity));
      if (grew) {
        this->capacity_end_ = this->data_ + new_capacity;
      } else {
        T *new_data =
            this->allocator_->template allocate_uninitialized_array<T>(
                narrow_cast<std::size_t>(new_capacity));
        T *new_data_end =
            std::uninitialized_move(this->data_, this->data_end_, new_data);
        this->clear();
        this->data_ = new_data;
        this->data_end_ = new_data_end;
        this->capacity_end_ = new_data + new_capacity;
      }
    } else {
      this->data_ = this->allocator_->template allocate_uninitialized_array<T>(
          narrow_cast<std::size_t>(new_capacity));
      this->data_end_ = this->data_;
      this->capacity_end_ = this->data_ + new_capacity;
    }
  }

  T &push_back(const T &value) { return this->emplace_back(value); }
  T &push_back(T &&value) { return this->emplace_back(std::move(value)); }

  template <class... Args>
  T &emplace_back(Args &&... args) {
    if (this->capacity_end_ == this->data_end_) {
      this->reserve_grow_by_at_least(1);
    }
    this->data_end_ = new (this->data_end_) T(std::forward<Args>(args)...);
    T &result = *this->data_end_++;
    return result;
  }

  // Similar to std::basic_string::append.
  void append(const T *begin, const T *end) {
    // TODO(strager): Make this more efficient.
    for (const T *p = begin; p != end; ++p) {
      this->emplace_back(*p);
    }
  }

  // Similar to std::basic_string::append.
  void append(size_type count, T value) {
    // TODO(strager): Make this more efficient.
    for (size_type i = 0; i < count; ++i) {
      this->emplace_back(value);
    }
  }

  // Similar to std::basic_string::operator+=.
  Raw_Bump_Vector &operator+=(std::basic_string_view<T> values) {
    this->append(values.data(), values.data() + values.size());
    return *this;
  }

  // Similar to std::basic_string::operator+=.
  Raw_Bump_Vector &operator+=(T value) {
    this->emplace_back(value);
    return *this;
  }

  void pop_back() {
    QLJS_ASSERT(!this->empty());
    this->data_end_ -= 1;
  }

  // Like clear(), but doesn't touch the allocated memory. Objects remain alive
  // and valid.
  void release() {
    this->data_ = nullptr;
    this->data_end_ = nullptr;
    this->capacity_end_ = nullptr;
  }

  // See release().
  Span<value_type> get_and_release() {
    Span<value_type> span(*this);
    this->release();
    return span;
  }

  void clear() {
    if (this->data_) {
      std::destroy(this->data_, this->data_end_);
      this->allocator_->deallocate(
          this->data_,
          narrow_cast<std::size_t>(this->size() *
                                   static_cast<Bump_Vector_Size>(sizeof(T))),
          alignof(T));
      this->release();
    }
  }

  void resize(size_type new_size) {
    size_type old_size = this->size();
    if (new_size == old_size) {
      // Do nothing.
    } else if (new_size < old_size) {
      T *new_end = this->data_ + new_size;
      std::destroy(new_end, this->data_end_);
      this->data_end_ = new_end;
    } else {
      size_type old_capacity = this->capacity();
      if (new_size > old_capacity) {
        this->reserve_grow_by_at_least(new_size - old_capacity);
      }
      T *new_end = this->data_ + new_size;
      for (T *p = this->data_end_; p != new_end; ++p) {
        new (p) T();
      }
      this->data_end_ = new_end;
    }
  }

  std::basic_string_view<value_type> to_string_view() const {
    return std::basic_string_view<value_type>(
        this->data_, narrow_cast<std::size_t>(this->size()));
  }

  // Like this->to_string_view() followed by this->release().
  std::basic_string_view<value_type> release_to_string_view() {
    std::basic_string_view<value_type> result = this->to_string_view();
    this->release();
    return result;
  }

  explicit operator Span<value_type>() {
    return Span<value_type>(this->data_, this->size());
  }

  explicit operator Span<const value_type>() const {
    return Span<const value_type>(this->data_, this->size());
  }

 private:
  void reserve_grow_by_at_least(size_type minimum_new_entries) {
    size_type old_capacity = this->capacity();
    constexpr size_type minimum_capacity = 4;
    size_type new_size = (std::max)(
        (std::max)(minimum_capacity, old_capacity + minimum_new_entries),
        old_capacity * 2);
    this->reserve_grow(new_size);
  }

  T *data_ = nullptr;
  T *data_end_ = nullptr;
  T *capacity_end_ = nullptr;

  Bump_Allocator *allocator_;
};

#if QLJS_FEATURE_VECTOR_PROFILING
template <class T, class Bump_Allocator>
using Bump_Vector = Instrumented_Vector<Raw_Bump_Vector<T, Bump_Allocator>>;
#else
template <class T, class Bump_Allocator>
using Bump_Vector = Uninstrumented_Vector<Raw_Bump_Vector<T, Bump_Allocator>>;
#endif
}

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
