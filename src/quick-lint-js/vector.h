// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VECTOR_H
#define QUICK_LINT_JS_VECTOR_H

#include <algorithm>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <cstddef>
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/force-inline.h>
#include <quick-lint-js/narrow-cast.h>
#include <string_view>
#include <type_traits>
#include <utility>

#if QLJS_FEATURE_VECTOR_PROFILING
#include <quick-lint-js/vector-profiler.h>
#endif

namespace quick_lint_js {
// Wraps a vector class so it has the same interface as
// instrumented_vector<Vector> (but without the instrumentation overhead).
template <class Vector>
class uninstrumented_vector : private Vector {
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

  explicit uninstrumented_vector(
      const char *, const typename Vector::allocator_type &allocator) noexcept
      : Vector(allocator) {}

  explicit uninstrumented_vector(
      const char *, const typename Vector::allocator_type &allocator,
      const typename Vector::value_type *begin,
      const typename Vector::value_type *end)
      : Vector(begin, end, allocator) {}

  uninstrumented_vector(const uninstrumented_vector &) = delete;
  uninstrumented_vector &operator=(const uninstrumented_vector &) = delete;

  uninstrumented_vector(uninstrumented_vector &&other) = default;

  uninstrumented_vector(const char *, uninstrumented_vector &&other)
      : uninstrumented_vector(std::move(other)) {}

  uninstrumented_vector &operator=(uninstrumented_vector &&other) = default;

  ~uninstrumented_vector() = default;

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
  using Vector::reserve;
  using Vector::resize;
  using Vector::size;

  // NOTE(strager): These are non-standard functions.
  using Vector::append;
  using Vector::operator std::basic_string_view<value_type>;
  using Vector::operator+=;
  using Vector::release;
};

template <class T, class BumpAllocator>
class raw_bump_vector {
 public:
  using value_type = T;
  using allocator_type = BumpAllocator *;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = const T *;

  static_assert(std::is_trivially_destructible_v<T>);

  explicit raw_bump_vector(BumpAllocator *allocator) noexcept
      : allocator_(allocator) {}

  raw_bump_vector(const raw_bump_vector &) = delete;
  raw_bump_vector &operator=(const raw_bump_vector &) = delete;

  ~raw_bump_vector() { this->clear(); }

  BumpAllocator *get_allocator() const noexcept { return this->allocator_; }

  bool empty() const noexcept { return this->data_ == this->data_end_; }
  std::size_t size() const noexcept {
    return narrow_cast<std::size_t>(this->data_end_ - this->data_);
  }
  std::size_t capacity() const noexcept {
    return narrow_cast<std::size_t>(this->capacity_end_ - this->data_);
  }

  QLJS_FORCE_INLINE T *data() const noexcept { return this->data_; }

  QLJS_FORCE_INLINE const T *begin() const noexcept { return this->data_; }
  QLJS_FORCE_INLINE const T *end() const noexcept { return this->data_end_; }

  T &front() noexcept {
    QLJS_ASSERT(!this->empty());
    return this->data_[0];
  }
  T &back() noexcept {
    QLJS_ASSERT(!this->empty());
    return this->data_end_[-1];
  }

  const T &front() const noexcept {
    QLJS_ASSERT(!this->empty());
    return this->data_[0];
  }
  const T &back() const noexcept {
    QLJS_ASSERT(!this->empty());
    return this->data_end_[-1];
  }

  T &operator[](size_type index) noexcept {
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }

  void reserve(std::size_t size) {
    if (this->capacity() < size) {
      this->reserve_grow(size);
    }
  }

  void reserve_grow(std::size_t new_size) {
    QLJS_ASSERT(new_size > this->capacity());
    if (this->data_) {
      bool grew = this->allocator_->try_grow_array_in_place(
          this->data_,
          /*old_size=*/this->capacity(),
          /*new_size=*/new_size);
      if (grew) {
        this->capacity_end_ = this->data_ + new_size;
      } else {
        T *new_data =
            this->allocator_->template allocate_uninitialized_array<T>(
                new_size);
        T *new_data_end =
            std::uninitialized_move(this->data_, this->data_end_, new_data);
        this->clear();
        this->data_ = new_data;
        this->data_end_ = new_data_end;
        this->capacity_end_ = new_data + new_size;
      }
    } else {
      this->data_ =
          this->allocator_->template allocate_uninitialized_array<T>(new_size);
      this->data_end_ = this->data_;
      this->capacity_end_ = this->data_ + new_size;
    }
  }

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
  raw_bump_vector &operator+=(std::basic_string_view<T> values) {
    this->append(values.data(), values.data() + values.size());
    return *this;
  }

  // Similar to std::basic_string::operator+=.
  raw_bump_vector &operator+=(T value) {
    this->emplace_back(value);
    return *this;
  }

  // Like clear(), but doesn't touch the allocated memory. Objects remain alive
  // and valid.
  void release() {
    this->data_ = nullptr;
    this->data_end_ = nullptr;
    this->capacity_end_ = nullptr;
  }

  void clear() {
    if (this->data_) {
      std::destroy(this->data_, this->data_end_);
      this->allocator_->deallocate(
          this->data_,
          narrow_cast<std::size_t>(this->data_end_ - this->data_) * sizeof(T),
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

  explicit operator std::basic_string_view<value_type>() const noexcept {
    return std::basic_string_view<value_type>(this->data_, this->size());
  }

 private:
  void reserve_grow_by_at_least(std::size_t minimum_new_entries) {
    std::size_t old_capacity = this->capacity();
    constexpr std::size_t minimum_capacity = 4;
    std::size_t new_size = (std::max)(
        (std::max)(minimum_capacity, old_capacity + minimum_new_entries),
        old_capacity * 2);
    this->reserve_grow(new_size);
  }

  T *data_ = nullptr;
  T *data_end_ = nullptr;
  T *capacity_end_ = nullptr;

  BumpAllocator *allocator_;
};

#if QLJS_FEATURE_VECTOR_PROFILING
template <class T, class BumpAllocator>
using bump_vector = instrumented_vector<raw_bump_vector<T, BumpAllocator>>;
#else
template <class T, class BumpAllocator>
using bump_vector = uninstrumented_vector<raw_bump_vector<T, BumpAllocator>>;
#endif
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
