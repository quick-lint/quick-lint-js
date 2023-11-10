// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/cast.h>
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
  using Vector::erase;
  using Vector::front;
  using Vector::get_allocator;
  using Vector::operator[];
  using Vector::pop_back;
  using Vector::push_back;
  using Vector::push_front;
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
  using Vector::release_to_span;
  using Vector::release_to_string_view;
  using Vector::to_string_view;

  void swap(Uninstrumented_Vector &other) {
    static_cast<Vector &>(*this).swap(static_cast<Vector &>(other));
  }
};

template <class V>
void swap(Uninstrumented_Vector<V> &lhs, Uninstrumented_Vector<V> &rhs) {
  lhs.swap(rhs);
}

using Vector_Size = std::ptrdiff_t;

// Like std::pmr::vector. Some differences:
//
// * No exception safety.
// * Extended interface for convenience.
template <class T>
class Raw_Vector {
 public:
  using value_type = T;
  using allocator_type = Memory_Resource *;
  using size_type = Vector_Size;
  using difference_type = Vector_Size;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = const T *;

  // Create an empty vector.
  explicit Raw_Vector(Memory_Resource *allocator) : allocator_(allocator) {}

  Raw_Vector(const Raw_Vector &) = delete;
  Raw_Vector &operator=(const Raw_Vector &) = delete;

  // Move items from another Raw_Vector by taking its allocation.
  //
  // This function does not move individual items.
  //
  // Postcondition: other.empty()
  Raw_Vector(Raw_Vector &&other)
      : data_(other.data_),
        data_end_(other.data_end_),
        capacity_end_(other.capacity_end_),
        allocator_(other.allocator_) {
    other.data_ = nullptr;
    other.data_end_ = nullptr;
    other.capacity_end_ = nullptr;
  }

  // Destruct items in the container, as in this->clear(), then release the
  // memory.
  //
  // If the allocator is a Linked_Bump_Allocator, then memory is only released
  // if this Raw_Vector's capacity is the last thing allocated with that
  // allocator.
  ~Raw_Vector() { this->clear(); }

  // Return the pointer given in Raw_Vector's constructor.
  Memory_Resource *get_allocator() const { return this->allocator_; }

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

  // Precondition: index is in bounds. This means that '&(*this)[this->size()]'
  // (indexing one past the end) is invalid. To get one past the end, call
  // this->end() instead or write '&this->data()[this->size()]'.
  T &operator[](size_type index) {
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }
  const T &operator[](size_type index) const {
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }

  // Precondition: (none)
  void reserve(size_type new_capacity) {
    QLJS_ASSERT(new_capacity >= 0);
    if (this->capacity() < new_capacity) {
      this->reserve_grow(new_capacity);
    }
  }

  // Precondition: new_capacity > this->capacity()
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
        Span<T> new_data =
            this->allocator_->template allocate_uninitialized_span<T>(
                narrow_cast<std::size_t>(new_capacity));
        T *new_data_end = std::uninitialized_move(this->data_, this->data_end_,
                                                  new_data.begin());
        this->clear();
        this->data_ = new_data.begin();
        this->data_end_ = new_data_end;
        this->capacity_end_ = new_data.end();
      }
    } else {
      Span<T> new_data =
          this->allocator_->template allocate_uninitialized_span<T>(
              narrow_cast<std::size_t>(new_capacity));
      this->data_ = new_data.begin();
      this->data_end_ = this->data_;
      this->capacity_end_ = new_data.end();
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
  Raw_Vector &operator+=(std::basic_string_view<T> values) {
    this->append(values.data(), values.data() + values.size());
    return *this;
  }

  // Similar to std::basic_string::operator+=.
  Raw_Vector &operator+=(T value) {
    this->emplace_back(value);
    return *this;
  }

  void pop_back() {
    QLJS_ASSERT(!this->empty());
    this->data_end_ -= 1;
  }

  void push_front(value_type &&value) {
    if (this->empty()) {
      this->push_back(std::move(value));
    } else {
      if (this->capacity_end_ == this->data_end_) {
        this->reserve_grow_by_at_least(1);
      }
      // Shift all items to the right one. The last item is move-constructed
      // into place, and the remaining items are move-assigned into place right
      // to left.
      new (&this->data_end_[0]) value_type(std::move(this->data_end_[-1]));
      for (value_type *p = this->data_end_; p-- > this->data_;) {
        p[1] = p[0];
      }
      this->data_[0] = std::move(value);
      this->data_end_ += 1;
    }
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

  // Call the destructor of each item, then deallocate memory used for the
  // items.
  //
  // Postcondition: this->empty()
  void clear() {
    if (this->data_) {
      std::destroy(this->data_, this->data_end_);
      this->allocator_->deallocate(
          this->data_,
          narrow_cast<std::size_t>(this->size() *
                                   static_cast<Vector_Size>(sizeof(T))),
          alignof(T));
      this->release();
    }
  }

  void erase(value_type *begin, value_type *end) {
    // Shift items left (via move assignment), then destruct items on the right.

    value_type *assign_to_begin = begin;
    value_type *assign_from_begin = end;
    value_type *assign_from_end = this->data_end_;
    value_type *assign_to_end =
        std::move(assign_from_begin, assign_from_end, assign_to_begin);

    value_type *destroy_begin = assign_to_end;
    value_type *destroy_end = this->data_end_;
    std::destroy(destroy_begin, destroy_end);

    this->data_end_ = destroy_begin;
  }

  void erase(value_type *item) { erase(item, item + 1); }

  // Swap capacity pointers and sizes between *this and other. All items of
  // *this and other are untouched.
  //
  // Precondition: this->get_allocator() == other.get_allocator()
  void swap(Raw_Vector &other) {
    QLJS_ALWAYS_ASSERT(this->get_allocator() == other.get_allocator());
    std::swap(this->data_, other.data_);
    std::swap(this->data_end_, other.data_end_);
    std::swap(this->capacity_end_, other.capacity_end_);
  }

  // If new_size > this->size(): default-construct new items at the end.
  // If new_size < this->size(): destruct items at the end.
  //
  // Postcondition: this->size() == new_size
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

  // Like operator Span<T>() followed by this->release().
  Span<value_type> release_to_span() {
    Span<value_type> result = Span<value_type>(*this);
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
  // Growth strategy.
  [[gnu::noinline]] void reserve_grow_by_at_least(
      size_type minimum_new_entries) {
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

  Memory_Resource *allocator_;
};

template <class T>
void swap(Raw_Vector<T> &lhs, Raw_Vector<T> &rhs) {
  lhs.swap(rhs);
}

#if QLJS_FEATURE_VECTOR_PROFILING
template <class T>
using Vector = Instrumented_Vector<Raw_Vector<T>>;
#else
template <class T>
using Vector = Uninstrumented_Vector<Raw_Vector<T>>;
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
