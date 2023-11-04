// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <memory>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/memory-resource.h>
#include <utility>

namespace quick_lint_js {
// Helper for Flexible_Array. Do not use directly.
//
// Flexible_Array_Base exists to have the compiler compute the required
// alignment for Header and the capacity field.
template <class Header>
class Flexible_Array_Base : public Header {
 protected:
  template <class... Args>
  explicit Flexible_Array_Base(std::size_t capacity, Args&&... args)
      : Header(std::forward<Args>(args)...), capacity_(capacity) {}

  std::size_t capacity_;
};

// Like a C99 flexible array member but with its size too.
template <class T, class Header>
class alignas(maximum(alignof(T),
                      alignof(Flexible_Array_Base<Header>))) Flexible_Array
    : public Flexible_Array_Base<Header> {
 public:
  // The guaranteed alignment for the capacity.
  //
  // Invariant: capacity_alignment >= alignof(T)
  static inline constexpr std::size_t capacity_alignment =
      maximum(alignof(T), alignof(Flexible_Array_Base<Header>));

  T* flexible_capacity_begin() { return reinterpret_cast<T*>(&this[1]); }

  T* flexible_capacity_end() {
    return this->flexible_capacity_begin() + this->flexible_capacity();
  }

  // Count of T items allocated in the flexible capacity.
  //
  // Equal to (this->flexible_capacity_end() - this->flexible_capacity_begin()).
  std::size_t flexible_capacity() const { return this->capacity_; }

  static std::size_t total_allocation_size(std::size_t flexible_capacity) {
    return sizeof(Flexible_Array) + flexible_capacity * sizeof(T);
  }

  // Constructs Header, but does not construct items inside the flexible
  // capacity.
  template <class... Args>
  static Flexible_Array* allocate_and_construct_header(
      Memory_Resource* memory, std::size_t flexible_capacity, Args&&... args) {
    void* p = memory->allocate(
        Flexible_Array::total_allocation_size(flexible_capacity),
        alignof(Flexible_Array));
    return new (p)
        Flexible_Array(flexible_capacity, std::forward<Args>(args)...);
  }

  // Calls Header's destructor, but not the destructor of items inside the
  // flexible capacity.
  static void destroy_header_and_deallocate(Memory_Resource* memory,
                                            Flexible_Array* p) {
    std::size_t capacity = p->capacity_;
    p->~Flexible_Array();
    memory->deallocate(p, Flexible_Array::total_allocation_size(capacity),
                       alignof(Flexible_Array));
  }

 private:
  template <class... Args>
  explicit Flexible_Array(std::size_t capacity, Args&&... args)
      : Flexible_Array_Base<Header>(capacity, std::forward<Args>(args)...) {}
};
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
