// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <memory>
#include <new>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/cast.h>
#include <utility>

namespace quick_lint_js {
// Like std::pmr::memory_resource.
//
// Helper functions are forcefully inlined with QLJS_FORCE_INLINE to increase
// the chance that devirtualization occurs.
class Memory_Resource {
 public:
  virtual ~Memory_Resource() = default;

  QLJS_FORCE_INLINE void* allocate(std::size_t bytes, std::size_t alignment) {
    return this->do_allocate(bytes, alignment);
  }

  QLJS_FORCE_INLINE void deallocate(void* p, std::size_t bytes,
                                    std::size_t alignment) {
    return this->do_deallocate(p, bytes, alignment);
  }

  // Allocate space for an instance of T, then construct T.
  template <class T, class... Args>
  QLJS_FORCE_INLINE T* new_object(Args&&... args) {
    return new (this->do_allocate(sizeof(T), alignof(T)))
        T(std::forward<Args>(args)...);
  }

  // Allocate space for an instance of T, then construct T via copy or move
  // construction.
  template <class T>
  QLJS_FORCE_INLINE T* new_object_copy(T&& value) {
    return this->new_object<T>(std::forward<T>(value));
  }

  // Allocate space for objects.size() instances of T, then construct result[i]
  // via copy or move construction from objects[i] for i from 0 to
  // objects.size()-1.
  template <class T>
  QLJS_FORCE_INLINE Span<T> new_objects_copy(Span<const T> objects) {
    Span<T> new_objects = this->allocate_uninitialized_span<T>(
        narrow_cast<std::size_t>(objects.size()));
    std::uninitialized_copy(objects.begin(), objects.end(),
                            new_objects.begin());
    return new_objects;
  }

  // Allocate space for object.size() instances of T. Does not construct any
  // T-s.
  template <class T>
  QLJS_FORCE_INLINE [[nodiscard]] Span<T> allocate_uninitialized_span(
      std::size_t size) {
    std::size_t byte_size = size * sizeof(T);
    T* items = reinterpret_cast<T*>(this->do_allocate(byte_size, alignof(T)));
    return Span<T>(items, narrow_cast<Span_Size>(size));
  }

  // Allocate space for object.size() instances of T, then default-construct
  // object.size() instances.
  template <class T>
  QLJS_FORCE_INLINE [[nodiscard]] Span<T> allocate_span(Span_Size size) {
    return this->allocate_span<T>(narrow_cast<std::size_t>(size));
  }

  // Allocate space for object.size() instances of T, then default-construct
  // object.size() instances.
  template <class T>
  QLJS_FORCE_INLINE [[nodiscard]] Span<T> allocate_span(std::size_t size) {
    Span<T> items = this->allocate_uninitialized_span<T>(size);
    for (T& item : items) {
      // FIXME(strager): I think we technically need to std::launder after
      // placement new.
      new (&item) T();
    }
    return items;
  }

  // Given previously-allocated space for old_size instances of T, allocate
  // adjacent space for (new_size-old_size) instances of T after the old
  // allocation and return true.
  //
  // If adjacent space is not available, do nothing and return false.
  template <class T>
  QLJS_FORCE_INLINE bool try_grow_array_in_place(T* array, std::size_t old_size,
                                                 std::size_t new_size) {
    return this->try_grow_in_place(array, old_size * sizeof(T),
                                   new_size * sizeof(T));
  }

  // Given previously-allocated space of old_byte_size bytes, allocate adjacent
  // space for (new_byte_size-old_byte_size) bytes after the old allocation and
  // return true.
  //
  // If adjacent space is not available, do nothing and return false.
  QLJS_FORCE_INLINE bool try_grow_in_place(void* p, std::size_t old_byte_size,
                                           std::size_t new_byte_size) {
    return this->do_try_grow_in_place(p, old_byte_size, new_byte_size);
  }

 protected:
  virtual void* do_allocate(std::size_t bytes, std::size_t alignment) = 0;
  virtual void do_deallocate(void* p, std::size_t bytes,
                             std::size_t alignment) = 0;
  virtual bool do_try_grow_in_place(
      [[maybe_unused]] void* p, [[maybe_unused]] std::size_t old_byte_size,
      [[maybe_unused]] std::size_t new_byte_size) {
    return false;
  }
};

// Like std::pmr::new_delete_resource.
Memory_Resource* new_delete_resource();
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
