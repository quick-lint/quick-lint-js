// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_FIXED_VECTOR_H
#define QUICK_LINT_JS_CONTAINER_FIXED_VECTOR_H

#include <new>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wgnu-anonymous-struct")
QLJS_WARNING_IGNORE_GCC("-Wpedantic")

namespace quick_lint_js {
using Fixed_Vector_Size = Span_Size;

// Like std::vector<T>, but with a maximum size.
//
// Like std::array<T, max_size>, but with a run-time number of items.
//
// Like boost::container::static_vector<T, max_size>.
template <class T, Fixed_Vector_Size max_size>
union Fixed_Vector {
 private:
  using Storage_Type = char[sizeof(T) * max_size];

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

  explicit Fixed_Vector() {
    this->size_ = 0;

    // Begin the lifetime of this->items_ as Storage_Type. This should not
    // generate any machine code. See NOTE[Fixed_Vector-union-init].
    new (this->storage()) Storage_Type;
  }

  Fixed_Vector(const Fixed_Vector &other) : Fixed_Vector() { *this = other; }

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
    return reinterpret_cast<T *>(this->storage());
  }
  QLJS_FORCE_INLINE const T *storage_slots() const {
    return reinterpret_cast<const T *>(this->storage());
  }

  QLJS_FORCE_INLINE Storage_Type *storage() {
    return reinterpret_cast<Storage_Type *>(&this->items_);
  }
  QLJS_FORCE_INLINE const Storage_Type *storage() const {
    return reinterpret_cast<const Storage_Type *>(&this->items_);
  }

  // NOTE[Fixed_Vector-union]:
  //
  // C++ language rules which state "In a union, a non-static data member is
  // active if its name refers to an object whose lifetime has begun and has not
  // ended. At most one of the non-static data members of an object of union
  // type can be active at any time" (C++17 [class.union]). My interpretation is
  // that it is illegal to construct a subobject of a union data member.
  // Therefore, in the following example you cannot construct only u.items[0];
  // you must construct the entirety of u.items:
  //
  //     // Example A:
  //     union U {
  //       std::string items[4];
  //     };
  //     U u;
  //     // Begin the lifetime of items[0] (illegal, I think):
  //     new (&u.items[0]) std::string("hello");
  //
  // To work around this issue, we use an array of char instead, where
  // construction of the array does nothing:
  //
  //     // Example B:
  //     union U {
  //       alignas(T) char storage[sizeof(std::string) * 4];
  //     };
  //     U u;
  //     // Begin the lifetime of the u.storage array of chars:
  //     new (&u.storage) char[sizeof(std::string) * 4];
  //     T* items = (T *)u.storage;
  //     // Begin the lifetime of a subobject inside u.storage (legal, I think):
  //     new (&items[0]) std::string("hello");
  //     // Make sure you std::launder if you want to access items[0] again! See
  //     // NOTE[Fixed_Vector-launder].
  //
  // However, this breaks debugging. Instead of a debugger showing the
  // programmer a nice list of subobjects, the debugger prints out the raw
  // storage bytes.
  //
  // To make the programmer happy while using a debugger, we make Fixed_Vector's
  // storage an array of T. To make the array of T work correctly, it is wrapped
  // in a union. NOTE[Fixed_Vector-union-init]: Importantly, instead of starting
  // its lifetime as an array of T we start its lifetime as an array of char
  // (like in example B above).
  //
  // This Fixed_Vector's data might look like this:
  //
  //     // Example C:
  //     template <class T, Fixed_Vector_Size max_size>
  //     class Fixed_Vector {
  //     public:
  //       /* ... */
  //
  //     private:
  //       Fixed_Vector_Size size_;
  //       union U {
  //         U() {}  // Required if T is nontrivial.
  //         T items_[max_size];
  //       } u_;
  //     };
  //
  // Unfortunately, this leads to an ugly 'u_' member:
  //
  //     (lldb) p v
  //     (quick_lint_js::Fixed_Vector<int, 4>) $0 = {
  //       size_ = 4
  //       u_ = {
  //         items_ = ([0] = 100, [1] = 200, [2] = 300, [3] = 400)
  //       }
  //     }
  //
  // (We can't make 'U' anonymous because it must have a constructor.)
  //
  // To work around this problem, instead of making a struct containing a union,
  // we make a union containing a struct:
  //
  //     // Example D:
  //     template <class T, Fixed_Vector_Size max_size>
  //     union Fixed_Vector {
  //     public:
  //       /* ... */
  //
  //     private:
  //       struct {
  //         Fixed_Vector_Size size_;
  //         T items_[max_size];
  //       };
  //     };
  //
  // This puts 'size_' and 'items_' on the same level. There is still a wrapper
  // in the debugger's pretty-printing, but I think this is the best we can do:
  //
  //     (lldb) p v
  //     (quick_lint_js::Fixed_Vector<int, 4>) $0 = {
  //        = {
  //         size_ = 4
  //         items_ = ([0] = 100, [1] = 200, [2] = 300, [3] = 400)
  //       }
  //     }
  struct {
    Fixed_Vector_Size size_;
    T items_[static_cast<std::size_t>(max_size)];
  };
};
}

QLJS_WARNING_POP

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
