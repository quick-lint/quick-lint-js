// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_RC_STRING_H
#define QUICK_LINT_JS_RC_STRING_H

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/math-overflow.h>
#include <quick-lint-js/tagged-pointer.h>

namespace quick_lint_js {
// A sometimes-reference-counted null-terminated string.
//
// An rc_string can either manage its own string (which is reference-counted) or
// refer to a foreign string pointer (which is not reference-counted).
//
// rc_string is not thread-safe.
class rc_string {
 public:
  // Create a pointer to an existing string.
  //
  // This function does not copy the data. The returned rc_string becomes
  // invalid (but destructable) when the given c_string is deallocated or
  // mutated.
  static rc_string adopt_c_string(const char* c_string) noexcept {
    return rc_string(tagged_pointer(c_string, payload_not_ref_counted));
  }

  // Create a reference-counted string copied from the given C string.
  static rc_string copy_c_string(const char* c_string) noexcept(false) {
    std::size_t length = std::strlen(c_string);
    std::size_t size = length + 1;
    ref_counted_string_data* data = reinterpret_cast<ref_counted_string_data*>(
        std::malloc(size + offsetof(ref_counted_string_data, data)));
    data = new (data) ref_counted_string_data;
    data->ref_count = initial_ref_count;
    std::memcpy(data->data, c_string, size);
    return rc_string(tagged_pointer(data->data, payload_ref_counted));
  }

  explicit rc_string() noexcept : data_(this->empty_string()) {}

  rc_string(const rc_string& other) noexcept : data_(other.data_) {
    this->increment_ref_count();
  }

  rc_string(rc_string&& other) noexcept
      : data_(std::exchange(other.data_, this->empty_string())) {
    other.increment_ref_count();
  }

  rc_string& operator=(const rc_string& other) noexcept {
    if (this != &other) {
      this->decrement_ref_count();
      this->data_ = other.data_;
      this->increment_ref_count();
    }
    return *this;
  }

  rc_string& operator=(rc_string&& other) noexcept {
    if (this != &other) {
      this->decrement_ref_count();
      this->data_ = std::exchange(other.data_, this->empty_string());
      other.increment_ref_count();
    }
    return *this;
  }

  ~rc_string() noexcept { this->decrement_ref_count(); }

  const char* c_str() const noexcept {
    return reinterpret_cast<const char*>(this->data_.pointer());
  }

 private:
  using ref_count_type = int;

  struct ref_counted_string_data {
    ref_count_type ref_count;
    char data[1];
  };

  static constexpr ref_count_type initial_ref_count = 0;

  static constexpr tagged_pointer::payload_type payload_not_ref_counted = false;
  static constexpr tagged_pointer::payload_type payload_ref_counted = true;

  explicit rc_string(tagged_pointer data) noexcept : data_(data) {}

  void increment_ref_count() noexcept {
    if (this->is_ref_counted()) {
      ref_count_type& ref_count = this->ref_count();
      std::optional<ref_count_type> new_ref_count = checked_add(ref_count, 1);
      QLJS_ASSERT(new_ref_count.has_value());
      ref_count = *new_ref_count;
    }
  }

  void decrement_ref_count() noexcept {
    if (this->is_ref_counted()) {
      ref_count_type& ref_count = this->ref_count();
      if (ref_count == initial_ref_count) {
        std::free(this->allocated_string_data());
      } else {
        ref_count -= 1;
      }
    }
  }

  ref_count_type& ref_count() noexcept {
    QLJS_ASSERT(this->is_ref_counted());
    return this->allocated_string_data()->ref_count;
  }

  ref_counted_string_data* allocated_string_data() noexcept {
    QLJS_ASSERT(this->is_ref_counted());
    return reinterpret_cast<ref_counted_string_data*>(
        reinterpret_cast<std::uintptr_t>(this->data_.pointer()) -
        offsetof(ref_counted_string_data, data));
  }

  bool is_ref_counted() const noexcept {
    return this->data_.payload() == payload_ref_counted;
  }

  static tagged_pointer empty_string() noexcept {
    return tagged_pointer("", payload_not_ref_counted);
  }

  tagged_pointer data_;
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
