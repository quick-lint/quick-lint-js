// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TAGGED_POINTER_H
#define QUICK_LINT_JS_TAGGED_POINTER_H

#include <cstdint>
#include <quick-lint-js/assert.h>

namespace quick_lint_js {
// A data pointer and a boolean.
//
// Use tagged_pointer instead. tagged_pointer_portable is space-inefficient.
class tagged_pointer_portable {
 public:
  using payload_type = bool;

  [[gnu::always_inline]] explicit tagged_pointer_portable(const void* pointer,
                                                          payload_type payload)
      : pointer_(const_cast<void*>(pointer)), payload_(payload) {}

  [[gnu::always_inline]] void* pointer() const noexcept {
    return this->pointer_;
  }

  [[gnu::always_inline]] payload_type payload() const noexcept {
    return this->payload_;
  }

 private:
  void* pointer_;
  payload_type payload_;
};

#if defined(__x86_64__) || defined(_M_X64)
// A compact representation of tagged_pointer_portable for AMD/Intel 64-bit x86.
//
// tagged_pointer_x86_64 takes advantage of the fact that, on all supported
// operating systems, the top bit of pointers is always zero.
class tagged_pointer_x86_64 {
 public:
  using payload_type = bool;

  explicit tagged_pointer_x86_64(const void* pointer, payload_type payload) {
    std::uintptr_t pointer_bits = reinterpret_cast<std::uintptr_t>(pointer);
    QLJS_ASSERT((pointer_bits & payload_mask) == 0);
    this->bits_ = pointer_bits | (payload ? payload_mask : 0);
  }

  [[gnu::always_inline]] void* pointer() const noexcept {
    return reinterpret_cast<void*>(this->bits_ & ~payload_mask);
  }

  [[gnu::always_inline]] payload_type payload() const noexcept {
    return !!(this->bits_ & payload_mask);
  }

 private:
  static constexpr std::uintptr_t payload_mask = 1ULL << 63;

  std::uintptr_t bits_;
};
static_assert(sizeof(tagged_pointer_x86_64) == sizeof(void*));
#endif

#if defined(__x86_64__) || defined(_M_X64)
using tagged_pointer = tagged_pointer_x86_64;
#else
#if defined(__GNUC__)
#warning "Unsupported architecture; using slow portable variant"
#elif defined(_MSC_VER)
#pragma message("Unsupported architecture; using slow portable variant")
#endif
using tagged_pointer = tagged_pointer_portable;
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
