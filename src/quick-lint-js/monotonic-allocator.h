// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_MONOTONIC_ALLOCATOR_H
#define QUICK_LINT_JS_MONOTONIC_ALLOCATOR_H

#include <boost/container/pmr/monotonic_buffer_resource.hpp>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <utility>

namespace quick_lint_js {
class monotonic_allocator {
 public:
  template <class T, class... Args>
  [[nodiscard]] T *new_object(Args &&...args) {
    T *result = this->standard_allocator<T>().allocate(1);
    result = new (result) T(std::forward<Args>(args)...);
    return result;
  }

  template <class T>
  [[nodiscard]] T *allocate_uninitialized_array(std::size_t count) {
    return this->standard_allocator<T>().allocate(count);
  }

  template <class T>
  boost::container::pmr::polymorphic_allocator<T> standard_allocator() {
    return boost::container::pmr::polymorphic_allocator<T>(&this->memory_);
  }

  boost::container::pmr::monotonic_buffer_resource *memory_resource() noexcept {
    return &this->memory_;
  }

 private:
  boost::container::pmr::monotonic_buffer_resource memory_;
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
