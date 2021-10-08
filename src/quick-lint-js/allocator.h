// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ALLOCATOR_H
#define QUICK_LINT_JS_ALLOCATOR_H

#include <boost/container/pmr/memory_resource.hpp>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <utility>

namespace quick_lint_js {
template <class T, class... Args>
T* new_object(boost::container::pmr::polymorphic_allocator<T>& allocator,
              Args&&... args) {
  T* result = allocator.allocate(1);
  result = new (result) T(std::forward<Args>(args)...);
  return result;
}

template <class T, class... Args>
T* new_object(boost::container::pmr::memory_resource* memory, Args&&... args) {
  boost::container::pmr::polymorphic_allocator<T> allocator(memory);
  return new_object(allocator, std::forward<Args>(args)...);
}

template <class T>
void delete_object(boost::container::pmr::polymorphic_allocator<T>& allocator,
                   T* object) {
  allocator.destroy(object);
  allocator.deallocate(object, sizeof(T));
}

template <class T>
void delete_object(boost::container::pmr::memory_resource* memory, T* object) {
  boost::container::pmr::polymorphic_allocator<T> allocator(memory);
  return delete_object(allocator, object);
}
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
