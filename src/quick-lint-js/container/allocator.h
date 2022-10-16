// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_ALLOCATOR_H
#define QUICK_LINT_JS_CONTAINER_ALLOCATOR_H

#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/warning.h>
#include <utility>

namespace quick_lint_js {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")
template <class T, class... Args>
T* new_object(memory_resource* memory, Args&&... args) {
  T* result = reinterpret_cast<T*>(memory->allocate(sizeof(T), alignof(T)));
  result = new (result) T(std::forward<Args>(args)...);
  return result;
}
QLJS_WARNING_POP

template <class T>
void delete_object(memory_resource* memory, T* object) {
  object->~T();
  memory->deallocate(object, sizeof(T), alignof(T));
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
