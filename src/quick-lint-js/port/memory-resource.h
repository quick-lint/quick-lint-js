// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>

namespace quick_lint_js {
// Like std::pmr::memory_resource.
class Memory_Resource {
 public:
  virtual ~Memory_Resource() = default;

  void* allocate(std::size_t bytes, std::size_t alignment) {
    return this->do_allocate(bytes, alignment);
  }

  void deallocate(void* p, std::size_t bytes, std::size_t alignment) {
    return this->do_deallocate(p, bytes, alignment);
  }

 protected:
  virtual void* do_allocate(std::size_t bytes, std::size_t alignment) = 0;
  virtual void do_deallocate(void* p, std::size_t bytes,
                             std::size_t alignment) = 0;
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
