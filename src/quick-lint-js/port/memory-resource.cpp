// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/memory-resource.h>

namespace quick_lint_js {
namespace {
class new_delete_resource_impl : public memory_resource {
 public:
  void* do_allocate(std::size_t bytes, std::size_t alignment) override {
#if QLJS_HAVE_SIZED_ALIGNED_NEW
    return ::operator new (bytes, std::align_val_t{alignment});
#else
    QLJS_ASSERT(alignment <= __STDCPP_DEFAULT_NEW_ALIGNMENT__);
    return ::operator new(bytes);
#endif
  }

  void do_deallocate(void* p, std::size_t bytes,
                     std::size_t alignment) override {
#if QLJS_HAVE_SIZED_ALIGNED_DELETE
    ::operator delete (p, bytes, std::align_val_t{alignment});
#else
    QLJS_ASSERT(alignment <= __STDCPP_DEFAULT_NEW_ALIGNMENT__);
    static_cast<void>(bytes);
    ::operator delete(p);
#endif
  }

  bool do_is_equal(const memory_resource& other) const noexcept override {
    return this == static_cast<const new_delete_resource_impl*>(&other);
  }
};
}

memory_resource* new_delete_resource() noexcept {
  static new_delete_resource_impl instance;
  return &instance;
}
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
