// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRACKING_MEMORY_RESOURCE_H
#define QUICK_LINT_JS_TRACKING_MEMORY_RESOURCE_H

#include <boost/container/pmr/global_resource.hpp>
#include <boost/container/pmr/memory_resource.hpp>
#include <cstdint>
#include <quick-lint-js/assert.h>

namespace quick_lint_js {
class tracking_memory_resource
    : public ::boost::container::pmr::memory_resource {
 public:
  std::uint64_t alive_bytes() const noexcept {
    return this->allocated_bytes_ - this->deallocated_bytes_;
  }

  std::uint64_t allocated_bytes() const noexcept {
    return this->allocated_bytes_;
  }

  std::uint64_t deallocated_bytes() const noexcept {
    return this->deallocated_bytes_;
  }

 protected:
  void* do_allocate(std::size_t bytes, std::size_t align) override {
    void* p = this->underlying_memory_->allocate(bytes, align);
    if (p) {
      this->allocated_bytes_ += bytes;
    }
    return p;
  }

  void do_deallocate(void* p, std::size_t bytes, std::size_t align) override {
    this->underlying_memory_->deallocate(p, bytes, align);
    if (p) {
      this->deallocated_bytes_ += bytes;
    }
  }

  bool do_is_equal(const boost::container::pmr::memory_resource&) const
      noexcept override {
    QLJS_UNIMPLEMENTED();
    return false;
  }

  ::boost::container::pmr::memory_resource* underlying_memory_ =
      ::boost::container::pmr::new_delete_resource();

  std::uint64_t allocated_bytes_ = 0;
  std::uint64_t deallocated_bytes_ = 0;
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
