// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/flexible-array.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/util/pointer.h>

namespace quick_lint_js {
namespace {
// Wraps any allocator with support for any arbitrary alignment.
//
// Designed for testing only.
//
// Each allocation has the following form:
//
//     [padding][header][data]
//     ^~~~~~~~~'       ^
//
// * The returned pointer points to the beginning of data. Padding ensures it is
//   suitably aligned.
// * The header contains a pointer pointing to the beginning of the underlying
//   allocation. This is used to free the memory.
struct Aligning_Memory_Resource : public Memory_Resource {
 public:
  explicit Aligning_Memory_Resource(Memory_Resource *underlying_memory)
      : underlying_memory_(underlying_memory) {}

  void *do_allocate(std::size_t bytes, std::size_t alignment) override {
    std::size_t underlying_size = this->underlying_size(bytes, alignment);
    void *underlying_allocation =
        this->underlying_memory_->allocate(underlying_size, 1);

    void *result = &reinterpret_cast<Header *>(underlying_allocation)[1];
    std::size_t space_after_result = underlying_size;
    bool ok =
        std::align(alignment, bytes, result, space_after_result) != nullptr;
    QLJS_ALWAYS_ASSERT(ok);

    // Write the header. memcpy must be used because the header might not be
    // aligned.
    Header header = {.underlying_allocation = underlying_allocation};
    std::memcpy(&reinterpret_cast<Header *>(result)[-1], &header,
                sizeof(Header));

    return result;
  }

  void do_deallocate(void *p, std::size_t bytes,
                     std::size_t alignment) override {
    // Read the header. memcpy must be used because the header might not be
    // aligned.
    Header header;
    std::memcpy(&header, &reinterpret_cast<Header *>(p)[-1], sizeof(Header));
    this->underlying_memory_->deallocate(
        header.underlying_allocation, this->underlying_size(bytes, alignment),
        1);
  }

 private:
  struct Header {
    void *underlying_allocation;
  };

  std::size_t underlying_size(std::size_t bytes, std::size_t alignment) {
    return bytes + alignment + sizeof(void *);
  }

  Memory_Resource *underlying_memory_;
};

Aligning_Memory_Resource memory =
    Aligning_Memory_Resource(new_delete_resource());

TEST(Test_Flexible_Array, allocating_header_creates_array) {
  struct My_Array_Header {};
  using My_Array = Flexible_Array<int, My_Array_Header>;
  My_Array *a = My_Array::allocate_and_construct_header(&memory, 10);

  int *begin = a->flexible_capacity_begin();
  int *end = a->flexible_capacity_end();
  ASSERT_EQ(end - begin, 10);
  ASSERT_EQ(a->flexible_capacity(), 10);

  // Ensure the array elements are writable. This is intended to cause an ASAN
  // or UBSAN report if Flexible_Array is buggy.
  for (int i = 0; i < 10; ++i) {
    begin[i] = i * 11111111;
  }
  for (int i = 0; i < 10; ++i) {
    ASSERT_EQ(begin[i], i * 11111111) << i;
  }

  My_Array::destroy_header_and_deallocate(&memory, a);
}

TEST(Test_Flexible_Array, allocating_header_over_aligned_by_type) {
  struct alignas(128) My_Type {};
  struct My_Array_Header {};
  using My_Array = Flexible_Array<My_Type, My_Array_Header>;
  My_Array *a = My_Array::allocate_and_construct_header(&memory, 10);

  My_Type *begin = a->flexible_capacity_begin();
  ASSERT_TRUE(is_aligned(begin, 128)) << begin;

  My_Array::destroy_header_and_deallocate(&memory, a);
}
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
