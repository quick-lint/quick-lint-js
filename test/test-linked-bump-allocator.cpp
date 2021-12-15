// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/linked-bump-allocator.h>
#include <quick-lint-js/warning.h>

namespace quick_lint_js {
namespace {
void assert_is_aligned(void*, unsigned alignment);

void assert_valid_memory(void* begin, void* end, unsigned alignment);
template <class T>
void assert_valid_memory(T*);

using test_linked_bump_allocator_with_type_types =
    ::testing::Types<char, int, void*>;
template <class T>
class test_linked_bump_allocator_with_type : public ::testing::Test {};
TYPED_TEST_SUITE(test_linked_bump_allocator_with_type,
                 test_linked_bump_allocator_with_type_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_linked_bump_allocator_with_type,
           separate_allocations_are_contiguous_without_padding) {
  using T = TypeParam;
  linked_bump_allocator<alignof(T)> alloc;
  std::intptr_t a =
      reinterpret_cast<std::intptr_t>(alloc.template new_object<T>());
  std::intptr_t b =
      reinterpret_cast<std::intptr_t>(alloc.template new_object<T>());
  std::intptr_t c =
      reinterpret_cast<std::intptr_t>(alloc.template new_object<T>());
  std::intptr_t d =
      reinterpret_cast<std::intptr_t>(alloc.template new_object<T>());

  std::ptrdiff_t delta = a - b;
  EXPECT_THAT(delta, ::testing::AnyOf(alignof(T), -alignof(T)))
      << "addresses should either go up or down with no padding";
  EXPECT_EQ(b - c, delta) << "delta should be consistent between allocations";
  EXPECT_EQ(c - d, delta) << "delta should be consistent between allocations";
}

TEST(test_linked_bump_allocator,
     less_aligned_object_keeps_next_allocation_aligned) {
  linked_bump_allocator<4> alloc;
  [[maybe_unused]] char* small = alloc.new_object<char>();
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(test_linked_bump_allocator,
     less_aligned_bytes_keeps_next_allocation_aligned) {
  linked_bump_allocator<4> alloc;
  [[maybe_unused]] void* small = alloc.allocate(1, /*align=*/1);
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(test_linked_bump_allocator, array_allocation_is_contiguous) {
  linked_bump_allocator<1> alloc;
  char* chars = alloc.allocate_uninitialized_array<char>(42);
  assert_valid_memory(chars, chars + 42, alignof(char));
}

TEST(test_linked_bump_allocator,
     less_aligned_array_keeps_next_allocation_aligned) {
  linked_bump_allocator<4> alloc;
  [[maybe_unused]] char* chars = alloc.allocate_uninitialized_array<char>(3);
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(test_linked_bump_allocator,
     less_aligned_pre_grown_and_grown_array_keeps_next_allocation_aligned) {
  linked_bump_allocator<4> alloc;

  char* chars = alloc.allocate_uninitialized_array<char>(3);
  bool grew = alloc.try_grow_array_in_place(chars, 3, 6);
  EXPECT_TRUE(grew);

  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(test_linked_bump_allocator,
     less_aligned_grown_array_keeps_next_allocation_aligned) {
  linked_bump_allocator<4> alloc;

  char* chars = alloc.allocate_uninitialized_array<char>(4);
  bool grew = alloc.try_grow_array_in_place(chars, 4, 7);
  EXPECT_TRUE(grew);

  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(test_linked_bump_allocator,
     allocate_bigger_than_remaining_space_in_chunk_allocates_in_new_chunk) {
  constexpr std::size_t chunk_size =
      4096 - sizeof(void*) * 2;  // Implementation detail.
  struct big_object {
    char c[chunk_size - 2];
  };

  linked_bump_allocator<alignof(std::uint32_t)> alloc;
  ASSERT_GE(chunk_size, sizeof(big_object))
      << "A big_object should fit in the chunk before allocating padding";
  [[maybe_unused]] void* padding = alloc.new_object<std::uint32_t>();

  ASSERT_LT(alloc.remaining_bytes_in_current_chunk(), sizeof(big_object))
      << "A big_object should not fit in the chunk after allocating padding";
  big_object* big = alloc.new_object<big_object>();

  assert_valid_memory(big);
}

TEST(test_linked_bump_allocator, filling_first_chunk_allocates_second_chunk) {
  linked_bump_allocator<1> alloc;

  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }

  [[maybe_unused]] char* new_chunk_object = alloc.new_object<char>();
  // TODO(strager): How do we verify that new_object is in its own chunk?
  assert_valid_memory(new_chunk_object);
}

TEST(test_linked_bump_allocator, rewinding_within_chunk_reuses_memory) {
  linked_bump_allocator<1> alloc;

  [[maybe_unused]] char* byte_0 = alloc.new_object<char>();
  [[maybe_unused]] char* byte_1 = alloc.new_object<char>();

  typename linked_bump_allocator<1>::rewind_state rewind =
      alloc.prepare_for_rewind();
  char* byte_2a = alloc.new_object<char>();
  char* byte_3a = alloc.new_object<char>();
  alloc.rewind(std::move(rewind));

  [[maybe_unused]] char* byte_2b = alloc.new_object<char>();
  EXPECT_EQ(byte_2b, byte_2a);
  assert_valid_memory(byte_2b);
  [[maybe_unused]] char* byte_3b = alloc.new_object<char>();
  EXPECT_EQ(byte_3b, byte_3a);
  assert_valid_memory(byte_3b);
}

TEST(test_linked_bump_allocator,
     rewinding_across_chunk_reuses_memory_of_first_chunk) {
  linked_bump_allocator<1> alloc;

  // First chunk:
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  typename linked_bump_allocator<1>::rewind_state rewind =
      alloc.prepare_for_rewind();
  std::vector<char*> reusable_allocations;
  for (std::size_t i = first_chunk_size / 2; i < first_chunk_size; ++i) {
    reusable_allocations.push_back(alloc.new_object<char>());
  }

  // Second chunk:
  std::size_t second_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < second_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }

  alloc.rewind(std::move(rewind));
  // First chunk:
  for (char* reusable_allocation : reusable_allocations) {
    char* new_allocation = alloc.new_object<char>();
    EXPECT_EQ(new_allocation, reusable_allocation);
    assert_valid_memory(new_allocation);
  }

  // Second chunk:
  char* second_chunk_byte = alloc.new_object<char>();
  assert_valid_memory(second_chunk_byte);
}

TEST(test_linked_bump_allocator,
     rewinding_across_chunk_uses_unallocated_memory_of_first_chunk) {
  linked_bump_allocator<1> alloc;

  // First chunk:
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  typename linked_bump_allocator<1>::rewind_state rewind =
      alloc.prepare_for_rewind();

  // Second chunk:
  [[maybe_unused]] char* big_allocation =
      alloc.allocate_uninitialized_array<char>(first_chunk_size + 64);

  // Third chunk:
  std::size_t third_chunk_size = first_chunk_size;
  for (std::size_t i = 0; i < third_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }

  alloc.rewind(std::move(rewind));
  // First chunk:
  char* new_allocation = alloc.new_object<char>();
  assert_valid_memory(new_allocation);
  // TODO(strager): How do we verify that new_allocation is in the same chunk as
  // the original allocations?
}

TEST(test_linked_bump_allocator, last_allocation_can_grow_in_place) {
  linked_bump_allocator<1> alloc;
  char* array = alloc.allocate_uninitialized_array<char>(10);
  bool ok = alloc.try_grow_array_in_place<char>(array, 10, 20);
  EXPECT_TRUE(ok);
  assert_valid_memory(array, array + 20, /*alignment=*/1);

  char* next = alloc.allocate_uninitialized_array<char>(1);
  EXPECT_THAT(reinterpret_cast<std::intptr_t>(next) -
                  reinterpret_cast<std::intptr_t>(array),
              ::testing::AnyOf(1, 20))
      << "future allocations should not overlap resized array";
}

TEST(test_linked_bump_allocator,
     last_allocation_cannot_grow_beyond_current_chunk) {
  linked_bump_allocator<1> alloc;

  [[maybe_unused]] char* first_byte =
      alloc.new_object<char>();  // Allocate the first chunk.
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size - 15; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  char* array = alloc.allocate_uninitialized_array<char>(10);
  EXPECT_EQ(alloc.remaining_bytes_in_current_chunk(), 5);

  bool ok = alloc.try_grow_array_in_place<char>(array, 10, 20);
  EXPECT_FALSE(ok);
  assert_valid_memory(array, array + 10, /*alignment=*/1);

  char* next = alloc.allocate_uninitialized_array<char>(1);
  EXPECT_THAT(reinterpret_cast<std::intptr_t>(next) -
                  reinterpret_cast<std::intptr_t>(array),
              ::testing::AnyOf(1, 10))
      << "future allocations should pretend realloc request never happened";
}

TEST(test_linked_bump_allocator, non_last_allocation_cannot_grow) {
  linked_bump_allocator<1> alloc;
  char* array = alloc.allocate_uninitialized_array<char>(10);
  [[maybe_unused]] char* last = alloc.new_object<char>();
  bool ok = alloc.try_grow_array_in_place<char>(array, 10, 20);
  EXPECT_FALSE(ok);
  assert_valid_memory(array, array + 10, /*alignment=*/1);

  char* next = alloc.allocate_uninitialized_array<char>(1);
  EXPECT_NE(next, last)
      << "future allocations should not overlap resized array";
}

void assert_is_aligned(void* p, unsigned alignment) {
  unsigned alignment_mask = alignment - 1;
  EXPECT_EQ(reinterpret_cast<std::uintptr_t>(p) & alignment_mask, 0)
      << std::hex << "pointer " << p << " should be aligned to " << alignment
      << " bytes";
}

void assert_valid_memory(void* begin, void* end, unsigned alignment) {
  assert_is_aligned(begin, alignment);
  assert_is_aligned(end, alignment);

  std::uintptr_t begin_i = reinterpret_cast<std::uintptr_t>(begin);
  std::uintptr_t end_i = reinterpret_cast<std::uintptr_t>(end);
  ASSERT_LE(begin_i, end_i);

  // Try to get the OS or malloc to detect heap corruption by writing over all
  // of the given memory.
  std::memset(begin, 'X', end_i - begin_i);
}

template <class T>
void assert_valid_memory(T* object) {
  assert_valid_memory(object, object + 1, alignof(T));
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
