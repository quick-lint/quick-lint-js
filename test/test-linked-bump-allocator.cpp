// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>

namespace quick_lint_js {
namespace {
void assert_is_aligned(void*, unsigned alignment);

void assert_valid_memory(void* begin, void* end, unsigned alignment);
template <class T>
void assert_valid_memory(T*);

using Test_Linked_Bump_Allocator_With_Type_Types =
    ::testing::Types<char, int, void*>;
template <class T>
class Test_Linked_Bump_Allocator_With_Type : public ::testing::Test {};
TYPED_TEST_SUITE(Test_Linked_Bump_Allocator_With_Type,
                 Test_Linked_Bump_Allocator_With_Type_Types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(Test_Linked_Bump_Allocator_With_Type,
           separate_allocations_are_contiguous_without_padding) {
  using T = TypeParam;
  Linked_Bump_Allocator<alignof(T)> alloc("test");
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

TEST(Test_Linked_Bump_Allocator,
     less_aligned_object_keeps_next_allocation_aligned) {
  Linked_Bump_Allocator<4> alloc("test");
  [[maybe_unused]] char* small = alloc.new_object<char>();
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(Test_Linked_Bump_Allocator,
     less_aligned_bytes_keeps_next_allocation_aligned) {
  Linked_Bump_Allocator<4> alloc("test");
  [[maybe_unused]] void* small = alloc.allocate(1, /*align=*/1);
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(Test_Linked_Bump_Allocator, array_allocation_is_contiguous) {
  Linked_Bump_Allocator<1> alloc("test");
  Span<char> chars = alloc.allocate_uninitialized_span<char>(42);
  assert_valid_memory(chars.data(), chars.data() + 42, alignof(char));
}

TEST(Test_Linked_Bump_Allocator,
     less_aligned_array_keeps_next_allocation_aligned) {
  Linked_Bump_Allocator<4> alloc("test");
  [[maybe_unused]] Span<char> chars =
      alloc.allocate_uninitialized_span<char>(3);
  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(Test_Linked_Bump_Allocator,
     less_aligned_pre_grown_and_grown_array_keeps_next_allocation_aligned) {
  Linked_Bump_Allocator<4> alloc("test");

  Span<char> chars = alloc.allocate_uninitialized_span<char>(3);
  bool grew = alloc.try_grow_array_in_place(chars.data(), 3, 6);
  EXPECT_TRUE(grew);

  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(Test_Linked_Bump_Allocator,
     less_aligned_grown_array_keeps_next_allocation_aligned) {
  Linked_Bump_Allocator<4> alloc("test");

  Span<char> chars = alloc.allocate_uninitialized_span<char>(4);
  bool grew = alloc.try_grow_array_in_place(chars.data(), 4, 7);
  EXPECT_TRUE(grew);

  std::uint32_t* after = alloc.new_object<std::uint32_t>();
  assert_valid_memory(after);
}

TEST(Test_Linked_Bump_Allocator,
     allocate_bigger_than_remaining_space_in_chunk_allocates_in_new_chunk) {
  constexpr std::size_t chunk_size =
      4096 - sizeof(void*) * 2;  // Implementation detail.
  struct Big_Object {
    char c[chunk_size - 2];
  };

  Linked_Bump_Allocator<alignof(std::uint32_t)> alloc("test");
  ASSERT_GE(chunk_size, sizeof(Big_Object))
      << "A big_object should fit in the chunk before allocating padding";
  [[maybe_unused]] void* padding = alloc.new_object<std::uint32_t>();

  ASSERT_LT(alloc.remaining_bytes_in_current_chunk(), sizeof(Big_Object))
      << "A big_object should not fit in the chunk after allocating padding";
  Big_Object* big = alloc.new_object<Big_Object>();

  assert_valid_memory(big);
}

TEST(Test_Linked_Bump_Allocator, filling_first_chunk_allocates_second_chunk) {
  Linked_Bump_Allocator<1> alloc("test");

  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }

  [[maybe_unused]] char* new_chunk_object = alloc.new_object<char>();
  // TODO(strager): How do we verify that new_chunk_object is in its own chunk?
  assert_valid_memory(new_chunk_object);
}

TEST(Test_Linked_Bump_Allocator, rewinding_within_chunk_reuses_memory) {
  Linked_Bump_Allocator<1> alloc("test");

  [[maybe_unused]] char* byte_0 = alloc.new_object<char>();
  [[maybe_unused]] char* byte_1 = alloc.new_object<char>();

  typename Linked_Bump_Allocator<1>::Rewind_State rewind =
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

TEST(Test_Linked_Bump_Allocator,
     rewinding_across_chunk_reuses_memory_of_first_chunk) {
  Linked_Bump_Allocator<1> alloc("test");

  // First chunk:
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  typename Linked_Bump_Allocator<1>::Rewind_State rewind =
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

TEST(Test_Linked_Bump_Allocator,
     rewinding_across_chunk_uses_unallocated_memory_of_first_chunk) {
  Linked_Bump_Allocator<1> alloc("test");

  // First chunk:
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size / 2; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  typename Linked_Bump_Allocator<1>::Rewind_State rewind =
      alloc.prepare_for_rewind();

  // Second chunk:
  [[maybe_unused]] Span<char> big_allocation =
      alloc.allocate_uninitialized_span<char>(first_chunk_size + 64);

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

TEST(Test_Linked_Bump_Allocator, last_allocation_can_grow_in_place) {
  Linked_Bump_Allocator<1> alloc("test");
  Span<char> array = alloc.allocate_uninitialized_span<char>(10);
  bool ok = alloc.try_grow_array_in_place<char>(array.data(), 10, 20);
  EXPECT_TRUE(ok);
  assert_valid_memory(array.data(), array.data() + 20, /*alignment=*/1);

  Span<char> next = alloc.allocate_uninitialized_span<char>(1);
  EXPECT_THAT(reinterpret_cast<std::intptr_t>(next.data()) -
                  reinterpret_cast<std::intptr_t>(array.data()),
              ::testing::AnyOf(1, 20))
      << "future allocations should not overlap resized array";
}

TEST(Test_Linked_Bump_Allocator,
     last_allocation_cannot_grow_beyond_current_chunk) {
  Linked_Bump_Allocator<1> alloc("test");

  [[maybe_unused]] char* first_byte =
      alloc.new_object<char>();  // Allocate the first chunk.
  std::size_t first_chunk_size = alloc.remaining_bytes_in_current_chunk();
  for (std::size_t i = 0; i < first_chunk_size - 15; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }
  Span<char> array = alloc.allocate_uninitialized_span<char>(10);
  EXPECT_EQ(alloc.remaining_bytes_in_current_chunk(), 5);

  bool ok = alloc.try_grow_array_in_place<char>(array.data(), 10, 20);
  EXPECT_FALSE(ok);
  assert_valid_memory(array.data(), array.data() + 10, /*alignment=*/1);

  Span<char> next = alloc.allocate_uninitialized_span<char>(1);
  EXPECT_THAT(reinterpret_cast<std::intptr_t>(next.data()) -
                  reinterpret_cast<std::intptr_t>(array.data()),
              ::testing::AnyOf(1, 10))
      << "future allocations should pretend realloc request never happened";
}

TEST(Test_Linked_Bump_Allocator, non_last_allocation_cannot_grow) {
  Linked_Bump_Allocator<1> alloc("test");
  Span<char> array = alloc.allocate_uninitialized_span<char>(10);
  [[maybe_unused]] char* last = alloc.new_object<char>();
  bool ok = alloc.try_grow_array_in_place<char>(array.data(), 10, 20);
  EXPECT_FALSE(ok);
  assert_valid_memory(array.data(), array.data() + 10, /*alignment=*/1);

  Span<char> next = alloc.allocate_uninitialized_span<char>(1);
  EXPECT_NE(next.data(), last)
      << "future allocations should not overlap resized array";
}

#if QLJS_DEBUG_BUMP_ALLOCATOR && \
    (defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST)
TEST(Test_Linked_Bump_Allocator, cannot_allocate_when_disabled) {
  auto check = [] {
    linked_bump_allocator<1> alloc("test");
    auto disable_guard = alloc.disable();
    // The following line should crash:
    [[maybe_unused]] char* c = alloc.new_object<char>();
  };
  EXPECT_DEATH(check(), "disabled")
      << "allocating should crash if allocation is disabled";
}
#endif

#if QLJS_DEBUG_BUMP_ALLOCATOR
TEST(Test_Linked_Bump_Allocator, can_allocate_after_disabling_then_reenabling) {
  linked_bump_allocator<1> alloc("test");
  {
    auto disable_guard = alloc.disable();
    // Destruct disable_guard, re-enabling allocation.
  }
  char* c = alloc.new_object<char>();
  assert_valid_memory(c);
}
#endif

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
