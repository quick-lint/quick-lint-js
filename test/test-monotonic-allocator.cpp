// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

#if QLJS_HAVE_X86_SSE2
#include <emmintrin.h>
#endif

// TODO(strager): Support over-aligned types in monotonic_allocator.
#if 0
#define QLJS_HAVE_OVERALIGNED 1
#else
#define QLJS_HAVE_OVERALIGNED 0
#endif

#if defined(_MSC_VER)
// TODO(strager): For some reason, allocating a __m128 crashes with
// std::bad_alloc.
#elif QLJS_HAVE_X86_SSE2
#define QLJS_CAN_ALIGN_SSE 1
#else
#define QLJS_CAN_ALIGN_SSE 0
#endif

namespace quick_lint_js {
namespace {
void assert_is_aligned(void*, unsigned alignment);

void assert_valid_memory(void* begin, void* end, unsigned alignment);

template <class T>
void assert_valid_memory(T*);

TEST(test_monotonic_allocator,
     separate_char_allocations_are_contiguous_without_padding) {
  monotonic_allocator alloc;
  std::intptr_t a = reinterpret_cast<std::intptr_t>(alloc.new_object<char>());
  std::intptr_t b = reinterpret_cast<std::intptr_t>(alloc.new_object<char>());
  std::intptr_t c = reinterpret_cast<std::intptr_t>(alloc.new_object<char>());
  std::intptr_t d = reinterpret_cast<std::intptr_t>(alloc.new_object<char>());

  std::ptrdiff_t delta = a - b;
  EXPECT_THAT(delta, ::testing::AnyOf(1, -1))
      << "Addresses should either go up (0xf00a, 0xf00b, 0xf00c) or down "
         "(0xf00a, 0xf009, 0xf008).";
  EXPECT_EQ(b - c, delta);
  EXPECT_EQ(c - d, delta);
}

TEST(test_monotonic_allocator, array_allocation_is_contiguous) {
  monotonic_allocator alloc;
  char* chars = alloc.allocate_uninitialized_array<char>(42);
  assert_valid_memory(chars, chars + 42, alignof(char));
}

TEST(test_monotonic_allocator,
     allocate_bigger_than_remaining_space_in_chunk_allocates_in_new_chunk) {
  struct big_object {
    char c[32 * sizeof(void*)];
  };

  monotonic_allocator alloc;
  ASSERT_GE(alloc.memory_resource()->initial_next_buffer_size,
            sizeof(big_object))
      << "A big_object should fit in the chunk before allocating padding";
  [[maybe_unused]] void* padding = alloc.new_object<int>();

  ASSERT_LT(alloc.memory_resource()->remaining_storage(alignof(big_object)),
            sizeof(big_object))
      << "A big_object should not fit in the chunk after allocating padding";
  big_object* big = alloc.new_object<big_object>();

  assert_valid_memory(big);
}

TEST(test_monotonic_allocator, filling_first_chunk_allocates_second_chunk) {
  monotonic_allocator alloc;

  std::size_t first_chunk_size =
      alloc.memory_resource()->initial_next_buffer_size;
  for (std::size_t i = 0; i < first_chunk_size; ++i) {
    [[maybe_unused]] char* byte = alloc.new_object<char>();
  }

  [[maybe_unused]] char* new_chunk_object = alloc.new_object<char>();
  // TODO(strager): How do we verify that new_object is in its own chunk?
  assert_valid_memory(new_chunk_object);
}

#if QLJS_HAVE_OVERALIGNED
struct alignas(std::max_align_t) overaligned_64 {};
#endif

#if QLJS_CAN_ALIGN_SSE
struct sse_aligned {
  __m128 m128;
  __m128i m128i;
};
#endif

using test_monotonic_allocator_typed_types = ::testing::Types<
#if QLJS_HAVE_OVERALIGNED
    overaligned_64,
#endif
#if QLJS_CAN_ALIGN_SSE
    sse_aligned,
#endif
    char, short, int, long, long long,  //
    float, double, long double, std::max_align_t>;
template <class T>
class test_monotonic_allocator_typed : public ::testing::Test {};
TYPED_TEST_SUITE(test_monotonic_allocator_typed,
                 test_monotonic_allocator_typed_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_monotonic_allocator_typed,
           type_alignment_is_respected_for_initial_allocation) {
  monotonic_allocator alloc;
  TypeParam* p = alloc.new_object<TypeParam>();
  assert_is_aligned(p, alignof(TypeParam));
}

TYPED_TEST(test_monotonic_allocator_typed,
           type_alignment_is_respected_for_second_allocation) {
  monotonic_allocator alloc;
  [[maybe_unused]] char* padding = alloc.new_object<char>();
  TypeParam* p = alloc.new_object<TypeParam>();
  assert_is_aligned(p, alignof(TypeParam));
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
