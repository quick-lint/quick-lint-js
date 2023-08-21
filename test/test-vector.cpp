// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/feature.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Bump_Vector, empty) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  EXPECT_TRUE(v.empty());
  EXPECT_EQ(v.size(), 0);
  EXPECT_EQ(v.capacity(), 0);
}

TEST(Test_Bump_Vector, append_into_reserved_memory) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 0);

  v.emplace_back(100);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 1);
  EXPECT_THAT(v, ElementsAreArray({100}));

  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 2);
  EXPECT_THAT(v, ElementsAreArray({100, 200}));
}

TEST(Test_Bump_Vector, reserve_0_does_nothing) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");

  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(0);
  EXPECT_EQ(v.capacity(), 0);
  EXPECT_EQ(v.size(), 0);

  v.push_back(100);
  v.push_back(200);
  v.push_back(300);
  Bump_Vector_Size old_capacity = v.capacity();
  v.reserve(0);
  EXPECT_EQ(v.capacity(), old_capacity);
  EXPECT_EQ(v.size(), 3);
}

TEST(Test_Bump_Vector, append_into_new_memory) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  EXPECT_EQ(v.capacity(), 0);
  EXPECT_EQ(v.size(), 0);

  v.emplace_back(100);
  EXPECT_GT(v.capacity(), 0);
  EXPECT_EQ(v.size(), 1);
  EXPECT_THAT(v, ElementsAreArray({100}));

  v.emplace_back(200);
  EXPECT_GT(v.capacity(), 0);
  EXPECT_EQ(v.size(), 2);
  EXPECT_THAT(v, ElementsAreArray({100, 200}));
}

TEST(Test_Bump_Vector, growing_allocation_in_place) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);

  v.emplace_back(100);
  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_THAT(v, ElementsAreArray({100, 200}));

  v.emplace_back(300);
  EXPECT_GT(v.capacity(), 2);
  v.emplace_back(400);
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300, 400}));
}

TEST(Test_Bump_Vector, growing_allocation_by_copy) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);

  v.emplace_back(100);
  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_THAT(v, ElementsAreArray({100, 200}));
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());

  // Prevent allocation from growing in-place.
  int *middle_number = alloc.new_object<int>(42);

  v.emplace_back(300);
  EXPECT_GT(v.capacity(), 2);
  v.emplace_back(400);
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300, 400}));

  EXPECT_NE(old_v_data_pointer, reinterpret_cast<std::uintptr_t>(v.data()))
      << "growing vector should use new data pointer";
  EXPECT_EQ(*middle_number, 42)
      << "growing vector shouldn't change unrelated allocation";
}

TEST(Test_Bump_Vector, resize_allows_same_size) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());
  Bump_Vector_Size old_capacity = v.capacity();

  v.resize(2);

  EXPECT_EQ(v.size(), 2) << "resizing vector should not change size";
  EXPECT_EQ(v.capacity(), old_capacity)
      << "resizing vector should not change capacity";
  EXPECT_THAT(v, ElementsAreArray({100, 200}));
  EXPECT_EQ(old_v_data_pointer, reinterpret_cast<std::uintptr_t>(v.data()))
      << "resizing vector should not change data pointer";
}

TEST(Test_Bump_Vector, resize_allows_shrinking) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);
  v.emplace_back(300);
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());
  Bump_Vector_Size old_capacity = v.capacity();

  v.resize(2);

  EXPECT_EQ(v.size(), 2) << "shrinking vector should change size";
  EXPECT_EQ(v.capacity(), old_capacity)
      << "shrinking vector should not change capacity";
  EXPECT_THAT(v, ElementsAreArray({100, 200}))
      << "shrinking vector should preserve some elements";
  EXPECT_EQ(old_v_data_pointer, reinterpret_cast<std::uintptr_t>(v.data()))
      << "shrinking vector should not change data pointer";
}

TEST(Test_Bump_Vector, resize_allows_growing_within_capacity) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());
  Bump_Vector_Size old_capacity = v.capacity();

  ASSERT_GE(old_capacity, 3);
  v.resize(3);

  EXPECT_EQ(v.size(), 3) << "growing vector should change size";
  EXPECT_EQ(v.capacity(), old_capacity)
      << "growing vector should not change capacity";
  EXPECT_THAT(v, ElementsAreArray({100, 200, 0}))
      << "growing vector should default-construct new elements";
  EXPECT_EQ(old_v_data_pointer, reinterpret_cast<std::uintptr_t>(v.data()))
      << "growing vector within capacity should not change data pointer";
}

TEST(Test_Bump_Vector, resize_allows_growing_outside_capacity) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);

  ASSERT_LT(v.capacity(), 10);
  v.resize(10);

  EXPECT_EQ(v.size(), 10) << "growing vector should change size";
  EXPECT_EQ(v.capacity(), 10) << "growing vector should change capacity";
  EXPECT_THAT(v, ElementsAreArray({100, 200, 0, 0, 0,  //
                                   0, 0, 0, 0, 0}))
      << "growing vector should default-construct new elements";
}

TEST(Test_Bump_Vector, pop_back_shrinks_vector) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);
  v.pop_back();

  EXPECT_THAT(v, ElementsAreArray({100, 200}));
  EXPECT_GE(v.capacity(), 3);
}

TEST(Test_Bump_Vector, pop_back_then_push_back_reuses_memory) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);
  v.pop_back();
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());
  v.push_back(400);
  std::uintptr_t v_data_pointer = reinterpret_cast<std::uintptr_t>(v.data());

  EXPECT_THAT(v, ElementsAreArray({100, 200, 400}));
  EXPECT_EQ(v_data_pointer, old_v_data_pointer);
  EXPECT_GE(v.capacity(), 3);
}

TEST(Test_Bump_Vector, move_constructing_clears_old_vector) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);

  Bump_Vector<int, decltype(alloc)> v2(std::move(v));
  EXPECT_THAT(v, IsEmpty());
}

TEST(Test_Bump_Vector, move_constructor_preserves_pointers) {
  Linked_Bump_Allocator<alignof(int)> alloc("test");
  Bump_Vector<int, decltype(alloc)> v("test", &alloc);
  v.emplace_back(100);
  v.emplace_back(200);

  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());
  Bump_Vector_Size old_v_capacity = v.capacity();
  Bump_Vector_Size old_v_size = v.size();

  Bump_Vector<int, decltype(alloc)> v2(std::move(v));

  EXPECT_EQ(reinterpret_cast<std::uintptr_t>(v2.data()), old_v_data_pointer);
  EXPECT_EQ(v2.capacity(), old_v_capacity);
  EXPECT_EQ(v2.size(), old_v_size);
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
