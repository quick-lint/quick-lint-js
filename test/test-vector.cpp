// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/linked-bump-allocator.h>
#include <quick-lint-js/vector.h>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_bump_vector, empty) {
  linked_bump_allocator<alignof(int)> alloc;
  bump_vector<int, decltype(alloc)> v("test", &alloc);
  EXPECT_TRUE(v.empty());
  EXPECT_EQ(v.size(), 0);
  EXPECT_EQ(v.capacity(), 0);
}

TEST(test_bump_vector, append_into_reserved_memory) {
  linked_bump_allocator<alignof(int)> alloc;
  bump_vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 0);

  v.emplace_back(100);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 1);
  EXPECT_THAT(v, ElementsAre(100));

  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_EQ(v.size(), 2);
  EXPECT_THAT(v, ElementsAre(100, 200));
}

TEST(test_bump_vector, append_into_new_memory) {
  linked_bump_allocator<alignof(int)> alloc;
  bump_vector<int, decltype(alloc)> v("test", &alloc);
  EXPECT_EQ(v.capacity(), 0);
  EXPECT_EQ(v.size(), 0);

  v.emplace_back(100);
  EXPECT_GT(v.capacity(), 0);
  EXPECT_EQ(v.size(), 1);
  EXPECT_THAT(v, ElementsAre(100));

  v.emplace_back(200);
  EXPECT_GT(v.capacity(), 0);
  EXPECT_EQ(v.size(), 2);
  EXPECT_THAT(v, ElementsAre(100, 200));
}

TEST(test_bump_vector, growing_allocation_in_place) {
  linked_bump_allocator<alignof(int)> alloc;
  bump_vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);

  v.emplace_back(100);
  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_THAT(v, ElementsAre(100, 200));

  v.emplace_back(300);
  EXPECT_GT(v.capacity(), 2);
  v.emplace_back(400);
  EXPECT_THAT(v, ElementsAre(100, 200, 300, 400));
}

TEST(test_bump_vector, growing_allocation_by_copy) {
  linked_bump_allocator<alignof(int)> alloc;
  bump_vector<int, decltype(alloc)> v("test", &alloc);
  v.reserve(2);

  v.emplace_back(100);
  v.emplace_back(200);
  EXPECT_EQ(v.capacity(), 2);
  EXPECT_THAT(v, ElementsAre(100, 200));
  std::uintptr_t old_v_data_pointer =
      reinterpret_cast<std::uintptr_t>(v.data());

  // Prevent allocation from growing in-place.
  int *middle_number = alloc.new_object<int>(42);

  v.emplace_back(300);
  EXPECT_GT(v.capacity(), 2);
  v.emplace_back(400);
  EXPECT_THAT(v, ElementsAre(100, 200, 300, 400));

  EXPECT_NE(old_v_data_pointer, reinterpret_cast<std::uintptr_t>(v.data()))
      << "growing vector should use new data pointer";
  EXPECT_EQ(*middle_number, 42)
      << "growing vector shouldn't change unrelated allocation";
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
