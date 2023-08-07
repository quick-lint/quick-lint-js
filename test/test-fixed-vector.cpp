// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wunused-member-function")

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Fixed_Vector, new_vector_is_empty) {
  Fixed_Vector<int, 4> v;
  EXPECT_TRUE(v.empty());
  EXPECT_EQ(v.size(), 0);
  EXPECT_THAT(v, IsEmpty());
}

TEST(Test_Fixed_Vector, push_items_until_full) {
  Fixed_Vector<int, 4> v;

  v.push_back(100);
  EXPECT_THAT(v, ElementsAreArray({100}));

  v.push_back(200);
  EXPECT_THAT(v, ElementsAreArray({100, 200}));

  v.push_back(300);
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300}));

  v.push_back(400);
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300, 400}));
}

TEST(Test_Fixed_Vector, copy_construct) {
  Fixed_Vector<int, 4> v;
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);

  Fixed_Vector<int, 4> v2(v);
  EXPECT_THAT(v2, ElementsAreArray({100, 200, 300}));
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300}))
      << "original vector should not be modified";
}

TEST(Test_Fixed_Vector, copy_assign_shrink) {
  Fixed_Vector<int, 4> v;
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);

  Fixed_Vector<int, 4> v2;
  v2.push_back(10);
  v2.push_back(20);

  v2 = v;
  EXPECT_THAT(v2, ElementsAreArray({100, 200, 300}));
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300}))
      << "source vector should not be modified";
}

TEST(Test_Fixed_Vector, copy_assign_grow) {
  Fixed_Vector<int, 4> v;
  v.push_back(100);
  v.push_back(200);

  Fixed_Vector<int, 4> v2;
  v2.push_back(10);
  v2.push_back(20);
  v2.push_back(30);

  v2 = v;
  EXPECT_THAT(v2, ElementsAreArray({100, 200}));
  EXPECT_THAT(v, ElementsAreArray({100, 200}))
      << "source vector should not be modified";
}

TEST(Test_Fixed_Vector, default_construct_with_non_default_constructible_item) {
  struct Non_Default_Constructible {
    Non_Default_Constructible() = delete;
    explicit Non_Default_Constructible(int value) : value(value) {}
    int value;
  };
  Fixed_Vector<Non_Default_Constructible, 4> v;
}

TEST(Test_Fixed_Vector, item_type_destructor_is_called_during_destruction) {
  static int destruct_count;
  destruct_count = 0;
  struct Destructor_Spy {
    ~Destructor_Spy() { destruct_count += 1; }
  };

  {
    Fixed_Vector<Destructor_Spy, 4> v;
    v.emplace_back();
    v.emplace_back();
    EXPECT_EQ(destruct_count, 0);
  }
  EXPECT_EQ(destruct_count, 2);
}

TEST(
    Test_Fixed_Vector,
    fixed_vector_is_trivially_destructible_if_item_type_is_trivially_destructible) {
  struct Item {};
  static_assert(std::is_trivially_destructible_v<Item>);

  using Vector_Of_Items = Fixed_Vector<Item, 4>;
  EXPECT_TRUE(std::is_trivially_destructible_v<Vector_Of_Items>);
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
