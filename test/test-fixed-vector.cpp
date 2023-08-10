// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")
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

TEST(Test_Fixed_Vector, pop_items_from_full_to_empty) {
  Fixed_Vector<int, 4> v;
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);
  v.push_back(400);

  v.pop_back();
  EXPECT_THAT(v, ElementsAreArray({100, 200, 300}));
  v.pop_back();
  EXPECT_THAT(v, ElementsAreArray({100, 200}));
  v.pop_back();
  EXPECT_THAT(v, ElementsAreArray({100}));
  v.pop_back();
  EXPECT_THAT(v, IsEmpty());
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

TEST(Test_Fixed_Vector, item_type_destructor_is_called_by_pop_back) {
  static int destruct_count;
  destruct_count = 0;
  struct Destructor_Spy {
    ~Destructor_Spy() { destruct_count += 1; }
  };

  Fixed_Vector<Destructor_Spy, 4> v;
  v.emplace_back();
  v.emplace_back();
  EXPECT_EQ(destruct_count, 0);
  v.pop_back();
  EXPECT_EQ(destruct_count, 1);
  v.pop_back();
  EXPECT_EQ(destruct_count, 2);
}

TEST(Test_Fixed_Vector, resize_grow_calls_default_constructor) {
  static int default_construct_count;
  default_construct_count = 0;
  struct Test_Spy {
    explicit Test_Spy() : value(1234) { default_construct_count += 1; }
    explicit Test_Spy(int v) : value(v) {}

    int value;
  };

  Fixed_Vector<Test_Spy, 4> v;
  v.emplace_back(100);
  v.resize(3);
  EXPECT_EQ(v[1].value, 1234);
  EXPECT_EQ(v[2].value, 1234);
  EXPECT_EQ(default_construct_count, 2)
      << "second and third elements should be default-constructed";
}

TEST(Test_Fixed_Vector, resize_grow_zero_initializes) {
  Fixed_Vector<int, 4> v;
  v.emplace_back(100);
  v.pop_back();
  // v should internally have {100, (undefined), (undefined), (undefined)}.
  v.resize(3);
  EXPECT_EQ(v[0], 0) << "v[0] should be modified by resize";
}

TEST(Test_Fixed_Vector, resize_shrink_removes_last_items) {
  Fixed_Vector<int, 4> v;
  v.push_back(100);
  v.push_back(200);
  v.push_back(300);
  v.resize(1);
  EXPECT_THAT(v, ElementsAreArray({100}));
}

TEST(Test_Fixed_Vector, resize_shrink_calls_destructor) {
  static int destruct_count;
  destruct_count = 0;
  struct Destructor_Spy {
    ~Destructor_Spy() { destruct_count += 1; }
  };

  Fixed_Vector<Destructor_Spy, 4> v;
  v.emplace_back();
  v.emplace_back();
  v.emplace_back();
  v.resize(1);
  EXPECT_EQ(destruct_count, 2)
      << "second and third elements should be destructed";
}

TEST(
    Test_Fixed_Vector,
    fixed_vector_is_trivially_destructible_if_item_type_is_trivially_destructible) {
  struct Item {};
  static_assert(std::is_trivially_destructible_v<Item>);

  using Vector_Of_Items = Fixed_Vector<Item, 4>;
  EXPECT_TRUE(std::is_trivially_destructible_v<Vector_Of_Items>);
}

#if defined(__has_feature)
#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST && \
    __has_feature(address_sanitizer)
TEST(Test_Fixed_Vector,
     accessing_item_after_removed_is_undefined_behavior_SLOW) {
  // These programs should fail with tools like Address Sanitizer, but will
  // appear to work fine without Address Sanitizer.

  // HACK(strager): Fixed_Vector only poisons if the element is not trivially
  // destructible. (See NOTE[Fixed_Vector-poison].) Test with an appropriate
  // type.
  // TODO(strager): Figure out how to make Fixed_Vector poison always. I think
  // this is only feasible if we make Fixed_Vector's destructor non-trivial.
  using Type = std::string;

  auto check_clear = [] {
    Fixed_Vector<Type, 4> v;
    Type& item = v.push_back("hello");
    v.clear();
    item = "hi";  // Should crash.
  };
  EXPECT_DEATH(check_clear(), "use-after-poison");

  auto check_pop_back = [] {
    Fixed_Vector<Type, 4> v;
    v.push_back("hello");
    Type& item_200 = v.push_back("world");
    v.pop_back();
    item_200 = "monde";  // Should crash.
  };
  EXPECT_DEATH(check_pop_back(), "use-after-poison");

  auto check_resize = [] {
    Fixed_Vector<Type, 4> v;
    v.push_back("hello");
    Type& item_200 = v.push_back("world");
    v.resize(1);
    item_200 = "monde";  // Should crash.
  };
  EXPECT_DEATH(check_resize(), "use-after-poison");
}
#endif
#endif
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
