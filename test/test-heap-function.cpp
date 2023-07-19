// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/container/heap-function.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wunused-member-function")

namespace quick_lint_js {
namespace {
// Copy_Spy keeps track of the number of instances.
struct Copy_Spy {
  inline static int instance_count;

  Copy_Spy() { instance_count += 1; }

  Copy_Spy(const Copy_Spy&) { instance_count += 1; }
  Copy_Spy& operator=(const Copy_Spy&) { return *this; }

  Copy_Spy(Copy_Spy&&) { instance_count += 1; }
  Copy_Spy& operator=(Copy_Spy&&) { return *this; }

  ~Copy_Spy() { instance_count -= 1; }

  void operator()() {}
};

TEST(Test_Heap_Function, default_constructed_has_no_target) {
  Heap_Function<void()> f;
  EXPECT_FALSE(f);
}

TEST(Test_Heap_Function, constructed_with_lambda_has_target) {
  Heap_Function<void()> f = []() -> void {};
  EXPECT_TRUE(f);
}

TEST(Test_Heap_Function, destructing_heap_function_calls_target_destructor) {
  Copy_Spy::instance_count = 0;
  {
    Heap_Function<void()> f = Copy_Spy();
    EXPECT_EQ(Copy_Spy::instance_count, 1);
  }
  EXPECT_EQ(Copy_Spy::instance_count, 0);
}

TEST(Test_Heap_Function, call_closureless_lambda) {
  static bool called;
  Heap_Function<void()> f = []() -> void { called = true; };
  called = false;
  f();
  EXPECT_TRUE(called);
}

TEST(Test_Heap_Function, call_lambda_with_closure) {
  bool called = false;
  Heap_Function<void()> f = [&called]() -> void { called = true; };
  ASSERT_FALSE(called);
  f();
  EXPECT_TRUE(called);
}

TEST(Test_Heap_Function, call_manual_functor_with_closure) {
  struct Functor {
    bool& called;

    void operator()() { called = true; }
  };
  bool called = false;
  Heap_Function<void()> f = Functor{called};
  ASSERT_FALSE(called);
  f();
  EXPECT_TRUE(called);
}

TEST(Test_Heap_Function, lambda_return_value) {
  Heap_Function<int()> f = []() -> int { return 42; };
  int result = f();
  EXPECT_EQ(result, 42);
}

TEST(Test_Heap_Function, single_parameter) {
  static int got_x;
  Heap_Function<void(int)> f = [](int x) -> void { got_x = x; };
  got_x = 0;
  f(42);
  EXPECT_EQ(got_x, 42);
}

TEST(Test_Heap_Function, move_only_parameter) {
  static int* got_o;
  Heap_Function<void(std::unique_ptr<int>)> f =
      [](std::unique_ptr<int> o) -> void { got_o = o.get(); };
  got_o = nullptr;

  std::unique_ptr<int> o = std::make_unique<int>(42);
  int* o_raw = o.get();
  f(std::move(o));

  EXPECT_EQ(got_o, o_raw);
}

TEST(Test_Heap_Function, assign_lambda_over_empty) {
  static bool called;
  called = false;

  Heap_Function<void()> f;
  f = []() -> void { called = true; };

  EXPECT_TRUE(f);
  f();
  EXPECT_TRUE(called);
}

TEST(Test_Heap_Function, assign_heap_function_over_empty) {
  static bool called;
  called = false;

  Heap_Function<void()> f;
  f = Heap_Function<void()>([]() -> void { called = true; });

  EXPECT_TRUE(f);
  f();
  EXPECT_TRUE(called);
}

TEST(Test_Heap_Function, assign_empty_over_empty) {
  Heap_Function<void()> f;
  f = Heap_Function<void()>();
  EXPECT_FALSE(f);
}

TEST(Test_Heap_Function, assign_over_existing_destructs_existing) {
  Copy_Spy::instance_count = 0;
  Heap_Function<void()> f = Copy_Spy();
  EXPECT_EQ(Copy_Spy::instance_count, 1);
  f = []() -> void {};
  EXPECT_EQ(Copy_Spy::instance_count, 0);
}

TEST(Test_Heap_Function, assign_empty_over_lambda) {
  Heap_Function<void()> f = []() -> void {};
  f = Heap_Function<void()>();
  EXPECT_FALSE(f);
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
