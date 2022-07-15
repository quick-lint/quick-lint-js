// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/heap-function.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wunused-member-function")

namespace quick_lint_js {
namespace {
// copy_spy keeps track of the number of instances.
struct copy_spy {
  inline static int instance_count;

  copy_spy() { instance_count += 1; }

  copy_spy(const copy_spy&) { instance_count += 1; }
  copy_spy& operator=(const copy_spy&) { return *this; }

  copy_spy(copy_spy&&) { instance_count += 1; }
  copy_spy& operator=(copy_spy&&) { return *this; }

  ~copy_spy() { instance_count -= 1; }

  void operator()() {}
};

TEST(test_heap_function, default_constructed_has_no_target) {
  heap_function<void()> f;
  EXPECT_FALSE(f);
}

TEST(test_heap_function, constructed_with_lambda_has_target) {
  heap_function<void()> f = []() -> void {};
  EXPECT_TRUE(f);
}

TEST(test_heap_function, destructing_heap_function_calls_target_destructor) {
  copy_spy::instance_count = 0;
  {
    heap_function<void()> f = copy_spy();
    EXPECT_EQ(copy_spy::instance_count, 1);
  }
  EXPECT_EQ(copy_spy::instance_count, 0);
}

TEST(test_heap_function, call_closureless_lambda) {
  static bool called;
  heap_function<void()> f = []() -> void { called = true; };
  called = false;
  f();
  EXPECT_TRUE(called);
}

TEST(test_heap_function, call_lambda_with_closure) {
  bool called = false;
  heap_function<void()> f = [&called]() -> void { called = true; };
  ASSERT_FALSE(called);
  f();
  EXPECT_TRUE(called);
}

TEST(test_heap_function, call_manual_functor_with_closure) {
  struct functor {
    bool& called;

    void operator()() { called = true; }
  };
  bool called = false;
  heap_function<void()> f = functor{called};
  ASSERT_FALSE(called);
  f();
  EXPECT_TRUE(called);
}

TEST(test_heap_function, lambda_return_value) {
  heap_function<int()> f = []() -> int { return 42; };
  int result = f();
  EXPECT_EQ(result, 42);
}

TEST(test_heap_function, single_parameter) {
  static int got_x;
  heap_function<void(int)> f = [](int x) -> void { got_x = x; };
  got_x = 0;
  f(42);
  EXPECT_EQ(got_x, 42);
}

TEST(test_heap_function, move_only_parameter) {
  static int* got_o;
  heap_function<void(std::unique_ptr<int>)> f =
      [](std::unique_ptr<int> o) -> void { got_o = o.get(); };
  got_o = nullptr;

  std::unique_ptr<int> o = std::make_unique<int>(42);
  int* o_raw = o.get();
  f(std::move(o));

  EXPECT_EQ(got_o, o_raw);
}

TEST(test_heap_function, assign_lambda_over_empty) {
  static bool called;
  called = false;

  heap_function<void()> f;
  f = []() -> void { called = true; };

  EXPECT_TRUE(f);
  f();
  EXPECT_TRUE(called);
}

TEST(test_heap_function, assign_heap_function_over_empty) {
  static bool called;
  called = false;

  heap_function<void()> f;
  f = heap_function<void()>([]() -> void { called = true; });

  EXPECT_TRUE(f);
  f();
  EXPECT_TRUE(called);
}

TEST(test_heap_function, assign_empty_over_empty) {
  heap_function<void()> f;
  f = heap_function<void()>();
  EXPECT_FALSE(f);
}

TEST(test_heap_function, assign_over_existing_destructs_existing) {
  copy_spy::instance_count = 0;
  heap_function<void()> f = copy_spy();
  EXPECT_EQ(copy_spy::instance_count, 1);
  f = []() -> void {};
  EXPECT_EQ(copy_spy::instance_count, 0);
}

TEST(test_heap_function, assign_empty_over_lambda) {
  heap_function<void()> f = []() -> void {};
  f = heap_function<void()>();
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
