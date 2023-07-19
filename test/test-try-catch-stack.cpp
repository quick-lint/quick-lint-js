// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/util/try-catch-stack.h>

namespace quick_lint_js {
namespace {
TEST(Test_Try_Catch_Stack, raise_if_have_handler_does_nothing_without_catch) {
  Try_Catch_Stack<int> stack;
  // Shouldn't crash.
  stack.raise_if_have_handler(10);
}

TEST(Test_Try_Catch_Stack, try_catch_does_not_call_catch_if_not_raised) {
  Try_Catch_Stack<int> stack;
  bool try_called = false;
  int result = stack.try_catch<int>(
      [&]() -> int {
        try_called = true;
        return 42;
      },
      []([[maybe_unused]] int exception) -> int {
        ADD_FAILURE() << "catch callback should not have been called";
        return 69;
      });
  EXPECT_TRUE(try_called) << "try callback should have been called";
  EXPECT_EQ(result, 42);
}

TEST(Test_Try_Catch_Stack, try_catch_calls_catch_if_raised) {
  Try_Catch_Stack<int> stack;
  bool catch_called = false;
  int result = stack.try_catch<int>(
      [&]() -> int {
        stack.raise_if_have_handler(420);
        return 42;
      },
      [&](int exception) -> int {
        EXPECT_EQ(exception, 420);
        catch_called = true;
        return 69;
      });
  EXPECT_TRUE(catch_called) << "catch callback should have been called";
  EXPECT_EQ(result, 69);
}

TEST(Test_Try_Catch_Stack, raise_does_not_return_inside_try) {
  Try_Catch_Stack<int> stack;
  bool try_called = false;
  stack.try_catch<int>(
      [&]() -> int {
        try_called = true;
        stack.raise_if_have_handler(420);
        ADD_FAILURE() << "raise_if_have_handler should not return";
        return 42;
      },
      [&]([[maybe_unused]] int exception) -> int { return 69; });
  EXPECT_TRUE(try_called) << "try callback should have been called";
}

TEST(Test_Try_Catch_Stack, if_catch_is_called_raise_stops_propagating) {
  Try_Catch_Stack<int> stack;
  bool inner_catch_called = false;
  int result = stack.try_catch<int>(
      [&]() -> int {
        stack.try_catch<int>(
            [&]() -> int {
              stack.raise_if_have_handler(420);
              return 0;
            },
            [&](int exception) -> int {
              inner_catch_called = true;
              EXPECT_EQ(exception, 420);
              return 0;
            });
        return 69;
      },
      [&]([[maybe_unused]] int exception) -> int {
        ADD_FAILURE()
            << "outer-most catch callback should not have been called";
        return 0;
      });

  EXPECT_TRUE(inner_catch_called)
      << "inner-most catch callback should have been called";
  EXPECT_EQ(result, 69) << "inner try_catch should have returned";
}

TEST(Test_Try_Catch_Stack,
     try_catch_calls_catch_if_raised_inside_nested_catch) {
  Try_Catch_Stack<int> stack;
  bool inner_catch_called = false;
  bool outer_catch_called = false;
  stack.try_catch<int>(
      [&]() -> int {
        stack.try_catch<int>(
            [&]() -> int {
              stack.raise_if_have_handler(420);
              return 0;
            },
            [&](int exception) -> int {
              inner_catch_called = true;
              EXPECT_EQ(exception, 420);
              stack.raise_if_have_handler(69);
              return 0;
            });
        return 0;
      },
      [&](int exception) -> int {
        outer_catch_called = true;
        EXPECT_EQ(exception, 69);
        return 0;
      });

  EXPECT_TRUE(inner_catch_called)
      << "inner-most catch callback should have been called";
  EXPECT_TRUE(outer_catch_called)
      << "outer-most catch callback should have been called";
}

TEST(Test_Try_Catch_Stack, raise_returns_if_only_finally_is_on_stack) {
  Try_Catch_Stack<int> stack;
  bool finally_called = false;
  bool raise_returned = false;
  stack.try_finally(
      [&]() -> void {
        stack.raise_if_have_handler(420);
        raise_returned = true;
      },
      [&]() -> void { finally_called = true; });

  EXPECT_TRUE(finally_called);
  EXPECT_TRUE(raise_returned);
}

TEST(Test_Try_Catch_Stack, raise_calls_catch_then_finally) {
  Try_Catch_Stack<int> stack;
  bool catch_called = false;
  bool finally_called = false;
  stack.try_finally(
      [&]() -> void {
        stack.try_catch<int>(
            [&]() -> int {
              stack.raise_if_have_handler(420);
              ADD_FAILURE() << "raise_if_have_handler should not have returned";
              return 0;
            },
            [&](int exception) -> int {
              catch_called = true;
              EXPECT_FALSE(finally_called)
                  << "inner catch should be called before outer finally";
              EXPECT_EQ(exception, 420);
              return 0;
            });
      },
      [&]() -> void { finally_called = true; });

  EXPECT_TRUE(catch_called);
  EXPECT_TRUE(finally_called);
}

TEST(Test_Try_Catch_Stack, raise_calls_finally_then_catch) {
  Try_Catch_Stack<int> stack;
  bool catch_called = false;
  bool finally_called = false;
  stack.try_catch<int>(
      [&]() -> int {
        stack.try_finally(
            [&]() -> void {
              stack.raise_if_have_handler(420);
              ADD_FAILURE() << "raise_if_have_handler should not have returned";
            },
            [&]() -> void {
              EXPECT_FALSE(catch_called)
                  << "inner finally should be called before outer catch";
              finally_called = true;
            });
        ADD_FAILURE() << "try_finally should not have returned";
        return 0;
      },
      [&](int exception) -> int {
        catch_called = true;
        EXPECT_EQ(exception, 420);
        return 0;
      });

  EXPECT_TRUE(catch_called);
  EXPECT_TRUE(finally_called);
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
