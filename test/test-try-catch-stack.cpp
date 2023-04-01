// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/util/try-catch-stack.h>

namespace quick_lint_js {
namespace {
TEST(test_try_catch_stack, raise_if_have_handler_does_nothing_without_catch) {
  try_catch_stack<int> stack;
  // Shouldn't crash.
  stack.raise_if_have_handler(10);
}

TEST(test_try_catch_stack, try_catch_does_not_call_catch_if_not_raised) {
  try_catch_stack<int> stack;
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

TEST(test_try_catch_stack, try_catch_calls_catch_if_raised) {
  try_catch_stack<int> stack;
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

TEST(test_try_catch_stack, raise_does_not_return_inside_try) {
  try_catch_stack<int> stack;
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

TEST(test_try_catch_stack, if_catch_is_called_raise_stops_propagating) {
  try_catch_stack<int> stack;
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

TEST(test_try_catch_stack,
     try_catch_calls_catch_if_raised_inside_nested_catch) {
  try_catch_stack<int> stack;
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
