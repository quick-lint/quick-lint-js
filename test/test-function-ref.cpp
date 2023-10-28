// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/port/function-ref.h>
#include <string>

namespace quick_lint_js {
namespace {
TEST(Test_Async_Function_Ref, references_temporary_lambda_with_no_captures) {
  static int calls;
  calls = 0;
  Async_Function_Ref<std::string()> f([]() -> std::string {
    calls += 1;
    return "called";
  });

  EXPECT_EQ(calls, 0);
  std::string result = f();
  EXPECT_EQ(calls, 1);
  EXPECT_EQ(result, "called");
}

TEST(Test_Async_Function_Ref,
     references_temporary_lambda_lvalue_with_no_captures) {
  static int calls;
  calls = 0;
  auto functor = []() -> std::string {
    calls += 1;
    return "called";
  };
  Async_Function_Ref<std::string()> f(functor);

  EXPECT_EQ(calls, 0);
  std::string result = f();
  EXPECT_EQ(calls, 1);
  EXPECT_EQ(result, "called");
}

TEST(Test_Async_Function_Ref,
     converts_implicitly_from_temporary_lambda_with_no_captures) {
  static int calls;
  calls = 0;
  auto test = [](Async_Function_Ref<std::string()> f) -> void {
    EXPECT_EQ(calls, 0);
    std::string result = f();
    EXPECT_EQ(calls, 1);
    EXPECT_EQ(result, "called");
  };
  test([]() -> std::string {
    calls += 1;
    return "called";
  });
}

TEST(Test_Async_Function_Ref,
     references_lambda_with_captures_if_stack_allocated) {
  static int calls;
  calls = 0;
  int captured;
  auto functor = [&captured]() -> int {
    calls += 1;
    return captured;
  };
  Async_Function_Ref<int()> f(functor);

  EXPECT_EQ(calls, 0);
  captured = 42;
  int result = f();
  EXPECT_EQ(calls, 1);
  EXPECT_EQ(result, 42);
}

TEST(Test_Async_Function_Ref,
     references_const_lambda_with_captures_if_stack_allocated) {
  static int calls;
  calls = 0;
  int captured;
  const auto functor = [&captured]() -> int {
    calls += 1;
    return captured;
  };
  Async_Function_Ref<int()> f(functor);

  EXPECT_EQ(calls, 0);
  captured = 42;
  int result = f();
  EXPECT_EQ(calls, 1);
  EXPECT_EQ(result, 42);
}

// TODO(strager): Test that the following programs do not compile:
#if 0
TEST(Test_Async_Function_Ref, cannot_reference_lambda_rvalue_with_captures) {
   int captured = 0;
   Async_Function_Ref<int()> f([captured]() -> int {
     return captured;
   });
   f();  // Undefined behavior (if the code compiled).
}
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
