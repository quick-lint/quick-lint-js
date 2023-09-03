// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/port/warning.h>
#include <string>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
using Test_Result_Error_Types = ::testing::Types<void, int>;
template <class T>
class Test_Result_Error : public ::testing::Test {};
TYPED_TEST_SUITE(Test_Result_Error, Test_Result_Error_Types,
                 ::testing::internal::DefaultNameGenerator);

TEST(Test_Result, store_void) {
  Result<void, std::string> r;
  EXPECT_TRUE(r.ok());
}

TEST(Test_Result, store_int) {
  Result<int, std::string> r(42);
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 42);
}

TEST(Test_Result, default_construct_int) {
  Result<int, std::string> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 0);
}

TEST(Test_Result, default_construct_non_trivial) {
  Result<std::unique_ptr<int>, std::string> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, nullptr);
}

TEST(Test_Result, store_move_only_type) {
  Result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
}

TEST(Test_Result, move_construct_void) {
  Result<void, std::string> r;
  Result<void, std::string> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
}

TEST(Test_Result, move_construct_of_move_only_type) {
  Result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  Result<std::unique_ptr<int>, std::string> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
  EXPECT_EQ(**copy, 42);
}

TEST(Test_Result, move_assign_of_move_only_type) {
  Result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  r = Result<std::unique_ptr<int>, std::string>(std::make_unique<int>(69));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 69);
}

TEST(Test_Result, move_assign_void) {
  Result<void, std::string> r;
  r = Result<void, std::string>();
  EXPECT_TRUE(r.ok());
}

TYPED_TEST(Test_Result_Error, store_error) {
  Result<TypeParam, std::string> r = failed_result("something bad happened"s);
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "something bad happened");
}

TYPED_TEST(Test_Result_Error, move_construct_error) {
  Result<TypeParam, std::string> r = failed_result("something bad happened"s);
  Result<TypeParam, std::string> copy = std::move(r);
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error(), "something bad happened");
}

TYPED_TEST(Test_Result_Error, move_assign_error) {
  Result<TypeParam, std::string> r = failed_result("something bad happened"s);
  r = failed_result("fatal error"s);
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(Test_Result, move_assign_error_atop_value) {
  Result<std::shared_ptr<int>, std::string> r =
      Result<std::shared_ptr<int>, std::string>(std::make_shared<int>(42));
  r = failed_result("fatal error"s);
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(Test_Result, move_assign_error_atop_void) {
  Result<void, std::string> r;
  r = failed_result("fatal error"s);
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(Test_Result, move_assign_value_atop_error) {
  Result<std::shared_ptr<int>, std::string> r = failed_result("fatal error"s);
  r = Result<std::shared_ptr<int>, std::string>(std::make_shared<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
}

TEST(Test_Result, move_assign_void_atop_error) {
  Result<void, std::string> r = failed_result("fatal error"s);
  r = Result<void, std::string>();
  EXPECT_TRUE(r.ok());
}

TYPED_TEST(Test_Result_Error, propagate_error_with_same_value_type) {
  struct E_A {
    int data;
  };
  Result<TypeParam, E_A> original = failed_result(E_A{.data = 42});
  ASSERT_FALSE(original.ok());

  Result<TypeParam, E_A> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(Test_Result_Error, propagate_error_to_different_value_type) {
  struct E_A {
    int data;
  };
  Result<TypeParam, E_A> original = failed_result(E_A{.data = 42});
  ASSERT_FALSE(original.ok());

  Result<std::string, E_A> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(Test_Result_Error, propagate_error_to_void_value_type) {
  struct E_A {
    int data;
  };
  Result<TypeParam, E_A> original = failed_result(E_A{.data = 42});
  ASSERT_FALSE(original.ok());

  Result<void, E_A> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(Test_Result_Error,
           propagate_error_with_implicitly_convertible_error_type) {
  struct E_A {
    int data;
  };
  struct E_B {
    /*implicit*/ E_B(E_A&& other) : data(other.data * 10) { other.data = 0; }
    int data;
  };
  Result<TypeParam, E_A> original = failed_result(E_A{.data = 42});
  ASSERT_FALSE(original.ok());

  Result<TypeParam, E_B> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 420);
}

TYPED_TEST(Test_Result_Error, error_to_string) {
  struct E_A {
    std::string data;
    std::string to_string() const { return "data = " + data; }
  };
  Result<TypeParam, E_A> error = failed_result(E_A{.data = "hello"});
  EXPECT_EQ(error.error_to_string(), "data = hello");
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
