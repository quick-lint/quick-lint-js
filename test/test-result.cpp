// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/result.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

namespace quick_lint_js {
namespace {
using test_result_error_types = ::testing::Types<void, int>;
template <class T>
class test_result_error : public ::testing::Test {};
TYPED_TEST_SUITE(test_result_error, test_result_error_types,
                 ::testing::internal::DefaultNameGenerator);

TEST(test_result, store_void) {
  result<void, std::string> r;
  EXPECT_TRUE(r.ok());
}

TEST(test_result, store_int) {
  result<int, std::string> r(42);
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 42);
}

TEST(test_result, default_construct_int) {
  result<int, std::string> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 0);
}

TEST(test_result, default_construct_non_trivial) {
  result<std::unique_ptr<int>, std::string> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, nullptr);
}

TEST(test_result, store_move_only_type) {
  result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
}

TEST(test_result, move_construct_void) {
  result<void, std::string> r;
  result<void, std::string> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
}

TEST(test_result, move_construct_of_move_only_type) {
  result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  result<std::unique_ptr<int>, std::string> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
  EXPECT_EQ(**copy, 42);
}

TEST(test_result, move_assign_of_move_only_type) {
  result<std::unique_ptr<int>, std::string> r(std::make_unique<int>(42));
  r = result<std::unique_ptr<int>, std::string>(std::make_unique<int>(69));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 69);
}

TEST(test_result, move_assign_void) {
  result<void, std::string> r;
  r = result<void, std::string>();
  EXPECT_TRUE(r.ok());
}

TYPED_TEST(test_result_error, store_error) {
  result<TypeParam, std::string> r =
      result<TypeParam, std::string>::failure("something bad happened");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "something bad happened");
}

TYPED_TEST(test_result_error, move_construct_error) {
  result<TypeParam, std::string> r =
      result<TypeParam, std::string>::failure("something bad happened");
  result<TypeParam, std::string> copy = std::move(r);
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error(), "something bad happened");
}

TYPED_TEST(test_result_error, move_assign_error) {
  result<TypeParam, std::string> r =
      result<TypeParam, std::string>::failure("something bad happened");
  r = result<TypeParam, std::string>::failure("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(test_result, move_assign_error_atop_value) {
  result<std::shared_ptr<int>, std::string> r =
      result<std::shared_ptr<int>, std::string>(std::make_shared<int>(42));
  r = result<std::shared_ptr<int>, std::string>::failure("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(test_result, move_assign_error_atop_void) {
  result<void, std::string> r;
  r = result<void, std::string>::failure("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(test_result, move_assign_value_atop_error) {
  result<std::shared_ptr<int>, std::string> r =
      result<std::shared_ptr<int>, std::string>::failure("fatal error");
  r = result<std::shared_ptr<int>, std::string>(std::make_shared<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
}

TEST(test_result, move_assign_void_atop_error) {
  result<void, std::string> r =
      result<void, std::string>::failure("fatal error");
  r = result<void, std::string>();
  EXPECT_TRUE(r.ok());
}

TEST(test_multi_error_result, store_void) {
  struct e_a {};
  struct e_b {};
  result<void, e_a, e_b> r;
  EXPECT_TRUE(r.ok());
  EXPECT_FALSE(r.has_error<e_a>());
  EXPECT_FALSE(r.has_error<e_b>());
}

TEST(test_multi_error_result, store_int) {
  struct e_a {};
  struct e_b {};
  result<int, e_a, e_b> r(42);
  EXPECT_TRUE(r.ok());
  EXPECT_FALSE(r.has_error<e_a>());
  EXPECT_FALSE(r.has_error<e_b>());
  EXPECT_EQ(*r, 42);
}

TEST(test_multi_error_result, move_construct_void) {
  struct e_a {};
  struct e_b {};
  result<void, e_a, e_b> r;
  result<void, e_a, e_b> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
  EXPECT_FALSE(copy.has_error<e_a>());
  EXPECT_FALSE(copy.has_error<e_b>());
}

TEST(test_multi_error_result, move_assign_void) {
  struct e_a {};
  struct e_b {};
  result<void, e_a, e_b> r;
  r = result<void, e_a, e_b>();
  EXPECT_TRUE(r.ok());
  EXPECT_FALSE(r.has_error<e_a>());
  EXPECT_FALSE(r.has_error<e_b>());
}

TYPED_TEST(test_result_error, multi_store_first_error_type) {
  struct e_a {
    std::string data;
  };
  struct e_b {
    int data;
  };
  result<TypeParam, e_a, e_b> r =
      result<TypeParam, e_a, e_b>::template failure<e_a>(
          e_a{"something bad happened"});
  EXPECT_FALSE(r.ok());
  EXPECT_TRUE(r.template has_error<e_a>());
  EXPECT_FALSE(r.template has_error<e_b>());
  EXPECT_EQ(r.template error<e_a>().data, "something bad happened");
}

TYPED_TEST(test_result_error, multi_store_second_error_type) {
  struct e_a {
    std::string data;
  };
  struct e_b {
    int data;
  };
  result<TypeParam, e_a, e_b> r =
      result<TypeParam, e_a, e_b>::template failure<e_b>(e_b{42});
  EXPECT_FALSE(r.ok());
  EXPECT_FALSE(r.template has_error<e_a>());
  EXPECT_TRUE(r.template has_error<e_b>());
  EXPECT_EQ(r.template error<e_b>().data, 42);
}

TYPED_TEST(test_result_error, multi_move_construct_first_error_type) {
  result<TypeParam, std::string, char> r =
      result<TypeParam, std::string, char>::template failure<std::string>(
          "error");
  result<TypeParam, std::string, char> copy = std::move(r);
  EXPECT_FALSE(copy.ok());
  EXPECT_TRUE(copy.template has_error<std::string>());
  EXPECT_FALSE(copy.template has_error<char>());
  EXPECT_EQ(copy.template error<std::string>(), "error");
}

TYPED_TEST(test_result_error, multi_move_assign_first_error_type) {
  result<TypeParam, std::string, char> r =
      result<TypeParam, std::string, char>::template failure<std::string>(
          "something bad happened");
  r = result<TypeParam, std::string, char>::template failure<std::string>(
      "fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_TRUE(r.template has_error<std::string>());
  EXPECT_FALSE(r.template has_error<char>());
  EXPECT_EQ(r.template error<std::string>(), "fatal error");
}

TEST(test_result, multi_move_assign_first_error_type_atop_value) {
  result<std::shared_ptr<int>, std::string, char> r =
      result<std::shared_ptr<int>, std::string, char>(
          std::make_shared<int>(42));
  r = result<std::shared_ptr<int>, std::string,
             char>::template failure<std::string>("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_TRUE(r.template has_error<std::string>());
  EXPECT_FALSE(r.template has_error<char>());
  EXPECT_EQ(r.template error<std::string>(), "fatal error");
}

TEST(test_result, multi_move_assign_first_error_type_atop_void) {
  result<void, std::string, char> r = result<void, std::string, char>();
  r = result<void, std::string, char>::template failure<std::string>(
      "fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_TRUE(r.template has_error<std::string>());
  EXPECT_FALSE(r.template has_error<char>());
  EXPECT_EQ(r.template error<std::string>(), "fatal error");
}

TEST(test_result, multi_move_assign_value_atop_first_error_type) {
  result<std::shared_ptr<int>, std::string, char> r =
      result<std::shared_ptr<int>, std::string, char>::failure<std::string>(
          "fatal error");
  r = result<std::shared_ptr<int>, std::string, char>(
      std::make_shared<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_FALSE(r.template has_error<std::string>());
  EXPECT_FALSE(r.template has_error<char>());
  EXPECT_EQ(**r, 42);
}

TEST(test_result, multi_move_assign_void_atop_first_error_type) {
  result<void, std::string, char> r =
      result<void, std::string, char>::failure<std::string>("fatal error");
  r = result<void, std::string, char>();
  EXPECT_TRUE(r.ok());
  EXPECT_FALSE(r.template has_error<std::string>());
  EXPECT_FALSE(r.template has_error<char>());
}

TYPED_TEST(test_result_error, widen_error_to_first_error_type) {
  struct e_a {};
  struct e_b {};
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::template failure(e_a{});
  result<TypeParam, e_a, e_b> copy = std::move(original);
  EXPECT_FALSE(copy.ok());
  EXPECT_TRUE(copy.template has_error<e_a>());
  EXPECT_FALSE(copy.template has_error<e_b>());
}

TYPED_TEST(test_result_error, widen_error_to_second_error_type) {
  struct e_a {};
  struct e_b {};
  result<TypeParam, e_b> original =
      result<TypeParam, e_b>::template failure(e_b{});
  result<TypeParam, e_a, e_b> copy = std::move(original);
  EXPECT_FALSE(copy.ok());
  EXPECT_FALSE(copy.template has_error<e_a>());
  EXPECT_TRUE(copy.template has_error<e_b>());
}

TYPED_TEST(test_result_error, swap_error_types) {
  struct e_a {};
  struct e_b {};
  result<TypeParam, e_a, e_b> original =
      result<TypeParam, e_a>::template failure(e_a{});
  result<TypeParam, e_b, e_a> copy = std::move(original);
  EXPECT_FALSE(copy.ok());
  EXPECT_TRUE(copy.template has_error<e_a>());
  EXPECT_FALSE(copy.template has_error<e_b>());
}

TYPED_TEST(test_result_error, propagate_error_with_same_value_type) {
  struct e_a {
    int data;
  };
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::failure(e_a{.data = 42});
  ASSERT_FALSE(original.ok());

  result<TypeParam, e_a> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(test_result_error, propagate_error_to_different_value_type) {
  struct e_a {
    int data;
  };
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::failure(e_a{.data = 42});
  ASSERT_FALSE(original.ok());

  result<std::string, e_a> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(test_result_error, propagate_error_to_void_value_type) {
  struct e_a {
    int data;
  };
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::failure(e_a{.data = 42});
  ASSERT_FALSE(original.ok());

  result<void, e_a> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error().data, 42);
}

TYPED_TEST(test_result_error, propagate_error_with_extra_second_error_type) {
  struct e_a {
    int data;
  };
  struct e_b {};
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::failure(e_a{.data = 42});
  ASSERT_FALSE(original.ok());

  result<TypeParam, e_a, e_b> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_TRUE(copy.template has_error<e_a>());
  EXPECT_FALSE(copy.template has_error<e_b>());
  EXPECT_EQ(copy.template error<e_a>().data, 42);
}

TYPED_TEST(test_result_error, propagate_error_with_extra_first_error_type) {
  struct e_a {
    int data;
  };
  struct e_b {};
  result<TypeParam, e_a> original =
      result<TypeParam, e_a>::failure(e_a{.data = 42});
  ASSERT_FALSE(original.ok());

  result<TypeParam, e_b, e_a> copy = original.propagate();
  EXPECT_FALSE(copy.ok());
  EXPECT_TRUE(copy.template has_error<e_a>());
  EXPECT_FALSE(copy.template has_error<e_b>());
  EXPECT_EQ(copy.template error<e_a>().data, 42);
}

TYPED_TEST(test_result_error, error_to_string_with_single_error_type) {
  struct e_a {
    std::string data;
    std::string to_string() const { return "data = " + data; }
  };
  result<TypeParam, e_a> error =
      result<TypeParam, e_a>::failure(e_a{.data = "hello"});
  EXPECT_EQ(error.error_to_string(), "data = hello");
}

TYPED_TEST(test_result_error, error_to_string_with_first_error_type) {
  struct e_a {
    std::string data;
    std::string to_string() const { return "e_a data = " + data; }
  };
  struct e_b {
    std::string data;
    std::string to_string() const { return "e_b data = " + data; }
  };
  result<TypeParam, e_a, e_b> error =
      result<TypeParam, e_a, e_b>::failure(e_a{.data = "hello"});
  EXPECT_EQ(error.error_to_string(), "e_a data = hello");
}

TYPED_TEST(test_result_error, copy_errors_with_single_error) {
  struct e_a {
    std::string data;
  };
  result<TypeParam, e_a> r =
      result<TypeParam, e_a>::failure(e_a{.data = "hello"});
  result<void, e_a> v = r.template copy_errors<e_a>();
  EXPECT_FALSE(v.ok());
  EXPECT_EQ(v.error().data, "hello");
}

TYPED_TEST(test_result_error, copy_errors_with_first_error) {
  struct e_a {
    std::string data;
  };
  struct e_b {
    int data;
  };
  result<TypeParam, e_a, e_b> r =
      result<TypeParam, e_a, e_b>::failure(e_a{.data = "hello"});
  result<void, e_a, e_b> v = r.template copy_errors<e_a, e_b>();
  EXPECT_FALSE(v.ok());
  EXPECT_TRUE(v.template has_error<e_a>());
  EXPECT_FALSE(v.template has_error<e_b>());
  EXPECT_EQ(v.template error<e_a>().data, "hello");
}

TYPED_TEST(test_result_error, copy_errors_with_second_error) {
  struct e_a {
    std::string data;
  };
  struct e_b {
    int data;
  };
  result<TypeParam, e_a, e_b> r =
      result<TypeParam, e_a, e_b>::failure(e_b{.data = 42});
  result<void, e_a, e_b> v = r.template copy_errors<e_a, e_b>();
  EXPECT_FALSE(v.ok());
  EXPECT_FALSE(v.template has_error<e_a>());
  EXPECT_TRUE(v.template has_error<e_b>());
  EXPECT_EQ(v.template error<e_b>().data, 42);
}

TYPED_TEST(test_result_error,
           copy_errors_with_single_error_adding_error_types) {
  struct e_a {};
  struct e_b {};
  struct e_c {};

  {
    result<TypeParam, e_b> r = result<TypeParam, e_b>::failure(e_b());
    result<void, e_a, e_b> v = r.template copy_errors<e_a, e_b>();
    EXPECT_FALSE(v.ok());
    EXPECT_FALSE(v.template has_error<e_a>());
    EXPECT_TRUE(v.template has_error<e_b>());
  }

  {
    result<TypeParam, e_b> r = result<TypeParam, e_b>::failure(e_b());
    result<void, e_b, e_c> v = r.template copy_errors<e_b, e_c>();
    EXPECT_FALSE(v.ok());
    EXPECT_TRUE(v.template has_error<e_b>());
    EXPECT_FALSE(v.template has_error<e_c>());
  }
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
