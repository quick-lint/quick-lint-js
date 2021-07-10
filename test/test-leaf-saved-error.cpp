// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/result.hpp>
#include <gtest/gtest.h>
#include <quick-lint-js/leaf-saved-error.h>

namespace quick_lint_js {
namespace {
TEST(test_leaf_saved_error, saved_error_on_no_error_is_empty) {
  struct e_a {};
  struct e_b {};

  auto [_result, saved_error] = leaf_save_error<e_a, e_b>(
      []() -> boost::leaf::result<void> { return {}; });
  EXPECT_EQ(saved_error.get<e_a>(), nullptr);
  EXPECT_EQ(saved_error.get<e_b>(), nullptr);
}

TEST(test_leaf_saved_error, saving_error_with_no_error_preserves_result) {
  struct e_a {};
  struct e_b {};

  auto [result, _saved_error] = leaf_save_error<e_a, e_b>(
      []() -> boost::leaf::result<int> { return 42; });
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, 42);
}

TEST(test_leaf_saved_error, saved_error_contains_filled_slots) {
  struct e_a {};
  struct e_b {
    int data;
  };
  struct e_c {};

  auto [_result, saved_error] =
      leaf_save_error<e_a, e_b, e_c>([]() -> boost::leaf::result<void> {
        return boost::leaf::new_error(e_c(), e_a(), e_b{.data = 42});
      });
  EXPECT_NE(saved_error.get<e_a>(), nullptr);
  EXPECT_NE(saved_error.get<e_c>(), nullptr);
  ASSERT_NE(saved_error.get<e_b>(), nullptr);
  EXPECT_EQ(saved_error.get<e_b>()->data, 42);
}

TEST(test_leaf_saved_error, save_error_with_missing_slots) {
  struct e_a {
    int data;
  };
  struct e_b {
    int data;
  };

  auto [_result, saved_error] =
      leaf_save_error<e_a, e_b>([]() -> boost::leaf::result<void> {
        return boost::leaf::new_error(e_b{.data = 42});
      });
  EXPECT_EQ(saved_error.get<e_a>(), nullptr);
  ASSERT_NE(saved_error.get<e_b>(), nullptr);
  EXPECT_EQ(saved_error.get<e_b>()->data, 42);
}

TEST(test_leaf_saved_error, does_not_default_construct_errors) {
  struct e_a {
    e_a() = delete;
    explicit e_a(int) {}
  };

  auto [_result, saved_error] = leaf_save_error<e_a>([]() -> boost::leaf::result<void> {
    return boost::leaf::new_error(e_a(42));
  });
  EXPECT_NE(saved_error.get<e_a>(), nullptr);
}

TEST(test_leaf_saved_error, does_not_copy_construct_result) {
  auto [result, _saved_error] = leaf_save_error<int>([]() -> boost::leaf::result<std::unique_ptr<int>> {
    return std::make_unique<int>(42);
  });
  ASSERT_TRUE(result);
  EXPECT_EQ(**result, 42);
}

TEST(test_leaf_saved_error, separately_saved_errors_are_equal) {
  auto [_result_1, saved_error_1] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(42); });
  auto [_result_2, saved_error_2] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(42); });
  EXPECT_EQ(saved_error_1, saved_error_2);
}

TEST(test_leaf_saved_error, separately_saved_errors_are_not_equal_by_value) {
  auto [_result_1, saved_error_1] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(42); });
  auto [_result_2, saved_error_2] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(69); });
  EXPECT_NE(saved_error_1, saved_error_2);
}

TEST(test_leaf_saved_error,
     separately_saved_errors_are_not_equal_if_some_errors_are_missing) {
  auto [_result_1, saved_error_1] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(42); });
  auto [_result_2, saved_error_2] = leaf_save_error<int>(
      []() -> boost::leaf::result<void> { return boost::leaf::new_error(); });
  EXPECT_NE(saved_error_1, saved_error_2);
  EXPECT_NE(saved_error_2, saved_error_1);
}

TEST(test_leaf_saved_error, propagate_copy_on_failure) {
  struct e_test { int data; };

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        auto [result, saved_error] = leaf_save_error<e_test>(
            []() -> boost::leaf::result<void> { return boost::leaf::new_error(e_test{.data = 42}); });
        EXPECT_FALSE(result);
        saved_error.propagate_copy(result);
        return result.error();
      },
      [](e_test test) {
        EXPECT_EQ(test.data, 42);
      },
      []() {
        ADD_FAILURE() << "e_test was not propagated";
      });
}

TEST(test_leaf_saved_error, propagate_copy_does_not_overwrite_uncaptured_slots) {
  struct e_a { int data; };
  struct e_b {};

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        auto a_guard = boost::leaf::on_error(e_a{.data = 69});
        auto [result, saved_error] = leaf_save_error<e_b>(
            []() -> boost::leaf::result<void> { return boost::leaf::new_error(e_a{.data = 42}, e_b()); });
        EXPECT_FALSE(result);
        saved_error.propagate_copy(result);
        return result.error();
      },
      [](e_a a) {
        EXPECT_EQ(test.data, 69);
      },
      []() {
        ADD_FAILURE() << "unknown error";
      });
}

TEST(test_leaf_saved_error, unsaved_errors_are_automatically_propagated) {
  struct e_a {};
  struct e_b {};

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        auto [result, _saved_error] = leaf_save_error<e_a>(
            []() -> boost::leaf::result<void> { return boost::leaf::new_error(e_a(), e_b()); });
        EXPECT_FALSE(result);
        return result.error();
      },
      [](e_b) {
        // Test passed.
      },
      []() {
        ADD_FAILURE() << "e_b was not propagated";
      });
}

TEST(test_leaf_saved_error, saved_errors_are_not_automatically_propagated) {
  struct e_test {};

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        auto [result, _saved_error] = leaf_save_error<e_test>(
            []() -> boost::leaf::result<void> { return boost::leaf::new_error(e_test()); });
        EXPECT_FALSE(result);
        return result.error();
      },
      [](e_test) {
        ADD_FAILURE() << "e_a should not have been propagated";
      },
      []() {
        // Test passed.
      });
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
