// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/result.h>

namespace quick_lint_js {
namespace {
TEST(test_sloppy_result, store_int) {
  sloppy_result<int> r(42);
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 42);
}

TEST(test_sloppy_result, default_construct_int) {
  sloppy_result<int> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, 0);
}

TEST(test_sloppy_result, default_construct_non_trivial) {
  sloppy_result<std::unique_ptr<int>> r;
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(*r, nullptr);
}

TEST(test_sloppy_result, store_move_only_type) {
  sloppy_result<std::unique_ptr<int>> r(std::make_unique<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
}

TEST(test_sloppy_result, move_construct_of_move_only_type) {
  sloppy_result<std::unique_ptr<int>> r(std::make_unique<int>(42));
  sloppy_result<std::unique_ptr<int>> copy = std::move(r);
  EXPECT_TRUE(copy.ok());
  EXPECT_EQ(**copy, 42);
}

TEST(test_sloppy_result, move_assign_of_move_only_type) {
  sloppy_result<std::unique_ptr<int>> r(std::make_unique<int>(42));
  r = sloppy_result<std::unique_ptr<int>>(std::make_unique<int>(69));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 69);
}

TEST(test_sloppy_result, store_error) {
  sloppy_result<int> r = sloppy_result<int>::failure("something bad happened");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "something bad happened");
}

TEST(test_sloppy_result, move_construct_error) {
  sloppy_result<int> r = sloppy_result<int>::failure("something bad happened");
  sloppy_result<int> copy = std::move(r);
  EXPECT_FALSE(copy.ok());
  EXPECT_EQ(copy.error(), "something bad happened");
}

TEST(test_sloppy_result, move_assign_error) {
  sloppy_result<int> r = sloppy_result<int>::failure("something bad happened");
  r = sloppy_result<int>::failure("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(test_sloppy_result, move_assign_error_atop_value) {
  sloppy_result<std::shared_ptr<int>> r =
      sloppy_result<std::shared_ptr<int>>(std::make_shared<int>(42));
  r = sloppy_result<std::shared_ptr<int>>::failure("fatal error");
  EXPECT_FALSE(r.ok());
  EXPECT_EQ(r.error(), "fatal error");
}

TEST(test_sloppy_result, move_assign_value_atop_error) {
  sloppy_result<std::shared_ptr<int>> r =
      sloppy_result<std::shared_ptr<int>>::failure("fatal error");
  r = sloppy_result<std::shared_ptr<int>>(std::make_shared<int>(42));
  EXPECT_TRUE(r.ok());
  EXPECT_EQ(**r, 42);
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
