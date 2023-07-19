// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/port/warning.h>

using ::testing::ElementsAreArray;

QLJS_WARNING_IGNORE_CLANG("-Wunused-member-function")

namespace quick_lint_js {
namespace {
TEST(Test_Array, concat) {
  std::array<int, 3> lhs = {100, 200, 300};
  std::array<int, 4> rhs = {400, 500, 600, 700};
  std::array<int, 7> result = concat(lhs, rhs);
  EXPECT_THAT(result, ElementsAreArray({100, 200, 300, 400, 500, 600, 700}));
}

TEST(Test_Array, concat_not_default_constructible) {
  struct Initialized_Int {
    Initialized_Int() = delete;

    /*implicit*/ Initialized_Int(int value) noexcept : value(value) {}

    bool operator==(Initialized_Int other) const noexcept {
      return this->value == other.value;
    }

    bool operator!=(Initialized_Int other) const noexcept {
      return !(*this == other);
    }

    int value;
  };

  std::array<Initialized_Int, 3> lhs = {100, 200, 300};
  std::array<Initialized_Int, 4> rhs = {400, 500, 600, 700};
  std::array<Initialized_Int, 7> result = concat(lhs, rhs);
  EXPECT_THAT(result, ElementsAreArray({100, 200, 300, 400, 500, 600, 700}));
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
