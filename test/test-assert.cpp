// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gtest/gtest.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

namespace quick_lint_js {
namespace {
TEST(test_assert, failing_assert_crashes) {
  auto check = [] {
    bool everything_is_okay = false;
    QLJS_ALWAYS_ASSERT(everything_is_okay);
  };
  EXPECT_DEATH(check(), "everything_is_okay");
}

TEST(test_assert, passing_assert_does_not_crash) { QLJS_ALWAYS_ASSERT(true); }

TEST(test_assert, passing_assert_executes_side_effects) {
  bool executed = false;
  QLJS_ALWAYS_ASSERT((executed = true));
  EXPECT_TRUE(executed);
}
}  // namespace
}  // namespace quick_lint_js
