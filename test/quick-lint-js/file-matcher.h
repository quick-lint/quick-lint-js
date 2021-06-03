// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_MATCHER_H
#define QUICK_LINT_JS_FILE_MATCHER_H

#include <gtest/gtest.h>
#include <quick-lint-js/file.h>

#define EXPECT_SAME_FILE(path_a, path_b) \
  EXPECT_PRED_FORMAT2(::quick_lint_js::assert_same_file, path_a, path_b)

namespace quick_lint_js {
template <class String>
::testing::AssertionResult assert_same_file(const char *lhs_expr,
                                            const char *rhs_expr,
                                            String lhs_path, String rhs_path) {
  canonical_path_result lhs_canonical = canonicalize_path(lhs_path);
  EXPECT_TRUE(lhs_canonical.ok()) << std::move(lhs_canonical).error();
  canonical_path_result rhs_canonical = canonicalize_path(rhs_path);
  EXPECT_TRUE(rhs_canonical.ok()) << std::move(rhs_canonical).error();
  if (lhs_canonical.path() == rhs_canonical.path()) {
    return ::testing::AssertionSuccess();
  } else {
    return ::testing::AssertionFailure()
           << lhs_expr << " (" << lhs_path << ") is not the same file as "
           << rhs_expr << " (" << rhs_path << ')';
  }
}
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
