// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_MATCHER_H
#define QUICK_LINT_JS_FILE_MATCHER_H

#include <gtest/gtest.h>
#include <quick-lint-js/file.h>

#define EXPECT_SAME_FILE(path_a, path_b)                           \
  do {                                                             \
    ::quick_lint_js::canonical_path_result path_a_canonical_ =     \
        ::quick_lint_js::canonicalize_path((path_a));              \
    ASSERT_TRUE(path_a_canonical_.ok())                            \
        << std::move(path_a_canonical_).error();                   \
    ::quick_lint_js::canonical_path_result path_b_canonical_ =     \
        ::quick_lint_js::canonicalize_path((path_b));              \
    ASSERT_TRUE(path_b_canonical_.ok())                            \
        << std::move(path_b_canonical_).error();                   \
    EXPECT_EQ(path_a_canonical_.path(), path_b_canonical_.path())  \
        << (path_a) << " should be the same file as " << (path_b); \
  } while (false)

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
