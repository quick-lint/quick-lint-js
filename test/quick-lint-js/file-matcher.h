// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_MATCHER_H
#define QUICK_LINT_JS_FILE_MATCHER_H

#include <cerrno>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#define EXPECT_SAME_FILE(path_a, path_b) \
  EXPECT_PRED_FORMAT2(::quick_lint_js::assert_same_file, path_a, path_b)

namespace quick_lint_js {
inline ::testing::AssertionResult assert_same_file(const char* lhs_expr,
                                                   const char* rhs_expr,
                                                   const char* lhs_path,
                                                   const char* rhs_path) {
  bool same;
#if QLJS_HAVE_STD_FILESYSTEM
  // TODO(strager): std::filesystem::equivalent treats differnt symlinks
  // pointing to the same file as equivalent. This behavior differs from our
  // lstat-based implementation below.
  same = std::filesystem::equivalent(lhs_path, rhs_path);
#elif QLJS_HAVE_SYS_STAT_H
  {
    struct stat lhs_stat = {};
    EXPECT_EQ(::lstat(lhs_path, &lhs_stat), 0) << std::strerror(errno);

    struct stat rhs_stat = {};
    EXPECT_EQ(::lstat(rhs_path, &rhs_stat), 0) << std::strerror(errno);

    same = lhs_stat.st_dev == rhs_stat.st_dev &&
           lhs_stat.st_ino == rhs_stat.st_ino;
  }
#else
#error "Unsupported platform"
#endif
  if (same) {
    return ::testing::AssertionSuccess();
  } else {
    return ::testing::AssertionFailure()
           << lhs_expr << " (" << lhs_path << ") is not the same file as "
           << rhs_expr << " (" << rhs_path << ')';
  }
}

inline ::testing::AssertionResult assert_same_file(
    const char* lhs_expr, const char* rhs_expr, const std::string& lhs_path,
    const std::string& rhs_path) {
  return assert_same_file(lhs_expr, rhs_expr, lhs_path.c_str(),
                          rhs_path.c_str());
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
