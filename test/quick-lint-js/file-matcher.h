// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_MATCHER_H
#define QUICK_LINT_JS_FILE_MATCHER_H

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <cerrno>
#include <cstring>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/windows.h>
#endif

#define EXPECT_SAME_FILE(path_a, path_b) \
  EXPECT_PRED_FORMAT2(::quick_lint_js::assert_same_file, path_a, path_b)

#define EXPECT_FILE_DOES_NOT_EXIST(path) \
  EXPECT_PRED_FORMAT1(::quick_lint_js::assert_file_does_not_exist, path)

namespace quick_lint_js {
inline ::testing::AssertionResult assert_same_file(const char* lhs_expr,
                                                   const char* rhs_expr,
                                                   const char* lhs_path,
                                                   const char* rhs_path) {
  bool same;
#if QLJS_HAVE_WINDOWS_H
  {
    auto get_file_id = [](const char* path) -> ::FILE_ID_INFO {
      ::FILE_ID_INFO id = {};
      // TODO(strager): Use CreateFileW instead.
      windows_handle_file handle(::CreateFileA(
          path, /*dwDesiredAccess=*/GENERIC_READ,
          /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ |
              FILE_SHARE_WRITE,
          /*lpSecurityAttributes=*/nullptr,
          /*dwCreationDisposition=*/OPEN_EXISTING,
          /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL |
              FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
          /*hTemplateFile=*/nullptr));
      if (!handle.valid()) {
        ADD_FAILURE() << path << ": "
                      << windows_handle_file_ref::get_last_error_message();
        return id;
      }
      EXPECT_TRUE(::GetFileInformationByHandleEx(handle.get(), ::FileIdInfo,
                                                 &id, sizeof(id)))
          << path << ": " << windows_handle_file_ref::get_last_error_message();
      return id;
    };
    ::FILE_ID_INFO lhs_id = get_file_id(lhs_path);
    ::FILE_ID_INFO rhs_id = get_file_id(rhs_path);
    same = file_ids_equal(lhs_id, rhs_id);
  }
#elif QLJS_HAVE_SYS_STAT_H
  {
    struct stat lhs_stat = {};
    EXPECT_EQ(::lstat(lhs_path, &lhs_stat), 0)
        << lhs_path << ": " << std::strerror(errno);

    struct stat rhs_stat = {};
    EXPECT_EQ(::lstat(rhs_path, &rhs_stat), 0)
        << rhs_path << ": " << std::strerror(errno);

    same = lhs_stat.st_dev == rhs_stat.st_dev &&
           lhs_stat.st_ino == rhs_stat.st_ino;
  }
#elif QLJS_HAVE_STD_FILESYSTEM
  // TODO(strager): std::filesystem::equivalent treats different symlinks
  // pointing to the same file as equivalent. This behavior differs from our
  // Win32 and lstat-based implementations above.
  same = std::filesystem::equivalent(lhs_path, rhs_path);
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

inline ::testing::AssertionResult assert_same_file(const char* lhs_expr,
                                                   const char* rhs_expr,
                                                   std::string_view lhs_path,
                                                   std::string_view rhs_path) {
  return assert_same_file(lhs_expr, rhs_expr, std::string(lhs_path).c_str(),
                          std::string(rhs_path).c_str());
}

inline ::testing::AssertionResult assert_same_file(
    const char* lhs_expr, const char* rhs_expr,
    const std::optional<canonical_path>& lhs_path,
    const std::string& rhs_path) {
  return assert_same_file(lhs_expr, rhs_expr,
                          lhs_path.has_value() ? lhs_path->c_str() : "",
                          rhs_path);
}

inline ::testing::AssertionResult assert_same_file(
    const char* lhs_expr, const char* rhs_expr, const canonical_path& lhs_path,
    const std::string& rhs_path) {
  return assert_same_file(lhs_expr, rhs_expr, lhs_path.c_str(), rhs_path);
}

inline ::testing::AssertionResult assert_file_does_not_exist(const char* expr,
                                                             const char* path) {
  bool exists;
#if QLJS_HAVE_STD_FILESYSTEM
  exists = std::filesystem::exists(std::filesystem::path(path));
#elif QLJS_HAVE_SYS_STAT_H
  struct ::stat s;
  if (::stat(path, &s) == 0) {
    exists = true;
  } else {
    switch (errno) {
    case ENOENT:
      exists = false;
      break;
    default:
      return ::testing::AssertionFailure()
             << "checking for existance of " << expr << " (" << path
             << ") failed: " << std::strerror(errno);
    }
  }
#else
#error "Unsupported platform"
#endif
  if (exists) {
    return ::testing::AssertionFailure()
           << expr << " (" << path << ") should not exist but it does";
  } else {
    return ::testing::AssertionSuccess();
  }
}

inline ::testing::AssertionResult assert_file_does_not_exist(
    const char* expr, const std::string& path) {
  return assert_file_does_not_exist(expr, path.c_str());
}

}

#endif

#endif

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
