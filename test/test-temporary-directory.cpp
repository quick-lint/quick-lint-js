// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/file-matcher.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/permissions.h>
#include <quick-lint-js/temporary-directory.h>
#include <string>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_UNISTD_H
TEST(test_temporary_directory, delete_directory_containing_unwritable_file) {
  std::string temp_dir = make_temporary_directory();
  std::string sub_dir = temp_dir + "/subdir";
  create_directory_or_exit(sub_dir);
  std::string unwritable_file = sub_dir + "/unwritable";
  write_file_or_exit(unwritable_file, u8"unwritable file");
  EXPECT_EQ(::chmod(unwritable_file.c_str(), 0000), 0)
      << "failed to make " << unwritable_file
      << " inaccessible: " << std::strerror(errno);

  delete_directory_recursive(sub_dir);

  EXPECT_FILE_DOES_NOT_EXIST(unwritable_file);
  EXPECT_FILE_DOES_NOT_EXIST(sub_dir);
}

TEST(test_temporary_directory,
     delete_directory_containing_non_empty_untraversable_directory) {
  std::string temp_dir = make_temporary_directory();
  std::string sub_dir = temp_dir + "/sub_dir";
  create_directory_or_exit(sub_dir);
  std::string untraversable_dir = sub_dir + "/untraversable_dir";
  create_directory_or_exit(untraversable_dir);
  std::string unfindable_file = untraversable_dir + "/unfindable_file";
  write_file_or_exit(unfindable_file, u8"can't see me!");
  EXPECT_EQ(::chmod(untraversable_dir.c_str(), 0600), 0)
      << "failed to make " << untraversable_dir
      << " untraversable: " << std::strerror(errno);

  delete_directory_recursive(sub_dir);

  EXPECT_FILE_DOES_NOT_EXIST(unfindable_file);
  EXPECT_FILE_DOES_NOT_EXIST(untraversable_dir);
  EXPECT_FILE_DOES_NOT_EXIST(sub_dir);
}
#endif

TEST(test_temporary_directory,
     creating_directory_over_existing_directory_fails) {
  std::string temp_dir = make_temporary_directory();

  std::string sub_dir = temp_dir + "/sub_dir";
  auto result = create_directory(sub_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  auto result_2 = create_directory(sub_dir);
  ASSERT_FALSE(result_2.ok());
  EXPECT_TRUE(result_2.error().is_directory_already_exists_error);
}

TEST(test_temporary_directory, creating_directory_over_existing_file_fails) {
  std::string temp_dir = make_temporary_directory();

  std::string file = temp_dir + "/file";
  write_file_or_exit(file, u8"hello");

  auto result_2 = create_directory(file);
  ASSERT_FALSE(result_2.ok());
  EXPECT_FALSE(result_2.error().is_directory_already_exists_error);
}

#if QLJS_HAVE_UNISTD_H
TEST(test_temporary_directory,
     creating_directory_in_unwritable_directory_fails) {
  if (process_ignores_filesystem_permissions()) {
    GTEST_SKIP() << "cannot run test as root";
  }

  std::string temp_dir = make_temporary_directory();

  EXPECT_EQ(::chmod(temp_dir.c_str(), 0555), 0)
      << "failed to make " << temp_dir
      << " unwritable: " << std::strerror(errno);

  auto result = create_directory(temp_dir + "/sub_dir");
  ASSERT_FALSE(result.ok());
  EXPECT_FALSE(result.error().is_directory_already_exists_error);
}
#endif
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
