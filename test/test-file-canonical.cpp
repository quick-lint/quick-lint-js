// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cassert>
#include <cerrno>
#include <chrono>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-matcher.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/temporary-directory.h>
#include <string>

#if defined(_WIN32)
// TODO(strager): This should be 1, not 0. Windows allows you to access
// 'c:\file.txt\.', for example.
#define QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS 0
#else
#define QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS 0
#endif

using ::testing::AnyOf;
using ::testing::HasSubstr;
using ::testing::Not;

namespace quick_lint_js {
namespace {
class test_file_canonical : public ::testing::Test {
 public:
  std::string make_temporary_directory() {
    temp_directory_path = quick_lint_js::make_temporary_directory();
    return temp_directory_path.value();
  }

  void set_current_working_directory(const std::string& path) {
    this->set_current_working_directory(path.c_str());
  }

  void set_current_working_directory(const char* path) {
    if (!this->old_working_directory_.has_value()) {
      this->old_working_directory_ = get_current_working_directory();
    }
    quick_lint_js::set_current_working_directory(path);
  }

 private:
  std::optional<std::string> temp_directory_path;
  std::optional<std::string> old_working_directory_;

 protected:
  void TearDown() override {
    if (this->old_working_directory_.has_value()) {
      set_current_working_directory(*this->old_working_directory_);
    }
    if (this->temp_directory_path.has_value()) {
      delete_directory_recursive(*this->temp_directory_path);
    }
  }
};

TEST_F(test_file_canonical, canonical_path_to_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file(temp_file_path, u8"hello\nworld!\n");

  canonical_path_result canonical = canonicalize_path(temp_file_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  read_file_result file_content = read_file(canonical.c_str());
  ASSERT_TRUE(file_content.ok()) << file_content.error;
  EXPECT_EQ(file_content.content, string8_view(u8"hello\nworld!\n"));
}

TEST_F(test_file_canonical, canonical_path_to_directory) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir";
  create_directory(input_path);

  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_of_empty_fails) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  canonical_path_result canonical = canonicalize_path("");
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("Invalid argument"));
}

TEST_F(test_file_canonical,
       canonical_path_to_directory_removes_trailing_slash) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir/";
  create_directory(input_path);

  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_SAME_FILE(canonical.path(), input_path);
  EXPECT_FALSE(ends_with(canonical.path(), "/"));
  EXPECT_FALSE(ends_with(canonical.path(), "\\"));
}

TEST_F(test_file_canonical, canonical_path_to_file_with_trailing_slash_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/file.txt", u8"");

  std::string input_path = temp_dir + "/file.txt/";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("file.txt"));
  EXPECT_THAT(error, AnyOf(HasSubstr("Not a directory"),
                           // TODO(strager): Improve error message.
                           HasSubstr("The filename, directory name, or volume "
                                     "label syntax is incorrect")));
}

TEST_F(test_file_canonical, canonical_path_to_non_existing_file_fails) {
  std::string temp_file_path =
      this->make_temporary_directory() + "/does-not-exist.js";

  canonical_path_result canonical = canonicalize_path(temp_file_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("does-not-exist.js"));
  EXPECT_THAT(error,
              AnyOf(HasSubstr("No such file"), HasSubstr("cannot find")));
}

TEST_F(test_file_canonical, canonical_path_with_file_parent_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/file", u8"");

  std::string input_path = temp_dir + "/file/subfile";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("file/subfile"));
  EXPECT_THAT(error, AnyOf(HasSubstr("Not a directory"),
                           HasSubstr("cannot find the path specified")));
}

TEST_F(test_file_canonical, canonical_path_removes_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");

  std::string input_path = temp_dir + "/./temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/./")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\.\\")));
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_removes_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();

  std::string input_path = temp_dir + "/.";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_FALSE(ends_with(canonical.path(), "/.")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical.path(), "\\.")) << canonical.path();
  EXPECT_SAME_FILE(canonical.path(), temp_dir);
}

TEST_F(test_file_canonical,
       canonical_path_fails_with_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/./something";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("just-a-file"));
  EXPECT_THAT(error,
              AnyOf(HasSubstr("Not a directory"),
                    HasSubstr("The system cannot find the path specified")));
}

// TODO(strager): This test is wrong if
// QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS.
TEST_F(test_file_canonical,
       canonical_path_fails_with_dot_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");
  write_file(temp_dir + "/other.txt", u8"");

  std::string input_path = temp_dir + "/just-a-file/../other.text";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("just-a-file"));
  EXPECT_THAT(error,
              AnyOf(HasSubstr("Not a directory"),
                    HasSubstr("No such file or directory"),
                    HasSubstr("The system cannot find the file specified")));
}

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(test_file_canonical,
       canonical_path_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_FALSE(ends_with(canonical.path(), "/..")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical.path(), "\\..")) << canonical.path();
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("fake-subdir")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/just-a-file");
}
#else
TEST_F(test_file_canonical,
       canonical_path_fails_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("just-a-file"));
  EXPECT_THAT(error,
              AnyOf(HasSubstr("Not a directory"),
                    HasSubstr("The system cannot find the path specified")));
}
#endif

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(test_file_canonical, canonical_path_with_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/.";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_FALSE(ends_with(canonical.path(), "/.")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical.path(), "\\.")) << canonical.path();
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/just-a-file");
}
#else
TEST_F(test_file_canonical,
       canonical_path_fails_with_trailing_dot_component_for_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/.";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("just-a-file"));
  EXPECT_THAT(error,
              AnyOf(HasSubstr("Not a directory"),
                    HasSubstr("The system cannot find the path specified")));
}
#endif

TEST_F(test_file_canonical, canonical_path_removes_redundant_slashes) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");

  std::string input_path;
  for (char c : temp_dir) {
    if (c == '/' || c == '\\') {
      // POSIX: Doubling the root '/' changes the path to implementation-defined
      // mode. We don't want to enter this mode, so avoid doubling the root '/'.
      //
      // Win32: Doubling an initial '\' can convert a drive-relative path (e.g.
      // \Windows\System32) to a share path (\\server\share\file.txt). We don't
      // want to change what the path refers to, so avoid doubling initial '\'s.
      bool allowed_to_double =
          input_path.find_first_not_of("/\\") != std::string::npos;
      if (allowed_to_double) {
        input_path.push_back(c);
      }
    }
    input_path.push_back(c);
  }

  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  // TODO(strager): Don't fail if // or \\ is at the beginning of a path.
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("//")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\\\")));
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_removes_dot_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  write_file(temp_dir + "/temp.js", u8"");

  std::string input_path = temp_dir + "/dir/../temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/../")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\..\\")));
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_removes_dot_and_dot_dot_components_from_root) {
  for (std::string component : {".", ".."}) {
    std::string temp_dir = this->make_temporary_directory();
    write_file(temp_dir + "/temp.js", u8"");

    bool is_definitely_root_relative_path;
#if QLJS_HAVE_STD_FILESYSTEM
    is_definitely_root_relative_path =
        std::filesystem::path(temp_dir).is_absolute();
#else
    is_definitely_root_relative_path =
        (temp_dir[0] == '/' || temp_dir[0] == '\\') &&
        !(temp_dir[1] == '/' || temp_dir[1] == '\\');
#endif
    if (!is_definitely_root_relative_path) {
      GTEST_SKIP();
    }

    std::string input_path = temp_dir + "/temp.js";
    input_path.replace(input_path.find_first_of("/\\") + 1, 0, component + "/");
    SCOPED_TRACE(input_path);

    canonical_path_result canonical = canonicalize_path(input_path);
    ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

    EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/./")));
    EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\.\\")));
    EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/../")));
    EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\..\\")));
    EXPECT_SAME_FILE(canonical.path(), input_path);
  }
}

TEST_F(test_file_canonical, canonical_path_makes_relative_paths_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");
  this->set_current_working_directory(temp_dir);

  std::string input_path = "temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_NE(std::string(canonical.path()), "temp.js");
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_makes_relative_paths_with_dot_dot_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  write_file(temp_dir + "/temp.js", u8"");
  this->set_current_working_directory(temp_dir + "/dir");

  std::string input_path = "../temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("../")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("..\\")));
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_of_dot_does_not_have_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  canonical_path_result canonical = canonicalize_path(".");
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir);
}

TEST_F(test_file_canonical, canonical_path_with_root_as_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");
#if defined(_WIN32)
  EXPECT_EQ(temp_dir[1], ':')
      << "Expected ':' after drive letter in " << temp_dir;
  EXPECT_EQ(temp_dir[2], '\\')
      << "Expected '\\' after drive letter and colon in " << temp_dir;
  this->set_current_working_directory(temp_dir.substr(0, 3));  // e.g. 'C:\'.
  std::string input_path = temp_dir.substr(3) + "/temp.js";
#else
  EXPECT_EQ(temp_dir[0], '/')
      << "Expected temp dir to be absolute: " << temp_dir;
  EXPECT_NE(temp_dir[1], '/')
      << "Expected temp dir to not be implementation-specific path: "
      << temp_dir;
  this->set_current_working_directory("/");
  std::string input_path = temp_dir.substr(1) + "/temp.js";
#endif

  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

// TODO(strager): Test symlinks on Windows too.
#if QLJS_HAVE_UNISTD_H
TEST_F(test_file_canonical, canonical_path_resolves_file_absolute_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/realfile", u8"");
  ASSERT_EQ(::symlink((temp_dir + "/realfile").c_str(),
                      (temp_dir + "/linkfile").c_str()),
            0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/linkfile";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realfile");
}

TEST_F(test_file_canonical, canonical_path_resolves_file_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/realfile", u8"");
  ASSERT_EQ(::symlink("realfile", (temp_dir + "/linkfile").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/linkfile";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realfile");
}

TEST_F(test_file_canonical,
       canonical_path_resolves_directory_absolute_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/realdir");
  ASSERT_EQ(::symlink((temp_dir + "/realdir").c_str(),
                      (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  write_file(temp_dir + "/realdir/temp.js", u8"");

  std::string input_path = temp_dir + "/linkdir/temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_resolves_directory_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/realdir/temp.js", u8"");

  std::string input_path = temp_dir + "/linkdir/temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_resolves_dot_dot_with_directory_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  create_directory(temp_dir + "/dir/subdir");
  ASSERT_EQ(::symlink((temp_dir + "/dir/subdir").c_str(),
                      (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  write_file(temp_dir + "/dir/temp.js", u8"");

  std::string input_path = temp_dir + "/linkdir/../temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/dir/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_resolves_dot_dot_inside_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  create_directory(temp_dir + "/otherdir");
  ASSERT_EQ(::symlink("../otherdir", (temp_dir + "/dir/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/otherdir/temp.js", u8"");

  std::string input_path = temp_dir + "/dir/linkdir/temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/otherdir/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_fails_with_symlink_loop_in_directory) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("link1", (temp_dir + "/link2").c_str()), 0)
      << std::strerror(errno);
  ASSERT_EQ(::symlink("link2", (temp_dir + "/link1").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/link1/file.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("link1/file.js"));
  EXPECT_THAT(error, HasSubstr("Too many levels"));
}

TEST_F(test_file_canonical,
       canonical_path_with_broken_symlink_directory_fails) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink/file.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("testlink"));
  EXPECT_THAT(error, HasSubstr("does-not-exist"));
  EXPECT_THAT(error, HasSubstr("No such file or directory"));
}

TEST_F(test_file_canonical, canonical_path_with_broken_symlink_file_fails) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink";
  canonical_path_result canonical = canonicalize_path(input_path);
  EXPECT_FALSE(canonical.ok());
  std::string error = std::move(canonical).error();
  EXPECT_THAT(error, HasSubstr("testlink"));
  EXPECT_THAT(error, HasSubstr("No such file or directory"));
}

TEST_F(test_file_canonical, canonical_path_resolves_symlinks_in_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/realdir/temp.js", u8"");
  this->set_current_working_directory(temp_dir + "/linkdir");

  std::string input_path = "temp.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_resolves_symlink_pointing_to_symlink) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("otherlinkdir/subdir", (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/otherlinkdir").c_str()), 0)
      << std::strerror(errno);
  create_directory(temp_dir + "/realdir");
  create_directory(temp_dir + "/realdir/subdir");
  write_file(temp_dir + "/realdir/subdir/hello.js", u8"");

  std::string input_path = temp_dir + "/linkdir/hello.js";
  canonical_path_result canonical = canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << std::move(canonical).error();

  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_THAT(std::string(canonical.path()), Not(HasSubstr("/otherlinkdir/")));
  EXPECT_THAT(std::string(canonical.path()),
              Not(HasSubstr("\\otherlinkdir\\")));
  EXPECT_SAME_FILE(canonical.path(), temp_dir + "/realdir/subdir/hello.js");
  EXPECT_SAME_FILE(canonical.path(), input_path);
}
#endif

#if defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L
TEST_F(test_file_canonical, canonical_path_posix_root) {
  {
    canonical_path_result canonical = canonicalize_path("/");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "/");
  }

  {
    canonical_path_result canonical = canonicalize_path("/.");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "/");
  }

  {
    canonical_path_result canonical = canonicalize_path("/..");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "/");
  }

  {
    canonical_path_result canonical = canonicalize_path("/../../../../..");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "/");
  }
}
#endif

#if defined(_WIN32)
TEST_F(test_file_canonical, canonical_path_win32_root) {
  // NOTE(strager): These tests assume that a drive named 'C' exists.

  {
    canonical_path_result canonical = canonicalize_path("c:/");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "c:\\");
  }

  {
    canonical_path_result canonical = canonicalize_path("c:/.");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "c:\\");
  }

  {
    canonical_path_result canonical = canonicalize_path("c:/..");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "c:\\");
  }

  {
    canonical_path_result canonical = canonicalize_path("c:/../../../../..");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "c:\\");
  }

  {
    canonical_path_result canonical = canonicalize_path(R"(c:\\\\\\\\\\\\)");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), "c:\\");
  }

  {
    canonical_path_result canonical = canonicalize_path(R"(\\?\C:\)");
    EXPECT_TRUE(canonical.ok());
    EXPECT_EQ(canonical.path(), R"(\\?\C:\)");
  }
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
