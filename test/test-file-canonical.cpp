// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/leaf/common.hpp>
#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/result.hpp>
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
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-matcher.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/leaf.h>
#include <quick-lint-js/sloppy-result.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/temporary-directory.h>
#include <string>
#include <tuple>

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

template <class Path>
boost::leaf::result<void> canonicalize_expecting_failure(const Path& path) {
  boost::leaf::result<canonical_path_result> canonical =
      canonicalize_path(path);
  if (!canonical) return canonical.error();
  ADD_FAILURE() << "canonicalize_path should have failed";
  return {};
}

auto fail_test_error_handlers() {
  return std::tuple(
      make_canonicalize_path_error_handlers([](const std::string& message) {
        ADD_FAILURE() << "error: " << message;
      }),
      []() { ADD_FAILURE() << "unknown error"; });
}

bool process_ignores_filesystem_permissions() noexcept {
#if QLJS_HAVE_UNISTD_H
  return ::geteuid() == 0;
#else
  return false;
#endif
}

TEST_F(test_file_canonical, canonical_path_to_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file(temp_file_path, u8"hello\nworld!\n");

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_file_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();
  EXPECT_FALSE(canonical->have_missing_components());

  sloppy_result<padded_string> file_content =
      read_file_sloppy(canonical->c_str());
  ASSERT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"hello\nworld!\n"));
}

TEST_F(test_file_canonical, canonical_path_to_directory) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir";
  create_directory(input_path);

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_SAME_FILE(canonical->path(), input_path);
  EXPECT_FALSE(canonical->have_missing_components());
}

TEST_F(test_file_canonical, canonical_path_of_empty_fails) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  boost::leaf::try_handle_all(
      [] { return canonicalize_expecting_failure(""); },
      [](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
         e_invalid_argument_empty_path) { EXPECT_EQ(path.value, ""); },
      fail_test_error_handlers());
}

TEST_F(test_file_canonical,
       canonical_path_to_directory_removes_trailing_slash) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir/";
  create_directory(input_path);

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_SAME_FILE(canonical->path(), input_path);
  EXPECT_FALSE(ends_with(canonical->path(), "/"));
  EXPECT_FALSE(ends_with(canonical->path(), "\\"));
}

TEST_F(test_file_canonical, canonical_path_to_file_with_trailing_slash_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/file.txt", u8"");

  std::string input_path = temp_dir + "/file.txt/";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("file.txt"));
      },
      fail_test_error_handlers());
}

TEST_F(test_file_canonical, canonical_path_to_non_existing_file_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist.js");
  EXPECT_TRUE(canonical.ok()) << canonical.error();
  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist.js");

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(test_file_canonical,
       canonical_path_to_non_existing_parent_directory_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist/file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error();
  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(test_file_canonical, canonical_path_with_file_parent_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/file", u8"");

  std::string input_path = temp_dir + "/file/subfile";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("file"));
      },
      fail_test_error_handlers());
}

TEST_F(test_file_canonical, canonical_path_removes_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");

  std::string input_path = temp_dir + "/./temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_removes_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();

  std::string input_path = temp_dir + "/.";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_FALSE(ends_with(canonical->path(), "/.")) << canonical->path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\.")) << canonical->path();
  EXPECT_SAME_FILE(canonical->path(), temp_dir);
}

TEST_F(test_file_canonical,
       canonical_path_fails_with_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/./something";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("just-a-file"));
      },
      fail_test_error_handlers());
}

TEST_F(test_file_canonical,
       canonical_path_removes_dot_components_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist/./file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
}

// TODO(strager): This test is wrong if
// QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS.
TEST_F(test_file_canonical,
       canonical_path_fails_with_dot_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");
  write_file(temp_dir + "/other.txt", u8"");

  std::string input_path = temp_dir + "/just-a-file/../other.text";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("just-a-file"));
      },
      fail_test_error_handlers());
}

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(test_file_canonical,
       canonical_path_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_FALSE(ends_with(canonical->path(), "/..")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\..")) << canonical.path();
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("fake-subdir")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/just-a-file");
}
#else
TEST_F(test_file_canonical,
       canonical_path_fails_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("just-a-file"));
      },
      fail_test_error_handlers());
}
#endif

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(test_file_canonical, canonical_path_with_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/.";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_FALSE(ends_with(canonical->path(), "/.")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\.")) << canonical.path();
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/just-a-file");
}
#else
TEST_F(test_file_canonical,
       canonical_path_fails_with_trailing_dot_component_for_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/just-a-file", u8"");

  std::string input_path = temp_dir + "/just-a-file/.";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, ENOTDIR) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("just-a-file"));
      },
      fail_test_error_handlers());
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

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  // TODO(strager): Don't fail if // or \\ is at the beginning of a path.
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("//")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_removes_redundant_slashes_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist///file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  // TODO(strager): Don't fail if // or \\ is at the beginning of a path.
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("//")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\\\")));
}

TEST_F(test_file_canonical,
       canonical_path_removes_trailing_slash_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist/");
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist");
}

TEST_F(test_file_canonical, canonical_path_removes_dot_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  write_file(temp_dir + "/temp.js", u8"");

  std::string input_path = temp_dir + "/dir/../temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/../")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\..\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
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

    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy(input_path);
    ASSERT_TRUE(canonical.ok()) << canonical.error();

    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/../")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\..\\")));
    EXPECT_SAME_FILE(canonical->path(), input_path);
  }
}

TEST_F(test_file_canonical,
       canonical_path_keeps_dot_dot_components_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();
  write_file(temp_dir + "/real.js", u8"");

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(temp_dir + "/does-not-exist/../real.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                ".." QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "real.js");
  EXPECT_TRUE(canonical->have_missing_components());
}

TEST_F(test_file_canonical, canonical_path_makes_relative_paths_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/temp.js", u8"");
  this->set_current_working_directory(temp_dir);

  std::string input_path = "temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_NE(std::string(canonical->path()), "temp.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_makes_relative_paths_with_dot_dot_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  write_file(temp_dir + "/temp.js", u8"");
  this->set_current_working_directory(temp_dir + "/dir");

  std::string input_path = "../temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("../")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("..\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_of_dot_does_not_have_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(".");
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir);
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

  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
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
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realfile");
}

TEST_F(test_file_canonical, canonical_path_resolves_file_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/realfile", u8"");
  ASSERT_EQ(::symlink("realfile", (temp_dir + "/linkfile").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/linkfile";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realfile");
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
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_resolves_directory_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/realdir/temp.js", u8"");

  std::string input_path = temp_dir + "/linkdir/temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
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
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/dir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical, canonical_path_resolves_dot_dot_inside_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  create_directory(temp_dir + "/otherdir");
  ASSERT_EQ(::symlink("../otherdir", (temp_dir + "/dir/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/otherdir/temp.js", u8"");

  std::string input_path = temp_dir + "/dir/linkdir/temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/otherdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(test_file_canonical,
       canonical_path_fails_with_symlink_loop_in_directory) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("link1", (temp_dir + "/link2").c_str()), 0)
      << std::strerror(errno);
  ASSERT_EQ(::symlink("link2", (temp_dir + "/link1").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/link1/file.js";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          e_too_many_symlinks) { EXPECT_EQ(path.value, input_path); },
      fail_test_error_handlers());
}

TEST_F(test_file_canonical,
       canonical_path_with_broken_symlink_directory_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink/file.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("testlink")));

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(test_file_canonical, canonical_path_with_broken_symlink_file_fails) {
  std::string temp_dir = this->make_temporary_directory();
  sloppy_result<canonical_path_result> temp_dir_canonical =
      canonicalize_path_sloppy(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok()) << temp_dir_canonical.error();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("testlink")));

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(test_file_canonical, canonical_path_resolves_symlinks_in_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file(temp_dir + "/realdir/temp.js", u8"");
  this->set_current_working_directory(temp_dir + "/linkdir");

  std::string input_path = "temp.js";
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
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
  sloppy_result<canonical_path_result> canonical =
      canonicalize_path_sloppy(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/otherlinkdir/")));
  EXPECT_THAT(std::string(canonical->path()),
              Not(HasSubstr("\\otherlinkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/subdir/hello.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}
#endif

#if QLJS_HAVE_UNISTD_H
TEST_F(test_file_canonical, unsearchable_parent_directory) {
  if (process_ignores_filesystem_permissions()) {
    GTEST_SKIP() << "cannot run test as root";
  }

  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  write_file(temp_dir + "/dir/file", u8"hello");
  ASSERT_EQ(::chmod((temp_dir + "/dir").c_str(), 0600), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/dir/file";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, EACCES) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("dir/file"));
      },
      fail_test_error_handlers());

  // Allow test cleanup to delete the directory.
  EXPECT_EQ(::chmod((temp_dir + "/dir").c_str(), 0700), 0)
      << std::strerror(errno);
}

TEST_F(test_file_canonical, unsearchable_grandparent_directory) {
  if (process_ignores_filesystem_permissions()) {
    GTEST_SKIP() << "cannot run test as root";
  }

  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  create_directory(temp_dir + "/dir/subdir");
  write_file(temp_dir + "/dir/subdir/file", u8"hello");
  ASSERT_EQ(::chmod((temp_dir + "/dir").c_str(), 0600), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/dir/subdir/file";
  boost::leaf::try_handle_all(
      [&] { return canonicalize_expecting_failure(input_path); },
      [&](e_api_canonicalize_path, const boost::leaf::e_file_name& path,
          const e_canonicalizing_path& canonicalizing, e_errno error) {
        EXPECT_EQ(path.value, input_path);
        EXPECT_EQ(error.error, EACCES) << std::strerror(error.error);
        EXPECT_THAT(canonicalizing.path, ::testing::EndsWith("dir/subdir"));
      },
      fail_test_error_handlers());

  // Allow test cleanup to delete the directory.
  EXPECT_EQ(::chmod((temp_dir + "/dir").c_str(), 0700), 0)
      << std::strerror(errno);
}
#endif

#if defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L
TEST_F(test_file_canonical, canonical_path_posix_root) {
  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("/");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("/.");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("/..");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("/../../../../..");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "/");
  }
}
#endif

#if defined(_WIN32)
TEST_F(test_file_canonical, canonical_path_win32_root) {
  // NOTE(strager): These tests assume that a drive named 'C' exists.

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("c:/");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("c:/.");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("c:/..");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy("c:/../../../../..");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy(R"(c:\\\\\\\\\\\\)");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    sloppy_result<canonical_path_result> canonical =
        canonicalize_path_sloppy(R"(\\?\C:\)");
    ASSERT_TRUE(canonical.ok()) << canonical.error();
    EXPECT_EQ(canonical->path(), R"(\\?\C:\)");
  }
}
#endif

#if defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L
TEST_F(test_file_canonical, parent_of_root_is_root_posix) {
  for (const char* root : {"/", "//"}) {
    canonical_path p(root);
    EXPECT_FALSE(p.parent());
    EXPECT_EQ(p.path(), root);
  }
}

TEST_F(test_file_canonical, parent_of_non_root_removes_component_posix) {
  struct test_case {
    const std::string_view before;
    const std::string_view after;
  };

  for (const test_case& tc : {
           test_case{"/hello", "/"},
           test_case{"/dir/subdir", "/dir"},
           test_case{"/a/b/c/d", "/a/b/c"},
           test_case{"//implementation-defined", "//"},
           test_case{"//implementation/defined", "//implementation"},
       }) {
    canonical_path p(std::string(tc.before));
    EXPECT_TRUE(p.parent());
    EXPECT_EQ(p.path(), tc.after) << "before = " << tc.before;
  }
}

TEST_F(test_file_canonical, append_component_posix) {
  struct test_case {
    const std::string_view before;
    const std::string_view component;
    const std::string_view after;
  };

  for (const test_case& tc : {
           test_case{"/", "hello", "/hello"},
           test_case{"/dir", "subdir", "/dir/subdir"},
           test_case{"//", "hello", "//hello"},
           test_case{"//dir", "subdir", "//dir/subdir"},
       }) {
    canonical_path p(std::string(tc.before));
    p.append_component(tc.component);
    EXPECT_EQ(p.path(), tc.after)
        << "before = " << tc.before << "\ncomponent = " << tc.component;
  }
}
#endif

#if defined(_WIN32)
TEST_F(test_file_canonical, parent_of_root_is_root_win32) {
  for (const char* root : {R"(C:\)", R"(\\?\X:\)", R"(\\server\share)"}) {
    canonical_path p(root);
    EXPECT_FALSE(p.parent());
    EXPECT_EQ(p.path(), root);
  }
}

TEST_F(test_file_canonical, parent_of_non_root_removes_component_win32) {
  struct test_case {
    const std::string_view before;
    const std::string_view after;
  };

  for (const test_case& tc : {
           test_case{R"(C:\hello)", R"(C:\)"},
           test_case{R"(C:\dir\subdir)", R"(C:\dir)"},
           test_case{R"(C:\a\b\c\d)", R"(C:\a\b\c)"},
           test_case{R"(\\?\X:\hello)", R"(\\?\X:\)"},
           test_case{R"(\\?\X:\dir\subdir)", R"(\\?\X:\dir)"},
           test_case{R"(\\?\X:\a\b\c\d)", R"(\\?\X:\a\b\c)"},
           test_case{R"(\\server\share\file)", R"(\\server\share)"},
       }) {
    canonical_path p(std::string(tc.before));
    EXPECT_TRUE(p.parent());
    EXPECT_EQ(p.path(), tc.after) << "before = " << tc.before;
  }
}

TEST_F(test_file_canonical, append_component_win32) {
  struct test_case {
    const std::string_view before;
    const std::string_view component;
    const std::string_view after;
  };

  for (const test_case& tc : {
           test_case{R"(C:\)", R"(hello)", R"(C:\hello)"},
           test_case{R"(C:\dir)", R"(subdir)", R"(C:\dir\subdir)"},
           test_case{R"(\\?\X:\)", R"(hello)", R"(\\?\X:\hello)"},
           test_case{R"(\\?\X:\dir)", R"(subdir)", R"(\\?\X:\dir\subdir)"},
           test_case{R"(\\server\share)", R"(dir)", R"(\\server\share\dir)"},
       }) {
    canonical_path p(std::string(tc.before));
    p.append_component(tc.component);
    EXPECT_EQ(p.path(), tc.after)
        << "before = " << tc.before << "\ncomponent = " << tc.component;
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
