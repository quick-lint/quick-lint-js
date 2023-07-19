// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

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
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/file-matcher.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
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
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
class Test_File_Canonical : public ::testing::Test,
                            protected Filesystem_Test {};

bool process_ignores_filesystem_permissions() noexcept {
#if QLJS_HAVE_UNISTD_H
  return ::geteuid() == 0;
#else
  return false;
#endif
}

TEST_F(Test_File_Canonical, canonical_path_to_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file_or_exit(temp_file_path, u8"hello\nworld!\n"_sv);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_file_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
  EXPECT_FALSE(canonical->have_missing_components());

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(canonical->c_str());
  ASSERT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello\nworld!\n"_sv);
}

TEST_F(Test_File_Canonical, canonical_path_to_directory) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir";
  create_directory_or_exit(input_path);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_SAME_FILE(canonical->path(), input_path);
  EXPECT_FALSE(canonical->have_missing_components());
}

TEST_F(Test_File_Canonical, canonical_path_of_empty_fails) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path("");
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, "");
  EXPECT_EQ(canonical.error().canonicalizing_path, "");
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_INVALID_PARAMETER);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, EINVAL);
#endif
}

TEST_F(Test_File_Canonical,
       canonical_path_to_directory_removes_trailing_slash) {
  std::string temp_dir = this->make_temporary_directory();
  std::string input_path = temp_dir + "/dir/";
  create_directory_or_exit(input_path);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_SAME_FILE(canonical->path(), input_path);
  EXPECT_FALSE(ends_with(canonical->path(), "/"sv));
  EXPECT_FALSE(ends_with(canonical->path(), "\\"sv));
}

TEST_F(Test_File_Canonical, canonical_path_to_file_with_trailing_slash_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/file.txt", u8""_sv);

  std::string input_path = temp_dir + "/file.txt/";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("file.txt"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}

TEST_F(Test_File_Canonical, canonical_path_to_non_existing_file_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist.js");

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(Test_File_Canonical,
       parent_of_present_canonical_components_of_non_existing_file) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  create_directory_or_exit(temp_dir + "/dir");
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalized =
      canonicalize_path(temp_dir + "/dir/does-not-exist.txt");
  ASSERT_TRUE(canonicalized.ok()) << canonicalized.error().to_string();
  canonicalized->drop_missing_components();

  Canonical_Path canonical = std::move(*canonicalized).canonical();
  canonical.parent();
  EXPECT_EQ(canonical.path(), temp_dir_canonical->path());
}

TEST_F(Test_File_Canonical,
       canonical_path_to_non_existing_parent_directory_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist/file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(Test_File_Canonical,
       canonical_path_to_non_existing_file_with_non_ascii_succeeds) {
  for (String8_View character8 : {u8"\u00e0"_sv, u8"\u0800"_sv}) {
    std::string character = to_string(character8);
    SCOPED_TRACE(character);

    std::string temp_dir = this->make_temporary_directory();
    create_directory_or_exit(temp_dir + "/parent" + character + "dir");
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
        parent_dir_canonical =
            canonicalize_path(temp_dir + "/parent" + character + "dir");
    ASSERT_TRUE(parent_dir_canonical.ok())
        << parent_dir_canonical.error().to_string();

    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path(temp_dir + "/parent" + character + "dir/does-not-" +
                          character + "exist/file" + character + "name.txt");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

    EXPECT_TRUE(canonical->have_missing_components());
    canonical->drop_missing_components();
    EXPECT_EQ(canonical->path(), parent_dir_canonical->path());
  }
}

TEST_F(Test_File_Canonical, canonical_path_with_file_parent_fails) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/file", u8""_sv);

  std::string input_path = temp_dir + "/file/subfile";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("file"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}

TEST_F(Test_File_Canonical, canonical_path_removes_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/./temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical, canonical_path_removes_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();

  std::string input_path = temp_dir + "/.";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_FALSE(ends_with(canonical->path(), "/."sv)) << canonical->path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\."sv)) << canonical->path();
  EXPECT_SAME_FILE(canonical->path(), temp_dir);
}

TEST_F(Test_File_Canonical,
       canonical_path_fails_with_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/./something";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("just-a-file"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}

TEST_F(Test_File_Canonical,
       canonical_path_removes_dot_components_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist/./file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
}

// TODO(strager): This test is wrong if
// QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS.
TEST_F(Test_File_Canonical,
       canonical_path_fails_with_dot_dot_component_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);
  write_file_or_exit(temp_dir + "/other.txt", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/../other.text";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("just-a-file"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(Test_File_Canonical,
       canonical_path_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_FALSE(ends_with(canonical->path(), "/..")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\..")) << canonical.path();
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("fake-subdir")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/just-a-file");
}
#else
TEST_F(Test_File_Canonical,
       canonical_path_fails_with_component_and_dot_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/fake-subdir/..";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("just-a-file"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}
#endif

#if QLJS_FILE_PATH_ALLOWS_FOLLOWING_COMPONENTS
TEST_F(Test_File_Canonical, canonical_path_with_dot_after_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/.";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_FALSE(ends_with(canonical->path(), "/.")) << canonical.path();
  EXPECT_FALSE(ends_with(canonical->path(), "\\.")) << canonical.path();
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/just-a-file");
}
#else
TEST_F(Test_File_Canonical,
       canonical_path_fails_with_trailing_dot_component_for_regular_file) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/just-a-file", u8""_sv);

  std::string input_path = temp_dir + "/just-a-file/.";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("just-a-file"));
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_DIRECTORY);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ENOTDIR);
#endif
}
#endif

TEST_F(Test_File_Canonical, canonical_path_removes_redundant_slashes) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);

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

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  // TODO(strager): Don't fail if // or \\ is at the beginning of a path.
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("//")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_removes_redundant_slashes_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist///file.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  // TODO(strager): Don't fail if // or \\ is at the beginning of a path.
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("//")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\\\")));
}

TEST_F(Test_File_Canonical,
       canonical_path_removes_trailing_slash_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist/");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist");
}

TEST_F(Test_File_Canonical, canonical_path_removes_dot_dot_components) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/dir/../temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/../")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\..\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_removes_dot_and_dot_dot_components_from_root) {
  for (std::string component : {".", ".."}) {
    std::string temp_dir = this->make_temporary_directory();
    write_file_or_exit(temp_dir + "/temp.js", u8""_sv);

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

    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path(input_path);
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/./")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\.\\")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/../")));
    EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\..\\")));
    EXPECT_SAME_FILE(canonical->path(), input_path);
  }
}

TEST_F(Test_File_Canonical,
       canonical_path_keeps_dot_dot_components_after_missing_path) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();
  write_file_or_exit(temp_dir + "/real.js", u8""_sv);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(temp_dir + "/does-not-exist/../real.js");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                ".." QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "real.js");
  EXPECT_TRUE(canonical->have_missing_components());
}

TEST_F(Test_File_Canonical, canonical_path_makes_relative_paths_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);
  this->set_current_working_directory(temp_dir);

  std::string input_path = "temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_NE(std::string(canonical->path()), "temp.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_makes_relative_paths_with_dot_dot_absolute) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);
  this->set_current_working_directory(temp_dir + "/dir");

  std::string input_path = "../temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("../")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("..\\")));
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_of_dot_does_not_have_trailing_dot_component) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(".");
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("./")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr(".\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir);
}

TEST_F(Test_File_Canonical, canonical_path_with_root_as_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/temp.js", u8""_sv);
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

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

// TODO(strager): Test symlinks on Windows too.
#if QLJS_HAVE_UNISTD_H
TEST_F(Test_File_Canonical, canonical_path_resolves_file_absolute_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/realfile", u8""_sv);
  ASSERT_EQ(::symlink((temp_dir + "/realfile").c_str(),
                      (temp_dir + "/linkfile").c_str()),
            0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/linkfile";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realfile");
}

TEST_F(Test_File_Canonical, canonical_path_resolves_file_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  write_file_or_exit(temp_dir + "/realfile", u8""_sv);
  ASSERT_EQ(::symlink("realfile", (temp_dir + "/linkfile").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/linkfile";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realfile"), HasSubstr("\\realfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkfile")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkfile")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realfile");
}

TEST_F(Test_File_Canonical,
       canonical_path_resolves_directory_absolute_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/realdir");
  ASSERT_EQ(::symlink((temp_dir + "/realdir").c_str(),
                      (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  write_file_or_exit(temp_dir + "/realdir/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/linkdir/temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_resolves_directory_relative_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file_or_exit(temp_dir + "/realdir/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/linkdir/temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()),
              AnyOf(HasSubstr("/realdir/"), HasSubstr("\\realdir\\")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_resolves_dot_dot_with_directory_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  create_directory_or_exit(temp_dir + "/dir/subdir");
  ASSERT_EQ(::symlink((temp_dir + "/dir/subdir").c_str(),
                      (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  write_file_or_exit(temp_dir + "/dir/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/linkdir/../temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/dir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical, canonical_path_resolves_dot_dot_inside_symlinks) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  create_directory_or_exit(temp_dir + "/otherdir");
  ASSERT_EQ(::symlink("../otherdir", (temp_dir + "/dir/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file_or_exit(temp_dir + "/otherdir/temp.js", u8""_sv);

  std::string input_path = temp_dir + "/dir/linkdir/temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/otherdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_fails_with_symlink_loop_in_directory) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("link1", (temp_dir + "/link2").c_str()), 0)
      << std::strerror(errno);
  ASSERT_EQ(::symlink("link2", (temp_dir + "/link1").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/link1/file.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
#if QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(canonical.error().io_error.error, ERROR_CANT_RESOLVE_FILENAME);
#endif
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(canonical.error().io_error.error, ELOOP);
#endif
}

TEST_F(Test_File_Canonical,
       canonical_path_with_broken_symlink_directory_succeeds) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink/file.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(),
            std::string(temp_dir_canonical->path()) +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "does-not-exist" +
                QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "file.js");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("testlink")));

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(Test_File_Canonical, canonical_path_with_broken_symlink_file_fails) {
  std::string temp_dir = this->make_temporary_directory();
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> temp_dir_canonical =
      canonicalize_path(temp_dir);
  ASSERT_TRUE(temp_dir_canonical.ok())
      << temp_dir_canonical.error().to_string();
  ASSERT_EQ(::symlink("does-not-exist", (temp_dir + "/testlink").c_str()), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/testlink";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_EQ(canonical->path(), std::string(temp_dir_canonical->path()) +
                                   QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR +
                                   "does-not-exist");
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("testlink")));

  EXPECT_TRUE(canonical->have_missing_components());
  canonical->drop_missing_components();
  EXPECT_EQ(canonical->path(), temp_dir_canonical->path());
}

TEST_F(Test_File_Canonical, canonical_path_resolves_symlinks_in_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/realdir");
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/linkdir").c_str()), 0)
      << std::strerror(errno);
  write_file_or_exit(temp_dir + "/realdir/temp.js", u8""_sv);
  this->set_current_working_directory(temp_dir + "/linkdir");

  std::string input_path = "temp.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("/linkdir/")));
  EXPECT_THAT(std::string(canonical->path()), Not(HasSubstr("\\linkdir\\")));
  EXPECT_SAME_FILE(canonical->path(), temp_dir + "/realdir/temp.js");
  EXPECT_SAME_FILE(canonical->path(), input_path);
}

TEST_F(Test_File_Canonical,
       canonical_path_resolves_symlink_pointing_to_symlink) {
  std::string temp_dir = this->make_temporary_directory();
  ASSERT_EQ(::symlink("otherlinkdir/subdir", (temp_dir + "/linkdir").c_str()),
            0)
      << std::strerror(errno);
  ASSERT_EQ(::symlink("realdir", (temp_dir + "/otherlinkdir").c_str()), 0)
      << std::strerror(errno);
  create_directory_or_exit(temp_dir + "/realdir");
  create_directory_or_exit(temp_dir + "/realdir/subdir");
  write_file_or_exit(temp_dir + "/realdir/subdir/hello.js", u8""_sv);

  std::string input_path = temp_dir + "/linkdir/hello.js";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();

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
TEST_F(Test_File_Canonical, unsearchable_parent_directory) {
  if (process_ignores_filesystem_permissions()) {
    GTEST_SKIP() << "cannot run test as root";
  }

  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  write_file_or_exit(temp_dir + "/dir/file", u8"hello"_sv);
  ASSERT_EQ(::chmod((temp_dir + "/dir").c_str(), 0600), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/dir/file";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("dir/file"));
  EXPECT_EQ(canonical.error().io_error.error, EACCES);
}

TEST_F(Test_File_Canonical, unsearchable_grandparent_directory) {
  if (process_ignores_filesystem_permissions()) {
    GTEST_SKIP() << "cannot run test as root";
  }

  std::string temp_dir = this->make_temporary_directory();
  create_directory_or_exit(temp_dir + "/dir");
  create_directory_or_exit(temp_dir + "/dir/subdir");
  write_file_or_exit(temp_dir + "/dir/subdir/file", u8"hello"_sv);
  ASSERT_EQ(::chmod((temp_dir + "/dir").c_str(), 0600), 0)
      << std::strerror(errno);

  std::string input_path = temp_dir + "/dir/subdir/file";
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
      canonicalize_path(input_path);
  ASSERT_FALSE(canonical.ok());
  EXPECT_EQ(canonical.error().input_path, input_path);
  EXPECT_THAT(canonical.error().canonicalizing_path,
              ::testing::EndsWith("dir/subdir"));
  EXPECT_EQ(canonical.error().io_error.error, EACCES);
}
#endif

#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
TEST_F(Test_File_Canonical, canonical_path_posix_root) {
  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("/");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("/.");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("/..");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "/");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("/../../../../..");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "/");
  }
}
#endif

#if defined(_WIN32)
TEST_F(Test_File_Canonical, canonical_path_win32_root) {
  // NOTE(strager): These tests assume that a drive named 'C' exists.

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("c:/");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("c:/.");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("c:/..");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path("c:/../../../../..");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path(R"(c:\\\\\\\\\\\\)");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), "c:\\");
  }

  {
    Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonical =
        canonicalize_path(R"(\\?\C:\)");
    ASSERT_TRUE(canonical.ok()) << canonical.error().to_string();
    EXPECT_EQ(canonical->path(), R"(\\?\C:\)");
  }
}
#endif

#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
TEST_F(Test_File_Canonical, parent_of_root_is_root_posix) {
  for (const char* root : {"/", "//"}) {
    Canonical_Path p(root);
    EXPECT_FALSE(p.parent());
    EXPECT_EQ(p.path(), root);
  }
}

TEST_F(Test_File_Canonical, parent_of_non_root_removes_component_posix) {
  struct Test_Case {
    const std::string_view before;
    const std::string_view after;
  };

  for (const Test_Case& tc : {
           Test_Case{"/hello", "/"},
           Test_Case{"/dir/subdir", "/dir"},
           Test_Case{"/a/b/c/d", "/a/b/c"},
           Test_Case{"//implementation-defined", "//"},
           Test_Case{"//implementation/defined", "//implementation"},
       }) {
    Canonical_Path p(std::string(tc.before));
    EXPECT_TRUE(p.parent());
    EXPECT_EQ(p.path(), tc.after) << "before = " << tc.before;
  }
}

TEST_F(Test_File_Canonical, append_component_posix) {
  struct Test_Case {
    const std::string_view before;
    const std::string_view component;
    const std::string_view after;
  };

  for (const Test_Case& tc : {
           Test_Case{"/", "hello", "/hello"},
           Test_Case{"/dir", "subdir", "/dir/subdir"},
           Test_Case{"//", "hello", "//hello"},
           Test_Case{"//dir", "subdir", "//dir/subdir"},
       }) {
    Canonical_Path p(std::string(tc.before));
    p.append_component(tc.component);
    EXPECT_EQ(p.path(), tc.after)
        << "before = " << tc.before << "\ncomponent = " << tc.component;
  }
}

TEST_F(Test_File_Canonical, remove_appended_component_posix) {
  Canonical_Path p("/dir/subdir");
  p.append_component("file.txt");
  p.parent();
  EXPECT_EQ(p.path(), "/dir/subdir");
}

TEST_F(Test_File_Canonical, remove_component_appended_to_root_posix) {
  Canonical_Path p("/");
  p.append_component("file.txt");
  p.parent();
  EXPECT_EQ(p.path(), "/");
}
#endif

#if defined(_WIN32)
TEST_F(Test_File_Canonical, parent_of_root_is_root_win32) {
  for (const char* root : {R"(C:\)", R"(\\?\X:\)", R"(\\server\share)"}) {
    Canonical_Path p(root);
    EXPECT_FALSE(p.parent());
    EXPECT_EQ(p.path(), root);
  }
}

TEST_F(Test_File_Canonical, parent_of_non_root_removes_component_win32) {
  struct Test_Case {
    const String8_View before;
    const String8_View after;
  };

  for (const Test_Case& tc : {
           Test_Case{u8R"(C:\hello)", u8R"(C:\)"},
           Test_Case{u8R"(C:\dir\subdir)", u8R"(C:\dir)"},
           Test_Case{u8R"(C:\a\b\c\d)", u8R"(C:\a\b\c)"},
           Test_Case{u8R"(\\?\X:\hello)", u8R"(\\?\X:\)"},
           Test_Case{u8R"(\\?\X:\dir\subdir)", u8R"(\\?\X:\dir)"},
           Test_Case{u8R"(\\?\X:\a\b\c\d)", u8R"(\\?\X:\a\b\c)"},
           Test_Case{u8R"(\\server\share\file)", u8R"(\\server\share)"},
           Test_Case{u8"X:\\parent\u0080dir\\sub\u0080dir",
                     u8"X:\\parent\u0080dir"},
           Test_Case{u8"X:\\parent\u0800dir\\sub\u0800dir",
                     u8"X:\\parent\u0800dir"},
           Test_Case{u8"X:\\parent\U00108000dir\\sub\U00108000dir",
                     u8"X:\\parent\U00108000dir"},
       }) {
    Canonical_Path p(to_string(tc.before));
    EXPECT_TRUE(p.parent());
    EXPECT_EQ(p.path(), to_string(tc.after))
        << "before = " << out_string8(tc.before);
  }
}

TEST_F(Test_File_Canonical, append_component_win32) {
  struct Test_Case {
    const std::string_view before;
    const std::string_view component;
    const std::string_view after;
  };

  for (const Test_Case& tc : {
           Test_Case{R"(C:\)", R"(hello)", R"(C:\hello)"},
           Test_Case{R"(C:\dir)", R"(subdir)", R"(C:\dir\subdir)"},
           Test_Case{R"(\\?\X:\)", R"(hello)", R"(\\?\X:\hello)"},
           Test_Case{R"(\\?\X:\dir)", R"(subdir)", R"(\\?\X:\dir\subdir)"},
           Test_Case{R"(\\server\share)", R"(dir)", R"(\\server\share\dir)"},
       }) {
    Canonical_Path p(std::string(tc.before));
    p.append_component(tc.component);
    EXPECT_EQ(p.path(), tc.after)
        << "before = " << tc.before << "\ncomponent = " << tc.component;
  }
}

TEST_F(Test_File_Canonical, remove_appended_component_win32) {
  Canonical_Path p(R"(X:\dir\subdir)");
  p.append_component("file.txt");
  p.parent();
  EXPECT_EQ(p.path(), R"(X:\dir\subdir)");
}

TEST_F(Test_File_Canonical, remove_component_appended_to_root_win32) {
  Canonical_Path p(R"(X:\)");
  p.append_component("file.txt");
  p.parent();
  EXPECT_EQ(p.path(), R"(X:\)");
}
#endif
}
}

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
