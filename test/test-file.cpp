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
#include <iostream>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/pty.h>
#include <quick-lint-js/port/unreachable.h>
#include <stdlib.h>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

#if QLJS_HAVE_MKFIFO
#include <sys/types.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

using ::testing::HasSubstr;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
class Test_File : public ::testing::Test, protected Filesystem_Test {};

TEST_F(Test_File, read_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file_or_exit(temp_file_path, u8"hello\nworld!\n"_sv);

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello\nworld!\n"_sv);
}

TEST_F(Test_File, read_empty_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file_or_exit(temp_file_path, u8""_sv);

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8""_sv);
}

TEST_F(Test_File, read_non_existing_file) {
  std::string temp_file_path =
      this->make_temporary_directory() + "/does-not-exist.js";

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_TRUE(file_content.error().is_file_not_found_error());
  EXPECT_THAT(file_content.error().to_string(), HasSubstr("does-not-exist.js"));
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(file_content.error().io_error.error, ENOENT);
#elif QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(file_content.error().io_error.error, ERROR_FILE_NOT_FOUND);
#else
#error "Unknown platform"
#endif
}

TEST_F(Test_File, read_directory) {
  std::string temp_file_path = this->make_temporary_directory();

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_FALSE(file_content.error().is_file_not_found_error());
  EXPECT_THAT(file_content.error().to_string(), HasSubstr(temp_file_path));
#if QLJS_HAVE_UNISTD_H
  EXPECT_EQ(file_content.error().io_error.error, EISDIR);
#elif QLJS_HAVE_WINDOWS_H
  EXPECT_EQ(file_content.error().io_error.error, ERROR_ACCESS_DENIED);
#else
#error "Unknown platform"
#endif
}

#if QLJS_HAVE_MKFIFO
TEST_F(Test_File, read_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file_or_exit(temp_file_path, u8"hello from fifo"_sv); });

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}

TEST_F(Test_File, read_empty_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file_or_exit(temp_file_path, u8""_sv); });

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8""_sv);

  writer_thread.join();
}

TEST_F(Test_File, read_fifo_multiple_writes) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread([&]() {
    std::ofstream file(temp_file_path,
                       std::ofstream::binary | std::ofstream::out);
    if (!file) {
      std::cerr << "failed to open file for writing\n";
      std::abort();
    }

    file << "hello" << std::flush;
    std::this_thread::sleep_for(1ms);
    file << " from" << std::flush;
    std::this_thread::sleep_for(1ms);
    file << " fifo" << std::flush;

    file.close();
    if (!file) {
      std::cerr << "failed to write file content\n";
      std::abort();
    }
  });

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}
#endif

TEST_F(Test_File, read_pipe_multiple_writes) {
  Pipe_FDs pipe = make_pipe();

  auto write_message = [&](const char* message) -> void {
    std::size_t message_size = std::strlen(message);
    auto write_result = pipe.writer.write_full(message, message_size);
    ASSERT_TRUE(write_result.ok()) << write_result.error_to_string();
  };
  std::thread writer_thread([&]() {
    write_message("hello");
    std::this_thread::sleep_for(1ms);
    write_message(" from");
    std::this_thread::sleep_for(1ms);
    write_message(" fifo");

    pipe.writer.close();
  });

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}

// Windows and POSIX handle end-of-file for pipes differently. POSIX reports
// end-of-file via an empty read; Windows reports end-of-file via an error code.
// Try to confuse read_file.
TEST_F(Test_File, read_pipe_empty_writes) {
  Pipe_FDs pipe = make_pipe();

  auto write_message = [&](const char* message) -> void {
    std::size_t message_size = std::strlen(message);
    auto write_result = pipe.writer.write_full(message, message_size);
    ASSERT_TRUE(write_result.ok()) << write_result.error_to_string();
  };
  std::thread writer_thread([&]() {
    write_message("");
    std::this_thread::sleep_for(1ms);
    write_message("hello");
    std::this_thread::sleep_for(1ms);
    write_message("");
    std::this_thread::sleep_for(1ms);
    write_message("world");

    pipe.writer.close();
  });

  Result<Padded_String, Read_File_IO_Error> file_content =
      read_file("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"helloworld"_sv);

  writer_thread.join();
}

#if QLJS_HAVE_FORKPTY
TEST_F(Test_File, read_file_reads_from_pty_master) {
  // Flush buffered Google Test output. Otherwise, the child inherits the buffer
  // and Google Test output gets mixed in with our test string.
  std::fflush(stdout);
  std::fflush(stderr);

  int raw_tty_fd;
  ::pid_t pid = forkpty(&raw_tty_fd, /*name=*/nullptr, /*termp=*/nullptr,
                        /*winp=*/nullptr);
  ASSERT_NE(pid, -1) << std::strerror(errno);
  if (pid == 0) {
    // Child.
    ::ssize_t rc = ::write(STDOUT_FILENO, "hello", 5);
    if (rc == -1) {
      std::exit(1);
    }
    if (rc != 5) {
      std::exit(2);
    }
    std::exit(0);
  } else {
    // Parent.
    POSIX_FD_File tty_fd(raw_tty_fd);
    auto output = read_file(tty_fd.ref());
    if (!output.ok()) {
      ADD_FAILURE() << output.error_to_string();
      return;
    }

    int status = 0;
    ::pid_t rc = ::waitpid(pid, &status, /*options=*/0);
    EXPECT_NE(rc, -1) << std::strerror(errno);
    EXPECT_EQ(rc, pid);
    EXPECT_TRUE(WIFEXITED(status)) << "child should not have crashed";
    if (WIFEXITED(status)) {
      EXPECT_EQ(WEXITSTATUS(status), 0)
          << "child write() should not have failed";
    }

    EXPECT_THAT(output->string_view(), u8"hello"_sv);
  }
}
#endif

TEST_F(
    Test_File,
    write_file_if_different_modifies_file_if_same_size_but_different_content) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file = temp_dir + "/file";
  write_file_or_exit(temp_file, u8"hello world"_sv);

  Result<bool, Read_File_IO_Error, Write_File_IO_Error> result =
      write_file_if_different(temp_file, u8"hello,world"_sv);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  EXPECT_TRUE(*result);

  EXPECT_EQ(read_file_or_exit(temp_file), u8"hello,world"_sv);
}

TEST_F(Test_File,
       write_file_if_different_modifies_file_if_bigger_size_but_same_prefix) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file = temp_dir + "/file";
  write_file_or_exit(temp_file, u8"hello"_sv);

  Result<bool, Read_File_IO_Error, Write_File_IO_Error> result =
      write_file_if_different(temp_file, u8"hello world"_sv);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  EXPECT_TRUE(*result);

  EXPECT_EQ(read_file_or_exit(temp_file), u8"hello world"_sv);
}

TEST_F(Test_File,
       write_file_if_different_modifies_file_if_smaller_size_but_same_prefix) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file = temp_dir + "/file";
  write_file_or_exit(temp_file, u8"hello world"_sv);

  Result<bool, Read_File_IO_Error, Write_File_IO_Error> result =
      write_file_if_different(temp_file, u8"hello"_sv);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  EXPECT_TRUE(*result);

  EXPECT_EQ(read_file_or_exit(temp_file), u8"hello"_sv);
}

TEST_F(Test_File, write_file_if_different_does_not_touch_file_if_same_SLOW) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file = temp_dir + "/file";
  write_file_or_exit(temp_file, u8"hello world"_sv);

#if QLJS_HAVE_SYS_STAT_H
  struct ::stat stat_before;
  ASSERT_EQ(::stat(temp_file.c_str(), &stat_before), 0) << std::strerror(errno);
#endif
#if defined(_WIN32)
  ::WIN32_FILE_ATTRIBUTE_DATA attributes_before;
  ASSERT_TRUE(::GetFileAttributesExA(temp_file.c_str(), ::GetFileExInfoStandard,
                                     &attributes_before));
#endif

#if QLJS_HAVE_SYS_STAT_H
  // HACK(strager): Wait for the filesystem's clock to change. This is just a
  // guess. If this duration is too small, we won't accurately assert that the
  // file's timestamps didn't change. If this duration is too large, we slow
  // down testing.
  std::this_thread::sleep_for(10ms);
#endif

  Result<bool, Read_File_IO_Error, Write_File_IO_Error> result =
      write_file_if_different(temp_file, u8"hello world"_sv);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  EXPECT_FALSE(*result);

#if QLJS_HAVE_SYS_STAT_H
  struct ::stat stat_after;
  ASSERT_EQ(::stat(temp_file.c_str(), &stat_after), 0) << std::strerror(errno);

  EXPECT_EQ(stat_before.st_dev, stat_after.st_dev);
  EXPECT_EQ(stat_before.st_ino, stat_after.st_ino);
  EXPECT_EQ(stat_before.st_mode, stat_after.st_mode);
  EXPECT_EQ(stat_before.st_uid, stat_after.st_uid);
  EXPECT_EQ(stat_before.st_gid, stat_after.st_gid);
#if defined(__APPLE__)
  EXPECT_EQ(stat_before.st_mtimespec.tv_sec, stat_after.st_mtimespec.tv_sec);
  EXPECT_EQ(stat_before.st_mtimespec.tv_nsec, stat_after.st_mtimespec.tv_nsec);
#elif defined(_WIN32)
  EXPECT_EQ(stat_before.st_mtime, stat_after.st_mtime);
  EXPECT_EQ(stat_before.st_mtime, stat_after.st_mtime);
#else
  EXPECT_EQ(stat_before.st_mtim.tv_sec, stat_after.st_mtim.tv_sec);
  EXPECT_EQ(stat_before.st_mtim.tv_nsec, stat_after.st_mtim.tv_nsec);
#endif
#endif
#if defined(_WIN32)
  ::WIN32_FILE_ATTRIBUTE_DATA attributes_after;
  ASSERT_TRUE(::GetFileAttributesExA(temp_file.c_str(), ::GetFileExInfoStandard,
                                     &attributes_after));
  EXPECT_EQ(attributes_before.dwFileAttributes,
            attributes_after.dwFileAttributes);
  EXPECT_EQ(attributes_before.ftCreationTime.dwHighDateTime,
            attributes_after.ftCreationTime.dwHighDateTime);
  EXPECT_EQ(attributes_before.ftCreationTime.dwLowDateTime,
            attributes_after.ftCreationTime.dwLowDateTime);
  EXPECT_EQ(attributes_before.ftLastWriteTime.dwHighDateTime,
            attributes_after.ftLastWriteTime.dwHighDateTime);
  EXPECT_EQ(attributes_before.ftLastWriteTime.dwLowDateTime,
            attributes_after.ftLastWriteTime.dwLowDateTime);
#endif

  EXPECT_EQ(read_file_or_exit(temp_file), u8"hello world"_sv);
}
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
