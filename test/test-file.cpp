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
#include <sys/stat.h>
#include <sys/types.h>
#endif

using ::testing::AnyOf;
using ::testing::HasSubstr;
using ::testing::Not;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
class test_file : public ::testing::Test, protected filesystem_test {};

TEST_F(test_file, read_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file_or_exit(temp_file_path, u8"hello\nworld!\n"_sv);

  result<padded_string, read_file_io_error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello\nworld!\n"_sv);
}

TEST_F(test_file, read_empty_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file_or_exit(temp_file_path, u8""_sv);

  result<padded_string, read_file_io_error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8""_sv);
}

TEST_F(test_file, read_non_existing_file) {
  std::string temp_file_path =
      this->make_temporary_directory() + "/does-not-exist.js";

  result<padded_string, read_file_io_error> file_content =
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

TEST_F(test_file, read_directory) {
  std::string temp_file_path = this->make_temporary_directory();

  result<padded_string, read_file_io_error> file_content =
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
TEST_F(test_file, read_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file_or_exit(temp_file_path, u8"hello from fifo"_sv); });

  result<padded_string, read_file_io_error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}

TEST_F(test_file, read_empty_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file_or_exit(temp_file_path, u8""_sv); });

  result<padded_string, read_file_io_error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8""_sv);

  writer_thread.join();
}

TEST_F(test_file, read_fifo_multiple_writes) {
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

  result<padded_string, read_file_io_error> file_content =
      read_file(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}
#endif

TEST_F(test_file, read_pipe_multiple_writes) {
  pipe_fds pipe = make_pipe();

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

  result<padded_string, read_file_io_error> file_content =
      read_file("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"hello from fifo"_sv);

  writer_thread.join();
}

// Windows and POSIX handle end-of-file for pipes differently. POSIX reports
// end-of-file via an empty read; Windows reports end-of-file via an error code.
// Try to confuse read_file.
TEST_F(test_file, read_pipe_empty_writes) {
  pipe_fds pipe = make_pipe();

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

  result<padded_string, read_file_io_error> file_content =
      read_file("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error().to_string();
  EXPECT_EQ(*file_content, u8"helloworld"_sv);

  writer_thread.join();
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
