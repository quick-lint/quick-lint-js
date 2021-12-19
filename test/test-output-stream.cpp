// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>
#include <string_view>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_memory_output_stream, no_data_is_written_before_flush) {
  memory_output_stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello world"sv);
  EXPECT_EQ(s.get_flushed_string8(), u8""sv);
}

TEST(test_memory_output_stream, initial_append_copy) {
  memory_output_stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello world"sv);
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"hello world"sv);
}

TEST(test_memory_output_stream, append_copy_multiple_times) {
  memory_output_stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello"sv);
  s.append_copy(u8" "sv);
  s.append_copy(u8"world"sv);
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"hello world"sv);
}

TEST(test_memory_output_stream, append_copy_overflowing_buffer) {
  memory_output_stream s(/*buffer_size=*/16);
  s.append_copy(u8"helloworld"sv);  // total written: 10
  s.append_copy(u8"HELLOWORLD"sv);  // total written: 20
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"helloworldHELLOWORLD"sv);
}

TEST(test_memory_output_stream,
     append_copy_more_than_buffer_size_flushes_immediately) {
  memory_output_stream s(/*buffer_size=*/16);
  s.append_copy(u8"the quick brown fox jumps over"sv);
  // Omitted: s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"the quick brown fox jumps over"sv);
}

TEST(test_memory_output_stream,
     append_copy_more_than_buffer_size_preserves_previous_data) {
  memory_output_stream s(/*buffer_size=*/16);
  s.append_copy(u8"first "sv);
  s.append_copy(u8"the quick brown fox jumps over"sv);
  // Omitted: s.flush();
  EXPECT_EQ(s.get_flushed_string8(),
            u8"first the quick brown fox jumps over"sv);
}

TEST(test_memory_output_stream, append_small_with_callback) {
  memory_output_stream s(/*buffer_size=*/1024);

  s.append(4, [](char8* piece) -> int {
    std::memcpy(piece, u8"ab", 2);
    return 2;
  });
  s.append(4, [](char8* piece) -> int {
    std::memcpy(piece, u8"cdef", 4);
    return 4;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), u8"abcdef"sv);
}

TEST(test_memory_output_stream,
     append_small_with_callback_more_than_buffer_size) {
  static constexpr int buffer_size = 16;
  memory_output_stream s(/*buffer_size=*/buffer_size);

  static constexpr char8 message_0[] = u8"the quick brown";
  static int message_0_length = narrow_cast<int>(strlen(message_0));
  static constexpr char8 message_1[] = u8" fox jumps";
  static int message_1_length = narrow_cast<int>(strlen(message_1));
  ASSERT_LT(message_0_length, buffer_size);
  ASSERT_GT(message_0_length + message_1_length, buffer_size);

  s.append(message_0_length, [](char8* piece) -> int {
    std::copy(message_0, &message_0[message_0_length], piece);
    return message_0_length;
  });
  s.append(message_1_length, [](char8* piece) -> int {
    std::copy(message_1, &message_1[message_1_length], piece);
    return message_1_length;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), string8(message_0) + message_1);
}

TEST(test_memory_output_stream, append_with_callback_more_than_buffer_size) {
  static constexpr int buffer_size = 16;
  memory_output_stream s(/*buffer_size=*/buffer_size);

  static constexpr char8 message_0[] = u8"the quick brown fox jumps";
  static int message_0_length = narrow_cast<int>(strlen(message_0));
  ASSERT_GT(message_0_length, buffer_size);

  s.append(message_0_length, [](char8* piece) -> int {
    std::copy(message_0, &message_0[message_0_length], piece);
    return message_0_length;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), message_0);
}

class test_file_output_stream : public ::testing::Test,
                                public filesystem_test {};

#if QLJS_HAVE_UNISTD_H
TEST_F(test_file_output_stream, write_to_posix_file) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file_path = temp_dir + "/test.txt";
  posix_fd_file file(::open(temp_file_path.c_str(), O_CREAT | O_WRONLY, 0644));
  ASSERT_TRUE(file.valid()) << std::strerror(errno);

  file_output_stream s(file.ref(), /*buffer_size=*/1024);
  s.append_copy(u8"hello world"sv);
  s.flush();

  result<padded_string, read_file_io_error> contents =
      read_file(temp_file_path.c_str());
  ASSERT_TRUE(contents.ok()) << contents.error_to_string();
  EXPECT_EQ(contents->string_view(), u8"hello world"sv);
}

TEST_F(test_file_output_stream, destructing_flushes_pending_data) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file_path = temp_dir + "/test.txt";
  posix_fd_file file(::open(temp_file_path.c_str(), O_CREAT | O_WRONLY, 0644));
  ASSERT_TRUE(file.valid()) << std::strerror(errno);

  {
    file_output_stream s(file.ref(), /*buffer_size=*/1024);
    s.append_copy(u8"hello world"sv);

    result<padded_string, read_file_io_error> before_contents =
        read_file(temp_file_path.c_str());
    ASSERT_TRUE(before_contents.ok()) << before_contents.error_to_string();
    EXPECT_EQ(before_contents->string_view(), u8""sv)
        << "data should be unwritten before closing";
  }

  result<padded_string, read_file_io_error> after_contents =
      read_file(temp_file_path.c_str());
  ASSERT_TRUE(after_contents.ok()) << after_contents.error_to_string();
  EXPECT_EQ(after_contents->string_view(), u8"hello world"sv)
      << "data should be written after closing";
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
