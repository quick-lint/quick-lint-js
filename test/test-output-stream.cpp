// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/narrow-cast.h>
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
TEST(Test_Memory_Output_Stream, no_data_is_written_before_flush) {
  Memory_Output_Stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello world"_sv);
  EXPECT_EQ(s.get_flushed_string8(), u8""_sv);
}

TEST(Test_Memory_Output_Stream, initial_append_copy) {
  Memory_Output_Stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello world"_sv);
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"hello world"_sv);
}

TEST(Test_Memory_Output_Stream, append_copy_multiple_times) {
  Memory_Output_Stream s(/*buffer_size=*/1024);
  s.append_copy(u8"hello"_sv);
  s.append_copy(u8" "_sv);
  s.append_copy(u8"world"_sv);
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"hello world"_sv);
}

TEST(Test_Memory_Output_Stream, append_copy_overflowing_buffer) {
  Memory_Output_Stream s(/*buffer_size=*/16);
  s.append_copy(u8"helloworld"_sv);  // total written: 10
  s.append_copy(u8"HELLOWORLD"_sv);  // total written: 20
  s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"helloworldHELLOWORLD"_sv);
}

TEST(Test_Memory_Output_Stream,
     append_copy_more_than_buffer_size_flushes_immediately) {
  Memory_Output_Stream s(/*buffer_size=*/16);
  s.append_copy(u8"the quick brown fox jumps over"_sv);
  // Omitted: s.flush();
  EXPECT_EQ(s.get_flushed_string8(), u8"the quick brown fox jumps over"_sv);
}

TEST(Test_Memory_Output_Stream,
     append_copy_more_than_buffer_size_preserves_previous_data) {
  Memory_Output_Stream s(/*buffer_size=*/16);
  s.append_copy(u8"first "_sv);
  s.append_copy(u8"the quick brown fox jumps over"_sv);
  // Omitted: s.flush();
  EXPECT_EQ(s.get_flushed_string8(),
            u8"first the quick brown fox jumps over"_sv);
}

TEST(Test_Memory_Output_Stream, append_small_with_callback) {
  Memory_Output_Stream s(/*buffer_size=*/1024);

  s.append(4, [](Char8* piece) -> int {
    std::memcpy(piece, u8"ab", 2);
    return 2;
  });
  s.append(4, [](Char8* piece) -> int {
    std::memcpy(piece, u8"cdef", 4);
    return 4;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), u8"abcdef"_sv);
}

TEST(Test_Memory_Output_Stream,
     append_small_with_callback_more_than_buffer_size) {
  static constexpr int buffer_size = 16;
  Memory_Output_Stream s(/*buffer_size=*/buffer_size);

  static constexpr Char8 message_0[] = u8"the quick brown";
  static int message_0_length = narrow_cast<int>(strlen(message_0));
  static constexpr Char8 message_1[] = u8" fox jumps";
  static int message_1_length = narrow_cast<int>(strlen(message_1));
  ASSERT_LT(message_0_length, buffer_size);
  ASSERT_GT(message_0_length + message_1_length, buffer_size);

  s.append(message_0_length, [](Char8* piece) -> int {
    std::copy(message_0, &message_0[message_0_length], piece);
    return message_0_length;
  });
  s.append(message_1_length, [](Char8* piece) -> int {
    std::copy(message_1, &message_1[message_1_length], piece);
    return message_1_length;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), String8(message_0) + message_1);
}

TEST(Test_Memory_Output_Stream, append_with_callback_more_than_buffer_size) {
  static constexpr int buffer_size = 16;
  Memory_Output_Stream s(/*buffer_size=*/buffer_size);

  static constexpr Char8 message_0[] = u8"the quick brown fox jumps";
  static int message_0_length = narrow_cast<int>(strlen(message_0));
  ASSERT_GT(message_0_length, buffer_size);

  s.append(message_0_length, [](Char8* piece) -> int {
    std::copy(message_0, &message_0[message_0_length], piece);
    return message_0_length;
  });
  s.flush();

  EXPECT_EQ(s.get_flushed_string8(), message_0);
}

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

class Test_File_Output_Stream : public ::testing::Test,
                                public Filesystem_Test {};

#if QLJS_HAVE_UNISTD_H
TEST_F(Test_File_Output_Stream, write_to_posix_file) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file_path = temp_dir + "/test.txt";
  POSIX_FD_File file(::open(temp_file_path.c_str(), O_CREAT | O_WRONLY, 0644));
  ASSERT_TRUE(file.valid()) << std::strerror(errno);

  File_Output_Stream s(file.ref(), /*buffer_size=*/1024);
  s.append_copy(u8"hello world"_sv);
  s.flush();

  Result<Padded_String, Read_File_IO_Error> contents =
      read_file(temp_file_path.c_str());
  ASSERT_TRUE(contents.ok()) << contents.error_to_string();
  EXPECT_EQ(contents->string_view(), u8"hello world"_sv);
}

TEST_F(Test_File_Output_Stream, destructing_flushes_pending_data) {
  std::string temp_dir = this->make_temporary_directory();
  std::string temp_file_path = temp_dir + "/test.txt";
  POSIX_FD_File file(::open(temp_file_path.c_str(), O_CREAT | O_WRONLY, 0644));
  ASSERT_TRUE(file.valid()) << std::strerror(errno);

  {
    File_Output_Stream s(file.ref(), /*buffer_size=*/1024);
    s.append_copy(u8"hello world"_sv);

    Result<Padded_String, Read_File_IO_Error> before_contents =
        read_file(temp_file_path.c_str());
    ASSERT_TRUE(before_contents.ok()) << before_contents.error_to_string();
    EXPECT_EQ(before_contents->string_view(), u8""_sv)
        << "data should be unwritten before closing";
  }

  Result<Padded_String, Read_File_IO_Error> after_contents =
      read_file(temp_file_path.c_str());
  ASSERT_TRUE(after_contents.ok()) << after_contents.error_to_string();
  EXPECT_EQ(after_contents->string_view(), u8"hello world"_sv)
      << "data should be written after closing";
}
#endif

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
