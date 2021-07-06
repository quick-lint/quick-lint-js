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
#include <iostream>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/sloppy-result.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/temporary-directory.h>
#include <quick-lint-js/unreachable.h>
#include <stdlib.h>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

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
class test_file : public ::testing::Test {
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

TEST_F(test_file, read_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file(temp_file_path, u8"hello\nworld!\n");

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"hello\nworld!\n"));
}

TEST_F(test_file, read_empty_regular_file) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  write_file(temp_file_path, u8"");

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8""));
}

TEST_F(test_file, read_non_existing_file) {
  std::string temp_file_path =
      this->make_temporary_directory() + "/does-not-exist.js";

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        boost::leaf::result<padded_string> file_content =
            read_file_2(temp_file_path.c_str());
        if (!file_content) return file_content.error();
        ADD_FAILURE() << "read_file_2 should have failed";
        return {};
      },
      make_file_not_found_handler([] {
        // Test passed.
      }),
      make_read_file_error_handlers([](const std::string& message) {
        ADD_FAILURE() << "expected file-not-found error, but got: " << message;
      }));
}

TEST_F(test_file, read_non_existing_file_sloppy_message) {
  std::string temp_file_path =
      this->make_temporary_directory() + "/does-not-exist.js";

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_THAT(file_content.error(), HasSubstr("does-not-exist.js"));
  EXPECT_THAT(file_content.error(),
              AnyOf(HasSubstr("No such file"), HasSubstr("cannot find")));
}

TEST_F(test_file, read_directory) {
  std::string temp_file_path = this->make_temporary_directory();

  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        boost::leaf::result<padded_string> file_content =
            read_file_2(temp_file_path.c_str());
        if (!file_content) return file_content.error();
        ADD_FAILURE() << "read_file_2 should have failed";
        return {};
      },
      make_file_not_found_handler(
          [] { ADD_FAILURE() << "expected not file-not-found error"; }),
      make_read_file_error_handlers([](const std::string&) {
        // Test passed.
      }));
}

TEST_F(test_file, read_directory_sloppy_message) {
  std::string temp_file_path = this->make_temporary_directory();

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_THAT(file_content.error(), HasSubstr(temp_file_path));
  EXPECT_THAT(
      file_content.error(),
      testing::AnyOf(
          HasSubstr("Is a directory"),
          HasSubstr("Access is denied")  // TODO(strager): Improve this message.
          ));
}

#if QLJS_HAVE_MKFIFO
TEST_F(test_file, read_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file(temp_file_path, u8"hello from fifo"); });

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}

TEST_F(test_file, read_empty_fifo) {
  std::string temp_file_path = this->make_temporary_directory() + "/fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread([&]() { write_file(temp_file_path, u8""); });

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8""));

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

  sloppy_result<padded_string> file_content =
      read_file_sloppy(temp_file_path.c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}
#endif

TEST_F(test_file, read_pipe_multiple_writes) {
  pipe_fds pipe = make_pipe();

  auto write_message = [&](const char* message) -> void {
    std::size_t message_size = std::strlen(message);
    std::optional<int> bytes_written =
        pipe.writer.write(message, narrow_cast<int>(message_size));
    ASSERT_TRUE(bytes_written.has_value())
        << pipe.writer.get_last_error_message();
    EXPECT_EQ(*bytes_written, message_size);
  };
  std::thread writer_thread([&]() {
    write_message("hello");
    std::this_thread::sleep_for(1ms);
    write_message(" from");
    std::this_thread::sleep_for(1ms);
    write_message(" fifo");

    pipe.writer.close();
  });

  sloppy_result<padded_string> file_content =
      read_file_sloppy("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}

// Windows and POSIX handle end-of-file for pipes differently. POSIX reports
// end-of-file via an empty read; Windows reports end-of-file via an error code.
// Try to confuse read_file.
TEST_F(test_file, read_pipe_empty_writes) {
  pipe_fds pipe = make_pipe();

  auto write_message = [&](const char* message) -> void {
    std::size_t message_size = std::strlen(message);
    std::optional<int> bytes_written =
        pipe.writer.write(message, narrow_cast<int>(message_size));
    ASSERT_TRUE(bytes_written.has_value())
        << pipe.writer.get_last_error_message();
    EXPECT_EQ(*bytes_written, message_size);
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

  sloppy_result<padded_string> file_content =
      read_file_sloppy("<pipe>", pipe.reader.ref());
  EXPECT_TRUE(file_content.ok()) << file_content.error();
  EXPECT_EQ(*file_content, string8_view(u8"helloworld"));

  writer_thread.join();
}
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
