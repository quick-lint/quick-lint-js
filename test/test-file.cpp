// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cassert>
#include <cerrno>
#include <chrono>
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
#include <quick-lint-js/std-filesystem.h>
#include <random>
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
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
filesystem::path make_temporary_directory();
void write_file(filesystem::path, const std::string &content);

class test_file : public ::testing::Test {
 public:
  filesystem::path make_temporary_directory() {
    temp_directory_path = quick_lint_js::make_temporary_directory();
    return temp_directory_path.value();
  }

 private:
  std::optional<filesystem::path> temp_directory_path;

 protected:
  void TearDown() override {
    if (temp_directory_path.has_value()) {
      filesystem::remove_all(temp_directory_path.value());
    }
  }
};

TEST_F(test_file, read_regular_file) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "temp.js";
  write_file(temp_file_path, "hello\nworld!\n");

  read_file_result file_content = read_file(temp_file_path.string().c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error;
  EXPECT_EQ(file_content.content, string8_view(u8"hello\nworld!\n"));
}

TEST_F(test_file, read_non_existing_file) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "does-not-exist.js";

  read_file_result file_content = read_file(temp_file_path.string().c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_THAT(file_content.error, HasSubstr("does-not-exist.js"));
  EXPECT_THAT(file_content.error,
              AnyOf(HasSubstr("No such file"), HasSubstr("cannot find")));
}

TEST_F(test_file, read_directory) {
  filesystem::path temp_file_path = this->make_temporary_directory();

  read_file_result file_content = read_file(temp_file_path.string().c_str());
  EXPECT_FALSE(file_content.ok());
  EXPECT_THAT(
      file_content.error,
      testing::AnyOf(
          HasSubstr("Is a directory"),
          HasSubstr("Access is denied")  // TODO(strager): Improve this message.
          ));
}

#if QLJS_HAVE_MKFIFO
TEST_F(test_file, read_fifo) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file(temp_file_path, "hello from fifo"); });

  read_file_result file_content = read_file(temp_file_path.string().c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error;
  EXPECT_EQ(file_content.content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}

TEST_F(test_file, read_fifo_multiple_writes) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "fifo.js";
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

  read_file_result file_content = read_file(temp_file_path.string().c_str());
  EXPECT_TRUE(file_content.ok()) << file_content.error;
  EXPECT_EQ(file_content.content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}
#endif

#if QLJS_HAVE_PIPE
class substitute_fd_guard {
 public:
  explicit substitute_fd_guard(int original_fd, int replacement_fd)
      : original_fd_(original_fd),
        original_file_(posix_fd_file::duplicate(original_fd)) {
    this->dup2(replacement_fd, original_fd);
  }

  substitute_fd_guard(const substitute_fd_guard &) = delete;
  substitute_fd_guard &operator=(const substitute_fd_guard &) = delete;

  ~substitute_fd_guard() {
    this->dup2(this->original_file_.get(), this->original_fd_);
  }

 private:
  static void dup2(int oldfd, int newfd) {
    int rc = ::dup2(oldfd, newfd);
    if (rc == -1) {
      std::cerr << "fatal: failed to dup2: " << std::strerror(errno) << '\n';
      std::abort();
    }
  }

  int original_fd_;
  posix_fd_file original_file_;
};

TEST_F(test_file, read_pipe_multiple_writes) {
  pipe_fds pipe = make_pipe();
  substitute_fd_guard stdin_substitution(/*original_fd=*/STDIN_FILENO,
                                         /*replacement_fd=*/pipe.reader.get());

  auto write_message = [&](const char *message) -> void {
    std::size_t message_size = std::strlen(message);
    ssize_t rc = ::write(pipe.writer.get(), message, message_size);
    EXPECT_EQ(rc, message_size) << std::strerror(errno);
  };
  std::thread writer_thread([&]() {
    write_message("hello");
    std::this_thread::sleep_for(1ms);
    write_message(" from");
    std::this_thread::sleep_for(1ms);
    write_message(" fifo");

    pipe.writer.close();
  });

  read_file_result file_content = read_file("/dev/stdin");
  EXPECT_TRUE(file_content.ok()) << file_content.error;
  EXPECT_EQ(file_content.content, string8_view(u8"hello from fifo"));

  writer_thread.join();
}
#endif

#if QLJS_HAVE_MKDTEMP
filesystem::path make_temporary_directory() {
  filesystem::path system_temp_dir_path = filesystem::temp_directory_path();
  std::string temp_directory_name =
      (system_temp_dir_path / "quick-lint-js.XXXXXX").string();
  if (!::mkdtemp(temp_directory_name.data())) {
    std::cerr << "failed to create temporary directory\n";
    std::abort();
  }
  return temp_directory_name;
}
#else
filesystem::path make_temporary_directory() {
  std::string_view characters = "abcdefghijklmnopqrstuvwxyz";
  std::uniform_int_distribution<std::size_t> character_index_distribution(
      0, characters.size() - 1);

  filesystem::path system_temp_dir_path = filesystem::temp_directory_path();
  std::random_device system_rng;
  std::mt19937 rng(/*seed=*/system_rng());

  for (int attempt = 0; attempt < 100; ++attempt) {
    std::string file_name = "quick-lint-js.";
    for (int i = 0; i < 10; ++i) {
      file_name += characters[character_index_distribution(rng)];
    }

    filesystem::path temp_directory_path = system_temp_dir_path / file_name;
    std::error_code error;
    if (!filesystem::create_directory(temp_directory_path, error)) {
      continue;
    }
    return temp_directory_path;
  }
  std::cerr << "failed to create temporary directory\n";
  std::abort();
}
#endif

void write_file(filesystem::path path, const std::string &content) {
  std::ofstream file(path, std::ofstream::binary | std::ofstream::out);
  if (!file) {
    std::cerr << "failed to open file for writing\n";
    std::abort();
  }
  file << content;
  file.close();
  if (!file) {
    std::cerr << "failed to write file content\n";
    std::abort();
  }
}
}
}
