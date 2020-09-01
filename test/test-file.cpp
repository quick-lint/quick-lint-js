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
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/std-filesystem.h>
#include <stdlib.h>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

#if defined(_WIN32)
#include <Windows.h>
#endif

#if QLJS_HAVE_MKFIFO
#include <sys/stat.h>
#include <sys/types.h>
#endif

namespace quick_lint_js {
namespace {
filesystem::path make_temporary_directory();
void write_file(filesystem::path, const std::string &content);

class test_file : public ::testing::Test {
 public:
  filesystem::path make_temporary_directory() {
    // TODO(strager): Delete the directory when the test finishes.
    return quick_lint_js::make_temporary_directory();
  }
};

TEST_F(test_file, read_regular_file) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "temp.js";
  write_file(temp_file_path, "hello\nworld!\n");

  string8 file_content = read_file(temp_file_path.string().c_str());
  EXPECT_EQ(file_content, u8"hello\nworld!\n");
}

#if QLJS_HAVE_MKFIFO
TEST_F(test_file, read_fifo) {
  filesystem::path temp_file_path =
      this->make_temporary_directory() / "fifo.js";
  ASSERT_EQ(::mkfifo(temp_file_path.c_str(), 0700), 0) << std::strerror(errno);

  std::thread writer_thread(
      [&]() { write_file(temp_file_path, "hello from fifo"); });

  string8 file_content = read_file(temp_file_path.string().c_str());
  EXPECT_EQ(file_content, u8"hello from fifo");

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
#elif defined(_WIN32)
filesystem::path make_temporary_directory() {
  static_assert(std::is_same_v<filesystem::path::value_type, wchar_t>);
  filesystem::path system_temp_dir_path = filesystem::temp_directory_path();
  for (int attempt = 0; attempt < 100; ++attempt) {
    std::vector<wchar_t> temp_directory_name;
    temp_directory_name.resize(MAX_PATH + 1);
    UINT unique = attempt + 1;
    assert(unique != 0);
    if (::GetTempFileNameW(/*lpPathName=*/system_temp_dir_path.c_str(),
                           /*lpPrefixString=*/L"QLJS", /*uUnique=*/attempt + 1,
                           /*lpTempFileName=*/temp_directory_name.data()) ==
        0) {
      continue;
    }
    filesystem::path temp_directory_path = temp_directory_name.data();
    std::error_code error;
    if (!filesystem::create_directory(temp_directory_path, error)) {
      continue;
    }
    return temp_directory_path;
  }
  std::cerr << "failed to create temporary directory\n";
  std::abort();
}
#else
#error "Unknown platform"
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
}  // namespace
}  // namespace quick_lint_js
