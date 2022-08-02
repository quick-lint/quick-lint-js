// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <exception>
#include <limits.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/utf-16.h>
#include <random>
#include <string>
#include <string_view>

#if QLJS_HAVE_FTS_H
#include <fts.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_MKDTEMP
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
namespace {
std::string get_temp_dir() {
  std::string path;
  // TODO(strager): Call GetTempPathW instead.
  ::DWORD path_buffer_size = ::GetTempPathA(0, path.data());
  if (path_buffer_size == 0) {
    std::fprintf(stderr, "failed to get path to temporary directory: %s\n",
                 windows_last_error_message().c_str());
    std::abort();
  }
  ::DWORD path_length = path_buffer_size - 1;
  path.resize(path_length);
  ::DWORD rc = ::GetTempPathA(path.size() + 1, path.data());
  QLJS_ALWAYS_ASSERT(rc == path_length);
  QLJS_ASSERT(path.back() == '\\');
  return path;
}
}
#endif

std::string create_directory_io_error::to_string() const {
  return this->io_error.to_string();
}

#if QLJS_HAVE_MKDTEMP
result<void, platform_file_io_error> make_unique_directory(std::string &path) {
  path += ".XXXXXX";
  if (!::mkdtemp(path.data())) {
    return failed_result(platform_file_io_error{.error = errno});
  }
  return {};
}
#elif QLJS_HAVE_STD_FILESYSTEM
result<void, platform_file_io_error> make_unique_directory(std::string &path) {
  std::string_view characters = "abcdefghijklmnopqrstuvwxyz";
  std::uniform_int_distribution<std::size_t> character_index_distribution(
      0, characters.size() - 1);

  std::random_device system_rng;
  std::mt19937 rng(/*seed=*/system_rng());

  for (int attempt = 0; attempt < 100; ++attempt) {
    std::string suffix = ".";
    for (int i = 0; i < 10; ++i) {
      suffix += characters[character_index_distribution(rng)];
    }

    result<void, create_directory_io_error> create_result =
        create_directory(path + suffix);
    if (!create_result.ok()) {
      continue;
    }

    path += suffix;
    return {};
  }

  // TODO(strager): Return the proper error code from 'create_result'.
  return failed_result(platform_file_io_error{
      .error = 0,
  });
}
#else
#error "Unsupported platform"
#endif

#if QLJS_HAVE_WINDOWS_H
std::string make_temporary_directory() {
  std::string temp_directory_name = get_temp_dir() + "quick-lint-js";
  auto result = make_unique_directory(temp_directory_name);
  if (!result.ok()) {
    std::fprintf(stderr, "failed to create temporary directory: %s\n",
                 result.error_to_string().c_str());
    std::abort();
  }
  return temp_directory_name;
}
#elif QLJS_HAVE_MKDTEMP
std::string make_temporary_directory() {
  std::string temp_directory_name = "/tmp/quick-lint-js";
  auto result = make_unique_directory(temp_directory_name);
  if (!result.ok()) {
    std::fprintf(stderr, "failed to create temporary directory: %s\n",
                 result.error_to_string().c_str());
    std::abort();
  }
  return temp_directory_name;
}
#elif QLJS_HAVE_STD_FILESYSTEM
std::string make_temporary_directory() {
  std::filesystem::path system_temp_dir_path =
      std::filesystem::temp_directory_path();
  std::string temp_directory_name =
      (system_temp_dir_path / "quick-lint-js").string();
  auto result = make_unique_directory(temp_directory_name);
  if (!result.ok()) {
    std::fprintf(stderr, "failed to create temporary directory: %s\n",
                 result.error_to_string().c_str());
    std::abort();
  }
  return temp_directory_name;
}
#else
#error "Unsupported platform"
#endif

result<void, create_directory_io_error> create_directory(
    const std::string &path) {
#if QLJS_HAVE_WINDOWS_H
  std::optional<std::wstring> wpath = mbstring_to_wstring(path.c_str());
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  if (!::CreateDirectoryW(wpath->c_str(), /*lpSecurityAttributes=*/nullptr)) {
    ::DWORD error = ::GetLastError();
    bool directory_existed = false;
    if (error == ERROR_ALREADY_EXISTS) {
      ::DWORD attributes = ::GetFileAttributesW(wpath->c_str());
      if (attributes != INVALID_FILE_ATTRIBUTES) {
        directory_existed = attributes & FILE_ATTRIBUTE_DIRECTORY;
      }
    }
    return failed_result(create_directory_io_error{
        .io_error =
            windows_file_io_error{
                .error = error,
            },
        .is_directory_already_exists_error = directory_existed,
    });
  }
  return {};
#elif QLJS_HAVE_FCNTL_H
  if (::mkdir(path.c_str(), 0755) != 0) {
    int error = errno;
    bool directory_existed = false;
    if (error == EEXIST) {
      struct ::stat s;
      if (::lstat(path.c_str(), &s) == 0) {
        directory_existed = S_ISDIR(s.st_mode);
      }
    }
    return failed_result(create_directory_io_error{
        .io_error =
            posix_file_io_error{
                .error = error,
            },
        .is_directory_already_exists_error = directory_existed,
    });
  }
  return {};
#elif QLJS_HAVE_STD_FILESYSTEM
  std::error_code error;
  if (!std::filesystem::create_directory(to_string8(path), error)) {
    // TODO(strager): Return the proper error code from 'error'.
    return failed_result(create_directory_io_error{
        .io_error =
            platform_file_io_error{
                .error = 0,
            },
    });
  }
  return {};
#else
#error "Unsupported platform"
#endif
}

void create_directory_or_exit(const std::string &path) {
  auto result = create_directory(path);
  if (!result.ok()) {
    std::fprintf(stderr, "error: failed to create directory %s: %s\n",
                 path.c_str(), result.error_to_string().c_str());
    std::terminate();
  }
}

result<std::string, platform_file_io_error> make_timestamped_directory(
    std::string_view parent_directory, const char *format) {
  std::time_t now = std::time(nullptr);
  std::tm *now_tm = std::localtime(&now);

  std::string directory(parent_directory);
  directory += QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR;
  std::size_t name_begin_index = directory.size();
  std::size_t name_size = 1;
retry:
  directory.resize(name_begin_index + name_size + 1);
  std::size_t actual_name_size = std::strftime(&directory[name_begin_index],
                                               name_size + 1, format, now_tm);
  if (actual_name_size == 0) {
    name_size *= 2;
    goto retry;
  }
  directory.resize(name_begin_index + actual_name_size);

  auto result = make_unique_directory(directory);
  if (!result.ok()) {
    return result.propagate();
  }
  return directory;
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
