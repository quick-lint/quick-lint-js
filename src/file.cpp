// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/math-overflow.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-16.h>
#include <stdlib.h>
#include <string>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#define QLJS_FILE_WINDOWS
#elif QLJS_HAVE_UNISTD_H
#define QLJS_FILE_POSIX
#else
#error "Unsupported platform"
#endif

namespace quick_lint_js {
void read_file_result::exit_if_not_ok() const {
  if (!this->ok()) {
    std::fprintf(stderr, "error: %s\n", this->error.c_str());
    std::exit(1);
  }
}

read_file_result read_file_result::failure(const std::string &error) {
  read_file_result result;
  result.error = error;
  return result;
}

namespace {
void read_file_buffered(platform_file_ref file, const char *path,
                        int buffer_size, read_file_result *out) {
  // TODO(strager): Use byte_buffer to avoid copying the file content every
  // iteration.
  for (;;) {
    int size_before = out->content.size();
    {
      std::optional<int> new_size = checked_add(size_before, buffer_size);
      if (!new_size.has_value()) {
        // TODO(strager): Should we try a small buffer size?
        out->error = "file too large to read into memory";
        return;
      }
      out->content.resize_grow_uninitialized(size_before + buffer_size);
    }

    file_read_result read_result =
        file.read(&out->content.data()[size_before], buffer_size);
    if (!read_result.at_end_of_file() &&
        read_result.error_message.has_value()) {
      out->error = std::string("failed to read from ") + path + ": " +
                   *read_result.error_message;
      return;
    }
    if (read_result.at_end_of_file()) {
      // We read the entire file.
      out->content.resize(size_before);
      return;
    }
    std::optional<int> new_size =
        checked_add(size_before, *read_result.bytes_read);
    QLJS_ASSERT(new_size.has_value());
    out->content.resize(*new_size);
  }
}

read_file_result read_file_with_expected_size(platform_file_ref file,
                                              const char *path, int file_size,
                                              int buffer_size) {
  read_file_result result;

  std::optional<int> size_to_read = checked_add(file_size, 1);
  if (!size_to_read.has_value()) {
    result.error = "file too large to read into memory";
    return result;
  }
  result.content.resize_grow_uninitialized(*size_to_read);

  file_read_result read_result =
      file.read(result.content.data(), *size_to_read);
  if (!read_result.at_end_of_file() && read_result.error_message.has_value()) {
    result.error = std::string("failed to read from ") + path + ": " +
                   *read_result.error_message;
    return result;
  }
  if (read_result.at_end_of_file()) {
    // The file was empty.
    result.content.resize(0);
    return result;
  }
  if (read_result.bytes_read == file_size) {
    // We possibly read the entire file. Make extra sure by reading one more
    // byte.
    file_read_result extra_read_result =
        file.read(result.content.data() + file_size, 1);
    if (!extra_read_result.at_end_of_file() &&
        extra_read_result.error_message.has_value()) {
      result.error = std::string("failed to read from ") + path + ": " +
                     *extra_read_result.error_message;
      return result;
    }
    if (extra_read_result.at_end_of_file()) {
      // We definitely read the entire file.
      result.content.resize(*read_result.bytes_read);
      return result;
    } else {
      // We didn't read the entire file the first time. Keep reading.
      result.content.resize(*read_result.bytes_read +
                            *extra_read_result.bytes_read);
      read_file_buffered(file, path, buffer_size, &result);
      return result;
    }
  } else {
    result.content.resize(*read_result.bytes_read);
    // We did not read the entire file. There is more data to read.
    read_file_buffered(file, path, buffer_size, &result);
    return result;
  }
}
}

#if defined(QLJS_FILE_WINDOWS)
read_file_result read_file(const char *path, windows_handle_file_ref file) {
  int buffer_size = 1024;  // TODO(strager): Compute a good buffer size.

  ::LARGE_INTEGER file_size;
  if (!::GetFileSizeEx(file.get(), &file_size)) {
    DWORD error = ::GetLastError();
    return read_file_result::failure(
        std::string("failed to get size of file ") + path + ": " +
        windows_error_message(error));
  }
  if (!in_range<int>(file_size.QuadPart)) {
    return read_file_result::failure(
        std::string("file too large to read into memory: ") + path);
  }
  return read_file_with_expected_size(
      /*file=*/file, /*path=*/path,
      /*file_size=*/narrow_cast<int>(file_size.QuadPart),
      /*buffer_size=*/buffer_size);
}
#endif

#if defined(QLJS_FILE_POSIX)
namespace {
int reasonable_buffer_size(const struct stat &s) noexcept {
  using size_type = decltype(s.st_blksize);
  size_type minimum_buffer_size = 512;
  size_type megabyte = 1 << 20;
  // Bound st_blksize in case the OS gives us crazy numbers (like 0 or
  // (size_t)-1).
  return narrow_cast<int>(
      std::clamp(s.st_blksize, /*lo=*/minimum_buffer_size, /*hi=*/megabyte));
}
}

read_file_result read_file(const char *path, posix_fd_file_ref file) {
  struct stat s;
  int rc = ::fstat(file.get(), &s);
  if (rc == -1) {
    int error = errno;
    return read_file_result::failure(
        std::string("failed to get file info from ") + path + ": " +
        std::strerror(error));
  }
  auto file_size = s.st_size;
  if (!in_range<int>(file_size)) {
    return read_file_result::failure(
        std::string("file too large to read into memory: ") + path);
  }
  return read_file_with_expected_size(
      /*file=*/file, /*path=*/path, /*file_size=*/narrow_cast<int>(file_size),
      /*buffer_size=*/reasonable_buffer_size(s));
}
#endif

#if defined(QLJS_FILE_WINDOWS)
read_file_result read_file(const char *path) {
  std::optional<std::wstring> wpath = quick_lint_js::mbstring_to_wstring(path);
  if (!wpath) {
    DWORD error = ::GetLastError();
    return read_file_result::failure(std::string("failed to convert ") + path +
                                     " to wstring\n" +
                                     windows_error_message(error));
  }
  HANDLE handle = ::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_READ,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/OPEN_EXISTING,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr);
  if (handle == INVALID_HANDLE_VALUE) {
    DWORD error = ::GetLastError();
    read_file_result result =
        read_file_result::failure(std::string("failed to open ") + path + ": " +
                                  windows_error_message(error));
    result.is_not_found_error = error == ERROR_FILE_NOT_FOUND;
    return result;
  }
  windows_handle_file file(handle);
  return read_file(path, file.ref());
}

read_file_result read_stdin() {
  windows_handle_file_ref file(::GetStdHandle(STD_INPUT_HANDLE));
  return read_file("<stdin>", file);
}
#endif

#if defined(QLJS_FILE_POSIX)
read_file_result read_file(const char *path) {
  int fd = ::open(path, O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    int error = errno;
    read_file_result result = read_file_result::failure(
        std::string("failed to open ") + path + ": " + std::strerror(error));
    result.is_not_found_error = error == ENOENT;
    return result;
  }
  posix_fd_file file(fd);
  return read_file(path, file.ref());
}

read_file_result read_stdin() {
  posix_fd_file_ref file(STDIN_FILENO);
  return read_file("<stdin>", file);
}
#endif

void write_file(const std::string &path, string8_view content) {
  write_file(path.c_str(), content);
}

void write_file(const char *path, string8_view content) {
  FILE *file = std::fopen(path, "wb");
  if (!file) {
    std::fprintf(stderr, "fatal: failed to open file %s for writing: %s\n",
                 path, std::strerror(errno));
    std::abort();
  }

  std::size_t written = std::fwrite(content.data(), 1, content.size(), file);
  if (written != content.size()) {
    std::fprintf(stderr, "fatal: failed to write entirely of file %s\n", path);
    std::abort();
  }
  std::fflush(file);
  if (std::ferror(file)) {
    std::fprintf(stderr, "fatal: failed to write file %s: %s\n", path,
                 std::strerror(errno));
    std::abort();
  }

  std::fclose(file);
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
