// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

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
#include <quick-lint-js/unreachable.h>
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
#include <quick-lint-js/windows.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#define QLJS_FILE_WINDOWS
#elif QLJS_HAVE_UNISTD_H
#define QLJS_FILE_POSIX
#else
#error "Unsupported platform"
#endif

using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
platform_file_io_error file_too_large_error() {
#if QLJS_HAVE_WINDOWS_H
  return windows_file_io_error{ERROR_FILE_TOO_LARGE};
#elif QLJS_HAVE_UNISTD_H
  return posix_file_io_error{EFBIG};
#else
#error "Unknown platform"
#endif
}

result<void, platform_file_io_error> read_file_buffered(
    platform_file_ref file, int buffer_size, padded_string *out_content) {
  // TODO(strager): Use byte_buffer to avoid copying the file content every
  // iteration.
  for (;;) {
    int size_before = out_content->size();
    {
      std::optional<int> new_size = checked_add(size_before, buffer_size);
      if (!new_size.has_value()) {
        // TODO(strager): Should we try a small buffer size?
        return result<void, platform_file_io_error>::failure(
            file_too_large_error());
      }
      out_content->resize_grow_uninitialized(size_before + buffer_size);
    }

    file_read_result read_result =
        file.read(&out_content->data()[size_before], buffer_size);
    if (!read_result.ok()) return read_result.propagate();
    if (read_result.at_end_of_file()) {
      // We read the entire file.
      out_content->resize(size_before);
      return {};
    }
    std::optional<int> new_size =
        checked_add(size_before, read_result.bytes_read());
    QLJS_ASSERT(new_size.has_value());
    out_content->resize(*new_size);
  }
}

result<padded_string, platform_file_io_error> read_file_with_expected_size(
    platform_file_ref file, int file_size, int buffer_size) {
  padded_string content;

  std::optional<int> size_to_read = checked_add(file_size, 1);
  if (!size_to_read.has_value()) {
    return result<padded_string, platform_file_io_error>::failure(
        file_too_large_error());
  }
  content.resize_grow_uninitialized(*size_to_read);

  file_read_result read_result = file.read(content.data(), *size_to_read);
  if (!read_result.ok()) return read_result.propagate();
  if (read_result.at_end_of_file()) {
    // The file was empty.
    content.resize(0);
    return content;
  }
  if (read_result.bytes_read() == file_size) {
    // We possibly read the entire file. Make extra sure by reading one more
    // byte.
    file_read_result extra_read_result =
        file.read(content.data() + file_size, 1);
    if (!extra_read_result.ok()) return extra_read_result.propagate();
    if (extra_read_result.at_end_of_file()) {
      // We definitely read the entire file.
      content.resize(read_result.bytes_read());
      return content;
    } else {
      // We didn't read the entire file the first time. Keep reading.
      content.resize(read_result.bytes_read() + extra_read_result.bytes_read());
      result<void, platform_file_io_error> r =
          read_file_buffered(file, buffer_size, &content);
      if (!r.ok()) return r.propagate();
      return content;
    }
  } else {
    content.resize(read_result.bytes_read());
    // We did not read the entire file. There is more data to read.
    result<void, platform_file_io_error> r =
        read_file_buffered(file, buffer_size, &content);
    if (!r.ok()) return r.propagate();
    return content;
  }
}
}

bool read_file_io_error::is_file_not_found_error() const noexcept {
  return this->io_error.is_file_not_found_error();
}

std::string read_file_io_error::to_string() const {
  return "failed to read from "s + this->path + ": "s +
         this->io_error.to_string();
}

[[noreturn]] void read_file_io_error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

std::string write_file_io_error::to_string() const {
  return "failed to write to "s + this->path + ": "s +
         this->io_error.to_string();
}

[[noreturn]] void write_file_io_error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

bool operator==(const read_file_io_error &lhs,
                const read_file_io_error &rhs) noexcept {
  return lhs.path == rhs.path && lhs.io_error == rhs.io_error;
}

bool operator!=(const read_file_io_error &lhs,
                const read_file_io_error &rhs) noexcept {
  return !(lhs == rhs);
}

#if defined(QLJS_FILE_WINDOWS)
result<padded_string, platform_file_io_error> read_file(
    windows_handle_file_ref file) {
  int buffer_size = 1024;  // TODO(strager): Compute a good buffer size.

  ::LARGE_INTEGER file_size;
  if (!::GetFileSizeEx(file.get(), &file_size)) {
    return result<padded_string, windows_file_io_error>::failure<
        windows_file_io_error>(windows_file_io_error{::GetLastError()});
  }
  if (!in_range<int>(file_size.QuadPart)) {
    return result<padded_string, platform_file_io_error>::failure(
        file_too_large_error());
  }

  return read_file_with_expected_size(
      /*file=*/file,
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

result<padded_string, platform_file_io_error> read_file(
    posix_fd_file_ref file) {
  struct stat s;
  int rc = ::fstat(file.get(), &s);
  if (rc == -1) {
    return result<padded_string, posix_file_io_error>::failure(
        posix_file_io_error{errno});
  }
  auto file_size = s.st_size;
  if (!in_range<int>(file_size)) {
    return result<padded_string, platform_file_io_error>::failure(
        file_too_large_error());
  }

  return read_file_with_expected_size(
      /*file=*/file, /*file_size=*/narrow_cast<int>(file_size),
      /*buffer_size=*/reasonable_buffer_size(s));
}
#endif

result<padded_string, read_file_io_error> read_file(const char *path,
                                                    platform_file_ref file) {
  result<padded_string, platform_file_io_error> r = read_file(file);
  if (!r.ok()) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{.path = path, .io_error = r.error()});
  }
  return *std::move(r);
}

#if defined(QLJS_FILE_WINDOWS)
result<padded_string, read_file_io_error> read_file(const char *path) {
  // TODO(strager): Avoid copying the path string, especially on success.
  std::optional<std::wstring> wpath = quick_lint_js::mbstring_to_wstring(path);
  if (!wpath) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{
            .path = path, .io_error = windows_file_io_error{::GetLastError()}});
  }
  HANDLE handle = ::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_READ,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/OPEN_EXISTING,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr);
  if (handle == INVALID_HANDLE_VALUE) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{
            .path = path, .io_error = windows_file_io_error{::GetLastError()}});
  }
  windows_handle_file file(handle);
  return read_file(path, file.ref());
}

result<padded_string, read_file_io_error> read_stdin() {
  windows_handle_file_ref file(::GetStdHandle(STD_INPUT_HANDLE));
  return read_file("<stdin>", file);
}
#endif

#if defined(QLJS_FILE_POSIX)
result<padded_string, read_file_io_error> read_file(const char *path) {
  int fd = ::open(path, O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{.path = path,
                           .io_error = posix_file_io_error{errno}});
  }
  posix_fd_file file(fd);
  return read_file(path, file.ref());
}

result<padded_string, read_file_io_error> read_stdin() {
  posix_fd_file_ref file(STDIN_FILENO);
  return read_file("<stdin>", file);
}
#endif

padded_string read_file_or_exit(const char *path) {
  result<padded_string, read_file_io_error> r = read_file(path);
  if (!r.ok()) {
    r.error().print_and_exit();
  }
  return *std::move(r);
}

result<void, write_file_io_error> write_file(const std::string &path,
                                             string8_view content) {
  return write_file(path.c_str(), content);
}

#if defined(QLJS_FILE_POSIX)
result<platform_file, write_file_io_error> open_file_for_writing(
    const char *path) {
  posix_fd_file file(
      ::open(path, O_CLOEXEC | O_CREAT | O_TRUNC | O_WRONLY, 0644));
  if (!file.valid()) {
    return result<platform_file, write_file_io_error>::failure(
        write_file_io_error{
            .path = path,
            .io_error = posix_file_io_error{errno},
        });
  }
  return std::move(file);
}
#endif

#if defined(QLJS_FILE_POSIX)
result<void, write_file_io_error> write_file(const char *path,
                                             string8_view content) {
  auto file = open_file_for_writing(path);
  if (!file.ok()) {
    return file.propagate();
  }

  ssize_t written = ::write(file->get(), content.data(), content.size());
  if (written == -1) {
    return result<void, write_file_io_error>::failure(write_file_io_error{
        .path = path,
        .io_error = posix_file_io_error{errno},
    });
  }
  if (narrow_cast<std::size_t>(written) != content.size()) {
    return result<void, write_file_io_error>::failure(write_file_io_error{
        .path = path,
        .io_error = posix_file_io_error{EIO},
    });
  }

  return {};
}
#endif

#if defined(QLJS_FILE_WINDOWS)
result<platform_file, write_file_io_error> open_file_for_writing(
    const char *path) {
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath) {
    return result<void, write_file_io_error>::failure(write_file_io_error{
        .path = path,
        .io_error = windows_file_io_error{::GetLastError()},
    });
  }
  windows_handle_file file(::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_WRITE,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/CREATE_ALWAYS,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr));
  if (!file.valid()) {
    return result<platform_file, write_file_io_error>::failure(
        write_file_io_error{
            .path = path,
            .io_error = windows_file_io_error{::GetLastError()},
        });
  }
  return std::move(file);
}
#endif

#if QLJS_FILE_WINDOWS
result<void, write_file_io_error> write_file(const char *path,
                                             string8_view content) {
  auto file = open_file_for_writing(path);
  if (!file.ok()) {
    return file.propagate();
  }

  ::DWORD bytes_written;
  if (!::WriteFile(
          /*hFile=*/file->get(),
          /*lpBuffer=*/content.data(),
          /*nNumberOfBytesToWrite=*/narrow_cast<::DWORD>(content.size()),
          /*lpNumberOfBytesWritten=*/&bytes_written,
          /*lpOverlapped=*/nullptr)) {
    return result<void, write_file_io_error>::failure(write_file_io_error{
        .path = path,
        .io_error = windows_file_io_error{::GetLastError()},
    });
  }
  if (bytes_written != content.size()) {
    return result<void, write_file_io_error>::failure(write_file_io_error{
        .path = path,
        .io_error = windows_file_io_error{ERROR_PARTIAL_COPY},
    });
  }

  return {};
}
#endif

void write_file_or_exit(const std::string &path, string8_view content) {
  write_file_or_exit(path.c_str(), content);
}

void write_file_or_exit(const char *path, string8_view content) {
  result<void, write_file_io_error> result = write_file(path, content);
  if (!result.ok()) {
    result.error().print_and_exit();
  }
}

#if QLJS_HAVE_WINDOWS_H
bool file_ids_equal(const ::FILE_ID_INFO &a, const ::FILE_ID_INFO &b) noexcept {
  return b.VolumeSerialNumber == a.VolumeSerialNumber &&
         std::memcmp(&b.FileId, &a.FileId, sizeof(b.FileId)) == 0;
}
#endif
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
