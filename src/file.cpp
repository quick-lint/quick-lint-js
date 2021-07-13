// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <boost/leaf/common.hpp>
#include <boost/leaf/context.hpp>
#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/on_error.hpp>
#include <boost/leaf/result.hpp>
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
#include <quick-lint-js/leaf.h>
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
struct read_file_io_error {
  std::string path;
  platform_file_io_error io_error;

  boost::leaf::error_id make_leaf_error() const {
    auto path_guard = boost::leaf::on_error(e_file_path{this->path});
    return this->io_error.make_leaf_error();
  }
};

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

#if defined(QLJS_FILE_WINDOWS)
namespace {
result<padded_string, platform_file_io_error> read_file_2(
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

result<padded_string, platform_file_io_error> read_file_2(
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
}
#endif

boost::leaf::result<padded_string> read_file(platform_file_ref file) {
  auto api_guard = boost::leaf::on_error(e_api_read_file());
  result<padded_string, platform_file_io_error> r = read_file_2(file);
  if (!r.ok()) return r.error().make_leaf_error();
  return *std::move(r);
}

namespace {
result<padded_string, read_file_io_error> read_file_2(const char *path,
                                                      platform_file_ref file) {
  result<padded_string, platform_file_io_error> r = read_file_2(file);
  if (!r.ok()) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{.path = path, .io_error = r.error()});
  }
  return *std::move(r);
}
}

#if defined(QLJS_FILE_WINDOWS)
namespace {
result<padded_string, read_file_io_error> read_file_2(const char *path) {
  auto api_guard = boost::leaf::on_error(e_api_read_file());
  // TODO(strager): Avoid copying the path string, especially on success.
  auto path_guard = boost::leaf::on_error(e_file_path{path});
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
  return read_file_2(path, file.ref());
}
}

boost::leaf::result<padded_string> read_stdin() {
  windows_handle_file_ref file(::GetStdHandle(STD_INPUT_HANDLE));
  return read_file(file);
}
#endif

#if defined(QLJS_FILE_POSIX)
namespace {
result<padded_string, read_file_io_error> read_file_2(const char *path) {
  int fd = ::open(path, O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    return result<padded_string, read_file_io_error>::failure(
        read_file_io_error{.path = path,
                           .io_error = posix_file_io_error{errno}});
  }
  posix_fd_file file(fd);
  return read_file_2(path, file.ref());
}
}

boost::leaf::result<padded_string> read_stdin() {
  posix_fd_file_ref file(STDIN_FILENO);
  return read_file(file);
}
#endif

boost::leaf::result<padded_string> read_file(const char *path) {
  auto api_guard = boost::leaf::on_error(e_api_read_file());
  result<padded_string, read_file_io_error> r = read_file_2(path);
  if (!r.ok()) return r.error().make_leaf_error();
  return *std::move(r);
}

sloppy_result<padded_string> read_file_sloppy(const char *path) {
  return boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<sloppy_result<padded_string>> {
        boost::leaf::result<padded_string> content = read_file(path);
        if (!content) return content.error();
        return sloppy_result<padded_string>(std::move(*content));
      },
      make_read_file_error_handlers(
          [](const std::string &message) -> sloppy_result<padded_string> {
            return sloppy_result<padded_string>::failure(message);
          }),
      []() -> sloppy_result<padded_string> { QLJS_UNREACHABLE(); });
}

sloppy_result<padded_string> read_file_sloppy(const char *path,
                                              platform_file_ref file) {
  return boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<sloppy_result<padded_string>> {
        // TODO(strager): Avoid copying the path string, especially on success.
        auto path_guard = boost::leaf::on_error(e_file_path{path});
        boost::leaf::result<padded_string> content = read_file(file);
        if (!content) return content.error();
        return sloppy_result<padded_string>(std::move(*content));
      },
      make_read_file_error_handlers(
          [](const std::string &message) -> sloppy_result<padded_string> {
            return sloppy_result<padded_string>::failure(message);
          }),
      []() -> sloppy_result<padded_string> { QLJS_UNREACHABLE(); });
}

padded_string read_file_or_exit(const char *path) {
  return boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<padded_string> { return read_file(path); },
      exit_on_read_file_error_handlers<padded_string>(),
      []() -> padded_string {
        QLJS_ASSERT(false);
        std::fprintf(stderr, "error: unknown error\n");
        std::exit(1);
      });
}

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
