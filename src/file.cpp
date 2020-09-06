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

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
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
#if defined(QLJS_FILE_WINDOWS)
std::string windows_error_message(DWORD error);
#endif

#if defined(QLJS_FILE_WINDOWS)
class windows_handle_file {
 public:
  explicit windows_handle_file(HANDLE handle) noexcept : handle_(handle) {}

  windows_handle_file(const windows_handle_file &) = delete;
  windows_handle_file &operator=(const windows_handle_file &) = delete;

  ~windows_handle_file() {
    if (!::CloseHandle(this->handle_)) {
      std::fprintf(stderr, "error: failed to close file\n");
    }
  }

  HANDLE get() noexcept { return this->handle_; }

  std::optional<int> read(void *buffer, int buffer_size) noexcept {
    DWORD read_size;
    if (!::ReadFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                    &read_size,
                    /*lpOverlapped=*/nullptr)) {
      return std::nullopt;
    }
    return narrow_cast<int>(read_size);
  }

  static std::string get_last_error_message() {
    return windows_error_message(::GetLastError());
  }

 private:
  HANDLE handle_;
};
using platform_file = windows_handle_file;
#endif

#if defined(QLJS_FILE_POSIX)
class posix_fd_file {
 public:
  explicit posix_fd_file(int fd) noexcept : fd_(fd) {}

  posix_fd_file(const posix_fd_file &) = delete;
  posix_fd_file &operator=(const posix_fd_file &) = delete;

  ~posix_fd_file() {
    int rc = ::close(this->fd_);
    if (rc != 0) {
      std::fprintf(stderr, "error: failed to close file: %s\n",
                   std::strerror(errno));
    }
  }

  int get() noexcept { return this->fd_; }

  std::optional<int> read(void *buffer, int buffer_size) noexcept {
    ::ssize_t read_size =
        ::read(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
    if (read_size == -1) {
      return std::nullopt;
    }
    return narrow_cast<int>(read_size);
  }

  static std::string get_last_error_message() { return std::strerror(errno); }

 private:
  int fd_;
};
using platform_file = posix_fd_file;
#endif

void read_file_buffered(platform_file &file, int buffer_size,
                        read_file_result *out) {
  for (;;) {
    int size_before = out->content.size();
    // TODO(strager): Check for overflow.
    out->content.resize(size_before + buffer_size);

    std::optional<int> read_size =
        file.read(&out->content.data()[size_before], buffer_size);
    if (!read_size.has_value()) {
      out->error = "failed to read from file: " + file.get_last_error_message();
      return;
    }
    out->content.resize(size_before + *read_size);
    if (*read_size < buffer_size) {
      // We read the entire file.
      return;
    }
  }
}

read_file_result read_file_with_expected_size(platform_file &file,
                                              int file_size, int buffer_size) {
  read_file_result result;
  // TODO(strager): Check for overflow.
  int size_to_read = file_size + 1;
  result.content.resize(size_to_read);
  std::optional<int> read_size = file.read(result.content.data(), size_to_read);
  if (!read_size.has_value()) {
    result.error = "failed to read from file: " + file.get_last_error_message();
    return result;
  }
  result.content.resize(*read_size);
  if (*read_size < size_to_read) {
    // We read the entire file.
    return result;
  } else {
    // We did not read the entire file. There is more data to read.
    read_file_buffered(file, buffer_size, &result);
    return result;
  }
}

#if defined(QLJS_FILE_WINDOWS)
read_file_result read_file(const char *path, windows_handle_file &file) {
  int buffer_size = 1024;  // TODO(strager): Compute a good buffer size.

  ::LARGE_INTEGER file_size;
  if (!::GetFileSizeEx(file.get(), &file_size)) {
    DWORD error = ::GetLastError();
    return read_file_result::failure(
        std::string("failed to get size of file ") + path + ": " +
        windows_error_message(error));
  }
  // TODO(strager): Fail if file_size exceeds int.
  return read_file_with_expected_size(
      /*file=*/file, /*file_size=*/narrow_cast<int>(file_size.QuadPart),
      /*buffer_size=*/buffer_size);
}
#endif

#if defined(QLJS_FILE_POSIX)
read_file_result read_file(const char *path, posix_fd_file &file) {
  int buffer_size = 1024;  // TODO(strager): Compute using stat.

  struct stat s;
  int rc = ::fstat(file.get(), &s);
  if (rc == -1) {
    int error = errno;
    return read_file_result::failure(
        std::string("failed to get file info from ") + path + ": " +
        std::strerror(error));
  }
  auto file_size = s.st_size;
  // TODO(strager): Fail if file_size exceeds int.

  return read_file_with_expected_size(
      /*file=*/file, /*file_size=*/narrow_cast<int>(file_size),
      /*buffer_size=*/buffer_size);
}
#endif
}

#if defined(QLJS_FILE_WINDOWS)
read_file_result read_file(const char *path) {
  // TODO(strager): Use CreateFileW.
  HANDLE handle = ::CreateFileA(
      path, /*dwDesiredAccess=*/GENERIC_READ,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/OPEN_EXISTING,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr);
  if (handle == INVALID_HANDLE_VALUE) {
    DWORD error = ::GetLastError();
    return read_file_result::failure(std::string("failed to open ") + path +
                                     ": " + windows_error_message(error));
  }
  windows_handle_file file(handle);
  return read_file(path, file);
}
#endif

#if defined(QLJS_FILE_POSIX)
read_file_result read_file(const char *path) {
  int fd = ::open(path, O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    int error = errno;
    return read_file_result::failure(std::string("failed to open ") + path +
                                     ": " + std::strerror(error));
  }
  posix_fd_file file(fd);
  return read_file(path, file);
}
#endif

namespace {
#if defined(QLJS_FILE_WINDOWS)
std::string_view remove_suffix_if_present(std::string_view s,
                                          std::string_view suffix) noexcept {
  if (s.ends_with(suffix)) {
    s.remove_suffix(suffix.size());
  }
  return s;
}

std::string windows_error_message(DWORD error) {
  // TODO(strager): Use FormatMessageW.
  LPSTR get_last_error_message;
  DWORD get_last_error_message_length = ::FormatMessageA(
      /*dwFlags=*/FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
      /*lpSource=*/nullptr,
      /*dwMessageId=*/::GetLastError(),
      /*dwLanguageId=*/0,
      /*lpBuffer=*/reinterpret_cast<LPSTR>(&get_last_error_message),
      /*nSize=*/(std::numeric_limits<DWORD>::max)(),
      /*Arguments=*/nullptr);
  if (get_last_error_message_length == 0) {
    // FormatMessageA failed.
    return "unknown error";
  }

  std::string_view message(
      get_last_error_message,
      narrow_cast<std::size_t>(get_last_error_message_length));
  message = remove_suffix_if_present(message, "\r\n");
  std::string message_copy(message);
  static_cast<void>(::LocalFree(get_last_error_message));
  return message_copy;
}
#endif
}
}
