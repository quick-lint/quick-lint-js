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
#include <cstring>
#include <limits>
#include <optional>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
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

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
windows_handle_file::windows_handle_file(HANDLE handle) noexcept
    : handle_(handle) {}

windows_handle_file::~windows_handle_file() {
  if (!::CloseHandle(this->handle_)) {
    std::fprintf(stderr, "error: failed to close file\n");
  }
}

HANDLE windows_handle_file::get() noexcept { return this->handle_; }

std::optional<int> windows_handle_file::read(void *buffer,
                                             int buffer_size) noexcept {
  DWORD read_size;
  if (!::ReadFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                  &read_size,
                  /*lpOverlapped=*/nullptr)) {
    return std::nullopt;
  }
  return narrow_cast<int>(read_size);
}

std::string windows_handle_file::get_last_error_message() {
  return windows_error_message(::GetLastError());
}
#endif

#if QLJS_HAVE_UNISTD_H
posix_fd_file::posix_fd_file(int fd) noexcept : fd_(fd) {}

posix_fd_file::~posix_fd_file() {
  int rc = ::close(this->fd_);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to close file: %s\n",
                 std::strerror(errno));
  }
}

int posix_fd_file::get() noexcept { return this->fd_; }

std::optional<int> posix_fd_file::read(void *buffer, int buffer_size) noexcept {
  ::ssize_t read_size =
      ::read(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
  if (read_size == -1) {
    return std::nullopt;
  }
  return narrow_cast<int>(read_size);
}

std::string posix_fd_file::get_last_error_message() {
  return std::strerror(errno);
}
#endif

#if QLJS_HAVE_WINDOWS_H
namespace {
std::string_view remove_suffix_if_present(std::string_view s,
                                          std::string_view suffix) noexcept {
  if (s.ends_with(suffix)) {
    s.remove_suffix(suffix.size());
  }
  return s;
}
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
