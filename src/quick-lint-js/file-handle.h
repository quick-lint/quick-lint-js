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

#ifndef QUICK_LINT_JS_FILE_HANDLE_H
#define QUICK_LINT_JS_FILE_HANDLE_H

#include <optional>
#include <quick-lint-js/have.h>
#include <string>

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
std::string windows_error_message(DWORD error);
#endif

#if QLJS_HAVE_WINDOWS_H
class windows_handle_file_ref;

// windows_handle_file is the owner of a Win32 file handle.
class windows_handle_file {
 public:
  explicit windows_handle_file(HANDLE) noexcept;

  windows_handle_file(const windows_handle_file &) = delete;
  windows_handle_file &operator=(const windows_handle_file &) = delete;

  ~windows_handle_file();

  HANDLE get() noexcept;

  std::optional<int> read(void *buffer, int buffer_size) noexcept;
  std::optional<int> write(const void *buffer, int buffer_size) noexcept;

  void close();

  windows_handle_file_ref ref() noexcept;

  static std::string get_last_error_message();

 private:
  static constexpr HANDLE invalid_handle = nullptr;

  HANDLE handle_;
};

// windows_handle_file_ref is a non-owning reference to a Win32 file handle.
class windows_handle_file_ref {
 public:
  explicit windows_handle_file_ref(HANDLE) noexcept;

  HANDLE get() noexcept;

  std::optional<int> read(void *buffer, int buffer_size) noexcept;
  std::optional<int> write(const void *buffer, int buffer_size) noexcept;

  static std::string get_last_error_message();

 private:
  HANDLE handle_;
};
#endif

#if QLJS_HAVE_UNISTD_H
class posix_fd_file_ref;

// posix_fd_file is the owner of a POSIX file descriptor.
class posix_fd_file {
 public:
  explicit posix_fd_file(int fd) noexcept;

  posix_fd_file(const posix_fd_file &) = delete;
  posix_fd_file &operator=(const posix_fd_file &) = delete;

  ~posix_fd_file();

  int get() noexcept;

  std::optional<int> read(void *buffer, int buffer_size) noexcept;
  std::optional<int> write(const void *buffer, int buffer_size) noexcept;

  void close();

  posix_fd_file_ref ref() noexcept;

  static std::string get_last_error_message();

 private:
  static constexpr int invalid_fd = -1;

  int fd_;
};

// posix_fd_file_ref is a non-owning reference to a POSIX file descriptor.
class posix_fd_file_ref {
 public:
  explicit posix_fd_file_ref(int fd) noexcept;

  int get() noexcept;

  std::optional<int> read(void *buffer, int buffer_size) noexcept;
  std::optional<int> write(const void *buffer, int buffer_size) noexcept;

  static std::string get_last_error_message();

 private:
  int fd_;
};
#endif

#if QLJS_HAVE_WINDOWS_H
using platform_file = windows_handle_file;
using platform_file_ref = windows_handle_file_ref;
#elif QLJS_HAVE_UNISTD_H
using platform_file = posix_fd_file;
using platform_file_ref = posix_fd_file_ref;
#else
#error "Unknown platform"
#endif
}

#endif
