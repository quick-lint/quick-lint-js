// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_HANDLE_H
#define QUICK_LINT_JS_FILE_HANDLE_H

#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/result.h>
#include <string>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
struct windows_file_io_error {
  // Error code returned by Win32's GetLastError().
  DWORD error;

  bool is_file_not_found_error() const noexcept;

  std::string to_string() const;

  friend bool operator==(windows_file_io_error, windows_file_io_error) noexcept;
  friend bool operator!=(windows_file_io_error, windows_file_io_error) noexcept;
};
#endif

#if QLJS_HAVE_UNISTD_H
struct posix_file_io_error {
  // Error code stored in POSIX' errno variable.
  int error;

  bool is_file_not_found_error() const noexcept;

  std::string to_string() const;

  friend bool operator==(posix_file_io_error, posix_file_io_error) noexcept;
  friend bool operator!=(posix_file_io_error, posix_file_io_error) noexcept;
};
#endif

#if defined(__EMSCRIPTEN__)
// No platform_file_io_error on the web.
#elif QLJS_HAVE_WINDOWS_H
using platform_file_io_error = windows_file_io_error;
#elif QLJS_HAVE_UNISTD_H
using platform_file_io_error = posix_file_io_error;
#else
#error "Unknown platform"
#endif

#if defined(__EMSCRIPTEN__)
// No file_read_result on the web.
#else
// A file_read_result represents the effect of a call to
// platform_file_ref::read.
//
// A file_read_result is in exactly one of three states:
//
//   state       | ->has_value() | .ok()
// --------------+---------------+----------------------------
//   end of file | false         | true
//   error       | false         | false
//   success     | true          | true
struct file_read_result
    : public result<std::optional<int>, platform_file_io_error> {
  using base = result<std::optional<int>, platform_file_io_error>;

  using base::result;

  /*implicit*/ file_read_result(base &&r) : base(std::move(r)) {}

  /*implicit*/ file_read_result(int bytes_read)
      : file_read_result(std::optional<int>(bytes_read)) {}

  static file_read_result end_of_file() noexcept {
    return file_read_result(std::optional<int>());
  }

  bool at_end_of_file() const noexcept {
    return this->ok() && !this->value().has_value();
  }

  int bytes_read() const noexcept {
    QLJS_ASSERT(this->ok());
    QLJS_ASSERT(!this->at_end_of_file());
    return ***this;
  }
};
#endif

#if QLJS_HAVE_WINDOWS_H
// windows_handle_file_ref is a non-owning reference to a Win32 file handle.
//
// windows_handle_file_ref may hold an invalid handle (NULL or
// INVALID_HANDLE_VALUE).
class windows_handle_file_ref {
 public:
  explicit windows_handle_file_ref() noexcept;
  explicit windows_handle_file_ref(HANDLE) noexcept;

  bool valid() const noexcept;

  HANDLE get() noexcept;

  file_read_result read(void *buffer, int buffer_size) noexcept;
  result<std::size_t, windows_file_io_error> write(
      const void *buffer, std::size_t buffer_size) noexcept;
  result<void, windows_file_io_error> write_full(
      const void *buffer, std::size_t buffer_size) noexcept;

  bool is_pipe_non_blocking();
  void set_pipe_non_blocking();
  std::size_t get_pipe_buffer_size();

  static std::string get_last_error_message();

  static windows_handle_file_ref get_stdout() noexcept;
  static windows_handle_file_ref get_stderr() noexcept;

 protected:
  HANDLE handle_;

  static constexpr HANDLE invalid_handle_1 = nullptr;
  static inline const HANDLE invalid_handle_2 = INVALID_HANDLE_VALUE;
};

// windows_handle_file is the owner of a Win32 file handle.
//
// windows_handle_file may hold an invalid handle (NULL or
// INVALID_HANDLE_VALUE).
class windows_handle_file : private windows_handle_file_ref {
 public:
  explicit windows_handle_file() noexcept;
  explicit windows_handle_file(HANDLE) noexcept;

  windows_handle_file(const windows_handle_file &) = delete;
  windows_handle_file &operator=(const windows_handle_file &) = delete;

  windows_handle_file(windows_handle_file &&) noexcept;
  windows_handle_file &operator=(windows_handle_file &&) noexcept;

  ~windows_handle_file();

  void close();

  windows_handle_file_ref ref() const noexcept;

  using windows_handle_file_ref::get;
  using windows_handle_file_ref::get_last_error_message;
  using windows_handle_file_ref::get_pipe_buffer_size;
  using windows_handle_file_ref::is_pipe_non_blocking;
  using windows_handle_file_ref::read;
  using windows_handle_file_ref::set_pipe_non_blocking;
  using windows_handle_file_ref::valid;
  using windows_handle_file_ref::write;
  using windows_handle_file_ref::write_full;
};
#endif

#if QLJS_HAVE_UNISTD_H
// posix_fd_file_ref is a non-owning reference to a POSIX file descriptor.
//
// posix_fd_file_ref may hold an invalid fd (-1).
class posix_fd_file_ref {
 public:
  explicit posix_fd_file_ref() noexcept;
  explicit posix_fd_file_ref(int fd) noexcept;

  bool valid() const noexcept;

  int get() const noexcept;

  file_read_result read(void *buffer, int buffer_size) noexcept;
  result<std::size_t, posix_file_io_error> write(
      const void *buffer, std::size_t buffer_size) noexcept;
  result<void, posix_file_io_error> write_full(
      const void *buffer, std::size_t buffer_size) noexcept;

  bool is_pipe_non_blocking();
  void set_pipe_non_blocking();
  std::size_t get_pipe_buffer_size();

  static std::string get_last_error_message();

  static posix_fd_file_ref get_stdout() noexcept;
  static posix_fd_file_ref get_stderr() noexcept;

 protected:
  static constexpr int invalid_fd = -1;

  int fd_;
};

// posix_fd_file is the owner of a POSIX file descriptor.
//
// posix_fd_file may hold an invalid fd (-1).
class posix_fd_file : private posix_fd_file_ref {
 public:
  explicit posix_fd_file() noexcept;
  explicit posix_fd_file(int fd) noexcept;

  posix_fd_file(const posix_fd_file &) = delete;
  posix_fd_file &operator=(const posix_fd_file &) = delete;

  posix_fd_file(posix_fd_file &&) noexcept;
  posix_fd_file &operator=(posix_fd_file &&) noexcept;

  ~posix_fd_file();

  void close();

  posix_fd_file_ref ref() const noexcept;

  using posix_fd_file_ref::get;
  using posix_fd_file_ref::get_last_error_message;
  using posix_fd_file_ref::get_pipe_buffer_size;
  using posix_fd_file_ref::is_pipe_non_blocking;
  using posix_fd_file_ref::read;
  using posix_fd_file_ref::set_pipe_non_blocking;
  using posix_fd_file_ref::valid;
  using posix_fd_file_ref::write;
  using posix_fd_file_ref::write_full;
};
#endif

#if defined(__EMSCRIPTEN__)
// No platform_file or platform_file_ref on web.
#elif QLJS_HAVE_WINDOWS_H
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
