// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_FILE_HANDLE_H
#define QUICK_LINT_JS_IO_FILE_HANDLE_H

#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/port/have.h>
#include <string>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
struct Windows_File_IO_Error {
  // Error code returned by Win32's GetLastError().
  DWORD error;

  bool is_file_not_found_error() const;
  bool is_not_a_directory_error() const;

  std::string to_string() const;

  friend bool operator==(Windows_File_IO_Error, Windows_File_IO_Error);
  friend bool operator!=(Windows_File_IO_Error, Windows_File_IO_Error);
};
#endif

#if QLJS_HAVE_UNISTD_H
struct POSIX_File_IO_Error {
  // Error code stored in POSIX' errno variable.
  int error;

  bool is_file_not_found_error() const;
  bool is_not_a_directory_error() const;

  std::string to_string() const;

  friend bool operator==(POSIX_File_IO_Error, POSIX_File_IO_Error);
  friend bool operator!=(POSIX_File_IO_Error, POSIX_File_IO_Error);
};
#endif

#if defined(__EMSCRIPTEN__)
// No Platform_File_IO_Error on the web.
#elif QLJS_HAVE_WINDOWS_H
using Platform_File_IO_Error = Windows_File_IO_Error;
#elif QLJS_HAVE_UNISTD_H
using Platform_File_IO_Error = POSIX_File_IO_Error;
#else
#error "Unknown platform"
#endif

#if defined(__EMSCRIPTEN__)
// No File_Read_Result on the web.
#else
// A File_Read_Result represents the effect of a call to
// Platform_File_Ref::read.
//
// A File_Read_Result is in exactly one of three states:
//
//   state       | ->has_value() | .ok()
// --------------+---------------+----------------------------
//   end of file | false         | true
//   error       | false         | false
//   success     | true          | true
struct File_Read_Result
    : public Result<std::optional<int>, Platform_File_IO_Error> {
  using Base = Result<std::optional<int>, Platform_File_IO_Error>;

  using Base::Result;

  /*implicit*/ File_Read_Result(Base &&r) : Base(std::move(r)) {}

  /*implicit*/ File_Read_Result(int bytes_read)
      : File_Read_Result(std::optional<int>(bytes_read)) {}

  static File_Read_Result end_of_file() {
    return File_Read_Result(std::optional<int>());
  }

  bool at_end_of_file() const {
    return this->ok() && !this->value().has_value();
  }

  int bytes_read() const {
    QLJS_ASSERT(this->ok());
    QLJS_ASSERT(!this->at_end_of_file());
    return ***this;
  }
};
#endif

#if QLJS_HAVE_WINDOWS_H
// Windows_Handle_File_Ref is a non-owning reference to a Win32 file handle.
//
// Windows_Handle_File_Ref may hold an invalid handle (NULL or
// INVALID_HANDLE_VALUE).
class Windows_Handle_File_Ref {
 public:
  explicit Windows_Handle_File_Ref();
  explicit Windows_Handle_File_Ref(HANDLE);

  bool valid() const;

  HANDLE get();

  File_Read_Result read(void *buffer, int buffer_size);
  Result<std::size_t, Windows_File_IO_Error> write(const void *buffer,
                                                   std::size_t buffer_size);
  Result<void, Windows_File_IO_Error> write_full(const void *buffer,
                                                 std::size_t buffer_size);

  bool is_pipe_non_blocking();
  void set_pipe_non_blocking();
  std::size_t get_pipe_buffer_size();

  static std::string get_last_error_message();

  static Windows_Handle_File_Ref get_stdout();
  static Windows_Handle_File_Ref get_stderr();

 protected:
  HANDLE handle_;

  static constexpr HANDLE invalid_handle_1 = nullptr;
  static inline const HANDLE invalid_handle_2 = INVALID_HANDLE_VALUE;
};

// Windows_Handle_File is the owner of a Win32 file handle.
//
// Windows_Handle_File may hold an invalid handle (NULL or
// INVALID_HANDLE_VALUE).
class Windows_Handle_File : private Windows_Handle_File_Ref {
 public:
  explicit Windows_Handle_File();
  explicit Windows_Handle_File(HANDLE);

  Windows_Handle_File(const Windows_Handle_File &) = delete;
  Windows_Handle_File &operator=(const Windows_Handle_File &) = delete;

  Windows_Handle_File(Windows_Handle_File &&);
  Windows_Handle_File &operator=(Windows_Handle_File &&);

  ~Windows_Handle_File();

  void close();

  Windows_Handle_File_Ref ref() const;

  using Windows_Handle_File_Ref::get;
  using Windows_Handle_File_Ref::get_last_error_message;
  using Windows_Handle_File_Ref::get_pipe_buffer_size;
  using Windows_Handle_File_Ref::is_pipe_non_blocking;
  using Windows_Handle_File_Ref::read;
  using Windows_Handle_File_Ref::set_pipe_non_blocking;
  using Windows_Handle_File_Ref::valid;
  using Windows_Handle_File_Ref::write;
  using Windows_Handle_File_Ref::write_full;
};
#endif

#if QLJS_HAVE_UNISTD_H
// POSIX_FD_File_Ref is a non-owning reference to a POSIX file descriptor.
//
// POSIX_FD_File_Ref may hold an invalid fd (-1).
class POSIX_FD_File_Ref {
 public:
  explicit POSIX_FD_File_Ref();
  explicit POSIX_FD_File_Ref(int fd);

  bool valid() const;

  int get() const;

  File_Read_Result read(void *buffer, int buffer_size);
  Result<std::size_t, POSIX_File_IO_Error> write(const void *buffer,
                                                 std::size_t buffer_size);
  Result<void, POSIX_File_IO_Error> write_full(const void *buffer,
                                               std::size_t buffer_size);

  bool is_pipe_non_blocking();
  void set_pipe_non_blocking();
  std::size_t get_pipe_buffer_size();

  // Returns true if the two file descriptors are the same. (After a dup(), the
  // two file descriptors are considered different.)
  friend bool operator==(POSIX_FD_File_Ref, POSIX_FD_File_Ref);
  friend bool operator!=(POSIX_FD_File_Ref, POSIX_FD_File_Ref);

  static std::string get_last_error_message();

  static POSIX_FD_File_Ref get_stdout();
  static POSIX_FD_File_Ref get_stderr();

 protected:
  static constexpr int invalid_fd = -1;

  int fd_;
};

// POSIX_FD_File is the owner of a POSIX file descriptor.
//
// POSIX_FD_File may hold an invalid fd (-1).
class POSIX_FD_File : private POSIX_FD_File_Ref {
 public:
  explicit POSIX_FD_File();
  explicit POSIX_FD_File(int fd);

  POSIX_FD_File(const POSIX_FD_File &) = delete;
  POSIX_FD_File &operator=(const POSIX_FD_File &) = delete;

  POSIX_FD_File(POSIX_FD_File &&);
  POSIX_FD_File &operator=(POSIX_FD_File &&);

  ~POSIX_FD_File();

  void close();

  POSIX_FD_File_Ref ref() const;

  // Remove ownership of the file.
  //
  // Returns this->get().
  //
  // Precondition: this->valid()
  // Postcondition: !this->valid()
  int release();

  using POSIX_FD_File_Ref::get;
  using POSIX_FD_File_Ref::get_last_error_message;
  using POSIX_FD_File_Ref::get_pipe_buffer_size;
  using POSIX_FD_File_Ref::is_pipe_non_blocking;
  using POSIX_FD_File_Ref::read;
  using POSIX_FD_File_Ref::set_pipe_non_blocking;
  using POSIX_FD_File_Ref::valid;
  using POSIX_FD_File_Ref::write;
  using POSIX_FD_File_Ref::write_full;
};
#endif

#if defined(__EMSCRIPTEN__)
// No Platform_File or Platform_File_Ref on web.
#elif QLJS_HAVE_WINDOWS_H
using Platform_File = Windows_Handle_File;
using Platform_File_Ref = Windows_Handle_File_Ref;
#elif QLJS_HAVE_UNISTD_H
using Platform_File = POSIX_FD_File;
using Platform_File_Ref = POSIX_FD_File_Ref;
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
