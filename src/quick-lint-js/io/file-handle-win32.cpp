// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_WINDOWS_H

#include <array>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <limits>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/port/windows.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <utility>

namespace quick_lint_js {
bool Windows_File_IO_Error::is_file_not_found_error() const noexcept {
  return this->error == ERROR_FILE_NOT_FOUND;
}

bool Windows_File_IO_Error::is_not_a_directory_error() const noexcept {
  return this->error == ERROR_DIRECTORY;
}

std::string Windows_File_IO_Error::to_string() const {
  return windows_error_message(this->error);
}

bool operator==(Windows_File_IO_Error lhs, Windows_File_IO_Error rhs) noexcept {
  return lhs.error == rhs.error;
}

bool operator!=(Windows_File_IO_Error lhs, Windows_File_IO_Error rhs) noexcept {
  return !(lhs == rhs);
}

Windows_Handle_File_Ref::Windows_Handle_File_Ref() noexcept
    : handle_(this->invalid_handle_1) {}

Windows_Handle_File_Ref::Windows_Handle_File_Ref(HANDLE handle) noexcept
    : handle_(handle) {}

bool Windows_Handle_File_Ref::valid() const noexcept {
  return this->handle_ != this->invalid_handle_1 &&
         this->handle_ != this->invalid_handle_2;
}

HANDLE Windows_Handle_File_Ref::get() noexcept { return this->handle_; }

File_Read_Result Windows_Handle_File_Ref::read(void *buffer,
                                               int buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  DWORD read_size;
  if (!::ReadFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                  &read_size,
                  /*lpOverlapped=*/nullptr)) {
    DWORD error = ::GetLastError();
    switch (error) {
    case ERROR_BROKEN_PIPE:
      return File_Read_Result::end_of_file();
    case ERROR_NO_DATA:
      return 0;
    default:
      return failed_result(Windows_File_IO_Error{error});
    };
  }
  // TODO(strager): Microsoft's documentation for ReadFile claims the following:
  //
  // > If the lpNumberOfBytesRead parameter is zero when ReadFile returns TRUE
  // > on a pipe, the other end of the pipe called the WriteFile function with
  // > nNumberOfBytesToWrite set to zero.
  //
  // https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
  //
  // In my experiments, I haven't been able to make ReadFile give 0-bytes-read
  // in this case. However, given the documentation, when we get 0 bytes read,
  // we should ask the pipe if we reached EOF.
  return read_size == 0 ? File_Read_Result::end_of_file()
                        : File_Read_Result(narrow_cast<int>(read_size));
}

Result<std::size_t, Windows_File_IO_Error> Windows_Handle_File_Ref::write(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  ::DWORD size_to_write = narrow_cast<::DWORD>(buffer_size);
  ::DWORD write_size;
  if (!::WriteFile(this->handle_, buffer, size_to_write, &write_size,
                   /*lpOverlapped=*/nullptr)) {
    return failed_result(Windows_File_IO_Error{::GetLastError()});
  }
  return write_size;
}

Result<void, Windows_File_IO_Error> Windows_Handle_File_Ref::write_full(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  auto write_result = this->write(buffer, buffer_size);
  if (!write_result.ok()) {
    return write_result.propagate();
  }
  if (*write_result != buffer_size) {
    // TODO(strager): Should we retry with the remaining buffer?
    return failed_result(Windows_File_IO_Error{ERROR_PARTIAL_COPY});
  }
  return {};
}

bool Windows_Handle_File_Ref::is_pipe_non_blocking() {
  QLJS_ASSERT(this->valid());
  DWORD state;
  BOOL ok = ::GetNamedPipeHandleStateA(this->get(),
                                       /*lpState=*/&state,
                                       /*lpCurInstances=*/nullptr,
                                       /*lpMaxCollectionCount=*/nullptr,
                                       /*lpCollectDataTimeout=*/nullptr,
                                       /*lpUserName=*/nullptr,
                                       /*nMaxUserNameSize=*/0);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
  return (state & PIPE_NOWAIT) == PIPE_NOWAIT;
}

void Windows_Handle_File_Ref::set_pipe_non_blocking() {
  QLJS_ASSERT(this->valid());
  DWORD mode = PIPE_READMODE_BYTE | PIPE_NOWAIT;
  BOOL ok = ::SetNamedPipeHandleState(this->get(), /*lpMode=*/&mode,
                                      /*lpMaxCollectionCount=*/nullptr,
                                      /*lpCollectDataTimeout=*/nullptr);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
}

std::size_t Windows_Handle_File_Ref::get_pipe_buffer_size() {
  QLJS_ASSERT(this->valid());
  DWORD outBufferSize = 0;
  BOOL ok =
      ::GetNamedPipeInfo(this->handle_, /*lpFlags=*/nullptr, &outBufferSize,
                         /*lpInBufferSize=*/nullptr,
                         /*lpMaxInstances=*/nullptr);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
  return outBufferSize;
}

std::string Windows_Handle_File_Ref::get_last_error_message() {
  return windows_last_error_message();
}

Windows_Handle_File_Ref Windows_Handle_File_Ref::get_stdout() noexcept {
  return Windows_Handle_File_Ref(::GetStdHandle(STD_OUTPUT_HANDLE));
}

Windows_Handle_File_Ref Windows_Handle_File_Ref::get_stderr() noexcept {
  // HACK(strager): During a death test, Google Test closes the standard error
  // handle. Make our own copy of the handle to avoid conflicts with Google
  // Test.
  // NOTE(strager): We don't need to close this handle on destruction, so a raw
  // HANDLE is fine.
  static ::HANDLE stderr_copy = []() -> ::HANDLE {
    ::HANDLE new_handle;
    ::HANDLE current_process = ::GetCurrentProcess();
    ::BOOL ok = ::DuplicateHandle(
        /*hSourceProcessHandle=*/current_process,
        /*hSourceHandle=*/::GetStdHandle(STD_ERROR_HANDLE),
        /*hTargetProcessHandle=*/current_process,
        /*lpTargetHandle=*/&new_handle,
        /*dwDesiredAccess=*/0,
        /*bInheritHandle=*/true,
        /*dwOptions=*/DUPLICATE_SAME_ACCESS);
    QLJS_ALWAYS_ASSERT(ok);
    return new_handle;
  }();
  return Windows_Handle_File_Ref(stderr_copy);
}

Windows_Handle_File::Windows_Handle_File() noexcept = default;

Windows_Handle_File::Windows_Handle_File(HANDLE handle) noexcept
    : Windows_Handle_File_Ref(handle) {}

Windows_Handle_File::Windows_Handle_File(Windows_Handle_File &&other) noexcept
    : Windows_Handle_File_Ref(
          std::exchange(other.handle_, this->invalid_handle_1)) {}

Windows_Handle_File &Windows_Handle_File::operator=(
    Windows_Handle_File &&other) noexcept {
  if (this->valid()) {
    this->close();
  }
  std::swap(this->handle_, other.handle_);
  QLJS_ASSERT(!other.valid());
  return *this;
}

Windows_Handle_File::~Windows_Handle_File() {
  if (this->valid()) {
    this->close();
  }
}

void Windows_Handle_File::close() {
  QLJS_ASSERT(this->valid());
  if (!::CloseHandle(this->handle_)) {
    std::fprintf(stderr, "error: failed to close file\n");
  }
  this->handle_ = this->invalid_handle_1;
}

Windows_Handle_File_Ref Windows_Handle_File::ref() const noexcept {
  return *this;
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
