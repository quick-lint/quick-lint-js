// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <limits>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/windows-error.h>
#include <string>
#include <string_view>
#include <utility>

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
#include <quick-lint-js/windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
bool windows_file_io_error::is_file_not_found_error() const noexcept {
  return this->error == ERROR_FILE_NOT_FOUND;
}

std::string windows_file_io_error::to_string() const {
  return windows_error_message(this->error);
}

bool operator==(windows_file_io_error lhs, windows_file_io_error rhs) noexcept {
  return lhs.error == rhs.error;
}

bool operator!=(windows_file_io_error lhs, windows_file_io_error rhs) noexcept {
  return !(lhs == rhs);
}
#endif

#if QLJS_HAVE_UNISTD_H
bool posix_file_io_error::is_file_not_found_error() const noexcept {
  return this->error == ENOENT;
}

std::string posix_file_io_error::to_string() const {
  return std::strerror(this->error);
}

bool operator==(posix_file_io_error lhs, posix_file_io_error rhs) noexcept {
  return lhs.error == rhs.error;
}

bool operator!=(posix_file_io_error lhs, posix_file_io_error rhs) noexcept {
  return !(lhs == rhs);
}
#endif

#if QLJS_HAVE_WINDOWS_H
windows_handle_file_ref::windows_handle_file_ref(HANDLE handle) noexcept
    : handle_(handle) {}

bool windows_handle_file_ref::valid() const noexcept {
  return this->handle_ != this->invalid_handle_1 &&
         this->handle_ != this->invalid_handle_2;
}

HANDLE windows_handle_file_ref::get() noexcept { return this->handle_; }

file_read_result windows_handle_file_ref::read(void *buffer,
                                               int buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  DWORD read_size;
  if (!::ReadFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                  &read_size,
                  /*lpOverlapped=*/nullptr)) {
    DWORD error = ::GetLastError();
    switch (error) {
    case ERROR_BROKEN_PIPE:
      return file_read_result::end_of_file();
    case ERROR_NO_DATA:
      return 0;
    default:
      return file_read_result::failure(windows_file_io_error{error});
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
  return read_size == 0 ? file_read_result::end_of_file()
                        : file_read_result(narrow_cast<int>(read_size));
}

result<void, windows_file_io_error> windows_handle_file_ref::write_full(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  ::DWORD size_to_write = narrow_cast<::DWORD>(buffer_size);
  ::DWORD write_size;
  if (!::WriteFile(this->handle_, buffer, size_to_write, &write_size,
                   /*lpOverlapped=*/nullptr)) {
    return result<void, windows_file_io_error>::failure(
        windows_file_io_error{::GetLastError()});
  }
  if (write_size != size_to_write) {
    // TODO(strager): Should we retry with the remaining buffer?
    return result<void, windows_file_io_error>::failure(
        windows_file_io_error{ERROR_PARTIAL_COPY});
  }
  return {};
}

bool windows_handle_file_ref::is_pipe_non_blocking() {
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

void windows_handle_file_ref::set_pipe_non_blocking() {
  QLJS_ASSERT(this->valid());
  DWORD mode = PIPE_READMODE_BYTE | PIPE_NOWAIT;
  BOOL ok = ::SetNamedPipeHandleState(this->get(), /*lpMode=*/&mode,
                                      /*lpMaxCollectionCount=*/nullptr,
                                      /*lpCollectDataTimeout=*/nullptr);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
}

std::size_t windows_handle_file_ref::get_pipe_buffer_size() {
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

std::string windows_handle_file_ref::get_last_error_message() {
  return windows_last_error_message();
}

windows_handle_file_ref windows_handle_file_ref::get_stdout() noexcept {
  return windows_handle_file_ref(::GetStdHandle(STD_OUTPUT_HANDLE));
}

windows_handle_file_ref windows_handle_file_ref::get_stderr() noexcept {
  return windows_handle_file_ref(::GetStdHandle(STD_ERROR_HANDLE));
}

windows_handle_file::windows_handle_file(HANDLE handle) noexcept
    : windows_handle_file_ref(handle) {}

windows_handle_file::windows_handle_file(windows_handle_file &&other) noexcept
    : windows_handle_file_ref(
          std::exchange(other.handle_, this->invalid_handle_1)) {}

windows_handle_file &windows_handle_file::operator=(
    windows_handle_file &&other) noexcept {
  if (this->valid()) {
    this->close();
  }
  std::swap(this->handle_, other.handle_);
  QLJS_ASSERT(!other.valid());
  return *this;
}

windows_handle_file::~windows_handle_file() {
  if (this->valid()) {
    this->close();
  }
}

void windows_handle_file::close() {
  QLJS_ASSERT(this->valid());
  if (!::CloseHandle(this->handle_)) {
    std::fprintf(stderr, "error: failed to close file\n");
  }
  this->handle_ = this->invalid_handle_1;
}

windows_handle_file_ref windows_handle_file::ref() const noexcept {
  return *this;
}
#endif

#if QLJS_HAVE_UNISTD_H
posix_fd_file_ref::posix_fd_file_ref() noexcept
    : posix_fd_file_ref(this->invalid_fd) {}

posix_fd_file_ref::posix_fd_file_ref(int fd) noexcept : fd_(fd) {
  QLJS_ASSERT(this->fd_ >= 0 || this->fd_ == this->invalid_fd);
}

bool posix_fd_file_ref::valid() const noexcept {
  return this->fd_ != this->invalid_fd;
}

int posix_fd_file_ref::get() const noexcept { return this->fd_; }

file_read_result posix_fd_file_ref::read(void *buffer,
                                         int buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  ::ssize_t read_size =
      ::read(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
  if (read_size == -1) {
    return file_read_result::failure(posix_file_io_error{errno});
  }
  return read_size == 0 ? file_read_result::end_of_file()
                        : file_read_result(narrow_cast<int>(read_size));
}

result<void, posix_file_io_error> posix_fd_file_ref::write_full(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  ::ssize_t written_size = ::write(this->fd_, buffer, buffer_size);
  if (written_size == -1) {
    return result<void, posix_file_io_error>::failure(
        posix_file_io_error{errno});
  }
  if (narrow_cast<std::size_t>(written_size) != buffer_size) {
    // TODO(strager): Should we retry with the remaining buffer?
    return result<void, posix_file_io_error>::failure(posix_file_io_error{EIO});
  }
  return {};
}

bool posix_fd_file_ref::is_pipe_non_blocking() {
  QLJS_ASSERT(this->valid());
#if QLJS_HAVE_FCNTL_H
  int rc = ::fcntl(this->get(), F_GETFL);
  if (rc == -1) {
    QLJS_UNIMPLEMENTED();
  }
  return (rc & O_NONBLOCK) != 0;
#else
#error "Unsupported platform"
#endif
}

#if !defined(__EMSCRIPTEN__)
std::size_t posix_fd_file_ref::get_pipe_buffer_size() {
  QLJS_ASSERT(this->valid());
#if QLJS_HAVE_F_GETPIPE_SZ
  int size = ::fcntl(this->fd_, F_GETPIPE_SZ);
  if (size == -1) {
    QLJS_UNIMPLEMENTED();
  }
  return narrow_cast<std::size_t>(size);
#elif defined(__APPLE__)
  // See BIG_PIPE_SIZE in <xnu>/bsd/sys/pipe.h.
  return 65536;
#elif QLJS_HAVE_PIPE
#warning "Size returned by get_pipe_buffer_size might be inaccurate"
  pipe_fds pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();
  std::size_t pipe_buffer_size = 0;
  for (;;) {
    unsigned char c = 0;
    std::optional<int> written = pipe.writer.write(&c, sizeof(c));
    if (!written.has_value()) {
      break;
    }
    pipe_buffer_size += *written;
  }
  return pipe_buffer_size;
#else
#error "Unknown platform"
#endif
}
#endif

void posix_fd_file_ref::set_pipe_non_blocking() {
  QLJS_ASSERT(this->valid());
#if QLJS_HAVE_FCNTL_H
  int rc = ::fcntl(this->get(), F_SETFL, O_NONBLOCK);
  if (rc != 0) {
    QLJS_UNIMPLEMENTED();
  }
#else
#error "Unsupported platform"
#endif
}

std::string posix_fd_file_ref::get_last_error_message() {
  return std::strerror(errno);
}

posix_fd_file_ref posix_fd_file_ref::get_stdout() noexcept {
  return posix_fd_file_ref(STDOUT_FILENO);
}

posix_fd_file_ref posix_fd_file_ref::get_stderr() noexcept {
  return posix_fd_file_ref(STDERR_FILENO);
}

posix_fd_file::posix_fd_file() noexcept = default;

posix_fd_file::posix_fd_file(int fd) noexcept : posix_fd_file_ref(fd) {}

posix_fd_file::posix_fd_file(posix_fd_file &&other) noexcept
    : posix_fd_file_ref(std::exchange(other.fd_, this->invalid_fd)) {}

posix_fd_file &posix_fd_file::operator=(posix_fd_file &&other) noexcept {
  if (this != &other) {
    std::swap(this->fd_, other.fd_);
    if (other.valid()) {
      other.close();
    }
  }
  return *this;
}

posix_fd_file::~posix_fd_file() {
  if (this->valid()) {
    this->close();
  }
}

void posix_fd_file::close() {
  QLJS_ASSERT(this->valid());
  int rc = ::close(this->fd_);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to close file: %s\n",
                 std::strerror(errno));
  }
  this->fd_ = invalid_fd;
}

posix_fd_file_ref posix_fd_file::ref() const noexcept { return *this; }
#endif

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
