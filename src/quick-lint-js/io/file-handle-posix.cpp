// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_UNISTD_H

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
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

namespace quick_lint_js {
bool POSIX_File_IO_Error::is_file_not_found_error() const {
  return this->error == ENOENT;
}

bool POSIX_File_IO_Error::is_not_a_directory_error() const {
  return this->error == ENOTDIR;
}

bool POSIX_File_IO_Error::is_would_block_try_again_error() const {
  QLJS_WARNING_PUSH
  // On many platforms, EAGAIN == EWOULDBLOCK.
  QLJS_WARNING_IGNORE_GCC("-Wlogical-op")

  return this->error == EAGAIN || this->error == EWOULDBLOCK;

  QLJS_WARNING_POP
}

std::string POSIX_File_IO_Error::to_string() const {
  return std::strerror(this->error);
}

bool operator==(POSIX_File_IO_Error lhs, POSIX_File_IO_Error rhs) {
  return lhs.error == rhs.error;
}

bool operator!=(POSIX_File_IO_Error lhs, POSIX_File_IO_Error rhs) {
  return !(lhs == rhs);
}

POSIX_FD_File_Ref::POSIX_FD_File_Ref() : POSIX_FD_File_Ref(this->invalid_fd) {}

POSIX_FD_File_Ref::POSIX_FD_File_Ref(int fd) : fd_(fd) {
  QLJS_ASSERT(this->fd_ >= 0 || this->fd_ == this->invalid_fd);
}

bool POSIX_FD_File_Ref::valid() const { return this->fd_ != this->invalid_fd; }

int POSIX_FD_File_Ref::get() const { return this->fd_; }

File_Read_Result POSIX_FD_File_Ref::read(void *buffer, int buffer_size) {
  QLJS_ASSERT(this->valid());
retry:
  ::ssize_t read_size =
      ::read(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
  if (read_size == -1) {
    int error = errno;
    if (error == EIO) {
#if QLJS_HAVE_POLL
      // On Linux, with a master terminal, read() fails with EIO if the slave
      // file descriptors are all closed. Detect this case using poll().
      ::pollfd poll_fds[] = {
          {.fd = this->fd_, .events = POLLHUP, .revents = 0},
      };
      int rc = ::poll(poll_fds, 1, /*timeout=*/0);
      if (rc == 1 && (poll_fds[0].revents & POLLHUP) == POLLHUP) {
        return File_Read_Result::end_of_file();
      }
#endif
    }
    if (error == EINTR) {
      // This happens on macOS when running
      // Test_File.read_file_reads_from_pty_master with a debugger. (It probably
      // happens in other cases with a debugger too.) Retry.
      // FIXME(strager): Do we need to poll for readiness? Should we implement
      // backoff to avoid 100% CPU usage?
      goto retry;
    }
    return failed_result(POSIX_File_IO_Error{error});
  }
  return read_size == 0 ? File_Read_Result::end_of_file()
                        : File_Read_Result(narrow_cast<int>(read_size));
}

Result<std::size_t, POSIX_File_IO_Error> POSIX_FD_File_Ref::write(
    const void *buffer, std::size_t buffer_size) {
  QLJS_ASSERT(this->valid());
  ::ssize_t written_size = ::write(this->fd_, buffer, buffer_size);
  if (written_size == -1) {
    return failed_result(POSIX_File_IO_Error{errno});
  }
  return narrow_cast<std::size_t>(written_size);
}

Result<void, POSIX_File_IO_Error> POSIX_FD_File_Ref::write_full(
    const void *buffer, std::size_t buffer_size) {
  QLJS_ASSERT(this->valid());
  auto write_result = this->write(buffer, buffer_size);
  if (!write_result.ok()) {
    return write_result.propagate();
  }
  if (*write_result != buffer_size) {
    // TODO(strager): Should we retry with the remaining buffer?
    return failed_result(POSIX_File_IO_Error{EIO});
  }
  return {};
}

bool POSIX_FD_File_Ref::is_pipe_non_blocking() {
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
std::size_t POSIX_FD_File_Ref::get_pipe_buffer_size() {
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
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();
  std::size_t pipe_buffer_size = 0;
  for (;;) {
    unsigned char c = 0;
    Result<std::size_t, POSIX_File_IO_Error> written =
        pipe.writer.write(&c, sizeof(c));
    if (!written.ok()) {
      QLJS_ASSERT(written.error().is_would_block_try_again_error());
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

bool operator==(POSIX_FD_File_Ref lhs, POSIX_FD_File_Ref rhs) {
  return lhs.get() == rhs.get();
}

bool operator!=(POSIX_FD_File_Ref lhs, POSIX_FD_File_Ref rhs) {
  return !(lhs == rhs);
}

void POSIX_FD_File_Ref::set_pipe_non_blocking() {
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

std::string POSIX_FD_File_Ref::get_last_error_message() {
  return std::strerror(errno);
}

POSIX_FD_File_Ref POSIX_FD_File_Ref::get_stdout() {
  return POSIX_FD_File_Ref(STDOUT_FILENO);
}

POSIX_FD_File_Ref POSIX_FD_File_Ref::get_stderr() {
  return POSIX_FD_File_Ref(STDERR_FILENO);
}

POSIX_FD_File::POSIX_FD_File() = default;

POSIX_FD_File::POSIX_FD_File(int fd) : POSIX_FD_File_Ref(fd) {}

POSIX_FD_File::POSIX_FD_File(POSIX_FD_File &&other)
    : POSIX_FD_File_Ref(std::exchange(other.fd_, this->invalid_fd)) {}

POSIX_FD_File &POSIX_FD_File::operator=(POSIX_FD_File &&other) {
  if (this != &other) {
    std::swap(this->fd_, other.fd_);
    if (other.valid()) {
      other.close();
    }
  }
  return *this;
}

POSIX_FD_File::~POSIX_FD_File() {
  if (this->valid()) {
    this->close();
  }
}

void POSIX_FD_File::close() {
  QLJS_ASSERT(this->valid());
  int rc = ::close(this->fd_);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to close file: %s\n",
                 std::strerror(errno));
  }
  this->fd_ = invalid_fd;
}

POSIX_FD_File_Ref POSIX_FD_File::ref() const { return *this; }

int POSIX_FD_File::release() {
  QLJS_ASSERT(this->valid());
  return std::exchange(this->fd_, this->invalid_fd);
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
