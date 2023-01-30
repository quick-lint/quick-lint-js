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
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

namespace quick_lint_js {
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
    return failed_result(posix_file_io_error{errno});
  }
  return read_size == 0 ? file_read_result::end_of_file()
                        : file_read_result(narrow_cast<int>(read_size));
}

result<std::size_t, posix_file_io_error> posix_fd_file_ref::write(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  ::ssize_t written_size = ::write(this->fd_, buffer, buffer_size);
  if (written_size == -1) {
    return failed_result(posix_file_io_error{errno});
  }
  return narrow_cast<std::size_t>(written_size);
}

result<void, posix_file_io_error> posix_fd_file_ref::write_full(
    const void *buffer, std::size_t buffer_size) noexcept {
  QLJS_ASSERT(this->valid());
  auto write_result = this->write(buffer, buffer_size);
  if (!write_result.ok()) {
    return write_result.propagate();
  }
  if (*write_result != buffer_size) {
    // TODO(strager): Should we retry with the remaining buffer?
    return failed_result(posix_file_io_error{EIO});
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
    result<std::size_t, posix_file_io_error> written =
        pipe.writer.write(&c, sizeof(c));
    if (!written.ok()) {
      QLJS_ASSERT(written.error().error == EAGAIN);
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
