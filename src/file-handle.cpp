// Copyright (C) 2020  Matthew Glazar
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
#include <string>
#include <string_view>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WINDOWS_H
windows_handle_file_ref::windows_handle_file_ref(HANDLE handle) noexcept
    : handle_(handle) {
  QLJS_ASSERT(this->handle_ != nullptr);
  QLJS_ASSERT(this->handle_ != INVALID_HANDLE_VALUE);
}

HANDLE windows_handle_file_ref::get() noexcept { return this->handle_; }

file_read_result windows_handle_file_ref::read(void *buffer,
                                               int buffer_size) noexcept {
  DWORD read_size;
  if (!::ReadFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                  &read_size,
                  /*lpOverlapped=*/nullptr)) {
    DWORD error = ::GetLastError();
    return file_read_result{
        .at_end_of_file = error == ERROR_BROKEN_PIPE,
        .bytes_read = 0,
        .error_message = windows_error_message(error),
    };
  }
  return file_read_result{
      // TODO(strager): Microsoft's documentation for ReadFile claims the
      // following:
      //
      // > If the lpNumberOfBytesRead parameter is zero when ReadFile returns
      // > TRUE on a pipe, the other end of the pipe called the WriteFile
      // > function with nNumberOfBytesToWrite set to zero.
      //
      // https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
      //
      // In my experiments, I haven't been able to make ReadFile give
      // 0-bytes-read in this case. However, given the documentation, when we
      // get 0 bytes read, we should ask the pipe if we reached EOF.
      .at_end_of_file = read_size == 0,
      .bytes_read = narrow_cast<int>(read_size),
      .error_message = std::nullopt,
  };
}

std::optional<int> windows_handle_file_ref::write(const void *buffer,
                                                  int buffer_size) noexcept {
  DWORD write_size;
  if (!::WriteFile(this->handle_, buffer, narrow_cast<DWORD>(buffer_size),
                   &write_size,
                   /*lpOverlapped=*/nullptr)) {
    return std::nullopt;
  }
  return narrow_cast<int>(write_size);
}

bool windows_handle_file_ref::is_pipe_non_blocking() {
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
  DWORD mode = PIPE_READMODE_BYTE | PIPE_NOWAIT;
  BOOL ok = ::SetNamedPipeHandleState(this->get(), /*lpMode=*/&mode,
                                      /*lpMaxCollectionCount=*/nullptr,
                                      /*lpCollectDataTimeout=*/nullptr);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
}

void windows_handle_file_ref::block_until_pipe_is_writeable_or_broken() {
  DWORD mode = PIPE_READMODE_BYTE;
  BOOL ok = ::SetNamedPipeHandleState(this->get(), /*lpMode=*/&mode,
                                      /*lpMaxCollectionCount=*/nullptr,
                                      /*lpCollectDataTimeout=*/nullptr);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }

  char buffer[1];
  DWORD write_size;
  if (!::WriteFile(this->handle_, &buffer, /*nNumberOfBytesToWrite=*/0,
                   &write_size,
                   /*lpOverlapped=*/nullptr)) {
    QLJS_UNIMPLEMENTED();
  }
  QLJS_ASSERT(write_size == 0);

  DWORD mode2 = PIPE_READMODE_BYTE | PIPE_NOWAIT;
  BOOL ok2 = ::SetNamedPipeHandleState(this->get(), /*lpMode=*/&mode2,
                                      /*lpMaxCollectionCount=*/nullptr,
                                      /*lpCollectDataTimeout=*/nullptr);
  if (!ok2) {
    QLJS_UNIMPLEMENTED();
  }

  //DWORD rc = ::WaitForSingleObject(this->get(), /*dwMilliseconds=*/INFINITE);
  //switch (rc) {
  //case WAIT_OBJECT_0:
  //  break;
  //case WAIT_ABANDONED:
  //  QLJS_UNIMPLEMENTED();
  //  break;
  //case WAIT_TIMEOUT:
  //  QLJS_ASSERT(false);
  //  break;
  //case WAIT_FAILED:
  //  QLJS_UNIMPLEMENTED();
  //  break;
  //}
  // @@@ we should block yo. but how?
  // https://docs.microsoft.com/en-us/windows/win32/ipc/named-pipe-type-read-and-wait-modes
}

std::string windows_handle_file_ref::get_last_error_message() {
  return windows_error_message(::GetLastError());
}

windows_handle_file::windows_handle_file(HANDLE handle) noexcept
    : windows_handle_file_ref(handle) {}

windows_handle_file::~windows_handle_file() {
  if (this->handle_ != this->invalid_handle) {
    this->close();
  }
}

void windows_handle_file::close() {
  if (!::CloseHandle(this->handle_)) {
    std::fprintf(stderr, "error: failed to close file\n");
  }
  this->handle_ = this->invalid_handle;
}

windows_handle_file_ref windows_handle_file::ref() noexcept { return *this; }
#endif

#if QLJS_HAVE_UNISTD_H
posix_fd_file_ref::posix_fd_file_ref(int fd) noexcept : fd_(fd) {
  QLJS_ASSERT(this->fd_ != -1);
}

int posix_fd_file_ref::get() noexcept { return this->fd_; }

file_read_result posix_fd_file_ref::read(void *buffer,
                                         int buffer_size) noexcept {
  ::ssize_t read_size =
      ::read(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
  if (read_size == -1) {
    return file_read_result{
        .at_end_of_file = false,
        .bytes_read = 0,
        .error_message = this->get_last_error_message(),
    };
  }
  return file_read_result{
      .at_end_of_file = read_size == 0,
      .bytes_read = narrow_cast<int>(read_size),
      .error_message = std::nullopt,
  };
}

std::optional<int> posix_fd_file_ref::write(const void *buffer,
                                            int buffer_size) noexcept {
  ::ssize_t written_size =
      ::write(this->fd_, buffer, narrow_cast<std::size_t>(buffer_size));
  if (written_size == -1) {
    return std::nullopt;
  }
  return narrow_cast<int>(written_size);
}

bool posix_fd_file_ref::is_pipe_non_blocking() {
#if QLJS_HAVE_FCNTL_H
  int rc = ::fcntl(this->get(), F_GETFL, O_NONBLOCK);
  if (rc == -1) {
    QLJS_UNIMPLEMENTED();
  }
  return rc != 0;
#else
#error "Unsupported platform"
#endif
}

void posix_fd_file_ref::set_pipe_non_blocking() {
#if QLJS_HAVE_FCNTL_H
  int rc = ::fcntl(this->get(), F_SETFL, O_NONBLOCK);
  if (rc != 0) {
    QLJS_UNIMPLEMENTED();
  }
#else
#error "Unsupported platform"
#endif
}

void posix_fd_file_ref::block_until_pipe_is_writeable_or_broken() {
#if QLJS_HAVE_POLL
retry:
  std::array<::pollfd, 1> fds;
  fds[0].fd = this->get();
  fds[0].events = POLLOUT;
  int rc =
      ::poll(fds.data(), narrow_cast<::nfds_t>(fds.size()), /*timeout=*/-1);
  if (rc == -1) {
    if (errno == EAGAIN || errno == EINTR) {
      goto retry;
    }
    QLJS_UNIMPLEMENTED();
  }
  QLJS_ASSERT(rc != 0);  // Shouldn't time out.
  QLJS_ASSERT(rc == 1);
  QLJS_ASSERT((fds[0].revents & (POLLOUT | POLLERR | POLLHUP)) != 0);
#else
#error "Unsupported platform"
#endif
}

std::string posix_fd_file_ref::get_last_error_message() {
  return std::strerror(errno);
}

posix_fd_file::posix_fd_file(int fd) noexcept : posix_fd_file_ref(fd) {
  QLJS_ASSERT(fd != invalid_fd);
}

posix_fd_file::~posix_fd_file() {
  if (this->fd_ != invalid_fd) {
    this->close();
  }
}

void posix_fd_file::close() {
  QLJS_ASSERT(this->fd_ != invalid_fd);
  int rc = ::close(this->fd_);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to close file: %s\n",
                 std::strerror(errno));
  }
  this->fd_ = invalid_fd;
}

posix_fd_file_ref posix_fd_file::ref() noexcept { return *this; }
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

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
