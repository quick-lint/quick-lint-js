// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdlib>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/pty.h>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_LIBUTIL_H
#include <libutil.h>
#endif

#if QLJS_HAVE_UTIL_H
#include <util.h>
#endif

#if QLJS_HAVE_PTY_H
#include <pty.h>
#endif

#if !QLJS_HAVE_FORKPTY
#define QLJS_HAVE_WORKING_FORKPTY 0
#elif defined(__APPLE__)
// macOS's forkpty
// (https://github.com/apple-oss-distributions/Libc/blob/a61a5933206342413af76579500f313a045c1c33/util/pty.c#L97)
// has a few bugs:
//
// * If fork() fails, file descriptors are leaked.
// * If login_tty fails, the error is not communicated to the caller.
#define QLJS_HAVE_WORKING_FORKPTY 0
#else
#define QLJS_HAVE_WORKING_FORKPTY 1
#endif

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_FORKPTY && !QLJS_HAVE_WORKING_FORKPTY
// A POSIX pipe which sends either an integer (indicating error) or no integer
// (indicating success).
class Error_Pipe {
 public:
  explicit Error_Pipe();

  void close_writer();
  void close_reader();

  void send_success();
  void send_error(int error);
  [[noreturn]] void send_error_and_exit(int error);

  Result<std::optional<int>, Platform_File_IO_Error> receive();

 private:
  Pipe_FDs pipe_;
};

Error_Pipe::Error_Pipe() : pipe_(make_pipe()) {}

void Error_Pipe::close_writer() { this->pipe_.writer.close(); }

void Error_Pipe::close_reader() { this->pipe_.reader.close(); }

void Error_Pipe::send_success() {
  bool is_error = false;
  this->pipe_.writer.write(&is_error, sizeof(is_error));
}

void Error_Pipe::send_error(int error) {
  bool is_error = true;
  this->pipe_.writer.write(&is_error, sizeof(is_error));
  this->pipe_.writer.write(&error, sizeof(error));
}

void Error_Pipe::send_error_and_exit(int error) {
  this->send_error(error);
  std::_Exit(EXIT_FAILURE);
}

Result<std::optional<int>, Platform_File_IO_Error> Error_Pipe::receive() {
  bool is_error;
  File_Read_Result is_error_result =
      this->pipe_.reader.read(&is_error, sizeof(is_error));
  if (!is_error_result.ok()) {
    return is_error_result.propagate();
  }
  if (is_error_result.at_end_of_file() ||
      is_error_result.bytes_read() < int{sizeof(is_error)}) {
    // The child's pipe closed before we could read the error indicator.
    return failed_result(POSIX_File_IO_Error{.error = EPIPE});
  }

  if (!is_error) {
    return std::nullopt;
  }

  int error;
  File_Read_Result error_result =
      this->pipe_.reader.read(&error, sizeof(error));
  if (!error_result.ok()) {
    return is_error_result.propagate();
  }
  if (error_result.at_end_of_file() ||
      error_result.bytes_read() < int{sizeof(error)}) {
    // The child's pipe closed before we could read the error indicator.
    return failed_result(POSIX_File_IO_Error{.error = EPIPE});
  }

  return error;
}
#endif
}

#if QLJS_HAVE_FORKPTY
::pid_t forkpty(int *amaster, char *name, struct ::termios *termp,
                struct ::winsize *winp) {
#if QLJS_HAVE_WORKING_FORKPTY
  return ::forkpty(amaster, name, termp, winp);
#else
  int raw_master_fd;
  int raw_slave_fd;
  int openpty_rc = ::openpty(&raw_master_fd, &raw_slave_fd, name, termp, winp);
  if (openpty_rc == -1) {
    QLJS_DEBUG_LOG("forkpty parent: openpty failed: %d (%s)\n", errno,
                   std::strerror(errno));
    return -1;
  }
  POSIX_FD_File master(raw_master_fd);
  POSIX_FD_File slave(raw_slave_fd);

  Error_Pipe error_pipe;
  ::pid_t pid = ::fork();
  if (pid == -1) {
    QLJS_DEBUG_LOG("forkpty parent: fork failed: %d (%s)\n", errno,
                   std::strerror(errno));
    return -1;
  }

  if (pid == 0) {
    // Child.
    master.close();
    error_pipe.close_reader();

    if (::login_tty(slave.get()) == -1) {
      QLJS_DEBUG_LOG("forkpty child: login_tty failed: %d (%s)\n", errno,
                     std::strerror(errno));
      error_pipe.send_error_and_exit(errno);
    }
    slave.release();  // ::login_tty claimed ownership of the file descriptor.

    error_pipe.send_success();
    return 0;
  } else {
    // Parent.
    slave.close();
    error_pipe.close_writer();

    Result<std::optional<int>, Platform_File_IO_Error> child_error =
        error_pipe.receive();
    if (!child_error.ok()) {
      QLJS_DEBUG_LOG("forkpty parent: reading error from child failed: %s\n",
                     child_error.error_to_string().c_str());
      errno = child_error.error().error;
      return -1;
    }
    if (child_error->has_value()) {
      int error = **child_error;
      QLJS_DEBUG_LOG("forkpty parent: received error from child: %d (%s)\n",
                     error, std::strerror(error));
      errno = error;
      return -1;
    }

    *amaster = master.release();
    return pid;
  }
#endif
}
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
