// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdlib>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/pty.h>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

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
// * If the slave (child) is closed before the master (parent) reads, the tty's
//   buffer is cleared thus the master (parent) reads nothing. See
//   NOTE[macOS-S_CTTYREF].
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

#if defined(__APPLE__)
void macos_set_cttyref_flag(Error_Pipe &);
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

#if defined(__APPLE__)
    // See NOTE[macOS-S_CTTYREF].
    macos_set_cttyref_flag(error_pipe);
#endif

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

namespace {
#if defined(__APPLE__)
// NOTE[macOS-S_CTTYREF]: On macOS, after a forkpty(), if the pty slave (child)
// is closed before the pty master (parent) reads, the pty's buffer is cleared
// thus the master (parent) reads nothing. This can happen if the child exits
// before the parent has a chance to call master.read().
//
// This issue has been reported to Apple, but has not been resolved:
// https://developer.apple.com/forums/thread/663632
//
// Work around this issue by opening /dev/tty then closing it. This ultimately
// causes the child's exit() to flush the slave pty's output buffer in a
// blocking way. This fixes the problem on macOS 13.2 in my testing.
//
// Here's how the workaround works in detail:
//
// If we open /dev/tty, it sets the S_CTTYREF flag on the process. This flag
// remains set if we close the /dev/tty file descriptor.
// https://github.com/apple-oss-distributions/xnu/blob/aca3beaa3dfbd42498b42c5e5ce20a938e6554e5/bsd/kern/tty_tty.c#L128
// Additionally, opening /dev/tty retains a reference to the pty slave.
// https://github.com/apple-oss-distributions/xnu/blob/aca3beaa3dfbd42498b42c5e5ce20a938e6554e5/bsd/kern/tty_tty.c#L147
//
// When the child process exits:
//
// 1. All open file descriptors (including stdin/stdout/stderr which are the pty
//    slave) are closed. This does *not* drain unread pty slave output.
//    * If S_CTTYREF was set, closing the file descriptors does not close the
//      last reference to the pty slave, so no cleanup happens yet.
//    * NOTE[macOS-pty-close-loss]: If S_CTTYREF was not set, closing the file
//      descriptors drops the last reference to the pty slave. Unread data is
//      dropped.
//
// 2. If the S_CTTYREF flag is set on the child process, the controlling
//    terminal (pty slave) is closed. XNU's ptsclose() ultimately calls
//    ttywait().
//    https://github.com/apple-oss-distributions/xnu/blob/aca3beaa3dfbd42498b42c5e5ce20a938e6554e5/bsd/kern/kern_exit.c#L2272
//    * ttywait() is the same as ioctl(slave, TIOCDRAIN); it blocks waiting for
//      output to be received.
//      https://github.com/apple-oss-distributions/xnu/blob/aca3beaa3dfbd42498b42c5e5ce20a938e6554e5/bsd/kern/tty.c#L1129-L1130
//    * NOTE[macOS-pty-waitpid-hang]: Because of the blocking ttywait(), the
//      process is in an exiting (but not zombie) state. waitpid() will hang.
//
//    * NOTE[macOS-pty-close-loss]: If the S_CTTYREF flag is not set on the
//      child process, ttywait() is not called, thus the pty slave does not
//      block waiting for the output to be received, and the output is dropped.
//      A well-behaving parent will use a poll() loop anyway, so this isn't a
//      problem. (It does make quick tests annoying to write though.)
//
// Demonstration of NOTE[macOS-pty-close-loss] (S_CTTYREF is not set before
// exit):
//
//     // On macOS, this program should report 'data = ""', demonstrating that
//     // writes are lost.
//
//     #include <stdlib.h>
//     #include <errno.h>
//     #include <stdio.h>
//     #include <string.h>
//     #include <unistd.h>
//     #include <util.h>
//
//     int main() {
//       int tty_fd;
//       pid_t pid = forkpty(&tty_fd, /*name=*/NULL, /*termp=*/NULL,
//                           /*winp=*/NULL);
//       if (pid == -1) { perror("forkpty"); abort(); }
//
//       if (pid == 0) {
//         // Child.
//         (void)write(STDOUT_FILENO, "y", 1);
//         exit(0);
//       } else {
//         // Parent.
//
//         // Cause the child to write() then exit(). exit() will drop written
//         // data.
//         sleep(1);
//
//         char buffer[10];
//         ssize_t rc = read(tty_fd, buffer, sizeof(buffer));
//         if (rc < 0) { perror("read"); abort(); }
//         fprintf(stderr, "data = \"%.*s\"\n", (int)rc, buffer);
//       }
//
//       return 0;
//     }
//
// Demonstration of NOTE[macOS-pty-waitpid-hang] (S_CTTYREF is set before exit):
//
//     // On macOS, this program should hang, demonstrating that the child
//     // process doesn't finish exiting.
//     //
//     // During the hang, observe that the child is in an exiting state ("E"):
//     //
//     //     $ ps -e -o pid,stat | grep 20125
//     //     20125 ?Es
//
//     #include <errno.h>
//     #include <fcntl.h>
//     #include <stdio.h>
//     #include <stdlib.h>
//     #include <string.h>
//     #include <unistd.h>
//     #include <util.h>
//
//     int main() {
//       int tty_fd;
//       pid_t pid = forkpty(&tty_fd, /*name=*/NULL, /*termp=*/NULL,
//                           /*winp=*/NULL);
//       if (pid == -1) { perror("forkpty"); abort(); }
//
//       if (pid == 0) {
//         // Child.
//         close(open("/dev/tty", O_WRONLY));
//         (void)write(STDOUT_FILENO, "y", 1);
//         exit(0);
//       } else {
//         // Parent.
//
//         fprintf(stderr, "child PID: %d\n", pid);
//
//         // This will hang because, despite the child being is an exiting
//         // state, the child is waiting for us to read().
//         pid_t rc = waitpid(pid, NULL, 0);
//         if (rc < 0) { perror("waitpid"); abort(); }
//       }
//
//       return 0;
//     }
void macos_set_cttyref_flag(Error_Pipe &error_pipe) {
  int tty_fd = ::open("/dev/tty", O_WRONLY);
  if (tty_fd == -1) {
    QLJS_DEBUG_LOG("forkpty child: opening /dev/tty failed: %d (%s)\n", errno,
                   std::strerror(errno));
    error_pipe.send_error_and_exit(errno);
  }
  int close_rc = ::close(tty_fd);
  if (close_rc == -1) {
    QLJS_DEBUG_LOG("forkpty child: closing /dev/tty failed: %d (%s)\n", errno,
                   std::strerror(errno));
    error_pipe.send_error_and_exit(errno);
  }
}
#endif
}
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
