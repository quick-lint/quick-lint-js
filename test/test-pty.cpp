// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No POSIX pseudo-terminals on the web.
#else

#include <chrono>
#include <cstdio>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/pty.h>
#include <thread>

#if QLJS_HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_FORKPTY
// This test verifies a fix for a macOS bug. See NOTE[macOS-S_CTTYREF].
TEST(Test_PTY, parent_can_read_from_child_if_child_runs_quickly_SLOW) {
  // Flush buffered Google Test output. Otherwise, the child inherits the buffer
  // and Google Test output gets mixed in with our test string.
  std::fflush(stdout);
  std::fflush(stderr);

  int raw_tty_fd;
  ::pid_t pid = forkpty(&raw_tty_fd, /*name=*/nullptr, /*termp=*/nullptr,
                        /*winp=*/nullptr);
  ASSERT_NE(pid, -1) << std::strerror(errno);
  if (pid == 0) {
    // Child.
    (void)::write(STDOUT_FILENO, "y", 1);
    std::_Exit(0);
  } else {
    // Parent.
    POSIX_FD_File tty_fd(raw_tty_fd);

    // Wait for the child process to exit.
    //
    // We can't call waitpid() because that will hang with our workaround
    // applied (see NOTE[macOS-pty-waitpid-hang]).
    //
    // We can't poll for the process's exit status using
    // proc_pidinfo(PROC_PIDTBSDINFO) because the status changes too early,
    // causing this test to not demonstrate the macOS bug. (We want to expose
    // NOTE[macOS-pty-close-loss] if we can.)
    //
    // We can't poll the master pty (tty_fd) because that will trigger before
    // the process exits.
    //
    // On strager's M1, sleeping for 596 milliseconds or more reliably causes
    // the test to fail if the NOTE[macOS-S_CTTYREF] workaround is not
    // applied.
    std::this_thread::sleep_for(600ms);

    char buffer[10] = {};
    ::ssize_t read_rc = ::read(tty_fd.get(), buffer, sizeof(buffer));
    ASSERT_NE(read_rc, -1) << std::strerror(errno);
    EXPECT_EQ(read_rc, 1) << "child should have sent data to parent";
    if (read_rc > 0) {
      EXPECT_EQ(buffer[0], 'y')
          << "parent should successfully read data sent by child";
    }

    int status;
    ::pid_t wait_rc = ::waitpid(-pid, &status, /*options=*/0);
    EXPECT_NE(wait_rc, -1) << std::strerror(errno);
    EXPECT_EQ(wait_rc, pid);
  }
}
#endif
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
