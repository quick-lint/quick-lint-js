// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <quick-lint-js/have.h>
#include <quick-lint-js/pipe.h>

#if QLJS_HAVE_PIPE
#include <fcntl.h>
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_PIPE
pipe_fds make_pipe() {
  int fds[2];
  int rc = ::pipe(fds);
  if (rc == -1) {
    std::fprintf(stderr, "error: failed to create pipe: %s\n",
                 std::strerror(errno));
    std::abort();
  }
  rc = ::fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  if (rc == -1) {
    std::fprintf(stderr, "warning: failed to make pipe reader CLOEXEC: %s\n",
                 std::strerror(errno));
  }
  rc = ::fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  if (rc == -1) {
    std::fprintf(stderr, "warning: failed to make pipe writer CLOEXEC: %s\n",
                 std::strerror(errno));
  }
  return pipe_fds{
      .reader = posix_fd_file(fds[0]),
      .writer = posix_fd_file(fds[1]),
  };
}
#elif defined(_WIN32)
pipe_fds make_pipe() {
  HANDLE readPipe;
  HANDLE writePipe;
  if (!::CreatePipe(&readPipe, &writePipe, /*lpPipeAttributes=*/nullptr,
                    /*nSize=*/0)) {
    std::fprintf(stderr, "error: failed to create pipe: %s\n",
                 windows_handle_file::get_last_error_message());
    std::abort();
  }
  return pipe_fds{
      .reader = windows_handle_file(readPipe),
      .writer = windows_handle_file(writePipe),
  };
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
