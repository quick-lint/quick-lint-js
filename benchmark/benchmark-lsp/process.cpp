// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/narrow-cast.h>
#include <sys/wait.h>
#include <unistd.h>

namespace quick_lint_js {
void wait_for_process_exit(::pid_t pid) {
retry:
  int status;
  ::pid_t rc = ::waitpid(pid, &status, /*options=*/0);
  if (rc == -1) {
    if (errno == EINTR) {
      goto retry;
    }
    std::fprintf(stderr, "error: failed to wait for process %lld: %s\n",
                 narrow_cast<long long>(pid), std::strerror(errno));
    std::exit(1);
  }
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
