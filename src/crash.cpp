// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <array>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <quick-lint-js/crash.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>

#if QLJS_HAVE_SETRLIMIT
#include <sys/resource.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
namespace {
#if defined(__linux__)
enum class core_style {
  // kernel.code_pattern cannot be determined.
  unknown,
  // kernel.code_pattern specifies a file pattern.
  file_path,
  // kernel.code_pattern specifies a program to execute.
  pipe,
};

core_style linux_detect_core_style() {
  int fd = ::open("/proc/sys/kernel/core_pattern", O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    std::fprintf(
        stderr,
        "warning: failed to determine method to disable core dumping: %s\n",
        std::strerror(errno));
    return core_style::unknown;
  }
  posix_fd_file file(fd);

  std::array<char, 1> core_pattern;
  file_read_result read_result =
      file.read(core_pattern.data(), core_pattern.size());
  if (!read_result.at_end_of_file && read_result.error_message.has_value()) {
    std::fprintf(
        stderr,
        "warning: failed to determine method to disable core dumping: %s\n",
        read_result.error_message->c_str());
    return core_style::unknown;
  }

  if (read_result.at_end_of_file || read_result.bytes_read == 0) {
    // kernel.core_pattern is empty. The file name is 'core' or 'core.PID'.
    return core_style::file_path;
  } else if (core_pattern[0] == '|') {
    return core_style::pipe;
  } else {
    return core_style::file_path;
  }
}
#endif
}

#if defined(__EMSCRIPTEN__)
void disable_core_dumping() {
  // Do nothing.
}
#elif QLJS_HAVE_SETRLIMIT
void disable_core_dumping() {
  int rc;

  ::rlimit limits;
  rc = ::getrlimit(RLIMIT_CORE, &limits);
  if (rc == -1) {
    std::fprintf(stderr, "warning: failed to disable core dumping: %s\n",
                 std::strerror(errno));
    return;
  }

#if defined(__linux__)
  core_style style = linux_detect_core_style();
  switch (style) {
  case core_style::unknown:
  case core_style::file_path:
    limits.rlim_cur = 0;
    break;
  case core_style::pipe:
    // HACK(strager): rlim_cur is not properly respected if
    // kernel.core_pattern is a pipe. However, if rlim_cur=1, core dumping is
    // skipped for pipe patterns:
    // https://github.com/torvalds/linux/blob/v4.2/fs/coredump.c#L577-L598
    // Take advantage of this quirk to disable core dumping for pipe patterns.
    limits.rlim_cur = 1;
    break;
  }
#else
  limits.rlim_cur = 0;
#endif

  rc = ::setrlimit(RLIMIT_CORE, &limits);
  if (rc == -1) {
    std::fprintf(stderr, "warning: failed to disable core dumping: %s\n",
                 std::strerror(errno));
  }
}
#elif defined(_WIN32)
void disable_core_dumping() {
  // Do nothing. Windows doesn't have core dumps like POSIX platforms do.
}
#else
#error "Unsupported platform"
#endif
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
