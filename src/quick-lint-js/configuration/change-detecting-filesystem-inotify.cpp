// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_INOTIFY

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <fcntl.h>
#include <limits.h>
#include <optional>
#include <poll.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/vector-erase.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <string>
#include <string_view>
#include <sys/inotify.h>
#include <unistd.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
int mock_inotify_force_init_error = 0;
int mock_inotify_force_add_watch_error = 0;

namespace {
std::vector<POSIX_FD_File> garbage_inotify_fds;

int mockable_inotify_init1(int flags) {
  if (mock_inotify_force_init_error != 0) {
    errno = mock_inotify_force_init_error;
    return -1;
  }
  return ::inotify_init1(flags);
}

int mockable_inotify_add_watch(int fd, const char* pathname,
                               std::uint32_t mask) {
  if (mock_inotify_force_add_watch_error != 0) {
    errno = mock_inotify_force_add_watch_error;
    return -1;
  }
  return ::inotify_add_watch(fd, pathname, mask);
}
}

Change_Detecting_Filesystem_Inotify::Change_Detecting_Filesystem_Inotify() {
  POSIX_FD_File inotify_fd(mockable_inotify_init1(IN_CLOEXEC | IN_NONBLOCK));
  if (inotify_fd.valid()) {
    this->inotify_fd_ =
        Result<POSIX_FD_File, POSIX_File_IO_Error>(std::move(inotify_fd));
  } else {
    POSIX_File_IO_Error error{errno};
    this->inotify_fd_ = failed_result(error);
    this->watch_errors_.emplace_back(Watch_IO_Error{
        .path = "",
        .io_error = error,
    });
  }
}

Change_Detecting_Filesystem_Inotify::~Change_Detecting_Filesystem_Inotify() {
  // HACK(strager): On Linux 5.4.86, close() becomes *very* slow (10
  // milliseconds or more) because it summons RCU synchronization demons.
  // (This performance problem only matters in tests.) More details:
  // https://lore.kernel.org/linux-fsdevel/CAC-ggsFLmFpz5Y=-9MMLwxuO2LOS9rhpewDp_-u2hrT9J79ryg@mail.gmail.com/
  //
  // Work around the slowness by deferring close() but manually clearing the
  // inotify.
  if (this->inotify_fd_.ok()) {
    for (int watch_descriptor : this->watch_descriptors_) {
      int rc = ::inotify_rm_watch(this->inotify_fd_->get(), watch_descriptor);
      if (rc == -1) {
        // HACK(strager): Sometimes, when a directory is deleted, we don't get
        // IN_IGNORED events. In these cases, inotify_rm_watch fails with
        // EINVAL. Ignore these errors.
        QLJS_ASSERT(errno == EINVAL);
      }
    }

    constexpr std::size_t closes_to_defer = 10;
    if (garbage_inotify_fds.size() > closes_to_defer) {
      garbage_inotify_fds.clear();  // Closes each fd.
    }
    garbage_inotify_fds.push_back(std::move(*this->inotify_fd_));
  }
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
Change_Detecting_Filesystem_Inotify::canonicalize_path(
    const std::string& path) {
  return quick_lint_js::canonicalize_path(path, this);
}

Result<Padded_String, Read_File_IO_Error>
Change_Detecting_Filesystem_Inotify::read_file(const Canonical_Path& path) {
  Canonical_Path directory = path;
  directory.parent();
  bool ok = this->watch_directory(directory);
  if (!ok) {
    this->watch_errors_.emplace_back(Watch_IO_Error{
        .path = std::move(directory).path(),
        .io_error = POSIX_File_IO_Error{errno},
    });
  }

  Result<Padded_String, Read_File_IO_Error> r =
      quick_lint_js::read_file(path.c_str());
  if (!r.ok()) return r.propagate();
  return *std::move(r);
}

void Change_Detecting_Filesystem_Inotify::on_canonicalize_child_of_directory(
    const char* path) {
  bool ok = this->watch_directory(path);
  if (!ok) {
    this->watch_errors_.emplace_back(Watch_IO_Error{
        .path = path,
        .io_error = POSIX_File_IO_Error{errno},
    });
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")
void Change_Detecting_Filesystem_Inotify::on_canonicalize_child_of_directory(
    const wchar_t*) {
  // We don't use wchar_t paths on Linux.
  QLJS_UNREACHABLE();
}
QLJS_WARNING_POP

std::optional<POSIX_FD_File_Ref>
Change_Detecting_Filesystem_Inotify::get_inotify_fd() {
  if (!this->inotify_fd_.ok()) {
    return std::nullopt;
  }
  return this->inotify_fd_->ref();
}

void Change_Detecting_Filesystem_Inotify::handle_poll_event(short revents) {
  if (revents & POLLIN) {
    this->read_inotify();
  }
  if (revents & POLLERR) {
    QLJS_UNIMPLEMENTED();
  }
}

std::vector<Watch_IO_Error>
Change_Detecting_Filesystem_Inotify::take_watch_errors() {
  return std::exchange(this->watch_errors_, std::vector<Watch_IO_Error>());
}

void Change_Detecting_Filesystem_Inotify::read_inotify() {
  union inotify_event_buffer {
    ::inotify_event event;
    char buffer[sizeof(::inotify_event) + NAME_MAX + 1];
  };

  if (!this->inotify_fd_.ok()) {
    return;
  }

  // TODO(strager): Optimize syscall usage by calling read once with a big
  // buffer.
  for (;;) {
    inotify_event_buffer buffer;
    ssize_t rc = ::read(this->inotify_fd_->get(), &buffer, sizeof(buffer));
    QLJS_ASSERT(rc <= narrow_cast<ssize_t>(sizeof(buffer)));
    if (rc == -1) {
      POSIX_File_IO_Error error{.error = errno};
      if (error.is_would_block_try_again_error()) {
        // We read all of the queuedevents.
        break;
      }
      QLJS_UNIMPLEMENTED();
    }
    if (rc == 0) {
      QLJS_UNIMPLEMENTED();
    }
    if (buffer.event.mask & IN_IGNORED) {
      QLJS_ASSERT(buffer.event.wd != -1);
      erase(this->watch_descriptors_, buffer.event.wd);
    }
  }
}

bool Change_Detecting_Filesystem_Inotify::watch_directory(
    const Canonical_Path& directory) {
  return this->watch_directory(directory.c_str());
}

bool Change_Detecting_Filesystem_Inotify::watch_directory(
    const char* directory) {
  if (!this->inotify_fd_.ok()) {
    // We already reported the error after calling inotify_init1. Don't report
    // another error.
    return true;
  }

  int watch_descriptor =
      mockable_inotify_add_watch(this->inotify_fd_->get(), directory,
                                 IN_ATTRIB | IN_CLOSE_WRITE | IN_CREATE |
                                     IN_DELETE | IN_MOVED_TO | IN_MOVE_SELF);
  if (watch_descriptor == -1) {
    return false;
  }
  // TODO(strager): Use a more efficient data structure, such as a sorted
  // interval set, for watch descriptors.
  if (!contains(this->watch_descriptors_, watch_descriptor)) {
    this->watch_descriptors_.emplace_back(watch_descriptor);
  }

  return true;
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
