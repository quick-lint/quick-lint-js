// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/have.h>

#if QLJS_HAVE_INOTIFY

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <fcntl.h>
#include <optional>
#include <poll.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
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
std::vector<posix_fd_file> garbage_inotify_fds;

int mockable_inotify_init1(int flags) noexcept {
  if (mock_inotify_force_init_error != 0) {
    errno = mock_inotify_force_init_error;
    return -1;
  }
  return ::inotify_init1(flags);
}

int mockable_inotify_add_watch(int fd, const char* pathname,
                               std::uint32_t mask) noexcept {
  if (mock_inotify_force_add_watch_error != 0) {
    errno = mock_inotify_force_add_watch_error;
    return -1;
  }
  return ::inotify_add_watch(fd, pathname, mask);
}
}

change_detecting_filesystem_inotify::change_detecting_filesystem_inotify() {
  posix_fd_file inotify_fd(mockable_inotify_init1(IN_CLOEXEC | IN_NONBLOCK));
  if (inotify_fd.valid()) {
    this->inotify_fd_ =
        result<posix_fd_file, posix_file_io_error>(std::move(inotify_fd));
  } else {
    posix_file_io_error error{errno};
    this->inotify_fd_ =
        result<posix_fd_file, posix_file_io_error>::failure(error);
    this->watch_errors_.emplace_back(watch_io_error{
        .path = "",
        .io_error = error,
    });
  }
}

change_detecting_filesystem_inotify::~change_detecting_filesystem_inotify() {
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

result<canonical_path_result, canonicalize_path_io_error>
change_detecting_filesystem_inotify::canonicalize_path(
    const std::string& path) {
  return quick_lint_js::canonicalize_path(path, this);
}

result<padded_string, read_file_io_error>
change_detecting_filesystem_inotify::read_file(const canonical_path& path) {
  canonical_path directory = path;
  directory.parent();
  bool ok = this->watch_directory(directory);
  if (!ok) {
    this->watch_errors_.emplace_back(watch_io_error{
        .path = std::move(directory).path(),
        .io_error = posix_file_io_error{errno},
    });
  }

  result<padded_string, read_file_io_error> r =
      quick_lint_js::read_file(path.c_str());
  if (!r.ok()) return r.propagate();
  return *std::move(r);
}

void change_detecting_filesystem_inotify::on_canonicalize_child_of_directory(
    const char* path) {
  bool ok = this->watch_directory(path);
  if (!ok) {
    this->watch_errors_.emplace_back(watch_io_error{
        .path = path,
        .io_error = posix_file_io_error{errno},
    });
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")
void change_detecting_filesystem_inotify::on_canonicalize_child_of_directory(
    const wchar_t*) {
  // We don't use wchar_t paths on Linux.
  QLJS_UNREACHABLE();
}
QLJS_WARNING_POP

std::optional<posix_fd_file_ref>
change_detecting_filesystem_inotify::get_inotify_fd() noexcept {
  if (!this->inotify_fd_.ok()) {
    return std::nullopt;
  }
  return this->inotify_fd_->ref();
}

void change_detecting_filesystem_inotify::handle_poll_event(
    const ::pollfd& event) {
  if (event.revents & POLLIN) {
    this->read_inotify();
  }
  if (event.revents & POLLERR) {
    QLJS_UNIMPLEMENTED();
  }
}

std::vector<watch_io_error>
change_detecting_filesystem_inotify::take_watch_errors() {
  return std::exchange(this->watch_errors_, std::vector<watch_io_error>());
}

void change_detecting_filesystem_inotify::read_inotify() {
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
      int error = errno;
      if (error == EAGAIN) {
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
      this->watch_descriptors_.erase(
          std::remove(this->watch_descriptors_.begin(),
                      this->watch_descriptors_.end(), buffer.event.wd),
          this->watch_descriptors_.end());
    }
  }
}

bool change_detecting_filesystem_inotify::watch_directory(
    const canonical_path& directory) {
  return this->watch_directory(directory.c_str());
}

bool change_detecting_filesystem_inotify::watch_directory(
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
  if (std::find(this->watch_descriptors_.begin(),
                this->watch_descriptors_.end(),
                watch_descriptor) == this->watch_descriptors_.end()) {
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
