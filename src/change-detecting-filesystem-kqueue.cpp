// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/have.h>

#if QLJS_HAVE_KQUEUE

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <fcntl.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/log.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/utf-16.h>
#include <string>
#include <string_view>
#include <sys/event.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unordered_map>
#include <utility>
#include <vector>

namespace quick_lint_js {
int mock_kqueue_force_directory_open_error = 0;

namespace {
int mockable_directory_open(const char* path, int flags) {
  if (mock_kqueue_force_directory_open_error) {
    errno = mock_kqueue_force_directory_open_error;
    return -1;
  }
  return ::open(path, flags);
}

std::string vnode_event_flags_to_string(std::uint32_t flags);
}

change_detecting_filesystem_kqueue::change_detecting_filesystem_kqueue(
    posix_fd_file_ref kqueue_fd, void* udata)
    : kqueue_fd_(kqueue_fd), udata_(udata) {}

change_detecting_filesystem_kqueue::~change_detecting_filesystem_kqueue() {
  // NOTE(strager): Closing the file descriptors will remove the filters from
  // kqueue_fd_ (I think).
}

result<canonical_path_result, canonicalize_path_io_error>
change_detecting_filesystem_kqueue::canonicalize_path(const std::string& path) {
  return quick_lint_js::canonicalize_path(path, this);
}

result<padded_string, read_file_io_error>
change_detecting_filesystem_kqueue::read_file(const canonical_path& path) {
  canonical_path directory = path;
  directory.parent();
  bool ok = this->watch_directory(directory);
  if (!ok) {
    this->watch_errors_.emplace_back(watch_io_error{
        .path = std::move(directory).path(),
        .io_error = posix_file_io_error{errno},
    });
    // FIXME(strager): Recovering now is probably pointless. If watch_directory
    // failed because open() returned EMFILE, then the following open() call is
    // going to fail too. We should probably clean up open watches to make room
    // for more file descriptors.
  }

  // TODO(strager): Use openat. watch_directory opened a directory fd.
  posix_fd_file file(::open(path.c_str(), O_RDONLY));
  if (!file.valid()) {
    return result<padded_string, read_file_io_error>::failure<
        read_file_io_error>(read_file_io_error{
        .path = path.c_str(),
        .io_error = posix_file_io_error{errno},
    });
  }

  auto watch_it = this->watch_file(canonical_path(path), std::move(file));
  result<padded_string, read_file_io_error> r =
      quick_lint_js::read_file(path.c_str(), watch_it->second.fd.ref());
  if (!r.ok()) return r.propagate();
  return *std::move(r);
}

void change_detecting_filesystem_kqueue::on_canonicalize_child_of_directory(
    const char* path) {
  bool ok = this->watch_directory(canonical_path(path));
  if (!ok) {
    this->watch_errors_.emplace_back(watch_io_error{
        .path = path,
        .io_error = posix_file_io_error{errno},
    });
  }
}

std::vector<watch_io_error>
change_detecting_filesystem_kqueue::take_watch_errors() {
  return std::exchange(this->watch_errors_, std::vector<watch_io_error>());
}

void change_detecting_filesystem_kqueue::on_canonicalize_child_of_directory(
    const wchar_t*) {
  // We don't use wchar_t paths on BSDs.
  QLJS_UNREACHABLE();
}

void change_detecting_filesystem_kqueue::handle_kqueue_event(
    const struct ::kevent& event) {
  QLJS_ASSERT(event.filter == EVFILT_VNODE);

  if (is_logging_enabled()) {
    auto watched_file_it = std::find_if(
        this->watched_files_.begin(), this->watched_files_.end(),
        [&](auto& pair) -> bool {
          return pair.second.fd.get() == narrow_cast<int>(event.ident);
        });
    if (watched_file_it == this->watched_files_.end()) {
      QLJS_DEBUG_LOG("warning: got EVFILT_VNODE event for unknown fd %d\n",
                     event.ident);
    } else {
      QLJS_DEBUG_LOG("note: got EVFILT_VNODE event for fd %d path %s: %s\n",
                     event.ident, watched_file_it->first.c_str(),
                     vnode_event_flags_to_string(event.fflags).c_str());
    }
  }
}

bool change_detecting_filesystem_kqueue::watch_directory(
    const canonical_path& directory) {
  int flags = O_RDONLY;
#if defined(O_EVTONLY)
  flags |= O_EVTONLY;
#endif
  posix_fd_file dir(mockable_directory_open(directory.c_str(), flags));
  if (!dir.valid()) {
    return false;
  }

  auto register_watch = [this](posix_fd_file_ref fd) -> void {
    struct ::kevent change;
    EV_SET(
        /*kev=*/&change,
        /*ident=*/fd.get(),
        /*filter=*/EVFILT_VNODE,
        /*flags=*/EV_ADD | EV_CLEAR | EV_ENABLE,
        /*fflags=*/NOTE_ATTRIB | NOTE_RENAME | NOTE_WRITE,
        /*data=*/0,
        /*udata=*/this->udata_);

    struct ::timespec timeout = {.tv_sec = 0, .tv_nsec = 0};
    int kqueue_rc = ::kevent(
        /*fd=*/this->kqueue_fd_.get(),
        /*changelist=*/&change,
        /*nchanges=*/1,
        /*eventlist=*/nullptr,
        /*nevents=*/0,
        /*timeout=*/&timeout);
    if (kqueue_rc == -1) {
      QLJS_UNIMPLEMENTED();
    }
  };

  watched_file new_watch(std::move(dir));
  auto [watch_it, inserted] =
      this->watched_files_.try_emplace(directory, std::move(new_watch));
  watched_file& existing_watch = watch_it->second;
  if (inserted) {
    // The directory is brand new. Start watching the new directory.
    register_watch(existing_watch.fd.ref());
  } else if (existing_watch.id == new_watch.id) {
    // The directory was already watched. We have two options:
    //
    // 1. Use the old directory. Close the new directory.
    // 2. Close the old directory. Watch the new directory and use it.
    //
    // We choose option 1 to avoid syscalls.
  } else {
    // Different directory with the same path. Stop watching the old directory,
    // and start watching the new directory.
    existing_watch = std::move(new_watch);
    register_watch(existing_watch.fd.ref());
  }

  return true;
}

std::unordered_map<canonical_path,
                   change_detecting_filesystem_kqueue::watched_file>::iterator
change_detecting_filesystem_kqueue::watch_file(canonical_path&& path,
                                               posix_fd_file file) {
  auto register_watch = [this](posix_fd_file_ref fd) -> void {
    struct ::kevent change;
    EV_SET(
        /*kev=*/&change,
        /*ident=*/fd.get(),
        /*filter=*/EVFILT_VNODE,
        /*flags=*/EV_ADD | EV_CLEAR | EV_ENABLE,
        /*fflags=*/NOTE_ATTRIB | NOTE_WRITE,
        /*data=*/0,
        /*udata=*/this->udata_);

    struct ::timespec timeout = {.tv_sec = 0, .tv_nsec = 0};
    int kqueue_rc = ::kevent(
        /*fd=*/this->kqueue_fd_.get(),
        /*changelist=*/&change,
        /*nchanges=*/1,
        /*eventlist=*/nullptr,
        /*nevents=*/0,
        /*timeout=*/&timeout);
    if (kqueue_rc == -1) {
      QLJS_UNIMPLEMENTED();
    }
  };

  watched_file new_watch(std::move(file));
  auto [watch_it, inserted] =
      this->watched_files_.try_emplace(path, std::move(new_watch));
  watched_file& existing_watch = watch_it->second;
  if (inserted) {
    // The file is brand new. Start watching the new file.
    register_watch(existing_watch.fd.ref());
  } else if (existing_watch.id == new_watch.id) {
    // The file was already watched. We have two options:
    //
    // 1. Seek the old file to the beginning and use it. Close the new file.
    // 2. Close the old file. Watch the new file and use it.
    //
    // We choose option 2 for implementation simplicity.
    existing_watch = std::move(new_watch);
    register_watch(existing_watch.fd.ref());
  } else {
    // Different file with the same path. Stop watching the old file, and start
    // watching the new file.
    existing_watch = std::move(new_watch);
    register_watch(existing_watch.fd.ref());
  }
  return watch_it;
}

change_detecting_filesystem_kqueue::file_id
change_detecting_filesystem_kqueue::file_id::from_open_file(
    posix_fd_file_ref fd) {
  struct ::stat s;
  if (::fstat(fd.get(), &s) == -1) {
    QLJS_UNIMPLEMENTED();
  }
  return file_id{.device = s.st_dev, .inode = s.st_ino};
}

bool change_detecting_filesystem_kqueue::file_id::operator==(
    const change_detecting_filesystem_kqueue::file_id& rhs) const noexcept {
  return this->device == rhs.device && this->inode == rhs.inode;
}

bool change_detecting_filesystem_kqueue::file_id::operator!=(
    const change_detecting_filesystem_kqueue::file_id& rhs) const noexcept {
  return !(*this == rhs);
}

change_detecting_filesystem_kqueue::watched_file::watched_file(
    posix_fd_file&& fd)
    : fd(std::move(fd)), id(file_id::from_open_file(this->fd.ref())) {}

namespace {
std::string vnode_event_flags_to_string(std::uint32_t flags) {
  struct flag_entry {
    std::uint32_t flag;
    const char name[13];
  };
  static constexpr flag_entry known_flags[] = {
      {NOTE_ATTRIB, "NOTE_ATTRIB"}, {NOTE_DELETE, "NOTE_DELETE"},
      {NOTE_EXTEND, "NOTE_EXTEND"}, {NOTE_FUNLOCK, "NOTE_FUNLOCK"},
      {NOTE_LINK, "NOTE_LINK"},     {NOTE_RENAME, "NOTE_RENAME"},
      {NOTE_REVOKE, "NOTE_REVOKE"}, {NOTE_WRITE, "NOTE_WRITE"},
  };

  if (flags == 0) {
    return "<none>";
  }

  std::string result;
  for (const flag_entry& flag : known_flags) {
    if (flags & flag.flag) {
      if (!result.empty()) {
        result += "|";
      }
      result += flag.name;
    }
  }
  return result;
}
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
