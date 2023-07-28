// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_KQUEUE

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <fcntl.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>
#include <string_view>
#include <sys/event.h>
#include <sys/stat.h>
#include <sys/time.h>
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

Change_Detecting_Filesystem_Kqueue::Change_Detecting_Filesystem_Kqueue(
    POSIX_FD_File_Ref kqueue_fd, void* udata)
    : kqueue_fd_(kqueue_fd), udata_(udata) {}

Change_Detecting_Filesystem_Kqueue::~Change_Detecting_Filesystem_Kqueue() {
  // NOTE(strager): Closing the file descriptors will remove the filters from
  // kqueue_fd_ (I think).
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
Change_Detecting_Filesystem_Kqueue::canonicalize_path(const std::string& path) {
  return quick_lint_js::canonicalize_path(path, this);
}

Result<Padded_String, Read_File_IO_Error>
Change_Detecting_Filesystem_Kqueue::read_file(const Canonical_Path& path) {
  Canonical_Path directory = path;
  directory.parent();
  bool ok = this->watch_directory(directory);
  if (!ok) {
    this->watch_errors_.emplace_back(Watch_IO_Error{
        .path = std::move(directory).path(),
        .io_error = POSIX_File_IO_Error{errno},
    });
    // FIXME(strager): Recovering now is probably pointless. If watch_directory
    // failed because open() returned EMFILE, then the following open() call is
    // going to fail too. We should probably clean up open watches to make room
    // for more file descriptors.
  }

  // TODO(strager): Use openat. watch_directory opened a directory fd.
  POSIX_FD_File file(::open(path.c_str(), O_RDONLY));
  if (!file.valid()) {
    return failed_result(Read_File_IO_Error{
        .path = path.c_str(),
        .io_error = POSIX_File_IO_Error{errno},
    });
  }

  auto watch_it = this->watch_file(Canonical_Path(path), std::move(file));
  Result<Padded_String, Read_File_IO_Error> r =
      quick_lint_js::read_file(path.c_str(), watch_it->second.fd.ref());
  if (!r.ok()) return r.propagate();
  return *std::move(r);
}

void Change_Detecting_Filesystem_Kqueue::on_canonicalize_child_of_directory(
    const char* path) {
  bool ok = this->watch_directory(Canonical_Path(path));
  if (!ok) {
    this->watch_errors_.emplace_back(Watch_IO_Error{
        .path = path,
        .io_error = POSIX_File_IO_Error{errno},
    });
  }
}

std::vector<Watch_IO_Error>
Change_Detecting_Filesystem_Kqueue::take_watch_errors() {
  return std::exchange(this->watch_errors_, std::vector<Watch_IO_Error>());
}

void Change_Detecting_Filesystem_Kqueue::on_canonicalize_child_of_directory(
    const wchar_t*) {
  // We don't use wchar_t paths on BSDs.
  QLJS_UNREACHABLE();
}

void Change_Detecting_Filesystem_Kqueue::handle_kqueue_event(
    const struct ::kevent& event) {
  QLJS_ASSERT(event.filter == EVFILT_VNODE);

  if (is_logging_enabled()) {
    auto watched_file_it =
        find_unique_if(this->watched_files_, [&](auto& pair) -> bool {
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

bool Change_Detecting_Filesystem_Kqueue::watch_directory(
    const Canonical_Path& directory) {
  int flags = O_RDONLY;
#if defined(O_EVTONLY)
  flags |= O_EVTONLY;
#endif
  POSIX_FD_File dir(mockable_directory_open(directory.c_str(), flags));
  if (!dir.valid()) {
    return false;
  }

  auto register_watch = [this](POSIX_FD_File_Ref fd) -> void {
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

  Watched_File new_watch(std::move(dir));
  auto [watch_it, inserted] =
      this->watched_files_.try_emplace(directory, std::move(new_watch));
  Watched_File& existing_watch = watch_it->second;
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

Hash_Map<Canonical_Path,
         Change_Detecting_Filesystem_Kqueue::Watched_File>::iterator
Change_Detecting_Filesystem_Kqueue::watch_file(Canonical_Path&& path,
                                               POSIX_FD_File file) {
  auto register_watch = [this](POSIX_FD_File_Ref fd) -> void {
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

  Watched_File new_watch(std::move(file));
  auto [watch_it, inserted] =
      this->watched_files_.try_emplace(path, std::move(new_watch));
  Watched_File& existing_watch = watch_it->second;
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

Change_Detecting_Filesystem_Kqueue::File_ID
Change_Detecting_Filesystem_Kqueue::File_ID::from_open_file(
    POSIX_FD_File_Ref fd) {
  struct ::stat s;
  if (::fstat(fd.get(), &s) == -1) {
    QLJS_UNIMPLEMENTED();
  }
  return File_ID{.device = s.st_dev, .inode = s.st_ino};
}

bool Change_Detecting_Filesystem_Kqueue::File_ID::operator==(
    const Change_Detecting_Filesystem_Kqueue::File_ID& rhs) const {
  return this->device == rhs.device && this->inode == rhs.inode;
}

bool Change_Detecting_Filesystem_Kqueue::File_ID::operator!=(
    const Change_Detecting_Filesystem_Kqueue::File_ID& rhs) const {
  return !(*this == rhs);
}

Change_Detecting_Filesystem_Kqueue::Watched_File::Watched_File(
    POSIX_FD_File&& fd)
    : fd(std::move(fd)), id(File_ID::from_open_file(this->fd.ref())) {}

namespace {
std::string vnode_event_flags_to_string(std::uint32_t flags) {
  struct Flag_Entry {
    std::uint32_t flag;
    const char name[13];
  };
  static constexpr Flag_Entry known_flags[] = {
    {NOTE_ATTRIB, "NOTE_ATTRIB"},
    {NOTE_DELETE, "NOTE_DELETE"},
    {NOTE_EXTEND, "NOTE_EXTEND"},
    {NOTE_LINK, "NOTE_LINK"},
    {NOTE_RENAME, "NOTE_RENAME"},
    {NOTE_REVOKE, "NOTE_REVOKE"},
    {NOTE_WRITE, "NOTE_WRITE"},
#if defined(__APPLE__)
    {NOTE_FUNLOCK, "NOTE_FUNLOCK"},
#endif
  };

  if (flags == 0) {
    return "<none>";
  }

  std::string result;
  for (const Flag_Entry& flag : known_flags) {
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
