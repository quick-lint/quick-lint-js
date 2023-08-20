// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <memory>
#include <optional>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>
#include <string>
#include <vector>

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_KQUEUE
struct kevent;
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#include <winioctl.h>
#endif

namespace quick_lint_js {
struct Watch_IO_Error {
  std::string path;
  Platform_File_IO_Error io_error;

  std::string to_string() const;

  friend bool operator==(const Watch_IO_Error&, const Watch_IO_Error&);
  friend bool operator!=(const Watch_IO_Error&, const Watch_IO_Error&);
};

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")
#if QLJS_HAVE_INOTIFY
// For testing only:
extern int mock_inotify_force_init_error;
extern int mock_inotify_force_add_watch_error;

// Not thread-safe.
class Change_Detecting_Filesystem_Inotify : public Configuration_Filesystem,
                                            public Canonicalize_Observer {
 public:
  explicit Change_Detecting_Filesystem_Inotify();
  ~Change_Detecting_Filesystem_Inotify() override;

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string&) override;
  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) override;

  void on_canonicalize_child_of_directory(const char*) override;
  void on_canonicalize_child_of_directory(const wchar_t*) override;

  std::optional<POSIX_FD_File_Ref> get_inotify_fd();
  void handle_poll_event(const ::pollfd& event);

  std::vector<Watch_IO_Error> take_watch_errors();

 private:
  // Sets errno and returns false on failure.
  bool watch_directory(const char*);
  bool watch_directory(const Canonical_Path&);

  void read_inotify();

  std::vector<int> watch_descriptors_;
  std::vector<Watch_IO_Error> watch_errors_;
  Result<POSIX_FD_File, POSIX_File_IO_Error> inotify_fd_;
};
#endif

#if QLJS_HAVE_KQUEUE
// For testing only:
extern int mock_kqueue_force_directory_open_error;

// Not thread-safe.
class Change_Detecting_Filesystem_Kqueue : public Configuration_Filesystem,
                                           Canonicalize_Observer {
 public:
  explicit Change_Detecting_Filesystem_Kqueue(POSIX_FD_File_Ref kqueue_fd,
                                              void* udata);
  ~Change_Detecting_Filesystem_Kqueue() override;

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string&) override;
  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) override;

  void on_canonicalize_child_of_directory(const char*) override;
  void on_canonicalize_child_of_directory(const wchar_t*) override;

  POSIX_FD_File_Ref kqueue_fd() const { return this->kqueue_fd_; }

  void handle_kqueue_event(const struct ::kevent&);

  std::vector<Watch_IO_Error> take_watch_errors();

 private:
  struct File_ID {
    ::dev_t device;
    ::ino_t inode;

    static File_ID from_open_file(POSIX_FD_File_Ref);

    bool operator==(const File_ID&) const;
    bool operator!=(const File_ID&) const;
  };

  // A watched directory or regular file.
  struct Watched_File {
    explicit Watched_File(POSIX_FD_File&&);

    POSIX_FD_File fd;
    File_ID id;
  };

  // Sets errno and returns false on failure.
  bool watch_directory(const Canonical_Path&);

  Hash_Map<Canonical_Path, Watched_File>::iterator watch_file(Canonical_Path&&,
                                                              POSIX_FD_File);

  POSIX_FD_File_Ref kqueue_fd_;
  void* udata_;

  Hash_Map<Canonical_Path, Watched_File> watched_files_;
  std::vector<Watch_IO_Error> watch_errors_;
};
#endif

#if defined(_WIN32)
// For testing only:
extern ::DWORD mock_win32_force_directory_open_error;
extern ::DWORD mock_win32_force_directory_file_id_error;
extern ::DWORD mock_win32_force_directory_ioctl_error;

// Not thread-safe.
class Change_Detecting_Filesystem_Win32 : public Configuration_Filesystem {
 public:
  explicit Change_Detecting_Filesystem_Win32(
      Windows_Handle_File_Ref io_completion_port, ::ULONG_PTR completion_key);
  ~Change_Detecting_Filesystem_Win32() override;

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string&) override;
  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) override;

  Windows_Handle_File_Ref io_completion_port() const {
    return this->io_completion_port_;
  }

  // Returns true if the filesystem possibly changed. Returns false if the
  // filesystem didn't change.
  bool handle_event(::OVERLAPPED*, ::DWORD number_of_bytes_transferred,
                    ::DWORD error);

  void clear_watches();

  std::vector<Watch_IO_Error> take_watch_errors();

 private:
  struct Watched_Directory {
    explicit Watched_Directory(Windows_Handle_File&& directory_handle,
                               const ::FILE_ID_INFO& directory_id);

    // Copying or moving a Watched_Directory is impossible. Pending I/O
    // operations maintain pointers into a Watched_Directory.
    Watched_Directory(const Watched_Directory&) = delete;
    Watched_Directory& operator=(const Watched_Directory&) = delete;

    bool valid() const { return this->directory_handle.valid(); }

    Windows_Handle_File directory_handle;
    ::FILE_ID_INFO directory_id;

    ::OVERLAPPED oplock_overlapped;
    ::REQUEST_OPLOCK_OUTPUT_BUFFER oplock_response;

    static Watched_Directory* from_oplock_overlapped(OVERLAPPED*);
  };

  // Calls SetLastError and returns false on failure.
  bool watch_directory(const Canonical_Path&);

  void cancel_watch(std::unique_ptr<Watched_Directory>&&);

  void handle_oplock_aborted_event(Watched_Directory*);
  void handle_oplock_broke_event(Watched_Directory*,
                                 ::DWORD number_of_bytes_transferred);

  Windows_Handle_File_Ref io_completion_port_;
  ::ULONG_PTR completion_key_;

  Hash_Map<Canonical_Path, std::unique_ptr<Watched_Directory>>
      watched_directories_;
  std::vector<std::unique_ptr<Watched_Directory>>
      cancelling_watched_directories_;
  std::vector<Watch_IO_Error> watch_errors_;
};
#endif
QLJS_WARNING_POP
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
