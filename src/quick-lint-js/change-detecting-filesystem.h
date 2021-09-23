// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CHANGE_DETECTING_FILESYSTEM_H
#define QUICK_LINT_JS_CHANGE_DETECTING_FILESYSTEM_H

#include <memory>
#include <optional>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <unordered_map>
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
#include <Windows.h>
#include <winioctl.h>
#endif

namespace quick_lint_js {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")
#if QLJS_HAVE_INOTIFY
// For testing only:
extern int mock_inotify_force_init_error;
extern int mock_inotify_force_add_watch_error;

// Not thread-safe.
class change_detecting_filesystem_inotify : public configuration_filesystem,
                                            public canonicalize_observer {
 public:
  explicit change_detecting_filesystem_inotify();
  ~change_detecting_filesystem_inotify() override;

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string&) override;
  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path&) override;

  void on_canonicalize_child_of_directory(const char*) override;
  void on_canonicalize_child_of_directory(const wchar_t*) override;

  std::optional<posix_fd_file_ref> get_inotify_fd() noexcept;
  void handle_poll_event(const ::pollfd& event);

  std::vector<watch_io_error> take_watch_errors();

 private:
  // Sets errno and returns false on failure.
  bool watch_directory(const char*);
  bool watch_directory(const canonical_path&);

  void read_inotify();

  std::vector<int> watch_descriptors_;
  std::vector<watch_io_error> watch_errors_;
  result<posix_fd_file, posix_file_io_error> inotify_fd_;
};
#endif

#if QLJS_HAVE_KQUEUE
// For testing only:
extern int mock_kqueue_force_directory_open_error;

// Not thread-safe.
class change_detecting_filesystem_kqueue : public configuration_filesystem,
                                           canonicalize_observer {
 public:
  explicit change_detecting_filesystem_kqueue(posix_fd_file_ref kqueue_fd,
                                              void* udata);
  ~change_detecting_filesystem_kqueue() override;

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string&) override;
  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path&) override;

  void on_canonicalize_child_of_directory(const char*) override;
  void on_canonicalize_child_of_directory(const wchar_t*) override;

  posix_fd_file_ref kqueue_fd() const noexcept { return this->kqueue_fd_; }

  std::vector<watch_io_error> take_watch_errors();

 private:
  struct file_id {
    ::dev_t device;
    ::ino_t inode;

    static file_id from_open_file(posix_fd_file_ref);

    bool operator==(const file_id&) const noexcept;
    bool operator!=(const file_id&) const noexcept;
  };

  // A watched directory or regular file.
  struct watched_file {
    explicit watched_file(posix_fd_file&&);

    posix_fd_file fd;
    file_id id;
  };

  // Sets errno and returns false on failure.
  bool watch_directory(const canonical_path&);

  std::unordered_map<canonical_path, watched_file>::iterator watch_file(
      canonical_path&&, posix_fd_file);

  posix_fd_file_ref kqueue_fd_;
  void* udata_;

  std::unordered_map<canonical_path, watched_file> watched_files_;
  std::vector<watch_io_error> watch_errors_;
};
#endif

#if defined(_WIN32)
// For testing only:
extern ::DWORD mock_win32_force_directory_open_error;
extern ::DWORD mock_win32_force_directory_file_id_error;
extern ::DWORD mock_win32_force_directory_ioctl_error;

// Not thread-safe.
class change_detecting_filesystem_win32 : public configuration_filesystem {
 public:
  explicit change_detecting_filesystem_win32(
      windows_handle_file_ref io_completion_port, ::ULONG_PTR completion_key);
  ~change_detecting_filesystem_win32() override;

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string&) override;
  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path&) override;

  windows_handle_file_ref io_completion_port() const noexcept {
    return this->io_completion_port_;
  }

  // Returns true if the filesystem possibly changed. Returns false if the
  // filesystem didn't change.
  bool handle_event(::OVERLAPPED*, ::DWORD number_of_bytes_transferred,
                    ::DWORD error);

  void clear_watches();

  std::vector<watch_io_error> take_watch_errors();

 private:
  struct watched_directory {
    explicit watched_directory(windows_handle_file&& directory_handle,
                               const ::FILE_ID_INFO& directory_id);

    // Copying or moving a watched_directory is impossible. Pending I/O
    // operations maintain pointers into a watched_directory.
    watched_directory(const watched_directory&) = delete;
    watched_directory& operator=(const watched_directory&) = delete;

    bool valid() const noexcept { return this->directory_handle.valid(); }

    windows_handle_file directory_handle;
    ::FILE_ID_INFO directory_id;

    ::OVERLAPPED oplock_overlapped;
    ::REQUEST_OPLOCK_OUTPUT_BUFFER oplock_response;

    static watched_directory* from_oplock_overlapped(OVERLAPPED*) noexcept;
  };

  // Calls SetLastError and returns false on failure.
  bool watch_directory(const canonical_path&);

  void cancel_watch(std::unique_ptr<watched_directory>&&);

  void handle_oplock_aborted_event(watched_directory*);
  void handle_oplock_broke_event(watched_directory*,
                                 ::DWORD number_of_bytes_transferred);

  windows_handle_file_ref io_completion_port_;
  ::ULONG_PTR completion_key_;

  std::unordered_map<canonical_path, std::unique_ptr<watched_directory>>
      watched_directories_;
  std::vector<std::unique_ptr<watched_directory>>
      cancelling_watched_directories_;
  std::vector<watch_io_error> watch_errors_;
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
