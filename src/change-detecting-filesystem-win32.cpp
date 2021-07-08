// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(_WIN32)

#include <Windows.h>
#include <boost/leaf/result.hpp>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <iterator>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/utf-16.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#if NDEBUG
#define QLJS_LOG(...) \
  do {                \
  } while (false)
#else
#define QLJS_LOG(...)                    \
  do {                                   \
    ::std::fprintf(stderr, __VA_ARGS__); \
  } while (false)
#endif

namespace quick_lint_js {
namespace {
void attach_handle_to_iocp(windows_handle_file_ref handle,
                           windows_handle_file_ref iocp,
                           ULONG_PTR completionKey) noexcept;
bool file_ids_equal(const FILE_ID_INFO&, const FILE_ID_INFO&) noexcept;

template <class Iterator, class Vector>
void swap_erase(Vector& v, Iterator it_to_remove) {
  QLJS_ASSERT(it_to_remove != std::end(v));
  std::swap(*it_to_remove, v.back());
  v.pop_back();
}
}

// change_detecting_filesystem_win32 implements directory and file change
// notifications using a little-known feature called oplocks.
//
// For each directory we want to watch, we acquire an oplock. When a change
// happens, the oplock is broken and we are notified.
//
// Well-known APIs, such as FindFirstChangeNotificationW and
// ReadDirectoryChangesW, don't work because they hold a directory handle. This
// handle prevents renaming any ancestor directory. Directory handles with an
// oplock don't have this problem.
//
// Documentation on oplocks:
// * https://github.com/pauldotknopf/WindowsSDK7-Samples/blob/3f2438b15c59fdc104c13e2cf6cf46c1b16cf281/winbase/io/Oplocks/Oplocks/Oplocks.cpp
// * https://docs.microsoft.com/en-us/windows/win32/api/winioctl/ni-winioctl-fsctl_request_oplock
//
// When an oplock is broken, the directory handle is signalled. We could wait
// for the directory handles using WaitForMultipleObjects, but WFMO has a limit
// of 64 handles. This limit is low for our use case. To wait for any number of
// directory handles, we wait for events using an I/O completion port
// (io_completion_port_) pumped by event_loop.
change_detecting_filesystem_win32::change_detecting_filesystem_win32(
    windows_handle_file_ref io_completion_port, ::ULONG_PTR completion_key)
    : io_completion_port_(io_completion_port),
      completion_key_(completion_key) {}

change_detecting_filesystem_win32::~change_detecting_filesystem_win32() {
  // Closing the directory handles will queue a bunch of errors onto the I/O
  // completion port. We don't care, though, because the LSP server exits by
  // calling ExitProcess thus shouldn't call this destructor anyway.
}

boost::leaf::result<canonical_path_result>
change_detecting_filesystem_win32::canonicalize_path(const std::string& path) {
  return quick_lint_js::canonicalize_path_2(path);
}

boost::leaf::result<padded_string> change_detecting_filesystem_win32::read_file(
    const canonical_path& path) {
  canonical_path directory = path;
  directory.parent();
  bool ok = this->watch_directory(directory);
  if (!ok) {
    return boost::leaf::new_error(
        boost::leaf::windows::e_LastError{::GetLastError()});
  }

  return quick_lint_js::read_file(path.c_str());
}

bool change_detecting_filesystem_win32::handle_event(
    ::OVERLAPPED* overlapped, ::DWORD number_of_bytes_transferred,
    ::DWORD error) {
  watched_directory* dir =
      watched_directory::from_oplock_overlapped(overlapped);
  switch (error) {
  case ERROR_SUCCESS:
  case WAIT_TIMEOUT:  // FIXME(strager): Why do we sometimes get WAIT_TIMEOUT?
    this->handle_oplock_broke_event(dir, number_of_bytes_transferred);
    return true;

  case ERROR_OPERATION_ABORTED:
    this->handle_oplock_aborted_event(dir);
    return false;

  default:
    QLJS_UNIMPLEMENTED();
    return true;
  }
}

void change_detecting_filesystem_win32::cancel_watch(
    std::unique_ptr<watched_directory>&& dir) {
  BOOL ok = ::CancelIoEx(dir->directory_handle.get(), nullptr);
  if (!ok) {
    DWORD error = ::GetLastError();
    if (error == ERROR_NOT_FOUND) {
      // TODO(strager): Figure out why this error happens sometimes.
    } else {
      QLJS_UNIMPLEMENTED();
    }
  }
  this->cancelling_watched_directories_.emplace_back(std::move(dir));
}

bool change_detecting_filesystem_win32::watch_directory(
    const canonical_path& directory) {
  std::optional<std::wstring> wpath = mbstring_to_wstring(directory.c_str());
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }

  windows_handle_file directory_handle(::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_READ,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/OPEN_EXISTING,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL |
          FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
      /*hTemplateFile=*/nullptr));
  if (!directory_handle.valid()) {
    return false;
  }
  FILE_ID_INFO directory_id;
  if (!::GetFileInformationByHandleEx(directory_handle.get(), ::FileIdInfo,
                                      &directory_id, sizeof(directory_id))) {
    QLJS_UNIMPLEMENTED();
  }

  std::unique_ptr<watched_directory> new_dir =
      std::make_unique<watched_directory>(std::move(directory_handle),
                                          directory_id);
  watched_directory* dir = new_dir.get();
  auto [watched_directory_it, inserted] =
      this->watched_directories_.try_emplace(directory, std::move(new_dir));
  if (!inserted) {
    watched_directory* old_dir = watched_directory_it->second.get();
    bool already_watched =
        file_ids_equal(old_dir->directory_id, new_dir->directory_id);
    if (already_watched) {
      return true;
    }

    QLJS_LOG(
        "note: Directory handle %#llx: %s: Directory identity changed\n",
        reinterpret_cast<unsigned long long>(old_dir->directory_handle.get()),
        directory.c_str());
    this->cancel_watch(std::move(watched_directory_it->second));
    watched_directory_it->second = std::move(new_dir);
  }

  attach_handle_to_iocp(dir->directory_handle.ref(), this->io_completion_port_,
                        this->completion_key_);

  REQUEST_OPLOCK_INPUT_BUFFER request = {
      .StructureVersion = REQUEST_OPLOCK_CURRENT_VERSION,
      .StructureLength = sizeof(REQUEST_OPLOCK_INPUT_BUFFER),
      .RequestedOplockLevel =
          OPLOCK_LEVEL_CACHE_READ | OPLOCK_LEVEL_CACHE_HANDLE,
      .Flags = REQUEST_OPLOCK_INPUT_FLAG_REQUEST,
  };
  BOOL ok = ::DeviceIoControl(/*hDevice=*/dir->directory_handle.get(),
                              /*dwIoControlCode=*/FSCTL_REQUEST_OPLOCK,
                              /*lpInBuffer=*/&request,
                              /*nInBufferSize=*/sizeof(request),
                              /*lpOutBuffer=*/&dir->oplock_response,
                              /*nOutBufferSize=*/sizeof(dir->oplock_response),
                              /*lpBytesReturned=*/nullptr,
                              /*lpOverlapped=*/&dir->oplock_overlapped);
  if (ok) {
    // TODO(strager): Can this happen? I assume if this happens, the oplock was
    // immediately broken.
    QLJS_UNIMPLEMENTED();
  } else {
    DWORD error = ::GetLastError();
    if (error == ERROR_IO_PENDING) {
      // run_io_thread will handle the oplock breaking.
    } else {
      QLJS_UNIMPLEMENTED();
    }
  }

  return true;
}

void change_detecting_filesystem_win32::handle_oplock_aborted_event(
    watched_directory* dir) {
  auto directory_it =
      std::find_if(this->cancelling_watched_directories_.begin(),
                   this->cancelling_watched_directories_.end(),
                   [&](const std::unique_ptr<watched_directory>& d) {
                     return d.get() == dir;
                   });
  QLJS_ASSERT(directory_it != this->cancelling_watched_directories_.end());
  swap_erase(this->cancelling_watched_directories_, directory_it);
}

void change_detecting_filesystem_win32::handle_oplock_broke_event(
    watched_directory* dir,
    [[maybe_unused]] ::DWORD number_of_bytes_transferred) {
  auto directory_it = std::find_if(
      this->watched_directories_.begin(), this->watched_directories_.end(),
      [&](const auto& entry) { return entry.second.get() == dir; });
  QLJS_ASSERT(directory_it != this->watched_directories_.end());

  // A directory oplock breaks if any of the following happens:
  //
  // * The directory or any of its ancestors is renamed. The rename blocks
  //   until we release the oplock.
  // * A file in the directory is created, modified, or deleted.
  //
  // https://docs.microsoft.com/en-us/windows/win32/api/winioctl/ni-winioctl-fsctl_request_oplock
  QLJS_LOG("note: Directory handle %#llx: %s: Oplock broke\n",
           reinterpret_cast<unsigned long long>(dir->directory_handle.get()),
           directory_it->first.c_str());
  QLJS_ASSERT(number_of_bytes_transferred == sizeof(dir->oplock_response));
  QLJS_ASSERT(dir->oplock_response.Flags &
              REQUEST_OPLOCK_OUTPUT_FLAG_ACK_REQUIRED);

  // Erasing the watched_directory will close dir.directory_handle, releasing
  // the oplock.
  this->watched_directories_.erase(directory_it);
}

change_detecting_filesystem_win32::watched_directory::watched_directory(
    windows_handle_file&& directory_handle, const FILE_ID_INFO& directory_id)
    : directory_handle(std::move(directory_handle)),
      directory_id(directory_id) {
  QLJS_ASSERT(this->directory_handle.valid());

  this->oplock_overlapped.Offset = 0;
  this->oplock_overlapped.OffsetHigh = 0;
  this->oplock_overlapped.hEvent = nullptr;
}

change_detecting_filesystem_win32::watched_directory*
change_detecting_filesystem_win32::watched_directory::from_oplock_overlapped(
    OVERLAPPED* overlapped) noexcept {
  return reinterpret_cast<watched_directory*>(
      reinterpret_cast<std::uintptr_t>(overlapped) -
      offsetof(watched_directory, oplock_overlapped));
}

namespace {
void attach_handle_to_iocp(windows_handle_file_ref handle,
                           windows_handle_file_ref iocp,
                           ULONG_PTR completionKey) noexcept {
  HANDLE iocp2 = CreateIoCompletionPort(
      /*FileHandle=*/handle.get(),
      /*ExistingCompletionPort=*/iocp.get(),
      /*CompletionKey=*/completionKey,
      /*NumberOfConcurrentThreads=*/1);
  if (iocp2 != iocp.get()) {
    QLJS_UNIMPLEMENTED();
  }
}

bool file_ids_equal(const FILE_ID_INFO& a, const FILE_ID_INFO& b) noexcept {
  return b.VolumeSerialNumber == a.VolumeSerialNumber &&
         memcmp(&b.FileId, &a.FileId, sizeof(b.FileId)) == 0;
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
