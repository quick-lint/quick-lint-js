// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/utf-16.h>
#include <stdlib.h>
#include <string>

#if QLJS_HAVE_DIRENT_H
#include <dirent.h>
#endif

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#define QLJS_FILE_WINDOWS
#elif QLJS_HAVE_UNISTD_H
#define QLJS_FILE_POSIX
#else
#error "Unsupported platform"
#endif

using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
Platform_File_IO_Error file_too_large_error() {
#if QLJS_HAVE_WINDOWS_H
  return Windows_File_IO_Error{ERROR_FILE_TOO_LARGE};
#elif QLJS_HAVE_UNISTD_H
  return POSIX_File_IO_Error{EFBIG};
#else
#error "Unknown platform"
#endif
}

Result<void, Platform_File_IO_Error> read_file_buffered(
    Platform_File_Ref file, Padded_String_Size buffer_size,
    Padded_String *out_content) {
  // TODO(strager): Use byte_buffer to avoid copying the file content every
  // iteration.
  for (;;) {
    Padded_String_Size size_before = out_content->size();
    {
      std::optional<Padded_String_Size> new_size =
          checked_add(size_before, buffer_size);
      if (!new_size.has_value()) {
        // TODO(strager): Should we try a small buffer size?
        return failed_result(file_too_large_error());
      }
      out_content->resize_grow_uninitialized(size_before + buffer_size);
    }

    // TODO(strager): Get rid of this narrow_cast.
    File_Read_Result read_result = file.read(&out_content->data()[size_before],
                                             narrow_cast<int>(buffer_size));
    if (!read_result.ok()) return read_result.propagate();
    if (read_result.at_end_of_file()) {
      // We read the entire file.
      out_content->resize(size_before);
      return {};
    }
    // TODO(strager): Get rid of this narrow_cast.
    std::optional<Padded_String_Size> new_size = checked_add(
        size_before, narrow_cast<Padded_String_Size>(read_result.bytes_read()));
    QLJS_ASSERT(new_size.has_value());
    out_content->resize(*new_size);
  }
}

Result<Padded_String, Platform_File_IO_Error> read_file_with_expected_size(
    Platform_File_Ref file, int file_size, int buffer_size) {
  Padded_String content;

  std::optional<int> size_to_read = checked_add(file_size, 1);
  if (!size_to_read.has_value()) {
    return failed_result(file_too_large_error());
  }
  content.resize_grow_uninitialized(*size_to_read);

  File_Read_Result read_result = file.read(content.data(), *size_to_read);
  if (!read_result.ok()) return read_result.propagate();
  if (read_result.at_end_of_file()) {
    // The file was empty.
    content.resize(0);
    return content;
  }
  if (read_result.bytes_read() == file_size) {
    // We possibly read the entire file. Make extra sure by reading one more
    // byte.
    File_Read_Result extra_read_result =
        file.read(content.data() + file_size, 1);
    if (!extra_read_result.ok()) return extra_read_result.propagate();
    if (extra_read_result.at_end_of_file()) {
      // We definitely read the entire file.
      content.resize(read_result.bytes_read());
      return content;
    } else {
      // We didn't read the entire file the first time. Keep reading.
      content.resize(read_result.bytes_read() + extra_read_result.bytes_read());
      Result<void, Platform_File_IO_Error> r =
          read_file_buffered(file, buffer_size, &content);
      if (!r.ok()) return r.propagate();
      return content;
    }
  } else {
    content.resize(read_result.bytes_read());
    // We did not read the entire file. There is more data to read.
    Result<void, Platform_File_IO_Error> r =
        read_file_buffered(file, buffer_size, &content);
    if (!r.ok()) return r.propagate();
    return content;
  }
}
}

bool Read_File_IO_Error::is_file_not_found_error() const {
  return this->io_error.is_file_not_found_error();
}

std::string Read_File_IO_Error::to_string() const {
  return "failed to read from "s + this->path + ": "s +
         this->io_error.to_string();
}

[[noreturn]] void Read_File_IO_Error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

std::string Write_File_IO_Error::to_string() const {
  return "failed to write to "s + this->path + ": "s +
         this->io_error.to_string();
}

[[noreturn]] void Write_File_IO_Error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

std::string Delete_File_IO_Error::to_string() const {
  return "failed to delete "s + this->path + ": "s + this->io_error.to_string();
}

[[noreturn]] void Delete_File_IO_Error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

std::string Symlink_IO_Error::to_string() const {
  return "failed to create symlink to "s + this->target + " at " + this->path +
         ": "s + this->io_error.to_string();
}

[[noreturn]] void Symlink_IO_Error::print_and_exit() const {
  std::fprintf(stderr, "error: %s\n", this->to_string().c_str());
  std::exit(1);
}

bool operator==(const Read_File_IO_Error &lhs, const Read_File_IO_Error &rhs) {
  return lhs.path == rhs.path && lhs.io_error == rhs.io_error;
}

bool operator!=(const Read_File_IO_Error &lhs, const Read_File_IO_Error &rhs) {
  return !(lhs == rhs);
}

#if defined(QLJS_FILE_WINDOWS)
Result<Padded_String, Platform_File_IO_Error> read_file(
    Windows_Handle_File_Ref file) {
  int buffer_size = 1024;  // TODO(strager): Compute a good buffer size.

  ::LARGE_INTEGER file_size;
  if (!::GetFileSizeEx(file.get(), &file_size)) {
    return failed_result(Windows_File_IO_Error{::GetLastError()});
  }
  if (!in_range<int>(file_size.QuadPart)) {
    return failed_result(file_too_large_error());
  }

  return read_file_with_expected_size(
      /*file=*/file,
      /*file_size=*/narrow_cast<int>(file_size.QuadPart),
      /*buffer_size=*/buffer_size);
}
#endif

#if defined(QLJS_FILE_POSIX)
namespace {
int reasonable_buffer_size(const struct stat &s) {
  using Size_Type = decltype(s.st_blksize);
  Size_Type minimum_buffer_size = 512;
  Size_Type megabyte = 1 << 20;
  // Bound st_blksize in case the OS gives us crazy numbers (like 0 or
  // (size_t)-1).
  return narrow_cast<int>(
      std::clamp(s.st_blksize, /*lo=*/minimum_buffer_size, /*hi=*/megabyte));
}
}

Result<Padded_String, Platform_File_IO_Error> read_file(
    POSIX_FD_File_Ref file) {
  struct stat s;
  int rc = ::fstat(file.get(), &s);
  if (rc == -1) {
    return failed_result(POSIX_File_IO_Error{errno});
  }
  auto file_size = s.st_size;
  if (!in_range<int>(file_size)) {
    return failed_result(file_too_large_error());
  }

  return read_file_with_expected_size(
      /*file=*/file, /*file_size=*/narrow_cast<int>(file_size),
      /*buffer_size=*/reasonable_buffer_size(s));
}
#endif

Result<Padded_String, Read_File_IO_Error> read_file(const char *path,
                                                    Platform_File_Ref file) {
  Result<Padded_String, Platform_File_IO_Error> r = read_file(file);
  if (!r.ok()) {
    return failed_result(
        Read_File_IO_Error{.path = path, .io_error = r.error()});
  }
  return *std::move(r);
}

Result<Padded_String, Read_File_IO_Error> read_file(const std::string &path) {
  return read_file(path.c_str());
}

#if defined(QLJS_FILE_WINDOWS)
Result<Padded_String, Read_File_IO_Error> read_file(const char *path) {
  // TODO(strager): Avoid copying the path string, especially on success.
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath) {
    return failed_result(Read_File_IO_Error{
        .path = path, .io_error = Windows_File_IO_Error{::GetLastError()}});
  }
  HANDLE handle = ::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_READ,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/OPEN_EXISTING,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr);
  if (handle == INVALID_HANDLE_VALUE) {
    return failed_result(Read_File_IO_Error{
        .path = path, .io_error = Windows_File_IO_Error{::GetLastError()}});
  }
  Windows_Handle_File file(handle);
  return read_file(path, file.ref());
}

Result<Padded_String, Read_File_IO_Error> read_stdin() {
  Windows_Handle_File_Ref file(::GetStdHandle(STD_INPUT_HANDLE));
  return read_file("<stdin>", file);
}
#endif

#if defined(QLJS_FILE_POSIX)
Result<Padded_String, Read_File_IO_Error> read_file(const char *path) {
  int fd = ::open(path, O_CLOEXEC | O_RDONLY);
  if (fd == -1) {
    return failed_result(Read_File_IO_Error{
        .path = path, .io_error = POSIX_File_IO_Error{errno}});
  }
  POSIX_FD_File file(fd);
  return read_file(path, file.ref());
}

Result<Padded_String, Read_File_IO_Error> read_stdin() {
  POSIX_FD_File_Ref file(STDIN_FILENO);
  return read_file("<stdin>", file);
}
#endif

Padded_String read_file_or_exit(const std::string &path) {
  return read_file_or_exit(path.c_str());
}

Padded_String read_file_or_exit(const char *path) {
  Result<Padded_String, Read_File_IO_Error> r = read_file(path);
  if (!r.ok()) {
    r.error().print_and_exit();
  }
  return *std::move(r);
}

Result<void, Write_File_IO_Error> write_file(const std::string &path,
                                             String8_View content) {
  return write_file(path.c_str(), content);
}

#if defined(QLJS_FILE_POSIX)
Result<Platform_File, Write_File_IO_Error> open_file_for_writing(
    const char *path) {
  POSIX_FD_File file(
      ::open(path, O_CLOEXEC | O_CREAT | O_TRUNC | O_WRONLY, 0644));
  if (!file.valid()) {
    return failed_result(Write_File_IO_Error{
        .path = path,
        .io_error = POSIX_File_IO_Error{errno},
    });
  }
  return file;
}
#endif

#if defined(QLJS_FILE_WINDOWS)
Result<Platform_File, Write_File_IO_Error> open_file_for_writing(
    const char *path) {
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath) {
    return failed_result(Write_File_IO_Error{
        .path = path,
        .io_error = Windows_File_IO_Error{::GetLastError()},
    });
  }
  Windows_Handle_File file(::CreateFileW(
      wpath->c_str(), /*dwDesiredAccess=*/GENERIC_WRITE,
      /*dwShareMode=*/FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
      /*lpSecurityAttributes=*/nullptr,
      /*dwCreationDisposition=*/CREATE_ALWAYS,
      /*dwFlagsAndAttributes=*/FILE_ATTRIBUTE_NORMAL,
      /*hTemplateFile=*/nullptr));
  if (!file.valid()) {
    return failed_result(Write_File_IO_Error{
        .path = path,
        .io_error = Windows_File_IO_Error{::GetLastError()},
    });
  }
  return std::move(file);
}
#endif

Result<void, Write_File_IO_Error> write_file(const char *path,
                                             String8_View content) {
  auto file = open_file_for_writing(path);
  if (!file.ok()) {
    return file.propagate();
  }

  auto write_result = file->write_full(content.data(), content.size());
  if (!write_result.ok()) {
    return failed_result(Write_File_IO_Error{
        .path = path,
        .io_error = write_result.error(),
    });
  }

  return {};
}

void write_file_or_exit(const std::string &path, String8_View content) {
  write_file_or_exit(path.c_str(), content);
}

void write_file_or_exit(const char *path, String8_View content) {
  Result<void, Write_File_IO_Error> result = write_file(path, content);
  if (!result.ok()) {
    result.error().print_and_exit();
  }
}

Result<bool, Generic_IO_Error> write_file_if_different(const std::string &path,
                                                       String8_View content) {
  return write_file_if_different(path.c_str(), content);
}

Result<bool, Generic_IO_Error> write_file_if_different(const char *path,
                                                       String8_View content) {
  Result<Padded_String, Read_File_IO_Error> read_result = read_file(path);
  bool file_is_different;
  if (read_result.ok()) {
    file_is_different = *read_result != content;
  } else if (read_result.error().is_file_not_found_error()) {
    file_is_different = true;
  } else {
    // FIXME(strager): Should we try to write the file anyway?
    // TODO(strager): Use Result<>::propagate.
    return failed_result(Generic_IO_Error(read_result.error_to_string()));
  }
  if (!file_is_different) {
    return false;
  }

  Result<void, Write_File_IO_Error> write_result = write_file(path, content);
  if (!write_result.ok()) {
    // TODO(strager): Use Result<>::propagate.
    return failed_result(Generic_IO_Error(read_result.error_to_string()));
  }
  return true;
}

#if QLJS_HAVE_WINDOWS_H
bool file_ids_equal(const ::FILE_ID_INFO &a, const ::FILE_ID_INFO &b) {
  return b.VolumeSerialNumber == a.VolumeSerialNumber &&
         std::memcmp(&b.FileId, &a.FileId, sizeof(b.FileId)) == 0;
}
#endif

namespace {
Result<void, Symlink_IO_Error> create_posix_symbolic_link(
    const char *path, const char *target, [[maybe_unused]] bool is_directory) {
#if defined(QLJS_FILE_POSIX)
  int rc = ::symlink(target, path);
  if (rc != 0) {
    return failed_result(Symlink_IO_Error{
        .path = path,
        .target = target,
        .io_error = POSIX_File_IO_Error{errno},
    });
  }
  return {};
#elif defined(QLJS_FILE_WINDOWS)
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  std::optional<std::wstring> wtarget = mbstring_to_wstring(target);
  if (!wpath.has_value() || !wtarget.has_value()) {
    return failed_result(Symlink_IO_Error{
        .path = path,
        .target = target,
        .io_error = Windows_File_IO_Error{ERROR_INVALID_PARAMETER},
    });
  }

  // TODO(strager): Ensure a relative target path creates relative symlinks, not
  // absolute symlinks resolved to the current working directory.
  //
  // FIXME(strager): With SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE,
  // ::CreateSymbolicLinkW can fail with ERROR_INVALID_PARAMETER or maybe
  // something else. Need to test more Windows versions.
  //
  // NOTE(strager): ::CreateSymbolicLinkW fails with ERROR_PRIVILEGE_NOT_HELD
  // (1314) if SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE is not set.
  if (!::CreateSymbolicLinkW(
          wpath->c_str(), wtarget->c_str(),
          SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE |
              (is_directory ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0))) {
    return failed_result(Symlink_IO_Error{
        .path = path,
        .target = target,
        .io_error = Windows_File_IO_Error{::GetLastError()},
    });
  }

  return {};
#else
#error "Unknown platform"
#endif
}
}

Result<void, Symlink_IO_Error> create_posix_directory_symbolic_link(
    const char *path, const char *target) {
  return create_posix_symbolic_link(path, target, /*is_directory=*/true);
}

Result<void, Symlink_IO_Error> create_posix_file_symbolic_link(
    const char *path, const char *target) {
  return create_posix_symbolic_link(path, target, /*is_directory=*/false);
}

void create_posix_directory_symbolic_link_or_exit(const char *path,
                                                  const char *target) {
  Result<void, Symlink_IO_Error> result =
      create_posix_directory_symbolic_link(path, target);
  if (!result.ok()) {
    result.error().print_and_exit();
  }
}

void create_posix_file_symbolic_link_or_exit(const char *path,
                                             const char *target) {
  Result<void, Symlink_IO_Error> result =
      create_posix_file_symbolic_link(path, target);
  if (!result.ok()) {
    result.error().print_and_exit();
  }
}

Result<void, Delete_File_IO_Error> delete_posix_symbolic_link(
    const char *path) {
#if defined(QLJS_FILE_POSIX)
  int rc = ::unlink(path);
  if (rc != 0) {
    return failed_result(Delete_File_IO_Error{
        .path = path,
        .io_error = POSIX_File_IO_Error{errno},
    });
  }
  return {};
#elif defined(QLJS_FILE_WINDOWS)
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath.has_value()) {
    return failed_result(Delete_File_IO_Error{
        .path = path,
        .io_error = Windows_File_IO_Error{ERROR_INVALID_PARAMETER},
    });
  }

  ::DWORD attributes = ::GetFileAttributesW(wpath->c_str());
  if (attributes == INVALID_FILE_ATTRIBUTES) {
    return failed_result(Delete_File_IO_Error{
        .path = path,
        .io_error = Windows_File_IO_Error{::GetLastError()},
    });
  }
  if (attributes & FILE_ATTRIBUTE_DIRECTORY) {
    if (!::RemoveDirectoryW(wpath->c_str())) {
      return failed_result(Delete_File_IO_Error{
          .path = path,
          .io_error = Windows_File_IO_Error{::GetLastError()},
      });
    }
  } else {
    if (!::DeleteFileW(wpath->c_str())) {
      return failed_result(Delete_File_IO_Error{
          .path = path,
          .io_error = Windows_File_IO_Error{::GetLastError()},
      });
    }
  }

  return {};
#else
#error "Unknown platform"
#endif
}

void delete_posix_symbolic_link_or_exit(const char *path) {
  Result<void, Delete_File_IO_Error> result = delete_posix_symbolic_link(path);
  if (!result.ok()) {
    result.error().print_and_exit();
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
