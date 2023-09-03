// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No file I/O on the web.
#else

#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/io-error.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <string>
#include <vector>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
struct Read_File_IO_Error {
  std::string path;
  Platform_File_IO_Error io_error;

  bool is_file_not_found_error() const;

  std::string to_string() const;
  [[noreturn]] void print_and_exit() const;

  friend bool operator==(const Read_File_IO_Error &,
                         const Read_File_IO_Error &);
  friend bool operator!=(const Read_File_IO_Error &,
                         const Read_File_IO_Error &);
};

struct Write_File_IO_Error {
  std::string path;
  Platform_File_IO_Error io_error;

  std::string to_string() const;
  [[noreturn]] void print_and_exit() const;
};

Result<Padded_String, Read_File_IO_Error> read_file(const std::string &path);
Result<Padded_String, Read_File_IO_Error> read_file(const char *path);
Result<Padded_String, Read_File_IO_Error> read_file(const char *path,
                                                    Platform_File_Ref);
Result<Padded_String, Platform_File_IO_Error> read_file(Platform_File_Ref);
Result<Padded_String, Read_File_IO_Error> read_stdin(void);

Padded_String read_file_or_exit(const std::string &path);
Padded_String read_file_or_exit(const char *path);

Result<void, Write_File_IO_Error> write_file(const std::string &path,
                                             String8_View content);
Result<void, Write_File_IO_Error> write_file(const char *path,
                                             String8_View content);

void write_file_or_exit(const std::string &path, String8_View content);
void write_file_or_exit(const char *path, String8_View content);

// Returns true if the file was different.
Result<bool, Generic_IO_Error> write_file_if_different(const std::string &path,
                                                       String8_View content);
Result<bool, Generic_IO_Error> write_file_if_different(const char *path,
                                                       String8_View content);

// Truncates the file if it exists.
Result<Platform_File, Write_File_IO_Error> open_file_for_writing(
    const char *path);

#if QLJS_HAVE_WINDOWS_H
bool file_ids_equal(const ::FILE_ID_INFO &, const ::FILE_ID_INFO &);
#endif
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
