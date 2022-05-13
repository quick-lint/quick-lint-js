// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_H
#define QUICK_LINT_JS_FILE_H

#if defined(__EMSCRIPTEN__)
// No file I/O on the web.
#else

#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/result.h>
#include <string>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/windows.h>
#endif

namespace quick_lint_js {
struct read_file_io_error {
  std::string path;
  platform_file_io_error io_error;

  bool is_file_not_found_error() const noexcept;

  std::string to_string() const;
  [[noreturn]] void print_and_exit() const;

  friend bool operator==(const read_file_io_error &,
                         const read_file_io_error &) noexcept;
  friend bool operator!=(const read_file_io_error &,
                         const read_file_io_error &) noexcept;
};

struct write_file_io_error {
  std::string path;
  platform_file_io_error io_error;

  std::string to_string() const;
  [[noreturn]] void print_and_exit() const;
};

result<padded_string, read_file_io_error> read_file(const char *path);
result<padded_string, read_file_io_error> read_file(const char *path,
                                                    platform_file_ref);
result<padded_string, platform_file_io_error> read_file(platform_file_ref);
result<padded_string, read_file_io_error> read_stdin(void);

padded_string read_file_or_exit(const char *path);

result<void, write_file_io_error> write_file(const std::string &path,
                                             string8_view content);
result<void, write_file_io_error> write_file(const char *path,
                                             string8_view content);

void write_file_or_exit(const std::string &path, string8_view content);
void write_file_or_exit(const char *path, string8_view content);

// Truncates the file if it exists.
result<platform_file, write_file_io_error> open_file_for_writing(
    const char *path);

#if QLJS_HAVE_WINDOWS_H
bool file_ids_equal(const ::FILE_ID_INFO &, const ::FILE_ID_INFO &) noexcept;
#endif
}

#endif

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
