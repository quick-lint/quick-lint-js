// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_H
#define QUICK_LINT_JS_FILE_H

#if defined(__EMSCRIPTEN__)
// No file I/O on the web.
#else

#include <cstdio>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/program-report.h>
#include <quick-lint-js/result.h>
#include <string>

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

result<padded_string, read_file_io_error> read_file(const char *path);
result<padded_string, read_file_io_error> read_file(const char *path,
                                                    platform_file_ref);
result<padded_string, platform_file_io_error> read_file(platform_file_ref);
result<padded_string, read_file_io_error> read_stdin(void);

padded_string read_file_or_exit(const char *path);

void write_file(const std::string &path, string8_view content);
void write_file(const char *path, string8_view content);

template <class Result>
auto exit_on_read_file_error_handlers() {
  return make_read_file_error_handlers(
      [](const std::string &message) -> Result {
        ::std:fprintf("error: %s\n", message.c_str());
        std::exit(1);
      });
}
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
