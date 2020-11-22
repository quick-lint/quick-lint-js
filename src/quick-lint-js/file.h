// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_FILE_H
#define QUICK_LINT_JS_FILE_H

#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/padded-string.h>
#include <string>

namespace quick_lint_js {
struct read_file_result {
  padded_string content;
  std::string error;

  bool ok() const noexcept { return this->error.empty(); }
  void exit_if_not_ok() const;

  static read_file_result failure(const std::string &error);
};

read_file_result read_file(const char *path);

read_file_result read_file(const char *path, platform_file_ref);
}

#endif
