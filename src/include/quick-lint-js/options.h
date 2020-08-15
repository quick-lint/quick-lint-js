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

#ifndef QUICK_LINT_JS_OPTIONS_H
#define QUICK_LINT_JS_OPTIONS_H

#include <vector>

namespace quick_lint_js {
enum class output_format {
  gnu_like,
  vim_qflist_json,
};

struct file_to_lint {
  explicit file_to_lint(const char *path) noexcept : path(path) {}

  const char *path;
};

struct options {
  bool print_parser_visits = false;
  quick_lint_js::output_format output_format =
      quick_lint_js::output_format::gnu_like;
  std::vector<file_to_lint> files_to_lint;

  std::vector<const char *> error_unrecognized_options;
};

options parse_options(int argc, char **argv);
}  // namespace quick_lint_js

#endif
