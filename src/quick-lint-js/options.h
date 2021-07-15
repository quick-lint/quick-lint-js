// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_OPTIONS_H
#define QUICK_LINT_JS_OPTIONS_H

#include <iosfwd>
#include <optional>
#include <quick-lint-js/error-list.h>
#include <vector>

namespace quick_lint_js {
enum class output_format {
  default_format,
  gnu_like,
  vim_qflist_json,
  emacs_lisp,
};

struct file_to_lint {
  const char *path;
  const char *config_file = nullptr;
  bool is_stdin = false;
  std::optional<int> vim_bufnr;
};

struct options {
  bool help = false;
  bool version = false;
  bool print_parser_visits = false;
  bool lsp_server = false;
  quick_lint_js::output_format output_format =
      quick_lint_js::output_format::default_format;
  std::vector<file_to_lint> files_to_lint;
  compiled_error_list exit_fail_on;

  std::vector<const char *> error_unrecognized_options;
  bool has_multiple_stdin = false;
  bool has_config_file = false;

  bool dump_errors(std::ostream &) const;
};

options parse_options(int argc, char **argv);
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
