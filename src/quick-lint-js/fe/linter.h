// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_LINTER_H
#define QUICK_LINT_JS_FE_LINTER_H

namespace quick_lint_js {
class diag_reporter;
class global_declared_variable_set;
class padded_string_view;

// TODO(#465): Accept parser options from quick-lint-js.config or CLI options.
struct linter_options {
  // If true, parse and lint JSX language extensions:
  // https://facebook.github.io/jsx/
  bool jsx = true;

  // If true, parse and lint TypeScript instead of JavaScript.
  bool typescript = false;

  // If true, print a human-readable representation of parser visits to stderr.
  bool print_parser_visits = false;
};

void parse_and_lint(padded_string_view code, diag_reporter&,
                    const global_declared_variable_set&, linter_options);
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
