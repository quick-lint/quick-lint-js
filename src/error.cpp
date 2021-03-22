// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/token.h>

namespace quick_lint_js {
void error_reporter::write_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin, const cli_locator *locator) {
  std::fprintf(stderr, "%s:%d: fatal: token not implemented in %s: %s",
               qljs_file_name, qljs_line, qljs_function_name, to_string(type));
  if (locator) {
    cli_source_position token_position = locator->position(token_begin);
    std::fprintf(stderr, " on line %d column %d", token_position.line_number,
                 token_position.column_number);
  }
  std::fprintf(stderr, "\n");
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
