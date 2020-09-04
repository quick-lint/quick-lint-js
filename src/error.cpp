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

#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
void error_reporter::write_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin, const locator *locator,
    std::ostream &out) {
  out << qljs_file_name << ":" << qljs_line
      << ": fatal: token not implemented in " << qljs_function_name << ": "
      << type;
  if (locator) {
    source_position token_position = locator->position(token_begin);
    out << " on line " << token_position.line_number << " column "
        << token_position.column_number;
  }
  out << '\n';
}
}
