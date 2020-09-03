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

#include <cstdio>
#include <quick-lint-js/assert.h>

namespace quick_lint_js {
void report_assertion_failure(const char *qljs_file_name, int qljs_line,
                              const char *qljs_function_name,
                              const char *message) {
  std::fprintf(stderr, "%s:%d: internal check failed in %s: %s\n",
               qljs_file_name, qljs_line, qljs_function_name, message);
}
}  // namespace quick_lint_js
