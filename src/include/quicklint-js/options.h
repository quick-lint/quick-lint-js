// quicklint-js finds bugs in JavaScript programs.
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

#ifndef QUICKLINT_JS_OPTIONS_H
#define QUICKLINT_JS_OPTIONS_H

#include <vector>

namespace quicklint_js {
struct options {
  bool print_parser_visits = false;
  std::vector<const char *> files_to_lint;

  std::vector<const char *> error_unrecognized_options;
};

options parse_options(int argc, char **argv);
}  // namespace quicklint_js

#endif
