// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_DOCUMENTATION_H
#define QUICK_LINT_JS_ERROR_DOCUMENTATION_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
struct error_documentation {
  std::string file_path;
  std::string title_error_code;
  string8 title_error_description;
  std::vector<padded_string> code_blocks;

  std::string_view file_path_error_code() const;
};

error_documentation parse_error_documentation(std::string&& file_path,
                                              string8_view markdown);
error_documentation parse_error_documentation_file(std::string&& file_path);
}

#endif

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
