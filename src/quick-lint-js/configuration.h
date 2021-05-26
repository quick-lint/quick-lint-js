// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_H
#define QUICK_LINT_JS_CONFIGURATION_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>
#include <vector>

namespace quick_lint_js {
class configuration {
 public:
  const global_declared_variable_set& globals() noexcept;

  void reset_global_groups();
  bool add_global_group(string8_view group_name);

  global_declared_variable* add_global_variable(string8_view name);
  void remove_global_variable(string8_view name);

 private:
  bool should_remove_global_variable(string8_view name);

  global_declared_variable_set globals_;
  std::vector<string8> globals_to_remove_;
  bool add_global_group_node_js_ = true;
  bool add_global_group_ecmascript_ = true;
};
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
