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

#ifndef QUICKLINT_JS_LINT_H
#define QUICKLINT_JS_LINT_H

#include <algorithm>
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/parse.h>
#include <string>
#include <vector>

namespace quicklint_js {
class linter {
 public:
  explicit linter(error_reporter *error_reporter) noexcept
      : error_reporter_(error_reporter) {}

  void visit_enter_function_scope() {}

  void visit_exit_function_scope() {}

  void visit_variable_declaration(identifier name, variable_kind) {
    this->declared_variables_.emplace_back(name.string_view());
  }

  void visit_variable_use(identifier name) {
    bool variable_is_declared =
        std::find(this->declared_variables_.begin(),
                  this->declared_variables_.end(),
                  name.string_view()) != this->declared_variables_.end();
    if (!variable_is_declared) {
      this->error_reporter_->report_error_variable_used_before_declaration(
          name);
    }
  }

 private:
  std::vector<std::string> declared_variables_;
  error_reporter *error_reporter_;
};
}  // namespace quicklint_js

#endif
