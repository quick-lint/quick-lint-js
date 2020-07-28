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

#ifndef QUICK_LINT_JS_BUFFERING_VISITOR_H
#define QUICK_LINT_JS_BUFFERING_VISITOR_H

#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <vector>

namespace quick_lint_js {
class buffering_visitor {
 public:
  template <class Visitor>
  void move_into(Visitor &target) {
    for (const visited_variable_declaration &visit :
         this->visited_variable_declarations_) {
      target.visit_variable_declaration(visit.name, visit.kind);
    }
  }

  void visit_end_of_module() {}

  void visit_enter_block_scope() {}

  void visit_enter_class_scope() {}

  void visit_enter_function_scope() {}

  void visit_exit_block_scope() {}

  void visit_exit_class_scope() {}

  void visit_exit_function_scope() {}

  void visit_property_declaration(identifier) {}

  void visit_variable_assignment(identifier) {}

  void visit_variable_declaration(identifier name, variable_kind kind) {
    this->visited_variable_declarations_.emplace_back(
        visited_variable_declaration{name, kind});
  }

  void visit_variable_use(identifier) {}

 private:
  struct visited_variable_declaration {
    identifier name;
    variable_kind kind;
  };
  std::vector<visited_variable_declaration> visited_variable_declarations_;
};
}  // namespace quick_lint_js

#endif
