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
#include <utility>
#include <variant>
#include <vector>

namespace quick_lint_js {
class buffering_visitor {
 public:
  template <class Visitor>
  void move_into(Visitor &target) {
    struct variant_visitor {
      explicit variant_visitor(Visitor &target) noexcept : target(target) {}

      void operator()(visited_variable_declaration &visit) {
        this->target.visit_variable_declaration(visit.name, visit.kind);
      }

      void operator()(visited_variable_use &visit) {
        this->target.visit_variable_use(visit.name);
      }

      Visitor &target;
    };

    for (auto &visit : this->visits_) {
      std::visit(variant_visitor(target), visit);
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
    this->visits_.emplace_back(visited_variable_declaration{name, kind});
  }

  void visit_variable_use(identifier name) {
    this->visits_.emplace_back(visited_variable_use{name});
  }

 private:
  struct visited_variable_declaration {
    identifier name;
    variable_kind kind;
  };
  struct visited_variable_use {
    identifier name;
  };
  std::vector<std::variant<visited_variable_use, visited_variable_declaration>>
      visits_;
};
}  // namespace quick_lint_js

#endif
