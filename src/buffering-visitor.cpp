// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/parse-visitor.h>

namespace quick_lint_js {
void buffering_visitor::move_into(parse_visitor_base &target) {
  this->copy_into(target);
}

void buffering_visitor::copy_into(parse_visitor_base &target) const {
  for (auto &v : this->visits_) {
    switch (v.kind) {
    case visit_kind::end_of_module:
      target.visit_end_of_module();
      break;
    case visit_kind::enter_block_scope:
      target.visit_enter_block_scope();
      break;
    case visit_kind::enter_with_scope:
      target.visit_enter_with_scope();
      break;
    case visit_kind::enter_class_scope:
      target.visit_enter_class_scope();
      break;
    case visit_kind::enter_for_scope:
      target.visit_enter_for_scope();
      break;
    case visit_kind::enter_function_scope:
      target.visit_enter_function_scope();
      break;
    case visit_kind::enter_function_scope_body:
      target.visit_enter_function_scope_body();
      break;
    case visit_kind::enter_named_function_scope:
      target.visit_enter_named_function_scope(v.name);
      break;
    case visit_kind::exit_block_scope:
      target.visit_exit_block_scope();
      break;
    case visit_kind::exit_with_scope:
      target.visit_exit_with_scope();
      break;
    case visit_kind::exit_class_scope:
      target.visit_exit_class_scope();
      break;
    case visit_kind::exit_for_scope:
      target.visit_exit_for_scope();
      break;
    case visit_kind::exit_function_scope:
      target.visit_exit_function_scope();
      break;
    case visit_kind::keyword_variable_use:
      target.visit_keyword_variable_use(v.name);
      break;
    case visit_kind::property_declaration_with_name:
      target.visit_property_declaration(v.name);
      break;
    case visit_kind::property_declaration_without_name:
      target.visit_property_declaration(std::nullopt);
      break;
    case visit_kind::variable_assignment:
      target.visit_variable_assignment(v.name);
      break;
    case visit_kind::variable_delete_use:
      target.visit_variable_delete_use(v.name, v.extra_span);
      break;
    case visit_kind::variable_export_use:
      target.visit_variable_export_use(v.name);
      break;
    case visit_kind::variable_namespace_use:
      target.visit_variable_namespace_use(v.name);
      break;
    case visit_kind::variable_type_use:
      target.visit_variable_type_use(v.name);
      break;
    case visit_kind::variable_typeof_use:
      target.visit_variable_typeof_use(v.name);
      break;
    case visit_kind::variable_use:
      target.visit_variable_use(v.name);
      break;
    case visit_kind::variable_declaration:
      target.visit_variable_declaration(v.name, v.var_decl.var_kind,
                                        v.var_decl.var_init_kind);
      break;
    }
  }
}
}

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
