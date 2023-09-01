// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse-visitor.h>

namespace quick_lint_js {
class Null_Visitor final : public Parse_Visitor_Base {
 public:
  void visit_end_of_module() override {}
  void visit_enter_block_scope() override {}
  void visit_enter_with_scope() override {}
  void visit_enter_class_construct_scope() override {}
  void visit_enter_class_scope() override {}
  void visit_enter_class_scope_body(const std::optional<Identifier>&) override {
  }
  void visit_enter_conditional_type_scope() override {}
  void visit_enter_declare_scope() override {}
  void visit_enter_enum_scope() override {}
  void visit_enter_for_scope() override {}
  void visit_enter_function_scope() override {}
  void visit_enter_function_scope_body() override {}
  void visit_enter_index_signature_scope() override {}
  void visit_enter_interface_scope() override {}
  void visit_enter_named_function_scope(Identifier) override {}
  void visit_enter_namespace_scope() override {}
  void visit_enter_type_alias_scope() override {}
  void visit_exit_block_scope() override {}
  void visit_exit_with_scope() override {}
  void visit_exit_class_construct_scope() override {}
  void visit_exit_class_scope() override {}
  void visit_exit_conditional_type_scope() override {}
  void visit_exit_declare_scope() override {}
  void visit_exit_enum_scope() override {}
  void visit_exit_for_scope() override {}
  void visit_exit_function_scope() override {}
  void visit_exit_index_signature_scope() override {}
  void visit_exit_interface_scope() override {}
  void visit_exit_namespace_scope() override {}
  void visit_exit_type_alias_scope() override {}
  void visit_keyword_variable_use(Identifier) override {}
  void visit_property_declaration(const std::optional<Identifier>&) override {}
  void visit_variable_assignment(Identifier) override {}
  void visit_variable_declaration(Identifier, Variable_Kind,
                                  Variable_Declaration_Flags) override {}
  void visit_variable_delete_use(Identifier, Source_Code_Span) override {}
  void visit_variable_export_use(Identifier) override {}
  void visit_variable_namespace_use(Identifier) override {}
  void visit_variable_type_predicate_use(Identifier) override {}
  void visit_variable_type_use(Identifier) override {}
  void visit_variable_typeof_use(Identifier) override {}
  void visit_variable_use(Identifier) override {}

  static Null_Visitor instance;
};
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
