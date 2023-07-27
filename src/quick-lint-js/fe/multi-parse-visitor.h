// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_MULTI_PARSE_VISITOR_H
#define QUICK_LINT_JS_FE_MULTI_PARSE_VISITOR_H

#include <quick-lint-js/fe/parse-visitor.h>

namespace quick_lint_js {
template <class Visitor1, class Visitor2>
class Multi_Parse_Visitor final : public Parse_Visitor_Base {
 public:
  explicit Multi_Parse_Visitor(Visitor1 *visitor_1, Visitor2 *visitor_2)
      : visitor_1_(visitor_1), visitor_2_(visitor_2) {}

  void visit_end_of_module() override {
    this->visitor_1_->visit_end_of_module();
    this->visitor_2_->visit_end_of_module();
  }

  void visit_enter_block_scope() override {
    this->visitor_1_->visit_enter_block_scope();
    this->visitor_2_->visit_enter_block_scope();
  }

  void visit_enter_with_scope() override {
    this->visitor_1_->visit_enter_with_scope();
    this->visitor_2_->visit_enter_with_scope();
  }

  void visit_enter_class_scope() override {
    this->visitor_1_->visit_enter_class_scope();
    this->visitor_2_->visit_enter_class_scope();
  }

  void visit_enter_class_scope_body(
      const std::optional<Identifier> &class_name) override {
    this->visitor_1_->visit_enter_class_scope_body(class_name);
    this->visitor_2_->visit_enter_class_scope_body(class_name);
  }

  void visit_enter_conditional_type_scope() override {
    this->visitor_1_->visit_enter_conditional_type_scope();
    this->visitor_2_->visit_enter_conditional_type_scope();
  }

  void visit_enter_enum_scope() override {
    this->visitor_1_->visit_enter_enum_scope();
    this->visitor_2_->visit_enter_enum_scope();
  }

  void visit_enter_for_scope() override {
    this->visitor_1_->visit_enter_for_scope();
    this->visitor_2_->visit_enter_for_scope();
  }

  void visit_enter_function_scope() override {
    this->visitor_1_->visit_enter_function_scope();
    this->visitor_2_->visit_enter_function_scope();
  }

  void visit_enter_function_scope_body() override {
    this->visitor_1_->visit_enter_function_scope_body();
    this->visitor_2_->visit_enter_function_scope_body();
  }

  void visit_enter_index_signature_scope() override {
    this->visitor_1_->visit_enter_index_signature_scope();
    this->visitor_2_->visit_enter_index_signature_scope();
  }

  void visit_enter_interface_scope() override {
    this->visitor_1_->visit_enter_interface_scope();
    this->visitor_2_->visit_enter_interface_scope();
  }

  void visit_enter_named_function_scope(Identifier name) override {
    this->visitor_1_->visit_enter_named_function_scope(name);
    this->visitor_2_->visit_enter_named_function_scope(name);
  }

  void visit_enter_namespace_scope() override {
    this->visitor_1_->visit_enter_namespace_scope();
    this->visitor_2_->visit_enter_namespace_scope();
  }

  void visit_enter_type_alias_scope() override {
    this->visitor_1_->visit_enter_type_alias_scope();
    this->visitor_2_->visit_enter_type_alias_scope();
  }

  void visit_exit_block_scope() override {
    this->visitor_1_->visit_exit_block_scope();
    this->visitor_2_->visit_exit_block_scope();
  }

  void visit_exit_with_scope() override {
    this->visitor_1_->visit_exit_with_scope();
    this->visitor_2_->visit_exit_with_scope();
  }

  void visit_exit_class_scope() override {
    this->visitor_1_->visit_exit_class_scope();
    this->visitor_2_->visit_exit_class_scope();
  }

  void visit_exit_conditional_type_scope() override {
    this->visitor_1_->visit_exit_conditional_type_scope();
    this->visitor_2_->visit_exit_conditional_type_scope();
  }

  void visit_exit_enum_scope() override {
    this->visitor_1_->visit_exit_enum_scope();
    this->visitor_2_->visit_exit_enum_scope();
  }

  void visit_exit_for_scope() override {
    this->visitor_1_->visit_exit_for_scope();
    this->visitor_2_->visit_exit_for_scope();
  }

  void visit_exit_function_scope() override {
    this->visitor_1_->visit_exit_function_scope();
    this->visitor_2_->visit_exit_function_scope();
  }

  void visit_exit_index_signature_scope() override {
    this->visitor_1_->visit_exit_index_signature_scope();
    this->visitor_2_->visit_exit_index_signature_scope();
  }

  void visit_exit_interface_scope() override {
    this->visitor_1_->visit_exit_interface_scope();
    this->visitor_2_->visit_exit_interface_scope();
  }

  void visit_exit_namespace_scope() override {
    this->visitor_1_->visit_exit_namespace_scope();
    this->visitor_2_->visit_exit_namespace_scope();
  }

  void visit_exit_type_alias_scope() override {
    this->visitor_1_->visit_exit_type_alias_scope();
    this->visitor_2_->visit_exit_type_alias_scope();
  }

  void visit_keyword_variable_use(Identifier name) override {
    this->visitor_1_->visit_keyword_variable_use(name);
    this->visitor_2_->visit_keyword_variable_use(name);
  }

  void visit_property_declaration(
      const std::optional<Identifier> &name) override {
    this->visitor_1_->visit_property_declaration(name);
    this->visitor_2_->visit_property_declaration(name);
  }

  void visit_variable_assignment(Identifier name) override {
    this->visitor_1_->visit_variable_assignment(name);
    this->visitor_2_->visit_variable_assignment(name);
  }

  void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                  Variable_Declaration_Flags flags) override {
    this->visitor_1_->visit_variable_declaration(name, kind, flags);
    this->visitor_2_->visit_variable_declaration(name, kind, flags);
  }

  void visit_variable_delete_use(Identifier name,
                                 Source_Code_Span delete_keyword) override {
    this->visitor_1_->visit_variable_delete_use(name, delete_keyword);
    this->visitor_2_->visit_variable_delete_use(name, delete_keyword);
  }

  void visit_variable_export_use(Identifier name) override {
    this->visitor_1_->visit_variable_export_use(name);
    this->visitor_2_->visit_variable_export_use(name);
  }

  void visit_variable_namespace_use(Identifier name) override {
    this->visitor_1_->visit_variable_namespace_use(name);
    this->visitor_2_->visit_variable_namespace_use(name);
  }

  void visit_variable_type_predicate_use(Identifier parameter_name) override {
    this->visitor_1_->visit_variable_type_predicate_use(parameter_name);
    this->visitor_2_->visit_variable_type_predicate_use(parameter_name);
  }

  void visit_variable_type_use(Identifier name) override {
    this->visitor_1_->visit_variable_type_use(name);
    this->visitor_2_->visit_variable_type_use(name);
  }

  void visit_variable_typeof_use(Identifier name) override {
    this->visitor_1_->visit_variable_typeof_use(name);
    this->visitor_2_->visit_variable_typeof_use(name);
  }

  void visit_variable_use(Identifier name) override {
    this->visitor_1_->visit_variable_use(name);
    this->visitor_2_->visit_variable_use(name);
  }

 private:
  Visitor1 *visitor_1_;
  Visitor2 *visitor_2_;
};
}

#endif

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
