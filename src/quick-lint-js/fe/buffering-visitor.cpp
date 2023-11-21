// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>

namespace quick_lint_js {
Buffering_Visitor::Buffering_Visitor(Memory_Resource *memory)
    : visits_(memory) {}

void Buffering_Visitor::move_into(Parse_Visitor_Base &target) {
  this->copy_into(target);
  this->visits_.clear();
}

void Buffering_Visitor::copy_into(Parse_Visitor_Base &target) const {
  this->visits_.for_each([&](const Visit &v) {
    switch (v.kind) {
    case Visit_Kind::end_of_module:
      target.visit_end_of_module();
      break;
    case Visit_Kind::enter_block_scope:
      target.visit_enter_block_scope();
      break;
    case Visit_Kind::enter_with_scope:
      target.visit_enter_with_scope();
      break;
    case Visit_Kind::enter_class_construct_scope:
      target.visit_enter_class_construct_scope();
      break;
    case Visit_Kind::enter_class_scope:
      target.visit_enter_class_scope();
      break;
    case Visit_Kind::enter_class_scope_body_with_name:
      target.visit_enter_class_scope_body(v.name);
      break;
    case Visit_Kind::enter_class_scope_body_without_name:
      target.visit_enter_class_scope_body(std::nullopt);
      break;
    case Visit_Kind::enter_conditional_type_scope:
      target.visit_enter_conditional_type_scope();
      break;
    case Visit_Kind::enter_declare_scope:
      target.visit_enter_declare_scope();
      break;
    case Visit_Kind::enter_enum_scope:
      target.visit_enter_enum_scope();
      break;
    case Visit_Kind::enter_for_scope:
      target.visit_enter_for_scope();
      break;
    case Visit_Kind::enter_function_scope:
      target.visit_enter_function_scope();
      break;
    case Visit_Kind::enter_function_scope_body:
      target.visit_enter_function_scope_body();
      break;
    case Visit_Kind::enter_index_signature_scope:
      target.visit_enter_index_signature_scope();
      break;
    case Visit_Kind::enter_interface_scope:
      target.visit_enter_interface_scope();
      break;
    case Visit_Kind::enter_named_function_scope:
      target.visit_enter_named_function_scope(v.name);
      break;
    case Visit_Kind::enter_namespace_scope:
      target.visit_enter_namespace_scope();
      break;
    case Visit_Kind::enter_type_alias_scope:
      target.visit_enter_type_alias_scope();
      break;
    case Visit_Kind::exit_block_scope:
      target.visit_exit_block_scope();
      break;
    case Visit_Kind::exit_with_scope:
      target.visit_exit_with_scope();
      break;
    case Visit_Kind::exit_class_construct_scope:
      target.visit_exit_class_construct_scope();
      break;
    case Visit_Kind::exit_class_scope:
      target.visit_exit_class_scope();
      break;
    case Visit_Kind::exit_conditional_type_scope:
      target.visit_exit_conditional_type_scope();
      break;
    case Visit_Kind::exit_declare_scope:
      target.visit_exit_declare_scope();
      break;
    case Visit_Kind::exit_enum_scope:
      target.visit_exit_enum_scope();
      break;
    case Visit_Kind::exit_for_scope:
      target.visit_exit_for_scope();
      break;
    case Visit_Kind::exit_function_scope:
      target.visit_exit_function_scope();
      break;
    case Visit_Kind::exit_index_signature_scope:
      target.visit_exit_index_signature_scope();
      break;
    case Visit_Kind::exit_interface_scope:
      target.visit_exit_interface_scope();
      break;
    case Visit_Kind::exit_namespace_scope:
      target.visit_exit_namespace_scope();
      break;
    case Visit_Kind::exit_type_alias_scope:
      target.visit_exit_type_alias_scope();
      break;
    case Visit_Kind::keyword_variable_use:
      target.visit_keyword_variable_use(v.name);
      break;
    case Visit_Kind::property_declaration_with_name:
      target.visit_property_declaration(v.name);
      break;
    case Visit_Kind::property_declaration_without_name:
      target.visit_property_declaration(std::nullopt);
      break;
    case Visit_Kind::variable_assignment:
      target.visit_variable_assignment(v.name);
      break;
    case Visit_Kind::variable_assertion_signature_use:
      target.visit_variable_assertion_signature_use(v.name);
      break;
    case Visit_Kind::variable_delete_use:
      target.visit_variable_delete_use(v.name, v.extra_span);
      break;
    case Visit_Kind::variable_export_default_use:
      target.visit_variable_export_default_use(v.name);
      break;
    case Visit_Kind::variable_export_use:
      target.visit_variable_export_use(v.name);
      break;
    case Visit_Kind::variable_namespace_use:
      target.visit_variable_namespace_use(v.name);
      break;
    case Visit_Kind::variable_type_predicate_use:
      target.visit_variable_type_predicate_use(v.name);
      break;
    case Visit_Kind::variable_type_use:
      target.visit_variable_type_use(v.name);
      break;
    case Visit_Kind::variable_typeof_use:
      target.visit_variable_typeof_use(v.name);
      break;
    case Visit_Kind::variable_use:
      target.visit_variable_use(v.name);
      break;
    case Visit_Kind::variable_declaration:
      target.visit_variable_declaration(v.name, v.var_decl.var_kind,
                                        v.var_decl.flags);
      break;
    }
  });
}

void Buffering_Visitor::visit_end_of_module() {
  this->add(Visit_Kind::end_of_module);
}

void Buffering_Visitor::visit_enter_block_scope() {
  this->add(Visit_Kind::enter_block_scope);
}

void Buffering_Visitor::visit_enter_with_scope() {
  this->add(Visit_Kind::enter_with_scope);
}

void Buffering_Visitor::visit_enter_class_construct_scope() {
  this->add(Visit_Kind::enter_class_construct_scope);
}

void Buffering_Visitor::visit_enter_class_scope() {
  this->add(Visit_Kind::enter_class_scope);
}

void Buffering_Visitor::visit_enter_class_scope_body(
    const std::optional<Identifier> &class_name) {
  if (class_name.has_value()) {
    this->add(*class_name, Visit_Kind::enter_class_scope_body_with_name);
  } else {
    this->add(Visit_Kind::enter_class_scope_body_without_name);
  }
}

void Buffering_Visitor::visit_enter_conditional_type_scope() {
  this->add(Visit_Kind::enter_conditional_type_scope);
}

void Buffering_Visitor::visit_enter_declare_scope() {
  this->add(Visit_Kind::enter_declare_scope);
}

void Buffering_Visitor::visit_enter_enum_scope() {
  this->add(Visit_Kind::enter_enum_scope);
}

void Buffering_Visitor::visit_enter_for_scope() {
  this->add(Visit_Kind::enter_for_scope);
}

void Buffering_Visitor::visit_enter_function_scope() {
  this->add(Visit_Kind::enter_function_scope);
}

void Buffering_Visitor::visit_enter_function_scope_body() {
  this->add(Visit_Kind::enter_function_scope_body);
}

void Buffering_Visitor::visit_enter_index_signature_scope() {
  this->add(Visit_Kind::enter_index_signature_scope);
}

void Buffering_Visitor::visit_enter_interface_scope() {
  this->add(Visit_Kind::enter_interface_scope);
}

void Buffering_Visitor::visit_enter_namespace_scope() {
  this->add(Visit_Kind::enter_namespace_scope);
}

void Buffering_Visitor::visit_enter_type_alias_scope() {
  this->add(Visit_Kind::enter_type_alias_scope);
}

void Buffering_Visitor::visit_enter_named_function_scope(Identifier name) {
  this->add(name, Visit_Kind::enter_named_function_scope);
}

void Buffering_Visitor::visit_exit_block_scope() {
  this->add(Visit_Kind::exit_block_scope);
}

void Buffering_Visitor::visit_exit_with_scope() {
  this->add(Visit_Kind::exit_with_scope);
}

void Buffering_Visitor::visit_exit_class_construct_scope() {
  this->add(Visit_Kind::exit_class_construct_scope);
}

void Buffering_Visitor::visit_exit_class_scope() {
  this->add(Visit_Kind::exit_class_scope);
}

void Buffering_Visitor::visit_exit_conditional_type_scope() {
  this->add(Visit_Kind::exit_conditional_type_scope);
}

void Buffering_Visitor::visit_exit_declare_scope() {
  this->add(Visit_Kind::exit_declare_scope);
}

void Buffering_Visitor::visit_exit_enum_scope() {
  this->add(Visit_Kind::exit_enum_scope);
}

void Buffering_Visitor::visit_exit_for_scope() {
  this->add(Visit_Kind::exit_for_scope);
}

void Buffering_Visitor::visit_exit_function_scope() {
  this->add(Visit_Kind::exit_function_scope);
}

void Buffering_Visitor::visit_exit_index_signature_scope() {
  this->add(Visit_Kind::exit_index_signature_scope);
}

void Buffering_Visitor::visit_exit_namespace_scope() {
  this->add(Visit_Kind::exit_namespace_scope);
}

void Buffering_Visitor::visit_exit_type_alias_scope() {
  this->add(Visit_Kind::exit_type_alias_scope);
}

void Buffering_Visitor::visit_exit_interface_scope() {
  this->add(Visit_Kind::exit_interface_scope);
}

void Buffering_Visitor::visit_keyword_variable_use(Identifier name) {
  this->add(name, Visit_Kind::keyword_variable_use);
}

void Buffering_Visitor::visit_property_declaration(
    const std::optional<Identifier> &name) {
  if (name.has_value()) {
    this->add(*name, Visit_Kind::property_declaration_with_name);
  } else {
    this->add(Visit_Kind::property_declaration_without_name);
  }
}

void Buffering_Visitor::visit_variable_assignment(Identifier name) {
  this->add(name, Visit_Kind::variable_assignment);
}

void Buffering_Visitor::visit_variable_declaration(
    Identifier name, Variable_Kind kind, Variable_Declaration_Flags flags) {
  this->visits_.emplace_back(Visit_Kind::variable_declaration, name, kind,
                             flags);
}

void Buffering_Visitor::visit_variable_assertion_signature_use(
    Identifier name) {
  this->add(name, Visit_Kind::variable_assertion_signature_use);
}

void Buffering_Visitor::visit_variable_delete_use(
    Identifier name, Source_Code_Span delete_keyword) {
  this->visits_.emplace_back(Visit_Kind::variable_delete_use, name,
                             delete_keyword);
}

void Buffering_Visitor::visit_variable_export_default_use(Identifier name) {
  this->add(name, Visit_Kind::variable_export_default_use);
}

void Buffering_Visitor::visit_variable_export_use(Identifier name) {
  this->add(name, Visit_Kind::variable_export_use);
}

void Buffering_Visitor::visit_variable_namespace_use(Identifier name) {
  this->add(name, Visit_Kind::variable_namespace_use);
}

void Buffering_Visitor::visit_variable_type_predicate_use(
    Identifier parameter_name) {
  this->add(parameter_name, Visit_Kind::variable_type_predicate_use);
}

void Buffering_Visitor::visit_variable_type_use(Identifier name) {
  this->add(name, Visit_Kind::variable_type_use);
}

void Buffering_Visitor::visit_variable_typeof_use(Identifier name) {
  this->add(name, Visit_Kind::variable_typeof_use);
}

void Buffering_Visitor::visit_variable_use(Identifier name) {
  this->add(name, Visit_Kind::variable_use);
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
