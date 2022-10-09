// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>

namespace quick_lint_js {
buffering_visitor::buffering_visitor(memory_resource *memory)
    : visits_(memory) {}

void buffering_visitor::move_into(parse_visitor_base &target) {
  this->copy_into(target);
}

void buffering_visitor::copy_into(parse_visitor_base &target) const {
  this->visits_.for_each([&](const visit &v) {
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
    case visit_kind::enter_class_scope_body_with_name:
      target.visit_enter_class_scope_body(v.name);
      break;
    case visit_kind::enter_class_scope_body_without_name:
      target.visit_enter_class_scope_body(std::nullopt);
      break;
    case visit_kind::enter_enum_scope:
      target.visit_enter_enum_scope();
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
    case visit_kind::enter_index_signature_scope:
      target.visit_enter_index_signature_scope();
      break;
    case visit_kind::enter_interface_scope:
      target.visit_enter_interface_scope();
      break;
    case visit_kind::enter_named_function_scope:
      target.visit_enter_named_function_scope(v.name);
      break;
    case visit_kind::enter_namespace_scope:
      target.visit_enter_namespace_scope();
      break;
    case visit_kind::enter_type_alias_scope:
      target.visit_enter_type_alias_scope();
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
    case visit_kind::exit_enum_scope:
      target.visit_exit_enum_scope();
      break;
    case visit_kind::exit_for_scope:
      target.visit_exit_for_scope();
      break;
    case visit_kind::exit_function_scope:
      target.visit_exit_function_scope();
      break;
    case visit_kind::exit_index_signature_scope:
      target.visit_exit_index_signature_scope();
      break;
    case visit_kind::exit_interface_scope:
      target.visit_exit_interface_scope();
      break;
    case visit_kind::exit_namespace_scope:
      target.visit_exit_namespace_scope();
      break;
    case visit_kind::exit_type_alias_scope:
      target.visit_exit_type_alias_scope();
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
    case visit_kind::variable_type_predicate_use:
      target.visit_variable_type_predicate_use(v.name);
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
  });
}

void buffering_visitor::visit_end_of_module() {
  this->add(visit_kind::end_of_module);
}

void buffering_visitor::visit_enter_block_scope() {
  this->add(visit_kind::enter_block_scope);
}

void buffering_visitor::visit_enter_with_scope() {
  this->add(visit_kind::enter_with_scope);
}

void buffering_visitor::visit_enter_class_scope() {
  this->add(visit_kind::enter_class_scope);
}

void buffering_visitor::visit_enter_class_scope_body(
    const std::optional<identifier> &class_name) {
  if (class_name.has_value()) {
    this->add(*class_name, visit_kind::enter_class_scope_body_with_name);
  } else {
    this->add(visit_kind::enter_class_scope_body_without_name);
  }
}

void buffering_visitor::visit_enter_enum_scope() {
  this->add(visit_kind::enter_enum_scope);
}

void buffering_visitor::visit_enter_for_scope() {
  this->add(visit_kind::enter_for_scope);
}

void buffering_visitor::visit_enter_function_scope() {
  this->add(visit_kind::enter_function_scope);
}

void buffering_visitor::visit_enter_function_scope_body() {
  this->add(visit_kind::enter_function_scope_body);
}

void buffering_visitor::visit_enter_index_signature_scope() {
  this->add(visit_kind::enter_index_signature_scope);
}

void buffering_visitor::visit_enter_interface_scope() {
  this->add(visit_kind::enter_interface_scope);
}

void buffering_visitor::visit_enter_namespace_scope() {
  this->add(visit_kind::enter_namespace_scope);
}

void buffering_visitor::visit_enter_type_alias_scope() {
  this->add(visit_kind::enter_type_alias_scope);
}

void buffering_visitor::visit_enter_named_function_scope(identifier name) {
  this->add(name, visit_kind::enter_named_function_scope);
}

void buffering_visitor::visit_exit_block_scope() {
  this->add(visit_kind::exit_block_scope);
}

void buffering_visitor::visit_exit_with_scope() {
  this->add(visit_kind::exit_with_scope);
}

void buffering_visitor::visit_exit_class_scope() {
  this->add(visit_kind::exit_class_scope);
}

void buffering_visitor::visit_exit_enum_scope() {
  this->add(visit_kind::exit_enum_scope);
}

void buffering_visitor::visit_exit_for_scope() {
  this->add(visit_kind::exit_for_scope);
}

void buffering_visitor::visit_exit_function_scope() {
  this->add(visit_kind::exit_function_scope);
}

void buffering_visitor::visit_exit_index_signature_scope() {
  this->add(visit_kind::exit_index_signature_scope);
}

void buffering_visitor::visit_exit_namespace_scope() {
  this->add(visit_kind::exit_namespace_scope);
}

void buffering_visitor::visit_exit_type_alias_scope() {
  this->add(visit_kind::exit_type_alias_scope);
}

void buffering_visitor::visit_exit_interface_scope() {
  this->add(visit_kind::exit_interface_scope);
}

void buffering_visitor::visit_keyword_variable_use(identifier name) {
  this->add(name, visit_kind::keyword_variable_use);
}

void buffering_visitor::visit_property_declaration(
    const std::optional<identifier> &name) {
  if (name.has_value()) {
    this->add(*name, visit_kind::property_declaration_with_name);
  } else {
    this->add(visit_kind::property_declaration_without_name);
  }
}

void buffering_visitor::visit_variable_assignment(identifier name) {
  this->add(name, visit_kind::variable_assignment);
}

void buffering_visitor::visit_variable_declaration(
    identifier name, variable_kind kind, variable_init_kind init_kind) {
  this->visits_.emplace_back(visit_kind::variable_declaration, name, kind,
                             init_kind);
}

void buffering_visitor::visit_variable_delete_use(
    identifier name, source_code_span delete_keyword) {
  this->visits_.emplace_back(visit_kind::variable_delete_use, name,
                             delete_keyword);
}

void buffering_visitor::visit_variable_export_use(identifier name) {
  this->add(name, visit_kind::variable_export_use);
}

void buffering_visitor::visit_variable_namespace_use(identifier name) {
  this->add(name, visit_kind::variable_namespace_use);
}

void buffering_visitor::visit_variable_type_predicate_use(
    identifier parameter_name) {
  this->add(parameter_name, visit_kind::variable_type_predicate_use);
}

void buffering_visitor::visit_variable_type_use(identifier name) {
  this->add(name, visit_kind::variable_type_use);
}

void buffering_visitor::visit_variable_typeof_use(identifier name) {
  this->add(name, visit_kind::variable_typeof_use);
}

void buffering_visitor::visit_variable_use(identifier name) {
  this->add(name, visit_kind::variable_use);
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
