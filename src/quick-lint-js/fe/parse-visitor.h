// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <optional>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>

namespace quick_lint_js {
// TODO(strager): Rename this class to Parse_Visitor.
class Parse_Visitor_Base {
 public:
  Parse_Visitor_Base() = default;

  Parse_Visitor_Base(const Parse_Visitor_Base &) = default;
  Parse_Visitor_Base &operator=(const Parse_Visitor_Base &) = default;

  Parse_Visitor_Base(Parse_Visitor_Base &&) = default;
  Parse_Visitor_Base &operator=(Parse_Visitor_Base &&) = default;

  virtual ~Parse_Visitor_Base() = default;

  virtual void visit_enter_block_scope() = 0;
  virtual void visit_enter_with_scope() = 0;
  virtual void visit_enter_class_scope() = 0;
  virtual void visit_enter_class_scope_body(
      const std::optional<Identifier> &class_name) = 0;
  virtual void visit_enter_conditional_type_scope() = 0;
  virtual void visit_enter_enum_scope() = 0;
  virtual void visit_enter_for_scope() = 0;
  virtual void visit_enter_function_scope() = 0;
  virtual void visit_enter_function_scope_body() = 0;
  virtual void visit_enter_index_signature_scope() = 0;
  virtual void visit_enter_interface_scope() = 0;
  virtual void visit_enter_named_function_scope(Identifier) = 0;
  virtual void visit_enter_namespace_scope() = 0;
  virtual void visit_enter_type_alias_scope() = 0;
  virtual void visit_exit_block_scope() = 0;
  virtual void visit_exit_with_scope() = 0;
  virtual void visit_exit_class_scope() = 0;
  virtual void visit_exit_conditional_type_scope() = 0;
  virtual void visit_exit_enum_scope() = 0;
  virtual void visit_exit_for_scope() = 0;
  virtual void visit_exit_function_scope() = 0;
  virtual void visit_exit_index_signature_scope() = 0;
  virtual void visit_exit_interface_scope() = 0;
  virtual void visit_exit_namespace_scope() = 0;
  virtual void visit_exit_type_alias_scope() = 0;
  virtual void visit_keyword_variable_use(Identifier name) = 0;
  virtual void visit_property_declaration(
      const std::optional<Identifier> &) = 0;
  virtual void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                          Variable_Declaration_Flags flags) = 0;
  virtual void visit_variable_assignment(Identifier name) = 0;
  virtual void visit_variable_delete_use(Identifier name,
                                         Source_Code_Span delete_keyword) = 0;
  virtual void visit_variable_export_use(Identifier name) = 0;
  virtual void visit_variable_namespace_use(Identifier name) = 0;
  virtual void visit_variable_type_predicate_use(Identifier parameter_name) = 0;
  virtual void visit_variable_type_use(Identifier name) = 0;
  virtual void visit_variable_typeof_use(Identifier name) = 0;
  virtual void visit_variable_use(Identifier name) = 0;
  virtual void visit_end_of_module() = 0;
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
