// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class Debug_Parse_Visitor final : public Parse_Visitor_Base {
 public:
  explicit Debug_Parse_Visitor(Output_Stream *output) : output_(output) {}

  void visit_end_of_module() override {
    this->output_->append_copy(u8"end of module\n"_sv);
    this->output_->flush();
  }

  void visit_enter_block_scope() override {
    this->output_->append_copy(u8"entered block scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_with_scope() override {
    this->output_->append_copy(u8"entered with scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_class_construct_scope() override {
    this->output_->append_copy(u8"entered class construct scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_class_scope() override {
    this->output_->append_copy(u8"entered class scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_class_scope_body(
      const std::optional<Identifier> &class_name) override {
    this->output_->append_copy(u8"entered class scope body"_sv);
    if (class_name.has_value()) {
      this->output_->append_copy(u8": "_sv);
      this->output_->append_copy(class_name->normalized_name());
    }
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_enter_conditional_type_scope() override {
    this->output_->append_copy(u8"entered conditional scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_declare_global_scope() override {
    this->output_->append_copy(u8"entered declare global scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_declare_scope() override {
    this->output_->append_copy(u8"entered declare scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_enum_scope() override {
    this->output_->append_copy(u8"entered enum scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_for_scope() override {
    this->output_->append_copy(u8"entered for scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_function_scope() override {
    this->output_->append_copy(u8"entered function scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_function_scope_body() override {
    this->output_->append_copy(u8"entered function scope body\n"_sv);
    this->output_->flush();
  }

  void visit_enter_index_signature_scope() override {
    this->output_->append_copy(u8"entered index signature scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_interface_scope() override {
    this->output_->append_copy(u8"entered interface scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_named_function_scope(Identifier) override {
    this->output_->append_copy(u8"entered named function scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_namespace_scope() override {
    this->output_->append_copy(u8"entered namespace scope\n"_sv);
    this->output_->flush();
  }

  void visit_enter_type_scope() override {
    this->output_->append_copy(u8"entered type alias scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_block_scope() override {
    this->output_->append_copy(u8"exited block scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_with_scope() override {
    this->output_->append_copy(u8"exited with scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_class_construct_scope() override {
    this->output_->append_copy(u8"exited class construct scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_class_scope() override {
    this->output_->append_copy(u8"exited class scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_conditional_type_scope() override {
    this->output_->append_copy(u8"exited conditional scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_declare_global_scope() override {
    this->output_->append_copy(u8"exited declare global scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_declare_scope() override {
    this->output_->append_copy(u8"exited declare scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_enum_scope() override {
    this->output_->append_copy(u8"exited enum scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_for_scope() override {
    this->output_->append_copy(u8"exited for scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_function_scope() override {
    this->output_->append_copy(u8"exited function scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_index_signature_scope() override {
    this->output_->append_copy(u8"exited index signature scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_interface_scope() override {
    this->output_->append_copy(u8"exited interface scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_namespace_scope() override {
    this->output_->append_copy(u8"exited namespace scope\n"_sv);
    this->output_->flush();
  }

  void visit_exit_type_scope() override {
    this->output_->append_copy(u8"exited type alias scope\n"_sv);
    this->output_->flush();
  }

  void visit_keyword_variable_use(Identifier name) override {
    this->output_->append_copy(u8"keyword variable use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_property_declaration(
      const std::optional<Identifier> &name) override {
    this->output_->append_copy(u8"property declaration"_sv);
    if (name.has_value()) {
      this->output_->append_copy(u8": "_sv);
      this->output_->append_copy(name->normalized_name());
    }
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_assignment(Identifier name,
                                 Variable_Assignment_Flags) override {
    this->output_->append_copy(u8"variable assignment: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                  Variable_Declaration_Flags) override {
    this->output_->append_copy(u8"variable declaration: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8" ("_sv);
    this->output_->append_copy(to_string(kind));
    this->output_->append_copy(u8")\n"_sv);
    this->output_->flush();
  }

  void visit_variable_assertion_signature_use(Identifier name) override {
    this->output_->append_copy(u8"variable assertion signature use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_delete_use(
      Identifier name,
      [[maybe_unused]] Source_Code_Span delete_keyword) override {
    this->output_->append_copy(u8"variable delete use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_export_default_use(Identifier name) override {
    this->output_->append_copy(u8"variable export default use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_export_use(Identifier name) override {
    this->output_->append_copy(u8"variable export use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_namespace_use(Identifier name) override {
    this->output_->append_copy(u8"variable namespace use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_type_use(Identifier name) override {
    this->output_->append_copy(u8"variable type use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_type_predicate_use(Identifier parameter_name) override {
    this->output_->append_copy(u8"variable type predicate use: "_sv);
    this->output_->append_copy(parameter_name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_typeof_use(Identifier name) override {
    this->output_->append_copy(u8"variable typeof use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_use(Identifier name) override {
    this->output_->append_copy(u8"variable use: "_sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  Output_Stream *output_;
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
