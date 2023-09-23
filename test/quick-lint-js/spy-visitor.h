// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iosfwd>
#include <optional>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
struct Visited_Variable_Declaration {
  String8 name;
  Variable_Kind kind;
  Variable_Declaration_Flags flags;

  bool operator==(const Visited_Variable_Declaration &other) const {
    return this->name == other.name && this->kind == other.kind &&
           this->flags == other.flags;
  }

  bool operator!=(const Visited_Variable_Declaration &other) const {
    return !(*this == other);
  }
};

// An function/method parameter. Not an arrow function parameter.
inline Visited_Variable_Declaration arrow_param_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_arrow_parameter,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration catch_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_catch,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration class_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_class,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'const' with an initializer.
// Example: const x = null;
inline Visited_Variable_Declaration const_init_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals};
}

// A variable declared with 'const' without an initializer.
// Example: for (const x of []) {}
inline Visited_Variable_Declaration const_noinit_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_const,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'const' with an initializer in the head of a 'for'
// loop.
// Example: for (const length = xs.length; i < length; ++i);
inline Visited_Variable_Declaration const_init_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_const,
      Variable_Declaration_Flags::inside_for_loop_head_initialized_with_equals};
}

// A variable declared with 'const' without an initializer in the head of a
// 'for' loop.
// Example: for (const x of xs);
inline Visited_Variable_Declaration const_noinit_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_const,
      Variable_Declaration_Flags::inside_for_loop_head};
}

inline Visited_Variable_Declaration enum_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_enum,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration function_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_function,
                                      Variable_Declaration_Flags::none};
}

// An function/method parameter. Not an arrow function parameter.
inline Visited_Variable_Declaration func_param_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_function_parameter,
                                      Variable_Declaration_Flags::none};
}

// An function parameter in a TypeScript type.
inline Visited_Variable_Declaration func_type_param_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_function_type_parameter,
                                      Variable_Declaration_Flags::none};
}

// A TypeScript namespace or module alias. Example: import A = B;
inline Visited_Variable_Declaration import_alias_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_import_alias,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration import_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_import,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration import_type_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_import_type,
                                      Variable_Declaration_Flags::none};
}

// A parameter in a TypeScript index signature.
//
// Example: [key: KeyType]: ValueType  // key is an index signature parameter.
inline Visited_Variable_Declaration index_signature_param_decl(
    String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_index_signature_parameter,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration infer_type_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_infer_type,
                                      Variable_Declaration_Flags::none};
}

inline Visited_Variable_Declaration interface_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_interface,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'let' with an initializer. Example: let x = null;
inline Visited_Variable_Declaration let_init_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_let,
      Variable_Declaration_Flags::initialized_with_equals};
}

// A variable declared with 'let' without an initializer. Example: let x;
inline Visited_Variable_Declaration let_noinit_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_let,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'let' with an initializer in the head of a 'for'
// loop.
// Example: for (let x = 0; x < 10; ++x);
inline Visited_Variable_Declaration let_init_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_let,
      Variable_Declaration_Flags::inside_for_loop_head_initialized_with_equals};
}

// A variable declared with 'let' without an initializer in the head of a 'for'
// loop.
// Example: for (let x of xs);
inline Visited_Variable_Declaration let_noinit_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_let,
      Variable_Declaration_Flags::inside_for_loop_head};
}

// A TypeScript namespace (declared with the 'module' or 'namespace' keyword)
// for which the TypeScript compiler generates JavaScript code.
inline Visited_Variable_Declaration non_empty_namespace_decl(
    String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_namespace,
      Variable_Declaration_Flags::non_empty_namespace};
}

// A TypeScript namespace for which the TypeScript compiler does not generate
// any JavaScript code.
inline Visited_Variable_Declaration empty_namespace_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_namespace,
                                      Variable_Declaration_Flags::none};
}

// A TypeScript generic function parameter.
inline Visited_Variable_Declaration generic_param_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name),
                                      Variable_Kind::_generic_parameter,
                                      Variable_Declaration_Flags::none};
}

// A TypeScript type alias. Example: type T = number;
inline Visited_Variable_Declaration type_alias_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_type_alias,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'var' with an initializer. Example: var x = null;
inline Visited_Variable_Declaration var_init_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_var,
      Variable_Declaration_Flags::initialized_with_equals};
}

// A variable declared with 'var' without an initializer. Example: var x;
inline Visited_Variable_Declaration var_noinit_decl(String8_View name) {
  return Visited_Variable_Declaration{String8(name), Variable_Kind::_var,
                                      Variable_Declaration_Flags::none};
}

// A variable declared with 'var' with an initializer in the head of a 'for'
// loop.
// Example: for (var length = xs.length; i < length; ++i);
inline Visited_Variable_Declaration var_init_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_var,
      Variable_Declaration_Flags::inside_for_loop_head_initialized_with_equals};
}

// A variable declared with 'var' without an initializer in the head of a
// 'for' loop.
// Example: for (var x of xs);
inline Visited_Variable_Declaration var_noinit_for_decl(String8_View name) {
  return Visited_Variable_Declaration{
      String8(name), Variable_Kind::_var,
      Variable_Declaration_Flags::inside_for_loop_head};
}

struct Parse_Visit_Collector : public Parse_Visitor_Base {
  std::vector<std::string_view> visits;

  void visit_end_of_module() override {
    this->visits.emplace_back("visit_end_of_module");
  }

  void visit_enter_block_scope() override {
    this->visits.emplace_back("visit_enter_block_scope");
  }

  void visit_enter_with_scope() override {
    this->visits.emplace_back("visit_enter_with_scope");
  }

  void visit_enter_class_construct_scope() override {
    this->visits.emplace_back("visit_enter_class_construct_scope");
  }

  void visit_enter_class_scope() override {
    this->visits.emplace_back("visit_enter_class_scope");
  }

  void visit_enter_class_scope_body(
      const std::optional<Identifier> &) override {
    this->visits.emplace_back("visit_enter_class_scope_body");
  }

  void visit_enter_conditional_type_scope() override {
    this->visits.emplace_back("visit_enter_conditional_type_scope");
  }

  void visit_enter_declare_scope() override {
    this->visits.emplace_back("visit_enter_declare_scope");
  }

  void visit_enter_enum_scope() override {
    this->visits.emplace_back("visit_enter_enum_scope");
  }

  void visit_enter_for_scope() override {
    this->visits.emplace_back("visit_enter_for_scope");
  }

  void visit_enter_function_scope() override {
    this->visits.emplace_back("visit_enter_function_scope");
  }

  void visit_enter_function_scope_body() override {
    this->visits.emplace_back("visit_enter_function_scope_body");
  }

  void visit_enter_index_signature_scope() override {
    this->visits.emplace_back("visit_enter_index_signature_scope");
  }

  void visit_enter_interface_scope() override {
    this->visits.emplace_back("visit_enter_interface_scope");
  }

  void visit_enter_named_function_scope(Identifier name) override {
    this->enter_named_function_scopes.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_enter_named_function_scope");
  }

  void visit_enter_namespace_scope() override {
    this->visits.emplace_back("visit_enter_namespace_scope");
  }

  void visit_enter_type_alias_scope() override {
    this->visits.emplace_back("visit_enter_type_alias_scope");
  }

  std::vector<String8> enter_named_function_scopes;

  void visit_exit_block_scope() override {
    this->visits.emplace_back("visit_exit_block_scope");
  }

  void visit_exit_with_scope() override {
    this->visits.emplace_back("visit_exit_with_scope");
  }

  void visit_exit_class_construct_scope() override {
    this->visits.emplace_back("visit_exit_class_construct_scope");
  }

  void visit_exit_class_scope() override {
    this->visits.emplace_back("visit_exit_class_scope");
  }

  void visit_exit_conditional_type_scope() override {
    this->visits.emplace_back("visit_exit_conditional_type_scope");
  }

  void visit_exit_declare_scope() override {
    this->visits.emplace_back("visit_exit_declare_scope");
  }

  void visit_exit_enum_scope() override {
    this->visits.emplace_back("visit_exit_enum_scope");
  }

  void visit_exit_for_scope() override {
    this->visits.emplace_back("visit_exit_for_scope");
  }

  void visit_exit_function_scope() override {
    this->visits.emplace_back("visit_exit_function_scope");
  }

  void visit_exit_index_signature_scope() override {
    this->visits.emplace_back("visit_exit_index_signature_scope");
  }

  void visit_exit_interface_scope() override {
    this->visits.emplace_back("visit_exit_interface_scope");
  }

  void visit_exit_namespace_scope() override {
    this->visits.emplace_back("visit_exit_namespace_scope");
  }

  void visit_exit_type_alias_scope() override {
    this->visits.emplace_back("visit_exit_type_alias_scope");
  }

  void visit_keyword_variable_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_keyword_variable_use");
  }

  void visit_property_declaration(
      const std::optional<Identifier> &name) override {
    if (name.has_value()) {
      this->property_declarations.emplace_back(name->normalized_name());
    } else {
      this->property_declarations.emplace_back(std::nullopt);
    }
    this->visits.emplace_back("visit_property_declaration");
  }

  std::vector<std::optional<String8>> property_declarations;

  void visit_variable_assignment(Identifier name) override {
    this->variable_assignments.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_assignment");
  }

  std::vector<String8> variable_assignments;

  void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                  Variable_Declaration_Flags flags) override {
    this->variable_declarations.emplace_back(Visited_Variable_Declaration{
        String8(name.normalized_name()), kind, flags});
    this->visits.emplace_back("visit_variable_declaration");
  }

  std::vector<Visited_Variable_Declaration> variable_declarations;

  void visit_variable_delete_use(
      Identifier name,
      [[maybe_unused]] Source_Code_Span delete_keyword) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_delete_use");
  }

  void visit_variable_export_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_export_use");
  }

  void visit_variable_namespace_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_namespace_use");
  }

  void visit_variable_type_predicate_use(Identifier parameter_name) override {
    this->variable_uses.emplace_back(parameter_name.normalized_name());
    this->visits.emplace_back("visit_variable_type_predicate_use");
  }

  void visit_variable_type_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_type_use");
  }

  void visit_variable_typeof_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_typeof_use");
  }

  void visit_variable_use(Identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_use");
  }

  std::vector<String8> variable_uses;
};

// TODO(strager): Rename this.
struct Spy_Visitor final : public Diag_Collector,
                           public Parse_Visit_Collector {};

void PrintTo(const Visited_Variable_Declaration &, std::ostream *);
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
