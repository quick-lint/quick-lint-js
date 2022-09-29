// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SPY_VISITOR_H
#define QUICK_LINT_JS_SPY_VISITOR_H

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
struct visited_variable_declaration {
  string8 name;
  variable_kind kind;
  variable_init_kind init_kind;

  bool operator==(const visited_variable_declaration &other) const {
    return this->name == other.name && this->kind == other.kind &&
           this->init_kind == other.init_kind;
  }

  bool operator!=(const visited_variable_declaration &other) const {
    return !(*this == other);
  }
};

// An function/method parameter. Not an arrow function parameter.
inline visited_variable_declaration arrow_param_decl(string8_view name) {
  return visited_variable_declaration{string8(name),
                                      variable_kind::_arrow_parameter,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration catch_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_catch,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration class_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_class,
                                      variable_init_kind::normal};
}

// A variable declared with 'const' with an initializer. Example: const x =
// null;
inline visited_variable_declaration const_init_decl(string8_view name) {
  return visited_variable_declaration{
      string8(name), variable_kind::_const,
      variable_init_kind::initialized_with_equals};
}

// A variable declared with 'const' without an initializer.
// Example: for (const x of []) {}
inline visited_variable_declaration const_noinit_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_const,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration enum_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_enum,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration function_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_function,
                                      variable_init_kind::normal};
}

// An function/method parameter. Not an arrow function parameter.
inline visited_variable_declaration func_param_decl(string8_view name) {
  return visited_variable_declaration{string8(name),
                                      variable_kind::_function_parameter,
                                      variable_init_kind::normal};
}

// An function parameter in a TypeScript type.
inline visited_variable_declaration func_type_param_decl(string8_view name) {
  return visited_variable_declaration{string8(name),
                                      variable_kind::_function_type_parameter,
                                      variable_init_kind::normal};
}

// A TypeScript namespace or module alias. Example: import A = B;
inline visited_variable_declaration import_alias_decl(string8_view name) {
  return visited_variable_declaration{
      string8(name), variable_kind::_import_alias, variable_init_kind::normal};
}

inline visited_variable_declaration import_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_import,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration import_type_decl(string8_view name) {
  return visited_variable_declaration{
      string8(name), variable_kind::_import_type, variable_init_kind::normal};
}

// A parameter in a TypeScript index signature.
//
// Example: [key: KeyType]: ValueType  // key is an index signature parameter.
inline visited_variable_declaration index_signature_param_decl(
    string8_view name) {
  return visited_variable_declaration{string8(name),
                                      variable_kind::_index_signature_parameter,
                                      variable_init_kind::normal};
}

inline visited_variable_declaration interface_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_interface,
                                      variable_init_kind::normal};
}

// A variable declared with 'let' with an initializer. Example: let x = null;
inline visited_variable_declaration let_init_decl(string8_view name) {
  return visited_variable_declaration{
      string8(name), variable_kind::_let,
      variable_init_kind::initialized_with_equals};
}

// A variable declared with 'let' without an initializer. Example: let x;
inline visited_variable_declaration let_noinit_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_let,
                                      variable_init_kind::normal};
}

// A TypeScript namespace (declared with the 'namespace' keyword).
inline visited_variable_declaration namespace_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_namespace,
                                      variable_init_kind::normal};
}

// A TypeScript generic function parameter.
inline visited_variable_declaration generic_param_decl(string8_view name) {
  return visited_variable_declaration{string8(name),
                                      variable_kind::_generic_parameter,
                                      variable_init_kind::normal};
}

// A TypeScript type alias. Example: type T = number;
inline visited_variable_declaration type_alias_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_type_alias,
                                      variable_init_kind::normal};
}

// A variable declared with 'var' with an initializer. Example: var x = null;
inline visited_variable_declaration var_init_decl(string8_view name) {
  return visited_variable_declaration{
      string8(name), variable_kind::_var,
      variable_init_kind::initialized_with_equals};
}

// A variable declared with 'var' without an initializer. Example: var x;
inline visited_variable_declaration var_noinit_decl(string8_view name) {
  return visited_variable_declaration{string8(name), variable_kind::_var,
                                      variable_init_kind::normal};
}

struct parse_visit_collector : public parse_visitor_base {
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

  void visit_enter_class_scope() override {
    this->visits.emplace_back("visit_enter_class_scope");
  }

  void visit_enter_class_scope_body(
      const std::optional<identifier> &) override {
    this->visits.emplace_back("visit_enter_class_scope_body");
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

  void visit_enter_named_function_scope(identifier name) override {
    this->enter_named_function_scopes.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_enter_named_function_scope");
  }

  void visit_enter_namespace_scope() override {
    this->visits.emplace_back("visit_enter_namespace_scope");
  }

  void visit_enter_type_alias_scope() override {
    this->visits.emplace_back("visit_enter_type_alias_scope");
  }

  std::vector<string8> enter_named_function_scopes;

  void visit_exit_block_scope() override {
    this->visits.emplace_back("visit_exit_block_scope");
  }

  void visit_exit_with_scope() override {
    this->visits.emplace_back("visit_exit_with_scope");
  }

  void visit_exit_class_scope() override {
    this->visits.emplace_back("visit_exit_class_scope");
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

  void visit_keyword_variable_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_keyword_variable_use");
  }

  void visit_property_declaration(
      const std::optional<identifier> &name) override {
    if (name.has_value()) {
      this->property_declarations.emplace_back(name->normalized_name());
    } else {
      this->property_declarations.emplace_back(std::nullopt);
    }
    this->visits.emplace_back("visit_property_declaration");
  }

  std::vector<std::optional<string8>> property_declarations;

  void visit_variable_assignment(identifier name) override {
    this->variable_assignments.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_assignment");
  }

  std::vector<string8> variable_assignments;

  void visit_variable_declaration(identifier name, variable_kind kind,
                                  variable_init_kind init_kind) override {
    this->variable_declarations.emplace_back(visited_variable_declaration{
        string8(name.normalized_name()), kind, init_kind});
    this->visits.emplace_back("visit_variable_declaration");
  }

  std::vector<visited_variable_declaration> variable_declarations;

  void visit_variable_delete_use(
      identifier name,
      [[maybe_unused]] source_code_span delete_keyword) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_delete_use");
  }

  void visit_variable_export_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_export_use");
  }

  void visit_variable_namespace_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_namespace_use");
  }

  void visit_variable_type_predicate_use(identifier parameter_name) override {
    this->variable_uses.emplace_back(parameter_name.normalized_name());
    this->visits.emplace_back("visit_variable_type_predicate_use");
  }

  void visit_variable_type_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_type_use");
  }

  void visit_variable_typeof_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_typeof_use");
  }

  void visit_variable_use(identifier name) override {
    this->variable_uses.emplace_back(name.normalized_name());
    this->visits.emplace_back("visit_variable_use");
  }

  std::vector<string8> variable_uses;
};

// TODO(strager): Rename this.
struct spy_visitor final : public diag_collector,
                           public parse_visit_collector {};

void PrintTo(const visited_variable_declaration &, std::ostream *);
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
