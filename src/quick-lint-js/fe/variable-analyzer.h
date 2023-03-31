// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_VARIABLE_ANALYZER_H
#define QUICK_LINT_JS_FE_VARIABLE_ANALYZER_H

#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
class diag_reporter;
class global_declared_variable_set;
struct global_declared_variable;

// TODO(strager): Accept parser options from quick-lint-js.config.
struct variable_analyzer_options {
  // If true, 'delete somevar;' is legal (but might be still issue a warning).
  //
  // If false, 'delete somevar;' is invalid, and variable_analyzer will report a
  // diagnostic mentioning TypeScript.
  bool allow_deleting_typescript_variable = true;

  // If true, eval can declare variables, like in vanilla JavaScript.
  //
  // If false, eval is not supposed to declare variables, like in TypeScript.
  bool eval_can_declare_variables = true;
};

// A variable_analyzer is a parse_visitor which implements variable lookup
// rules.
//
// variable_analyzer-s detect the following bugs (and possibly more):
//
// * Assignments to const-declared variables
// * Assignments to let-declared variables before their initialization
// * Use of undeclared variables
class variable_analyzer final : public parse_visitor_base {
 public:
  explicit variable_analyzer(
      diag_reporter *diag_reporter,
      const global_declared_variable_set *global_variables,
      variable_analyzer_options options);

  void visit_enter_block_scope() override;
  void visit_enter_with_scope() override;
  void visit_enter_class_scope() override;
  void visit_enter_class_scope_body(
      const std::optional<identifier> &class_name) override;
  void visit_enter_enum_scope() override;
  void visit_enter_for_scope() override;
  void visit_enter_function_scope() override;
  void visit_enter_function_scope_body() override;
  void visit_enter_index_signature_scope() override;
  void visit_enter_interface_scope() override;
  void visit_enter_named_function_scope(identifier) override;
  void visit_enter_namespace_scope() override;
  void visit_enter_type_alias_scope() override;
  void visit_exit_block_scope() override;
  void visit_exit_with_scope() override;
  void visit_exit_class_scope() override;
  void visit_exit_enum_scope() override;
  void visit_exit_for_scope() override;
  void visit_exit_function_scope() override;
  void visit_exit_index_signature_scope() override;
  void visit_exit_interface_scope() override;
  void visit_exit_namespace_scope() override;
  void visit_exit_type_alias_scope() override;
  void visit_keyword_variable_use(identifier name) override;
  void visit_property_declaration(const std::optional<identifier> &) override;
  void visit_variable_declaration(identifier name, variable_kind kind,
                                  variable_init_kind init_kind) override;
  void visit_variable_assignment(identifier name) override;
  void visit_variable_delete_use(identifier name,
                                 source_code_span delete_keyword) override;
  void visit_variable_export_use(identifier name) override;
  void visit_variable_namespace_use(identifier name) override;
  void visit_variable_type_predicate_use(identifier name) override;
  void visit_variable_type_use(identifier name) override;
  void visit_variable_typeof_use(identifier name) override;
  void visit_variable_use(identifier name) override;
  void visit_end_of_module() override;

 private:
  enum class declared_variable_scope {
    declared_in_current_scope,
    declared_in_descendant_scope,
  };

  struct declared_variable {
    identifier declaration;
    variable_kind kind;
    declared_variable_scope declaration_scope;
    // If false, this variable was declared and possibly initialized, but not
    // used or assigned to after declaration. If true, this variable was used or
    // assigned (or both) after its declaration.
    bool is_used;
    // If true, the programmer might have intended the variable declaration to
    // be an assignment to an existing variable instead. This happens iff the
    // variable has an initializer with '='. See
    // variable_init_kind::initialized_with_equals.
    bool declaration_possibly_looks_like_assignment;

    // Returns true if this variable can be used in expressions.
    //
    // Returns false if this variable can only be used in TypeScript type
    // signatures and module exports.
    bool is_runtime() const noexcept;

    // Returns true if this variable can be used in TypeScript type signatures
    // or module exports.
    //
    // Returns false if this variable can only be used at run-time.
    bool is_type() const noexcept;
  };

  enum class used_variable_kind {
    _delete,
    _export,
    _typeof,
    assignment,
    type,  // TypeScript only.
    use,
  };

  struct used_variable {
    explicit used_variable(identifier name, used_variable_kind kind) noexcept
        : name(name), kind(kind) {
      QLJS_ASSERT(kind != used_variable_kind::_delete);
    }

    // kind must be used_variable_kind::_delete.
    explicit used_variable(identifier name, used_variable_kind kind,
                           const char8 *delete_keyword_begin) noexcept
        : name(name), delete_keyword_begin(delete_keyword_begin), kind(kind) {
      QLJS_ASSERT(kind == used_variable_kind::_delete);
    }

    // Returns true if this variable was used in an expression or in a module
    // export.
    //
    // Returns false if this variable was used in a TypeScript type signature.
    bool is_runtime() const noexcept;

    // Returns true if this variable was used in a TypeScript type signature.
    //
    // Returns false if this variable was used in a run-time expression.
    bool is_type() const noexcept;

    identifier name;
    const char8 *delete_keyword_begin;  // used_variable_kind::_delete only
    used_variable_kind kind;
  };

  class declared_variable_set {
   public:
    using found_variable_type = declared_variable *;

    declared_variable *add_variable_declaration(
        identifier name, variable_kind, declared_variable_scope,
        bool declaration_possibly_looks_like_assignment);

    const declared_variable *find(identifier name) const noexcept;
    declared_variable *find(identifier name) noexcept;

    // Like find, but ignores type-only variables (e.g. interfaces).
    declared_variable *find_runtime(identifier name) noexcept;

    // Like find, but ignores runtime-only variables (e.g. functions).
    declared_variable *find_type(identifier name) noexcept;

    void clear() noexcept;

    bool empty() const noexcept;
    std::vector<declared_variable>::const_iterator begin() const noexcept;
    std::vector<declared_variable>::const_iterator end() const noexcept;

   private:
    std::vector<declared_variable> variables_;
  };

  // A scope tracks variable declarations and references in a lexical JavaScript
  // scope.
  //
  // A scope is introduced by many syntax forms, including the following:
  //
  // * { } (block statement)
  // * function f() {} (function declaration)
  // * () => {} (arrow function)
  // * for(let x of y)
  struct scope {
    declared_variable_set declared_variables;
    std::vector<used_variable> variables_used;
    std::vector<used_variable> variables_used_in_descendant_scope;
    std::optional<declared_variable> function_expression_declaration;

    // If true, the magic 'eval' function was used in this scope or in a
    // descendant non-function scope.
    bool used_eval_in_this_scope = false;

    // If true, the magic 'eval' function was used in a descendant function
    // scope.
    bool used_eval_in_descendant_scope = false;

    void clear();
  };

  struct global_scope {
    explicit global_scope(
        const global_declared_variable_set *declared_variables)
        : declared_variables(*declared_variables) {}

    const global_declared_variable_set &declared_variables;
    std::vector<used_variable> variables_used;
    std::vector<used_variable> variables_used_in_descendant_scope;
  };

  // A stack of scope objects.
  class scopes {
   public:
    explicit scopes();

    // The module scope which holds properties not on the globalThis object.
    //
    // Variables declared with 'let', 'class', etc. at the top level of the
    // program are declared in this scope.
    //
    // CommonJS in Node.js uses the module scope for variables such as 'require'
    // and '__filename'.
    //
    // The module scope always exists, except possibly at the end of linting.
    scope &module_scope() noexcept;

    scope &current_scope() noexcept;
    scope &parent_scope() noexcept;

    scope &push();
    void pop();

    bool empty() const noexcept;
    int size() const noexcept;

   private:
    int scope_count_ = 0;
    std::vector<scope> scopes_;
  };

  void declare_variable(scope &, identifier name, variable_kind kind,
                        declared_variable_scope declared_scope,
                        bool declaration_possibly_looks_like_assignment);
  void visit_variable_use(identifier name, used_variable_kind);

  void propagate_variable_uses_to_parent_scope(
      bool allow_variable_use_before_declaration, bool consume_arguments);
  template <class Scope>
  void propagate_variable_uses_to_parent_scope(
      Scope &parent_scope, bool allow_variable_use_before_declaration,
      bool consume_arguments);

  void propagate_variable_declarations_to_parent_scope();

  void report_error_if_assignment_is_illegal(
      const declared_variable *var, const identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      const declared_variable &var, const identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      const global_declared_variable &var, const identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      variable_kind kind, bool is_global_variable,
      const identifier *declaration, const identifier &assignment,
      bool is_assigned_before_declaration) const;

  template <class DeclaredVariableType>
  void report_errors_for_variable_use(const used_variable &,
                                      const DeclaredVariableType &,
                                      bool use_is_before_declaration) const;

  void report_error_if_variable_declaration_conflicts_in_scope(
      const scope &scope, identifier name, variable_kind kind,
      declared_variable_scope declaration_scope) const;
  void report_error_if_variable_declaration_conflicts_in_scope(
      const global_scope &scope, const declared_variable &var) const;
  void report_error_if_variable_declaration_conflicts(
      const identifier *already_declared, variable_kind already_declared_kind,
      declared_variable_scope already_declared_declaration_scope,
      bool already_declared_is_global_variable, identifier newly_declared_name,
      variable_kind newly_declared_kind,
      declared_variable_scope newly_declared_declaration_scope) const;

  scope &current_scope() noexcept { return this->scopes_.current_scope(); }
  scope &parent_scope() noexcept { return this->scopes_.parent_scope(); }

  scopes scopes_;

  // The scope which holds properties of the globalThis object.
  //
  // The global scope cannot be modified lexically by user programs. Variables
  // declared with 'let', 'class', etc. at the top level of the program are
  // declared in the module scope, not the global scope.
  global_scope global_scope_;

  diag_reporter *diag_reporter_;

  variable_analyzer_options options_;
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
