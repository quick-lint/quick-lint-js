// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LINT_H
#define QUICK_LINT_JS_LINT_H

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>
#include <string>
#include <unordered_set>
#include <vector>

namespace quick_lint_js {
struct global_declared_variable {
  string8_view name;
  bool is_writable;
  // If false, the variable was already lexically declared in the module thus
  // cannot be declared by the user with 'let'.
  bool is_shadowable;

  variable_kind kind() const noexcept;
};

class global_declared_variable_set {
 public:
  void add_predefined_global_variable(const char8 *name, bool is_writable);

  // FIXME(strager): Bug: if we add a variable with one set of flags (e.g.
  // is_writable=false), then add it with a different set of flags (e.g.
  // is_writable=true), then bad things might happen.
  void add_global_variable(global_declared_variable);

  void add_literally_everything();

  void reserve_more_global_variables(std::size_t extra_count,
                                     bool is_shadowable, bool is_writable);

  std::optional<global_declared_variable> find(identifier name) const noexcept;
  std::optional<global_declared_variable> find(string8_view name) const
      noexcept;

  // For testing only:
  std::vector<string8_view> get_all_variable_names() const;

 private:
  // First index: is_shadowable
  // Second index: is_writable
  std::unordered_set<string8_view> variables_[2][2];
  bool all_variables_declared_ = false;
};

// A linter is a parse_visitor which finds non-syntax bugs.
//
// linter-s detect the following bugs (and possibly more):
//
// * Assignments to const-declared variables
// * Assignments to let-declared variables before their initialization
// * Use of undeclared variables
//
// The linter class implements variable lookup internally.
class linter {
 public:
  explicit linter(error_reporter *error_reporter,
                  const global_declared_variable_set *global_variables);

  void visit_enter_block_scope();
  void visit_enter_with_scope();
  void visit_enter_class_scope();
  void visit_enter_for_scope();
  void visit_enter_function_scope();
  void visit_enter_function_scope_body();
  void visit_enter_named_function_scope(identifier);
  void visit_exit_block_scope();
  void visit_exit_with_scope();
  void visit_exit_class_scope();
  void visit_exit_for_scope();
  void visit_exit_function_scope();
  void visit_keyword_variable_use(identifier name);
  void visit_property_declaration(std::optional<identifier>);
  void visit_variable_declaration(identifier name, variable_kind kind);
  void visit_variable_assignment(identifier name);
  void visit_variable_delete_use(identifier name);
  void visit_variable_export_use(identifier name);
  void visit_variable_typeof_use(identifier name);
  void visit_variable_use(identifier name);
  void visit_end_of_module();

 private:
  enum class declared_variable_scope {
    declared_in_current_scope,
    declared_in_descendant_scope,
  };

  struct declared_variable {
    identifier declaration;
    variable_kind kind;
    declared_variable_scope declaration_scope;
  };

  enum class used_variable_kind {
    _export,
    _typeof,
    assignment,
    use,
  };

  struct used_variable {
    explicit used_variable(identifier name, used_variable_kind kind) noexcept
        : name(name), kind(kind) {}

    identifier name;
    used_variable_kind kind;
  };

  class declared_variable_set {
   public:
    const declared_variable *add_variable_declaration(identifier name,
                                                      variable_kind,
                                                      declared_variable_scope);

    const declared_variable *find(identifier name) const noexcept;

    void clear() noexcept;

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
                        declared_variable_scope declared_scope);
  void visit_variable_use(identifier name, used_variable_kind);

  void propagate_variable_uses_to_parent_scope(
      bool allow_variable_use_before_declaration, bool consume_arguments,
      bool propagate_eval_use);
  template <class Scope>
  void propagate_variable_uses_to_parent_scope(
      Scope &parent_scope, bool allow_variable_use_before_declaration,
      bool consume_arguments, bool propagate_eval_use);

  void propagate_variable_declarations_to_parent_scope();

  void report_error_if_assignment_is_illegal(
      const declared_variable *var, const identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      const std::optional<global_declared_variable> &var,
      const identifier &assignment, bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      variable_kind kind, bool is_global_variable,
      const identifier *declaration, const identifier &assignment,
      bool is_assigned_before_declaration) const;

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

  error_reporter *error_reporter_;
};
QLJS_STATIC_ASSERT_IS_PARSE_VISITOR(linter);
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
