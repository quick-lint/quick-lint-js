// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LINT_H
#define QUICK_LINT_JS_LINT_H

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>
#include <string>
#include <vector>

namespace quick_lint_js {
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
  explicit linter(error_reporter *error_reporter);

  void visit_enter_block_scope();
  void visit_enter_class_scope();
  void visit_enter_for_scope();
  void visit_enter_function_scope();
  void visit_enter_function_scope_body();
  void visit_enter_named_function_scope(identifier);
  void visit_exit_block_scope();
  void visit_exit_class_scope();
  void visit_exit_for_scope();
  void visit_exit_function_scope();
  void visit_property_declaration(std::optional<identifier>);
  void visit_variable_declaration(identifier name, variable_kind kind);
  void visit_variable_assignment(identifier name);
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
    static declared_variable make_local(
        identifier name, variable_kind kind,
        declared_variable_scope declaration_scope) noexcept {
      return declared_variable(name, kind, declaration_scope);
    }

    static declared_variable make_global(string8_view global_variable_name,
                                         variable_kind kind) noexcept {
      return declared_variable(global_variable_name, kind);
    }

    identifier declaration() const noexcept {
      QLJS_ASSERT(!this->is_global_variable());
      return this->declaration_;
    }

    string8_view name() const noexcept {
      if (this->is_global_variable()) {
        return this->global_variable_name_;
      } else {
        return this->declaration_.normalized_name();
      }
    }

    variable_kind kind() const noexcept { return this->kind_; }

    declared_variable_scope declaration_scope() const noexcept {
      return this->declaration_scope_;
    }

    bool is_global_variable() const noexcept {
      return this->is_global_variable_;
    }

   private:
    explicit declared_variable(string8_view global_variable_name,
                               variable_kind kind) noexcept
        : kind_(kind),
          declaration_scope_(
              declared_variable_scope::declared_in_current_scope),
          is_global_variable_(true),
          global_variable_name_(global_variable_name) {}

    explicit declared_variable(
        identifier name, variable_kind kind,
        declared_variable_scope declaration_scope) noexcept
        : kind_(kind),
          declaration_scope_(declaration_scope),
          is_global_variable_(false),
          declaration_(name) {}

    variable_kind kind_;
    declared_variable_scope declaration_scope_;
    bool is_global_variable_;
    union {
      // If is_global_variable_ is false:
      identifier declaration_;
      // If is_global_variable_ is true:
      string8_view global_variable_name_;
    };
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
    void add_predefined_variable_declaration(const char8 *name, variable_kind);

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
    explicit global_scope(const declared_variable_set *declared_variables)
        : declared_variables(*declared_variables) {}

    const declared_variable_set &declared_variables;
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
      bool allow_variable_use_before_declaration, bool consume_arguments);
  template <class Scope>
  void propagate_variable_uses_to_parent_scope(
      Scope &parent_scope, bool allow_variable_use_before_declaration,
      bool consume_arguments);

  void propagate_variable_declarations_to_parent_scope();

  void report_error_if_assignment_is_illegal(
      const declared_variable *var, const identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_variable_declaration_conflicts_in_scope(
      const scope &scope, identifier name, variable_kind kind,
      declared_variable_scope declaration_scope) const;

  scope &current_scope() noexcept { return this->scopes_.current_scope(); }
  scope &parent_scope() noexcept { return this->scopes_.parent_scope(); }

  static linter::declared_variable_set make_global_variables();
  const static linter::declared_variable_set *get_global_variables();

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
// Copyright (C) 2020  Matthew Glazar
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
