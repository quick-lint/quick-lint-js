// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
class Diag_Reporter;
class Global_Declared_Variable_Set;
struct Global_Declared_Variable;

// TODO(strager): Accept parser options from quick-lint-js.config.
struct Variable_Analyzer_Options {
  // If true, 'delete somevar;' is legal (but might be still issue a warning).
  //
  // If false, 'delete somevar;' is invalid, and variable_analyzer will report a
  // diagnostic mentioning TypeScript.
  bool allow_deleting_typescript_variable = true;

  // If true, eval can declare variables, like in vanilla JavaScript.
  //
  // If false, eval is not supposed to declare variables, like in TypeScript.
  bool eval_can_declare_variables = true;

  // If true, assigning to a 'class'-declared variable is legal, like in vanilla
  // JavaScript.
  //
  // If false, assigning to a 'class'-declared variable is invalid, like in
  // TypeScript.
  bool can_assign_to_class = true;
};

// A variable_analyzer is a parse_visitor which implements variable lookup
// rules.
//
// variable_analyzer-s detect the following bugs (and possibly more):
//
// * Assignments to const-declared variables
// * Assignments to let-declared variables before their initialization
// * Use of undeclared variables
class Variable_Analyzer final : public Parse_Visitor_Base {
 public:
  explicit Variable_Analyzer(
      Diag_Reporter *diag_reporter,
      const Global_Declared_Variable_Set *global_variables,
      Variable_Analyzer_Options options);

  void visit_enter_block_scope() override;
  void visit_enter_with_scope() override;
  void visit_enter_class_construct_scope() override;
  void visit_enter_class_scope() override;
  void visit_enter_class_scope_body(
      const std::optional<Identifier> &class_name) override;
  void visit_enter_conditional_type_scope() override;
  void visit_enter_declare_global_scope() override;
  void visit_enter_declare_scope() override;
  void visit_enter_enum_scope() override;
  void visit_enter_for_scope() override;
  void visit_enter_function_scope() override;
  void visit_enter_function_scope_body() override;
  void visit_enter_index_signature_scope() override;
  void visit_enter_interface_scope() override;
  void visit_enter_named_function_scope(Identifier) override;
  void visit_enter_namespace_scope() override;
  void visit_enter_type_scope() override;
  void visit_exit_block_scope() override;
  void visit_exit_with_scope() override;
  void visit_exit_class_construct_scope() override;
  void visit_exit_class_scope() override;
  void visit_exit_conditional_type_scope() override;
  void visit_exit_declare_global_scope() override;
  void visit_exit_declare_scope() override;
  void visit_exit_enum_scope() override;
  void visit_exit_for_scope() override;
  void visit_exit_function_scope() override;
  void visit_exit_index_signature_scope() override;
  void visit_exit_interface_scope() override;
  void visit_exit_namespace_scope() override;
  void visit_exit_type_scope() override;
  void visit_keyword_variable_use(Identifier name) override;
  void visit_property_declaration(const std::optional<Identifier> &) override;
  void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                  Variable_Declaration_Flags flags) override;
  void visit_variable_assignment(Identifier name) override;
  void visit_variable_assertion_signature_use(Identifier name) override;
  void visit_variable_delete_use(Identifier name,
                                 Source_Code_Span delete_keyword) override;
  void visit_variable_export_default_use(Identifier name) override;
  void visit_variable_export_use(Identifier name) override;
  void visit_variable_namespace_use(Identifier name) override;
  void visit_variable_type_predicate_use(Identifier name) override;
  void visit_variable_type_use(Identifier name) override;
  void visit_variable_typeof_use(Identifier name) override;
  void visit_variable_use(Identifier name) override;
  void visit_end_of_module() override;

 private:
  enum class Declared_Variable_Scope {
    declared_in_current_scope,
    declared_in_descendant_scope,
  };

  struct Declared_Variable {
    Identifier declaration;
    Variable_Kind kind;
    Declared_Variable_Scope declaration_scope;
    // If false, this variable was declared and possibly initialized, but not
    // used or assigned to after declaration. If true, this variable was used or
    // assigned (or both) after its declaration.
    bool is_used;
    // If Variable_Init_Kind::initialized_with_equals is set, the programmer
    // might have intended the variable declaration to be an assignment to an
    // existing variable instead. This happens iff the variable has an
    // initializer with '='.
    Variable_Declaration_Flags flags;
    // If true, this variable was declared in a TypeScript ambient context, i.e.
    // with the 'declare' keyword directly or inside a 'declare' block.
    //
    // declare var x;          // ambient==true
    // var y;                  // ambient==false
    // declare namespace ns {
    //    var z;               // ambient==true
    //    function f(          // ambient==true
    //      myParameter);      // ambient==true
    // };
    bool ambient;

    // Returns true if this variable can be used in expressions.
    //
    // Returns false if this variable can only be used in TypeScript type
    // signatures and module exports.
    bool is_runtime() const;

    // Returns true if this variable can be used in TypeScript type signatures
    // or module exports.
    //
    // Returns false if this variable can only be used at run-time.
    bool is_type() const;
  };

  enum class Used_Variable_Kind {
    _delete,
    _export,
    _export_default,
    _typeof,
    assignment,
    type,  // TypeScript only.
    use,
    // A run-time variable was used in a type. For example:
    //
    //    let x: string;
    //    let y: typeof x;  // use_in_type for 'x'.
    //
    // TypeScript only.
    use_in_type,
  };

  static Is_Runtime_Or_Type is_runtime_or_type(Used_Variable_Kind);

  struct Used_Variable {
    explicit Used_Variable(Identifier name, Used_Variable_Kind kind)
        : name(name), kind(kind) {
      QLJS_ASSERT(kind != Used_Variable_Kind::_delete);
    }

    // kind must be Used_Variable_Kind::_delete.
    explicit Used_Variable(Identifier name, Used_Variable_Kind kind,
                           const Char8 *delete_keyword_begin)
        : name(name), delete_keyword_begin(delete_keyword_begin), kind(kind) {
      QLJS_ASSERT(kind == Used_Variable_Kind::_delete);
    }

    // Returns true if this variable was used in an expression or in a module
    // export.
    //
    // Returns false if this variable was used in a TypeScript type signature.
    bool is_runtime() const;

    // Returns true if this variable was used in a TypeScript type signature.
    //
    // Returns false if this variable was used in a run-time expression.
    bool is_type() const;

    Is_Runtime_Or_Type is_runtime_or_type() const;

    Identifier name;
    const Char8 *delete_keyword_begin;  // Used_Variable_Kind::_delete only
    Used_Variable_Kind kind;
  };

  class Declared_Variable_Set {
   public:
    using Found_Variable_Type = Declared_Variable *;

    Declared_Variable *add_variable_declaration(const Declared_Variable &);

    // Find a variable, if any, which matches the given filter.
    //
    // * Ignores types-only variables (e.g. interfaces) if options.is_type is
    //   false.
    // * Ignores runtime-only variables (e.g. functions) if options.is_runtime
    //   is false.
    const Declared_Variable *find(Identifier name,
                                  Is_Runtime_Or_Type options) const;
    Declared_Variable *find(Identifier name, Is_Runtime_Or_Type options);

    // Like find(name, {.is_runtime = true, .is_type = false}).
    Declared_Variable *find_runtime(Identifier name);

    void clear();

    bool empty() const;
    std::vector<Declared_Variable>::const_iterator begin() const;
    std::vector<Declared_Variable>::const_iterator end() const;

   private:
    std::vector<Declared_Variable> variables_;
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
  struct Scope {
    Declared_Variable_Set declared_variables;
    std::vector<Used_Variable> variables_used;
    std::vector<Used_Variable> variables_used_in_descendant_scope;
    std::optional<Declared_Variable> function_expression_declaration;

    // If true, the magic 'eval' function was used in this scope or in a
    // descendant non-function scope.
    bool used_eval_in_this_scope = false;

    // If true, the magic 'eval' function was used in a descendant function
    // scope.
    bool used_eval_in_descendant_scope = false;

    void clear();
  };

  struct Global_Scope {
    explicit Global_Scope(
        const Global_Declared_Variable_Set *declared_variables)
        : declared_variables(*declared_variables) {}

    const Global_Declared_Variable_Set &declared_variables;
    std::vector<Used_Variable> variables_used;
    std::vector<Used_Variable> variables_used_in_descendant_scope;
  };

  // A stack of scope objects.
  class Scopes {
   public:
    explicit Scopes();

    // The module scope which holds properties not on the globalThis object.
    //
    // Variables declared with 'let', 'class', etc. at the top level of the
    // program are declared in this scope.
    //
    // CommonJS in Node.js uses the module scope for variables such as 'require'
    // and '__filename'.
    //
    // The module scope always exists, except possibly at the end of linting.
    Scope &module_scope();

    // An augmentation of Variable_Analyzer::global_scope_ which is modifiable
    // by the user's code.
    //
    // Variables are declared into the shadow global scope using TypeScript's
    // 'declare global' feature.
    //
    // Variables in the shadow global scope shadow/override global variables,
    // hence its name.
    //
    // The shadow global scope always exists, except possibly at the end of
    // linting.
    Scope &shadow_global_scope();

    Scope &current_scope();
    Scope &parent_scope();

    Scope &push();
    void pop();

    bool empty() const;
    int size() const;

   private:
    int scope_count_ = 0;
    std::vector<Scope> scopes_;
  };

  void declare_variable(Scope &, Identifier name, Variable_Kind kind,
                        Declared_Variable_Scope declared_scope,
                        Variable_Declaration_Flags flags);
  void visit_variable_use(Identifier name, Used_Variable_Kind);

  void add_variable_use_to_current_scope(Used_Variable &&);

  void propagate_variable_uses_to_parent_scope(
      bool allow_variable_use_before_declaration, bool consume_arguments);
  template <class Parent_Scope>
  void propagate_variable_uses_to_parent_scope(
      Parent_Scope &parent_scope, bool allow_variable_use_before_declaration,
      bool consume_arguments);

  void propagate_variable_declarations_to_parent_scope();

  void report_error_if_assignment_is_illegal(
      const Declared_Variable *var, const Identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      const Declared_Variable &var, const Identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      const Global_Declared_Variable &var, const Identifier &assignment,
      bool is_assigned_before_declaration) const;
  void report_error_if_assignment_is_illegal(
      Variable_Kind kind, bool is_global_variable,
      const Identifier *declaration, const Identifier &assignment,
      bool is_assigned_before_declaration) const;

  template <class Declared_Variable_Type>
  void report_errors_for_variable_use(const Used_Variable &,
                                      const Declared_Variable_Type &,
                                      bool use_is_before_declaration) const;

  struct Declared_Variable_Options {
    // If nullptr, the variable is a global variable.
    const Identifier *name;
    // See Declared_Variable::kind.
    Variable_Kind kind;
    // See Declared_Variable::declaration_scope.
    Declared_Variable_Scope declaration_scope;
    // See Declared_Variable::flags.
    Variable_Declaration_Flags flags;
    // See Declared_Variable::ambient;
    bool ambient;
  };

  void report_error_if_variable_declaration_conflicts_in_scope(
      const Scope &scope, const Declared_Variable &var) const;
  void report_error_if_variable_declaration_conflicts_in_scope(
      const Global_Scope &scope, const Declared_Variable &var) const;
  // Returns true if the variable did conflict (thus a diagnostic was reported).
  bool report_error_if_variable_declaration_conflicts(
      const Declared_Variable_Options &already_declared_var,
      const Declared_Variable &newly_declared_var) const;

  Scope &current_scope() { return this->scopes_.current_scope(); }
  Scope &parent_scope() { return this->scopes_.parent_scope(); }

  // Returns true if this is a TypeScript ambient context introduced with the
  // 'declare' keyword.
  bool in_typescript_ambient_context() const;

  // Returns true if we are inside a TypeScript 'declare global' scope.
  bool in_typescript_declare_global_scope() const;

  Scopes scopes_;

  // The scope which holds properties of the globalThis object.
  //
  // The global scope cannot be modified lexically by user programs. Variables
  // declared with 'let', 'class', etc. at the top level of the program are
  // declared in the module scope, not the global scope.
  Global_Scope global_scope_;

  // If greater than zero, this is a TypeScript ambient context introduced
  // with the 'declare' keyword.
  unsigned typescript_ambient_context_depth_ = 0;

  // If greater than zero, we are inside a TypeScript 'declare global' scope,
  // thus declared variables should be hoisted into the global scope.
  unsigned typescript_declare_global_scope_depth_ = 0;

  Diag_Reporter *diag_reporter_;

  Variable_Analyzer_Options options_;
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
