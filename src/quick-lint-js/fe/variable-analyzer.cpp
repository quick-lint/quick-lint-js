// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/vector-erase.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")

// The Variable_Analyzer class implements single-pass variable lookup. A
// single-pass algorithm is complicated in JavaScript for a few reasons:
//
// * Variables declared with 'var' or 'function' statements are hoisted. This
//   means that the declaration might be *textually-after* a use of that
//   variable:
//
//     console.log(x);  // OK; x holds undefined.
//     var x = 3;
//
// * Simiarly, variables declared with 'class', 'const', or 'let' statements are
//   pseudo-hoisted: it is an error to reference the variable textually-before
//   its declaration:
//
//     console.log(x);  // ERROR; x is uninitialized.
//     let x = 3;
//
// * Variables of any type can referenced textually-before their declaration if
//   the variable is declared in a containing function:
//
//     function f() {
//       console.log(x);  // OK, if f is called after x' declaration.
//     }
//     let x = 3;
//     f();
//
// * Pseudo-hoisted variables shadow other variables:
//
//     let x;
//     {
//       console.log(x);  // ERROR; x refers to the variable declared below, so
//                        // x is uninitialized.
//       let x;
//     }
//
// To satisfy these requirements, the Variable_Analyzer class implements the
// following algorithm (simplified for digestion):
//
// * When we see a variable declaration (visit_variable_declaration):
//   * Remember the declaration for the current scope (declared_variables).
//   * If the variable was already used in the current scope (variables_used or
//     variables_used_in_descendant_scope):
//     * Report a use-before-declaration error if necessary.
//     * Check use legality [1].
//     * Forget the variable use in the current scope (variables_used or
//       variables_used_in_descendant_scope).
// * When we see a variable use (visit_variable_assignment or
//   visit_variable_use):
//   * If the variable is declared in the current scope:
//     * Check use legality [1].
//   * Otherwise (if the variable is not declared in the current scope):
//     * Remember the use for the current scope (variables_used).
// * When we reach the end of a scope:
//   * For each remembered variable use in the current scope (variables_used and
//     variables_used_in_descendant_scope):
//     * If the current scope is a function scope:
//       * Use the variable in the parent scope (as if calling
//         visit_variable_assignment or visit_variable_use), except permit
//         use-before-declaration (variables_used_in_descendant_scope).
//     * Otherwise (if the current scope is not a function scope):
//       * Use the variable in the parent scope (as if calling
//         visit_variable_assignment or visit_variable_use).
//     * Remember: the variable use is not in the current scope's remembered
//       variable declarations (declared_variables).
// * When we reach the end of the module (visit_end_of_module):
//   * For each remember variable use in the current scope (variables_used and
//     variables_used_in_descendant_scope):
//     * Report a use-of-undeclared-variable error.
//
// Note: Counter to your likely intuition, when we see a variable use, we do
// *not* look for declarations in all ancestor scopes. We only ever look for
// declarations in the current scope. Looking in ancestor scopes would work for
// a two-pass linter (find declaration pass; bind uses to declarations pass),
// but not for a one-pass linter like ours.
//
// [1] "Check use legality" includes checks unrelated to variable lookup, such
//     as reporting an error if a 'const'-declared variable is assigned to.

namespace quick_lint_js {
namespace {
bool is_runtime(Variable_Kind);
bool is_type(Variable_Kind);
}

Variable_Analyzer::Variable_Analyzer(
    Diag_Reporter *diag_reporter,
    const Global_Declared_Variable_Set *global_variables,
    Variable_Analyzer_Options options)
    : global_scope_(global_variables),
      diag_reporter_(diag_reporter),
      options_(options) {}

void Variable_Analyzer::visit_enter_block_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_with_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_class_construct_scope() {
  this->scopes_.push();
}

void Variable_Analyzer::visit_enter_class_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_class_scope_body(
    const std::optional<Identifier> &class_name) {
  if (class_name.has_value()) {
    this->declare_variable(
        /*scope=*/this->current_scope(),
        /*name=*/*class_name,
        /*kind=*/Variable_Kind::_class,
        /*declared_scope=*/Declared_Variable_Scope::declared_in_current_scope,
        /*flags=*/Variable_Declaration_Flags::none);
  }
}

void Variable_Analyzer::visit_enter_conditional_type_scope() {
  this->scopes_.push();
}

void Variable_Analyzer::visit_enter_declare_scope() {}

void Variable_Analyzer::visit_enter_enum_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_for_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_function_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_function_scope_body() {
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
}

void Variable_Analyzer::visit_enter_index_signature_scope() {
  this->scopes_.push();
}

void Variable_Analyzer::visit_enter_interface_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_named_function_scope(
    Identifier function_name) {
  Scope &current_scope = this->scopes_.push();
  current_scope.function_expression_declaration = Declared_Variable{
      .declaration = function_name,
      .kind = Variable_Kind::_function,
      .declaration_scope = Declared_Variable_Scope::declared_in_current_scope,
      .is_used = false,
      .flags = Variable_Declaration_Flags::none,
  };
}

void Variable_Analyzer::visit_enter_namespace_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_enter_type_alias_scope() { this->scopes_.push(); }

void Variable_Analyzer::visit_exit_block_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_with_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // Don't propagate variable uses, only declarations
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_class_construct_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_class_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_conditional_type_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_declare_scope() {}

void Variable_Analyzer::visit_exit_enum_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // TODO(#756): For now, we don't propagate variable uses. We should declare
  // enum members so we can find typos in enum initializers.
  QLJS_ASSERT(this->current_scope().declared_variables.empty());
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_for_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_function_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_index_signature_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_interface_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_namespace_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // Do not propagate variable uses. Namespaced code can see variables exported
  // from other namespace blocks in other files (which we can't see), so assume
  // that all undeclared variables might exist.
  this->scopes_.pop();
}

void Variable_Analyzer::visit_exit_type_alias_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void Variable_Analyzer::visit_keyword_variable_use(Identifier) {
  // Ignore. The parser should have already reported E0023.
}

void Variable_Analyzer::visit_property_declaration(
    const std::optional<Identifier> &) {}

void Variable_Analyzer::visit_variable_declaration(
    Identifier name, Variable_Kind kind, Variable_Declaration_Flags flags) {
  this->declare_variable(
      /*scope=*/this->current_scope(),
      /*name=*/name,
      /*kind=*/kind,
      /*declared_scope=*/Declared_Variable_Scope::declared_in_current_scope,
      /*flags=*/flags);
}

void Variable_Analyzer::declare_variable(Scope &scope, Identifier name,
                                         Variable_Kind kind,
                                         Declared_Variable_Scope declared_scope,
                                         Variable_Declaration_Flags flags) {
  bool is_function_or_var =
      kind == Variable_Kind::_function || kind == Variable_Kind::_var;
  if (declared_scope == Declared_Variable_Scope::declared_in_descendant_scope) {
    QLJS_ASSERT(is_function_or_var);
  }

  this->report_error_if_variable_declaration_conflicts_in_scope(
      scope, name, kind, flags, declared_scope);

  if (is_function_or_var && name.normalized_name() == u8"eval"_sv) {
    scope.used_eval_in_this_scope = false;
  }

  Declared_Variable *declared =
      scope.declared_variables.add_variable_declaration(name, kind,
                                                        declared_scope, flags);

  erase_if(scope.variables_used, [&](const Used_Variable &used_var) {
    if (name.normalized_name() != used_var.name.normalized_name()) {
      return false;
    }
    declared->is_used = true;
    if (kind == Variable_Kind::_function &&
        declared_scope ==
            Declared_Variable_Scope::declared_in_descendant_scope &&
        used_var.kind == Used_Variable_Kind::use) {
      this->diag_reporter_->report(
          Diag_Function_Call_Before_Declaration_In_Block_Scope{
              .use = used_var.name.span(),
              .declaration = name.span(),
          });
    }
    this->report_errors_for_variable_use(
        used_var, *declared,
        /*use_is_before_declaration=*/kind == Variable_Kind::_class ||
            kind == Variable_Kind::_const || kind == Variable_Kind::_let);
    switch (used_var.kind) {
    case Used_Variable_Kind::assignment:
      break;
    case Used_Variable_Kind::_typeof:
    case Used_Variable_Kind::use:
      if (kind == Variable_Kind::_class || kind == Variable_Kind::_const ||
          kind == Variable_Kind::_let) {
        this->diag_reporter_->report(Diag_Variable_Used_Before_Declaration{
            .use = used_var.name.span(),
            .declaration = name.span(),
        });
      }
      break;
    case Used_Variable_Kind::_delete:
      // Use before declaration is legal for delete.
      break;
    case Used_Variable_Kind::_export:
      // Use before declaration is legal for variable exports.
      break;
    case Used_Variable_Kind::type:
      if (kind == Variable_Kind::_generic_parameter) {
        this->diag_reporter_->report(Diag_Variable_Used_Before_Declaration{
            .use = used_var.name.span(),
            .declaration = name.span(),
        });
      }
      // Use before declaration is normally legal for types.
      break;
    }
    return true;
  });
  erase_if(scope.variables_used_in_descendant_scope,
           [&](const Used_Variable &used_var) {
             if (name.normalized_name() != used_var.name.normalized_name()) {
               return false;
             }
             if (!((declared->is_runtime() && used_var.is_runtime()) ||
                   (declared->is_type() && used_var.is_type()))) {
               return false;
             }
             if (declared->is_runtime()) {
               this->report_errors_for_variable_use(
                   used_var, declared,
                   /*is_assigned_before_declaration=*/false);
             }
             switch (used_var.kind) {
             case Used_Variable_Kind::assignment:
               if (declared->is_runtime()) {
                 declared->is_used = true;
               }
               break;
             case Used_Variable_Kind::_export:
               // TODO(strager): This shouldn't happen. export statements are
               // not allowed inside functions.
               break;
             case Used_Variable_Kind::_delete:
             case Used_Variable_Kind::_typeof:
             case Used_Variable_Kind::use:
               declared->is_used = true;
               break;
             case Used_Variable_Kind::type:
               // TODO(strager): Do we need to set declared->is_used?
               break;
             }
             return true;
           });
}

void Variable_Analyzer::visit_variable_assignment(Identifier name) {
  QLJS_ASSERT(!this->scopes_.empty());
  Scope &current_scope = this->current_scope();
  Declared_Variable *var = current_scope.declared_variables.find_runtime(name);
  if (var) {
    var->is_used = true;
    this->report_error_if_assignment_is_illegal(
        var, name, /*is_assigned_before_declaration=*/false);
  } else {
    this->add_variable_use_to_current_scope(
        Used_Variable(name, Used_Variable_Kind::assignment));
  }
}

void Variable_Analyzer::visit_variable_delete_use(
    Identifier name, Source_Code_Span delete_keyword) {
  QLJS_ASSERT(delete_keyword.end() <= name.span().begin());

  if (this->options_.allow_deleting_typescript_variable) {
    QLJS_ASSERT(!this->scopes_.empty());
    Scope &current_scope = this->current_scope();

    Used_Variable used_var(name, Used_Variable_Kind::_delete,
                           delete_keyword.begin());
    Declared_Variable *already_declared =
        current_scope.declared_variables.find_runtime(name);
    if (already_declared) {
      this->report_errors_for_variable_use(used_var, *already_declared,
                                           /*use_is_before_declaration=*/false);
    } else {
      this->add_variable_use_to_current_scope(std::move(used_var));
    }
  } else {
    this->diag_reporter_->report(Diag_TypeScript_Delete_Cannot_Delete_Variables{
        .delete_expression =
            Source_Code_Span(delete_keyword.begin(), name.span().end()),
    });
  }
}

void Variable_Analyzer::visit_variable_export_use(Identifier name) {
  this->visit_variable_use(name, Used_Variable_Kind::_export);
}

void Variable_Analyzer::visit_variable_namespace_use(Identifier) {
  // TODO(#690): Look up TypeScript namespace variables and imports.
}

void Variable_Analyzer::visit_variable_type_predicate_use(Identifier name) {
  // TODO(#690)
  static_cast<void>(name);

  QLJS_ASSERT(!this->scopes_.empty());
  Scope &current_scope = this->current_scope();
  Declared_Variable *var = current_scope.declared_variables.find_runtime(name);
  if (var) {
    // FIXME(strager): Should we mark the parameter as used?
  } else {
    this->diag_reporter_->report(
        Diag_Use_Of_Undeclared_Parameter_In_Type_Predicate{
            .name = name.span(),
        });
  }
}

void Variable_Analyzer::visit_variable_type_use(Identifier name) {
  this->visit_variable_use(name, Used_Variable_Kind::type);
}

void Variable_Analyzer::visit_variable_typeof_use(Identifier name) {
  this->visit_variable_use(name, Used_Variable_Kind::_typeof);
}

void Variable_Analyzer::visit_variable_use(Identifier name) {
  this->visit_variable_use(name, Used_Variable_Kind::use);
}

void Variable_Analyzer::visit_variable_use(Identifier name,
                                           Used_Variable_Kind use_kind) {
  QLJS_ASSERT(!this->scopes_.empty());
  Scope &current_scope = this->current_scope();
  Declared_Variable *var =
      use_kind == Used_Variable_Kind::type
          ? current_scope.declared_variables.find_type(name)
          : use_kind == Used_Variable_Kind::_export
                ? current_scope.declared_variables.find(name)
                : current_scope.declared_variables.find_runtime(name);
  if (var) {
    var->is_used = true;
  } else {
    this->add_variable_use_to_current_scope(Used_Variable(name, use_kind));
    if (name.normalized_name() == u8"eval"_sv) {
      current_scope.used_eval_in_this_scope = true;
    }
  }
}

void Variable_Analyzer::add_variable_use_to_current_scope(Used_Variable &&var) {
  this->current_scope().variables_used.push_back(std::move(var));
}

void Variable_Analyzer::visit_end_of_module() {
  // We expect only the module scope.
  QLJS_ASSERT(this->scopes_.size() == 1);

  Variable_Analyzer::Global_Scope &global_scope = this->global_scope_;

  for (const Declared_Variable &var :
       this->scopes_.module_scope().declared_variables) {
    this->report_error_if_variable_declaration_conflicts_in_scope(global_scope,
                                                                  var);
  }

  this->propagate_variable_uses_to_parent_scope(
      /*parent_scope=*/global_scope,
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);

  std::vector<Identifier> typeof_variables;
  for (const Used_Variable &used_var : global_scope.variables_used) {
    if (used_var.kind == Used_Variable_Kind::_typeof) {
      typeof_variables.emplace_back(used_var.name);
    }
  }
  for (const Used_Variable &used_var :
       global_scope.variables_used_in_descendant_scope) {
    if (used_var.kind == Used_Variable_Kind::_typeof) {
      typeof_variables.emplace_back(used_var.name);
    }
  }
  auto is_variable_declared_by_typeof = [&](const Used_Variable &var) -> bool {
    return any_of(typeof_variables, [&](const Identifier &typeof_variable) {
      return typeof_variable.normalized_name() == var.name.normalized_name();
    });
  };
  auto is_variable_declared = [&](const Used_Variable &var) -> bool {
    // If a variable appears in declared_variables, then
    // propagate_variable_uses_to_parent_scope should have already removed it
    // from variables_used and variables_used_in_descendant_scope.
    switch (var.kind) {
    case Used_Variable_Kind::_export:
    case Used_Variable_Kind::_delete:
    case Used_Variable_Kind::_typeof:
    case Used_Variable_Kind::assignment:
    case Used_Variable_Kind::use:
      QLJS_ASSERT(!global_scope.declared_variables.find_runtime(var.name));
      break;
    case Used_Variable_Kind::type:
      QLJS_ASSERT(!global_scope.declared_variables.find_type(var.name));
      break;
    }

    // TODO(#690): This should not affect type uses.
    return is_variable_declared_by_typeof(var);
  };

  auto check_if_variable_is_undeclared =
      [&](const Used_Variable &used_var) -> void {
    if (!is_variable_declared(used_var)) {
      switch (used_var.kind) {
      case Used_Variable_Kind::assignment:
        this->diag_reporter_->report(Diag_Assignment_To_Undeclared_Variable{
            .assignment = used_var.name.span()});
        break;
      case Used_Variable_Kind::_delete:
        // TODO(strager): Report a warning if the global variable is not
        // deletable.
        break;
      case Used_Variable_Kind::type:
        this->diag_reporter_->report(
            Diag_Use_Of_Undeclared_Type{.name = used_var.name.span()});
        break;
      case Used_Variable_Kind::_export:
      case Used_Variable_Kind::use:
        this->diag_reporter_->report(
            Diag_Use_Of_Undeclared_Variable{.name = used_var.name.span()});
        break;
      case Used_Variable_Kind::_typeof:
        // 'typeof foo' is often used to detect if the variable 'foo' is
        // declared. Do not report that the variable is undeclared.
        break;
      }
    }
  };
  for (const Used_Variable &used_var : global_scope.variables_used) {
    check_if_variable_is_undeclared(used_var);
  }
  for (const Used_Variable &used_var :
       global_scope.variables_used_in_descendant_scope) {
    check_if_variable_is_undeclared(used_var);
  }
}

void Variable_Analyzer::propagate_variable_uses_to_parent_scope(
    bool allow_variable_use_before_declaration, bool consume_arguments) {
  this->propagate_variable_uses_to_parent_scope(
      /*parent_scope=*/this->parent_scope(),
      /*allow_variable_use_before_declaration=*/
      allow_variable_use_before_declaration,
      /*consume_arguments=*/consume_arguments);
}

template <class Parent_Scope>
void Variable_Analyzer::propagate_variable_uses_to_parent_scope(
    Parent_Scope &parent_scope, bool allow_variable_use_before_declaration,
    bool consume_arguments) {
  // Found_Variable_Type is either Declared_Variable* or
  // std::optional<Global_Declared_Variable>.
  using Found_Variable_Type = typename std::decay_t<decltype(
      Parent_Scope::declared_variables)>::Found_Variable_Type;

  constexpr bool parent_scope_is_global_scope =
      std::is_same_v<Parent_Scope, Global_Scope>;

  Scope &current_scope = this->current_scope();

  auto is_current_scope_function_name = [&](const Used_Variable &var) {
    return current_scope.function_expression_declaration.has_value() &&
           current_scope.function_expression_declaration->declaration
                   .normalized_name() == var.name.normalized_name();
  };

  if constexpr (!parent_scope_is_global_scope) {
    if (!allow_variable_use_before_declaration) {
      parent_scope.used_eval_in_this_scope =
          parent_scope.used_eval_in_this_scope ||
          current_scope.used_eval_in_this_scope;
    }
    parent_scope.used_eval_in_descendant_scope =
        parent_scope.used_eval_in_descendant_scope ||
        current_scope.used_eval_in_this_scope ||
        current_scope.used_eval_in_descendant_scope;
  }

  if (!(this->options_.eval_can_declare_variables &&
        current_scope.used_eval_in_this_scope)) {
    for (const Used_Variable &used_var : current_scope.variables_used) {
      Found_Variable_Type var = {};
      switch (used_var.kind) {
      case Used_Variable_Kind::_export:
        QLJS_ASSERT(!current_scope.declared_variables.find(used_var.name));
        var = parent_scope.declared_variables.find(used_var.name);
        break;
      case Used_Variable_Kind::_delete:
      case Used_Variable_Kind::_typeof:
      case Used_Variable_Kind::assignment:
      case Used_Variable_Kind::use:
        QLJS_ASSERT(
            !current_scope.declared_variables.find_runtime(used_var.name));
        var = parent_scope.declared_variables.find_runtime(used_var.name);
        break;
      case Used_Variable_Kind::type:
        QLJS_ASSERT(!current_scope.declared_variables.find_type(used_var.name));
        var = parent_scope.declared_variables.find_type(used_var.name);
        break;
      }

      if (var) {
        // This variable was declared in the parent scope. Don't propagate.
        this->report_errors_for_variable_use(
            used_var, *var,
            /*use_is_before_declaration=*/false);
        if constexpr (!parent_scope_is_global_scope) {
          var->is_used = true;
        }
      } else if (consume_arguments &&
                 used_var.name.normalized_name() == u8"arguments"_sv) {
        // Treat this variable as declared in the current scope.
      } else if (is_current_scope_function_name(used_var)) {
        // Treat this variable as declared in the current scope.
      } else {
        (allow_variable_use_before_declaration
             ? parent_scope.variables_used_in_descendant_scope
             : parent_scope.variables_used)
            .emplace_back(used_var);
      }
    }

    for (const Used_Variable &used_var :
         current_scope.variables_used_in_descendant_scope) {
      Found_Variable_Type var = {};
      switch (used_var.kind) {
      case Used_Variable_Kind::_export:
        var = parent_scope.declared_variables.find(used_var.name);
        break;
      case Used_Variable_Kind::_delete:
      case Used_Variable_Kind::_typeof:
      case Used_Variable_Kind::assignment:
      case Used_Variable_Kind::use:
        var = parent_scope.declared_variables.find_runtime(used_var.name);
        break;
      case Used_Variable_Kind::type:
        var = parent_scope.declared_variables.find_type(used_var.name);
        break;
      }

      if (var) {
        // This variable was declared in the parent scope. Don't propagate.
        this->report_errors_for_variable_use(
            used_var, *var,
            /*use_is_before_declaration=*/false);
      } else if (is_current_scope_function_name(used_var)) {
        // Treat this variable as declared in the current scope.
      } else {
        parent_scope.variables_used_in_descendant_scope.emplace_back(used_var);
      }
    }
  }
  current_scope.variables_used.clear();
  current_scope.variables_used_in_descendant_scope.clear();
}

void Variable_Analyzer::propagate_variable_declarations_to_parent_scope() {
  Scope &current_scope = this->current_scope();
  Scope &parent_scope = this->parent_scope();

  for (const Declared_Variable &var : current_scope.declared_variables) {
    if (var.kind == Variable_Kind::_function ||
        var.kind == Variable_Kind::_var) {
      this->declare_variable(
          /*scope=*/parent_scope,
          /*name=*/var.declaration,
          /*kind=*/var.kind,
          /*declared_scope=*/
          Declared_Variable_Scope::declared_in_descendant_scope,
          /*flags=*/var.flags);
    }

    bool declaration_possibly_looks_like_assignment =
        (var.flags & Variable_Declaration_Flags::initialized_with_equals);
    if (declaration_possibly_looks_like_assignment && !var.is_used &&
        (var.kind == Variable_Kind::_const ||
         var.kind == Variable_Kind::_let) &&
        !(current_scope.used_eval_in_this_scope ||
          current_scope.used_eval_in_descendant_scope)) {
      // TODO(strager): NOTE[unused-var-shadows-nested-block]: Check multiple
      // parent scopes, not just the immediate parent.
      const Declared_Variable *already_declared_variable =
          parent_scope.declared_variables.find(var.declaration);
      if (already_declared_variable &&
          (already_declared_variable->kind == Variable_Kind::_const ||
           already_declared_variable->kind == Variable_Kind::_let ||
           already_declared_variable->kind == Variable_Kind::_var)) {
        this->diag_reporter_->report(Diag_Unused_Variable_Shadows{
            .shadowing_declaration = var.declaration.span(),
            .shadowed_declaration =
                already_declared_variable->declaration.span(),
        });
      }
    }
  }
}

void Variable_Analyzer::report_error_if_assignment_is_illegal(
    const Declared_Variable *var, const Identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(*var, assignment,
                                              is_assigned_before_declaration);
}

void Variable_Analyzer::report_error_if_assignment_is_illegal(
    const Declared_Variable &var, const Identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(
      /*kind=*/var.kind,
      /*is_global_variable=*/false,
      /*declaration=*/&var.declaration,
      /*assignment=*/assignment,
      /*is_assigned_before_declaration=*/is_assigned_before_declaration);
}

void Variable_Analyzer::report_error_if_assignment_is_illegal(
    const Global_Declared_Variable &var, const Identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(
      /*kind=*/var.kind(),
      /*is_global_variable=*/true,
      /*declaration=*/nullptr,
      /*assignment=*/assignment,
      /*is_assigned_before_declaration=*/is_assigned_before_declaration);
}

void Variable_Analyzer::report_error_if_assignment_is_illegal(
    Variable_Kind kind, bool is_global_variable, const Identifier *declaration,
    const Identifier &assignment, bool is_assigned_before_declaration) const {
  if (is_global_variable) {
    QLJS_ASSERT(!declaration);
  } else {
    QLJS_ASSERT(declaration);
  }

  switch (kind) {
  case Variable_Kind::_const:
    if (is_global_variable) {
      this->diag_reporter_->report(Diag_Assignment_To_Const_Global_Variable{
          .assignment = assignment.span()});
    } else {
      if (is_assigned_before_declaration) {
        this->diag_reporter_->report(
            Diag_Assignment_To_Const_Variable_Before_Its_Declaration{
                .declaration = declaration->span(),
                .assignment = assignment.span(),
                .var_kind = kind,
            });
      } else {
        this->diag_reporter_->report(Diag_Assignment_To_Const_Variable{
            .declaration = declaration->span(),
            .assignment = assignment.span(),
            .var_kind = kind});
      }
    }
    break;
  case Variable_Kind::_import:
    // Avoid false positive when building GCC 8 Release
    QLJS_WARNING_PUSH
    QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")

    this->diag_reporter_->report(Diag_Assignment_To_Imported_Variable{
        .declaration = declaration->span(),
        .assignment = assignment.span(),
        .var_kind = kind,
    });

    QLJS_WARNING_POP
    break;
  case Variable_Kind::_arrow_parameter:
  case Variable_Kind::_catch:
  case Variable_Kind::_class:
  case Variable_Kind::_function:
  case Variable_Kind::_function_parameter:
  case Variable_Kind::_function_type_parameter:
  // FIXME(strager): Is _index_signature_parameter correct here?
  case Variable_Kind::_index_signature_parameter:
  case Variable_Kind::_let:
  case Variable_Kind::_var:
    if (is_assigned_before_declaration) {
      QLJS_WARNING_PUSH
      QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")

      this->diag_reporter_->report(Diag_Assignment_Before_Variable_Declaration{
          .assignment = assignment.span(),
          .declaration = declaration->span(),
      });

      QLJS_WARNING_POP
    }
    break;
  case Variable_Kind::_interface:
    // Interfaces can't be assigned to.
    QLJS_UNREACHABLE();
    break;
  case Variable_Kind::_enum:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_generic_parameter:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_import_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_import_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_infer_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_namespace:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case Variable_Kind::_type_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  }
}

template <class Declared_Variable_Type>
void Variable_Analyzer::report_errors_for_variable_use(
    const Used_Variable &used_var, const Declared_Variable_Type &declared,
    bool use_is_before_declaration) const {
  constexpr bool declared_in_global_scope =
      std::is_same_v<Declared_Variable_Type, Global_Declared_Variable>;

  if (used_var.kind == Used_Variable_Kind::assignment) {
    this->report_error_if_assignment_is_illegal(
        declared, used_var.name,
        /*is_assigned_before_declaration=*/use_is_before_declaration);
  }

  if (!declared_in_global_scope &&
      used_var.kind == Used_Variable_Kind::_delete) {
    // TODO(strager): What if the variable was parenthesized? We should
    // include the closing parenthesis.
    this->diag_reporter_->report(Diag_Redundant_Delete_Statement_On_Variable{
        .delete_expression = Source_Code_Span(used_var.delete_keyword_begin,
                                              used_var.name.span().end()),
    });
  }
}

void Variable_Analyzer::report_error_if_variable_declaration_conflicts_in_scope(
    const Variable_Analyzer::Scope &scope, Identifier name, Variable_Kind kind,
    Variable_Declaration_Flags flags,
    Variable_Analyzer::Declared_Variable_Scope declaration_scope) const {
  const Declared_Variable *already_declared_variable =
      scope.declared_variables.find(name);
  if (already_declared_variable) {
    this->report_error_if_variable_declaration_conflicts(
        /*already_declared=*/&already_declared_variable->declaration,
        /*already_declared_kind=*/already_declared_variable->kind,
        /*already_declared_flags=*/already_declared_variable->flags,
        /*already_declared_declaration_scope=*/
        already_declared_variable->declaration_scope,
        /*already_declared_is_global_variable=*/false,
        /*newly_declared_name=*/name,
        /*newly_declared_kind=*/kind,
        /*newly_declared_flags=*/flags,
        /*newly_declared_declaration_scope=*/declaration_scope);
  }
}

void Variable_Analyzer::report_error_if_variable_declaration_conflicts_in_scope(
    const Global_Scope &scope, const Declared_Variable &var) const {
  std::optional<Global_Declared_Variable> already_declared_variable =
      scope.declared_variables.find(var.declaration);
  if (already_declared_variable) {
    if (!already_declared_variable->is_shadowable) {
      this->report_error_if_variable_declaration_conflicts(
          /*already_declared=*/nullptr,
          /*already_declared_kind=*/already_declared_variable->kind(),
          /*already_declared_flags=*/already_declared_variable->flags(),
          /*already_declared_declaration_scope=*/
          Declared_Variable_Scope::declared_in_current_scope,
          /*already_declared_is_global_variable=*/true,
          /*newly_declared_name=*/var.declaration,
          /*newly_declared_kind=*/var.kind,
          /*newly_declared_flags=*/var.flags,
          /*newly_declared_declaration_scope=*/var.declaration_scope);
    }
  }
}

void Variable_Analyzer::report_error_if_variable_declaration_conflicts(
    const Identifier *already_declared, Variable_Kind already_declared_kind,
    Variable_Declaration_Flags already_declared_flags,
    Declared_Variable_Scope already_declared_declaration_scope,
    bool already_declared_is_global_variable, Identifier newly_declared_name,
    Variable_Kind newly_declared_kind,
    [[maybe_unused]] Variable_Declaration_Flags newly_declared_flags,
    Declared_Variable_Scope newly_declared_declaration_scope) const {
  using VK = Variable_Kind;
  VK kind = newly_declared_kind;
  VK other_kind = already_declared_kind;

  switch (other_kind) {
  case VK::_catch:
    QLJS_ASSERT(kind != VK::_arrow_parameter);
    QLJS_ASSERT(kind != VK::_function_parameter);
    QLJS_ASSERT(kind != VK::_function_type_parameter);
    QLJS_ASSERT(kind != VK::_import);
    // FIXME(strager): Is _index_signature_parameter correct here?
    QLJS_ASSERT(kind != VK::_index_signature_parameter);
    break;
  case VK::_class:
  case VK::_const:
  case VK::_function:
  case VK::_let:
  case VK::_namespace:
  case VK::_var:
    QLJS_ASSERT(kind != VK::_arrow_parameter);
    QLJS_ASSERT(kind != VK::_catch);
    QLJS_ASSERT(kind != VK::_function_parameter);
    QLJS_ASSERT(kind != VK::_function_type_parameter);
    // FIXME(strager): Is _index_signature_parameter correct here?
    QLJS_ASSERT(kind != VK::_index_signature_parameter);
    break;
  case VK::_arrow_parameter:
  case VK::_function_parameter:
  case VK::_function_type_parameter:
  // FIXME(strager): Is _index_signature_parameter correct here?
  case VK::_index_signature_parameter:
    QLJS_ASSERT(kind != VK::_catch);
    QLJS_ASSERT(kind != VK::_import);
    break;
  case VK::_enum:
  case VK::_generic_parameter:
  case VK::_import:
  case VK::_interface:
    break;
  case VK::_import_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case VK::_import_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case VK::_infer_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case VK::_type_alias:
    break;
  }

  // FIXME(strager): Is _function_type_parameter correct here?
  // FIXME(strager): Is _index_signature_parameter correct here?
  auto is_parameter = [](VK k) {
    return k == VK::_arrow_parameter || k == VK::_function_parameter ||
           k == VK::_function_type_parameter ||
           k == VK::_index_signature_parameter;
  };
  bool kind_is_parameter = is_parameter(kind);
  bool other_kind_is_parameter = is_parameter(other_kind);

  bool redeclaration_ok =
      // clang-format off
      (kind == VK::_class              && other_kind == VK::_interface) ||
      (kind == VK::_const              && other_kind == VK::_namespace) ||
      (kind == VK::_const              && other_kind == VK::_type_alias) ||
      (kind == VK::_enum               && other_kind == VK::_catch) ||
      (kind == VK::_enum               && other_kind == VK::_enum) ||
      (kind == VK::_enum               && other_kind == VK::_namespace) ||
      (kind == VK::_function           && other_kind == VK::_function) ||
      (kind == VK::_function           && other_kind == VK::_var) ||
      (kind == VK::_function           && other_kind_is_parameter) ||
      (kind == VK::_function_parameter && other_kind == VK::_generic_parameter) ||
      (kind == VK::_import             && other_kind == VK::_interface) ||
      (kind == VK::_interface          && other_kind == VK::_class) ||
      (kind == VK::_interface          && other_kind == VK::_import) ||
      (kind == VK::_interface          && other_kind == VK::_interface) ||
      (kind == VK::_interface          && !is_type(other_kind)) ||
      (kind == VK::_let                && other_kind == VK::_namespace) ||
      (kind == VK::_let                && other_kind == VK::_type_alias) ||
      (kind == VK::_namespace          && other_kind == VK::_class) ||
      (kind == VK::_namespace          && other_kind == VK::_const) ||
      (kind == VK::_namespace          && other_kind == VK::_enum) ||
      (kind == VK::_namespace          && other_kind == VK::_function) ||
      (kind == VK::_namespace          && other_kind == VK::_let) ||
      (kind == VK::_namespace          && other_kind == VK::_namespace) ||
      (kind == VK::_namespace          && other_kind == VK::_var) ||
      (kind == VK::_type_alias         && other_kind == VK::_const) ||
      (kind == VK::_type_alias         && other_kind == VK::_let) ||
      (kind == VK::_type_alias         && other_kind == VK::_var) ||
      (kind == VK::_var                && other_kind == VK::_catch) ||
      (kind == VK::_var                && other_kind == VK::_function) ||
      (kind == VK::_var                && other_kind == VK::_namespace) ||
      (kind == VK::_var                && other_kind == VK::_type_alias) ||
      (kind == VK::_var                && other_kind == VK::_var) ||
      (kind == VK::_var                && other_kind_is_parameter) ||
      (kind_is_parameter               && other_kind == VK::_function) ||
      (kind_is_parameter               && other_kind_is_parameter) ||
      (!is_type(kind)                  && other_kind == VK::_interface) ||
      // clang-format on
      (other_kind == VK::_namespace &&
       (kind == VK::_class || kind == VK::_function) &&
       !(already_declared_flags &
         Variable_Declaration_Flags::non_empty_namespace)) ||
      (kind == VK::_function &&
       newly_declared_declaration_scope ==
           Declared_Variable_Scope::declared_in_descendant_scope) ||
      (other_kind == VK::_function &&
       already_declared_declaration_scope ==
           Declared_Variable_Scope::declared_in_descendant_scope) ||
      false;
  if (!redeclaration_ok) {
    if (already_declared_is_global_variable) {
      this->diag_reporter_->report(Diag_Redeclaration_Of_Global_Variable{
          .redeclaration = newly_declared_name.span(),
      });
    } else {
      this->diag_reporter_->report(Diag_Redeclaration_Of_Variable{
          .redeclaration = newly_declared_name.span(),
          .original_declaration = already_declared->span(),
      });
    }
  }
}

bool Variable_Analyzer::Declared_Variable::is_runtime() const {
  return quick_lint_js::is_runtime(kind);
}

bool Variable_Analyzer::Declared_Variable::is_type() const {
  return quick_lint_js::is_type(this->kind);
}

bool Variable_Analyzer::Used_Variable::is_runtime() const {
  switch (this->kind) {
  case Used_Variable_Kind::_delete:
  case Used_Variable_Kind::_export:
  case Used_Variable_Kind::_typeof:
  case Used_Variable_Kind::assignment:
  case Used_Variable_Kind::use:
    return true;
  case Used_Variable_Kind::type:
    return false;
  }
  QLJS_UNREACHABLE();
}

bool Variable_Analyzer::Used_Variable::is_type() const {
  switch (this->kind) {
  case Used_Variable_Kind::_export:
  case Used_Variable_Kind::type:
    return true;
  case Used_Variable_Kind::_delete:
  case Used_Variable_Kind::_typeof:
  case Used_Variable_Kind::assignment:
  case Used_Variable_Kind::use:
    return false;
  }
  QLJS_UNREACHABLE();
}

Variable_Analyzer::Declared_Variable *
Variable_Analyzer::Declared_Variable_Set::add_variable_declaration(
    Identifier name, Variable_Kind kind, Declared_Variable_Scope declared_scope,
    Variable_Declaration_Flags flags) {
  this->variables_.emplace_back(Declared_Variable{
      .declaration = name,
      .kind = kind,
      .declaration_scope = declared_scope,
      .is_used = false,
      .flags = flags,
  });
  return &this->variables_.back();
}

const Variable_Analyzer::Declared_Variable *
Variable_Analyzer::Declared_Variable_Set::find(Identifier name) const {
  return const_cast<Declared_Variable_Set *>(this)->find(name);
}

Variable_Analyzer::Declared_Variable *
Variable_Analyzer::Declared_Variable_Set::find(Identifier name) {
  String8_View name_view = name.normalized_name();
  for (Declared_Variable &var : this->variables_) {
    if (var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

Variable_Analyzer::Declared_Variable *
Variable_Analyzer::Declared_Variable_Set::find_runtime(Identifier name) {
  String8_View name_view = name.normalized_name();
  for (Declared_Variable &var : this->variables_) {
    if (var.is_runtime() && var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

Variable_Analyzer::Declared_Variable *
Variable_Analyzer::Declared_Variable_Set::find_type(Identifier name) {
  String8_View name_view = name.normalized_name();
  for (Declared_Variable &var : this->variables_) {
    if (var.is_type() && var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

void Variable_Analyzer::Declared_Variable_Set::clear() {
  this->variables_.clear();
}

bool Variable_Analyzer::Declared_Variable_Set::empty() const {
  return this->variables_.empty();
}

std::vector<Variable_Analyzer::Declared_Variable>::const_iterator
Variable_Analyzer::Declared_Variable_Set::begin() const {
  return this->variables_.cbegin();
}

std::vector<Variable_Analyzer::Declared_Variable>::const_iterator
Variable_Analyzer::Declared_Variable_Set::end() const {
  return this->variables_.cend();
}

void Variable_Analyzer::Scope::clear() {
  this->declared_variables.clear();
  this->variables_used.clear();
  this->variables_used_in_descendant_scope.clear();
  this->function_expression_declaration.reset();
  this->used_eval_in_this_scope = false;
  this->used_eval_in_descendant_scope = false;
}

Variable_Analyzer::Scopes::Scopes() {
  this->push();  // module_scope
}

Variable_Analyzer::Scope &Variable_Analyzer::Scopes::module_scope() {
  return this->scopes_[0];
}

Variable_Analyzer::Scope &Variable_Analyzer::Scopes::current_scope() {
  QLJS_ASSERT(!this->empty());
  return this->scopes_[narrow_cast<std::size_t>(this->size()) - 1];
}

Variable_Analyzer::Scope &Variable_Analyzer::Scopes::parent_scope() {
  QLJS_ASSERT(this->size() >= 2);
  return this->scopes_[narrow_cast<std::size_t>(this->size()) - 2];
}

Variable_Analyzer::Scope &Variable_Analyzer::Scopes::push() {
  bool full = this->scope_count_ == narrow_cast<int>(this->scopes_.size());
  Scope *s;
  if (full) {
    s = &this->scopes_.emplace_back();
  } else {
    s = &this->scopes_[narrow_cast<std::size_t>(this->scope_count_)];
    s->clear();
  }
  this->scope_count_ += 1;
  return *s;
}

void Variable_Analyzer::Scopes::pop() {
  QLJS_ASSERT(!this->empty());
  this->scope_count_ -= 1;
}

bool Variable_Analyzer::Scopes::empty() const {
  return this->scope_count_ == 0;
}

int Variable_Analyzer::Scopes::size() const { return this->scope_count_; }

namespace {
bool is_runtime(Variable_Kind kind) {
  switch (kind) {
  case Variable_Kind::_arrow_parameter:
  case Variable_Kind::_catch:
  case Variable_Kind::_class:
  case Variable_Kind::_const:
  case Variable_Kind::_enum:
  case Variable_Kind::_function:
  case Variable_Kind::_function_parameter:
  case Variable_Kind::_function_type_parameter:
  case Variable_Kind::_import:
  case Variable_Kind::_import_alias:
  case Variable_Kind::_index_signature_parameter:
  case Variable_Kind::_let:
  case Variable_Kind::_namespace:
  case Variable_Kind::_var:
    return true;
  case Variable_Kind::_generic_parameter:
  case Variable_Kind::_import_type:
  case Variable_Kind::_infer_type:
  case Variable_Kind::_interface:
  case Variable_Kind::_type_alias:
    return false;
  }
  QLJS_UNREACHABLE();
}

bool is_type(Variable_Kind kind) {
  switch (kind) {
  case Variable_Kind::_class:
  case Variable_Kind::_enum:
  case Variable_Kind::_generic_parameter:
  case Variable_Kind::_import:
  case Variable_Kind::_import_alias:
  case Variable_Kind::_import_type:
  case Variable_Kind::_infer_type:
  case Variable_Kind::_interface:
  case Variable_Kind::_namespace:
  case Variable_Kind::_type_alias:
    return true;
  case Variable_Kind::_arrow_parameter:
  case Variable_Kind::_catch:
  case Variable_Kind::_const:
  case Variable_Kind::_function:
  case Variable_Kind::_function_parameter:
  case Variable_Kind::_function_type_parameter:
  case Variable_Kind::_index_signature_parameter:
  case Variable_Kind::_let:
  case Variable_Kind::_var:
    return false;
  }
  QLJS_UNREACHABLE();
}
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
