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

// The variable_analyzer class implements single-pass variable lookup. A
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
// To satisfy these requirements, the variable_analyzer class implements the
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
bool is_runtime(variable_kind) noexcept;
bool is_type(variable_kind) noexcept;
}

variable_analyzer::variable_analyzer(
    diag_reporter *diag_reporter,
    const global_declared_variable_set *global_variables,
    variable_analyzer_options options)
    : global_scope_(global_variables),
      diag_reporter_(diag_reporter),
      options_(options) {}

void variable_analyzer::visit_enter_block_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_with_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_class_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_class_scope_body(
    const std::optional<identifier> &class_name) {
  if (class_name.has_value()) {
    this->declare_variable(
        /*scope=*/this->current_scope(),
        /*name=*/*class_name,
        /*kind=*/variable_kind::_class,
        /*declared_scope=*/declared_variable_scope::declared_in_current_scope,
        /*declaration_possibly_looks_like_assignment=*/false);
  }
}

void variable_analyzer::visit_enter_conditional_type_scope() {
  this->scopes_.push();
}

void variable_analyzer::visit_enter_enum_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_for_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_function_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_function_scope_body() {
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
}

void variable_analyzer::visit_enter_index_signature_scope() {
  this->scopes_.push();
}

void variable_analyzer::visit_enter_interface_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_named_function_scope(
    identifier function_name) {
  scope &current_scope = this->scopes_.push();
  current_scope.function_expression_declaration = declared_variable{
      .declaration = function_name,
      .kind = variable_kind::_function,
      .declaration_scope = declared_variable_scope::declared_in_current_scope,
      .is_used = false,
      .declaration_possibly_looks_like_assignment = false,
  };
}

void variable_analyzer::visit_enter_namespace_scope() { this->scopes_.push(); }

void variable_analyzer::visit_enter_type_alias_scope() { this->scopes_.push(); }

void variable_analyzer::visit_exit_block_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_with_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // Don't propagate variable uses, only declarations
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_class_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_conditional_type_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_enum_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // TODO(#756): For now, we don't propagate variable uses. We should declare
  // enum members so we can find typos in enum initializers.
  QLJS_ASSERT(this->current_scope().declared_variables.empty());
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_for_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->propagate_variable_declarations_to_parent_scope();
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_function_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_index_signature_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/true,
      /*consume_arguments=*/true);
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_interface_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_namespace_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  // Do not propagate variable uses. Namespaced code can see variables exported
  // from other namespace blocks in other files (which we can't see), so assume
  // that all undeclared variables might exist.
  this->scopes_.pop();
}

void variable_analyzer::visit_exit_type_alias_scope() {
  QLJS_ASSERT(!this->scopes_.empty());
  this->propagate_variable_uses_to_parent_scope(
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);
  this->scopes_.pop();
}

void variable_analyzer::visit_keyword_variable_use(identifier) {
  // Ignore. The parser should have already reported E0023.
}

void variable_analyzer::visit_property_declaration(
    const std::optional<identifier> &) {}

void variable_analyzer::visit_variable_declaration(
    identifier name, variable_kind kind, variable_init_kind init_kind) {
  this->declare_variable(
      /*scope=*/this->current_scope(),
      /*name=*/name,
      /*kind=*/kind,
      /*declared_scope=*/declared_variable_scope::declared_in_current_scope,
      /*declaration_possibly_looks_like_assignment=*/init_kind ==
          variable_init_kind::initialized_with_equals);
}

void variable_analyzer::declare_variable(
    scope &scope, identifier name, variable_kind kind,
    declared_variable_scope declared_scope,
    bool declaration_possibly_looks_like_assignment) {
  bool is_function_or_var =
      kind == variable_kind::_function || kind == variable_kind::_var;
  if (declared_scope == declared_variable_scope::declared_in_descendant_scope) {
    QLJS_ASSERT(is_function_or_var);
  }

  this->report_error_if_variable_declaration_conflicts_in_scope(
      scope, name, kind, declared_scope);

  if (is_function_or_var && name.normalized_name() == u8"eval"_sv) {
    scope.used_eval_in_this_scope = false;
  }

  declared_variable *declared =
      scope.declared_variables.add_variable_declaration(
          name, kind, declared_scope,
          /*declaration_possibly_looks_like_assignment=*/
          declaration_possibly_looks_like_assignment);

  erase_if(scope.variables_used, [&](const used_variable &used_var) {
    if (name.normalized_name() != used_var.name.normalized_name()) {
      return false;
    }
    declared->is_used = true;
    if (kind == variable_kind::_function &&
        declared_scope ==
            declared_variable_scope::declared_in_descendant_scope &&
        used_var.kind == used_variable_kind::use) {
      this->diag_reporter_->report(
          diag_function_call_before_declaration_in_block_scope{used_var.name,
                                                               name});
    }
    this->report_errors_for_variable_use(
        used_var, *declared,
        /*use_is_before_declaration=*/kind == variable_kind::_class ||
            kind == variable_kind::_const || kind == variable_kind::_let);
    switch (used_var.kind) {
    case used_variable_kind::assignment:
      break;
    case used_variable_kind::_typeof:
    case used_variable_kind::use:
      if (kind == variable_kind::_class || kind == variable_kind::_const ||
          kind == variable_kind::_let) {
        this->diag_reporter_->report(
            diag_variable_used_before_declaration{used_var.name, name});
      }
      break;
    case used_variable_kind::_delete:
      // Use before declaration is legal for delete.
      break;
    case used_variable_kind::_export:
      // Use before declaration is legal for variable exports.
      break;
    case used_variable_kind::type:
      if (kind == variable_kind::_generic_parameter) {
        this->diag_reporter_->report(diag_variable_used_before_declaration{
            .use = used_var.name,
            .declaration = name,
        });
      }
      // Use before declaration is normally legal for types.
      break;
    }
    return true;
  });
  erase_if(scope.variables_used_in_descendant_scope,
           [&](const used_variable &used_var) {
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
             case used_variable_kind::assignment:
               if (declared->is_runtime()) {
                 declared->is_used = true;
               }
               break;
             case used_variable_kind::_export:
               // TODO(strager): This shouldn't happen. export statements are
               // not allowed inside functions.
               break;
             case used_variable_kind::_delete:
             case used_variable_kind::_typeof:
             case used_variable_kind::use:
               declared->is_used = true;
               break;
             case used_variable_kind::type:
               // TODO(strager): Do we need to set declared->is_used?
               break;
             }
             return true;
           });
}

void variable_analyzer::visit_variable_assignment(identifier name) {
  QLJS_ASSERT(!this->scopes_.empty());
  scope &current_scope = this->current_scope();
  declared_variable *var = current_scope.declared_variables.find_runtime(name);
  if (var) {
    var->is_used = true;
    this->report_error_if_assignment_is_illegal(
        var, name, /*is_assigned_before_declaration=*/false);
  } else {
    current_scope.variables_used.emplace_back(name,
                                              used_variable_kind::assignment);
  }
}

void variable_analyzer::visit_variable_delete_use(
    identifier name, source_code_span delete_keyword) {
  QLJS_ASSERT(delete_keyword.end() <= name.span().begin());

  if (this->options_.allow_deleting_typescript_variable) {
    QLJS_ASSERT(!this->scopes_.empty());
    scope &current_scope = this->current_scope();

    used_variable used_var(name, used_variable_kind::_delete,
                           delete_keyword.begin());
    declared_variable *already_declared =
        current_scope.declared_variables.find_runtime(name);
    if (already_declared) {
      this->report_errors_for_variable_use(used_var, *already_declared,
                                           /*use_is_before_declaration=*/false);
    } else {
      current_scope.variables_used.push_back(std::move(used_var));
    }
  } else {
    this->diag_reporter_->report(diag_typescript_delete_cannot_delete_variables{
        .delete_expression =
            source_code_span(delete_keyword.begin(), name.span().end()),
    });
  }
}

void variable_analyzer::visit_variable_export_use(identifier name) {
  this->visit_variable_use(name, used_variable_kind::_export);
}

void variable_analyzer::visit_variable_namespace_use(identifier) {
  // TODO(#690): Look up TypeScript namespace variables and imports.
}

void variable_analyzer::visit_variable_type_predicate_use(identifier name) {
  // TODO(#690)
  static_cast<void>(name);

  QLJS_ASSERT(!this->scopes_.empty());
  scope &current_scope = this->current_scope();
  declared_variable *var = current_scope.declared_variables.find_runtime(name);
  if (var) {
    // FIXME(strager): Should we mark the parameter as used?
  } else {
    this->diag_reporter_->report(
        diag_use_of_undeclared_parameter_in_type_predicate{
            .name = name,
        });
  }
}

void variable_analyzer::visit_variable_type_use(identifier name) {
  this->visit_variable_use(name, used_variable_kind::type);
}

void variable_analyzer::visit_variable_typeof_use(identifier name) {
  this->visit_variable_use(name, used_variable_kind::_typeof);
}

void variable_analyzer::visit_variable_use(identifier name) {
  this->visit_variable_use(name, used_variable_kind::use);
}

void variable_analyzer::visit_variable_use(identifier name,
                                           used_variable_kind use_kind) {
  QLJS_ASSERT(!this->scopes_.empty());
  scope &current_scope = this->current_scope();
  declared_variable *var =
      use_kind == used_variable_kind::type
          ? current_scope.declared_variables.find_type(name)
          : use_kind == used_variable_kind::_export
                ? current_scope.declared_variables.find(name)
                : current_scope.declared_variables.find_runtime(name);
  if (var) {
    var->is_used = true;
  } else {
    current_scope.variables_used.emplace_back(name, use_kind);
    if (name.normalized_name() == u8"eval"_sv) {
      current_scope.used_eval_in_this_scope = true;
    }
  }
}

void variable_analyzer::visit_end_of_module() {
  // We expect only the module scope.
  QLJS_ASSERT(this->scopes_.size() == 1);

  variable_analyzer::global_scope &global_scope = this->global_scope_;

  for (const declared_variable &var :
       this->scopes_.module_scope().declared_variables) {
    this->report_error_if_variable_declaration_conflicts_in_scope(global_scope,
                                                                  var);
  }

  this->propagate_variable_uses_to_parent_scope(
      /*parent_scope=*/global_scope,
      /*allow_variable_use_before_declaration=*/false,
      /*consume_arguments=*/false);

  std::vector<identifier> typeof_variables;
  for (const used_variable &used_var : global_scope.variables_used) {
    if (used_var.kind == used_variable_kind::_typeof) {
      typeof_variables.emplace_back(used_var.name);
    }
  }
  for (const used_variable &used_var :
       global_scope.variables_used_in_descendant_scope) {
    if (used_var.kind == used_variable_kind::_typeof) {
      typeof_variables.emplace_back(used_var.name);
    }
  }
  auto is_variable_declared_by_typeof = [&](const used_variable &var) -> bool {
    return any_of(typeof_variables, [&](const identifier &typeof_variable) {
      return typeof_variable.normalized_name() == var.name.normalized_name();
    });
  };
  auto is_variable_declared = [&](const used_variable &var) -> bool {
    // If a variable appears in declared_variables, then
    // propagate_variable_uses_to_parent_scope should have already removed it
    // from variables_used and variables_used_in_descendant_scope.
    switch (var.kind) {
    case used_variable_kind::_export:
    case used_variable_kind::_delete:
    case used_variable_kind::_typeof:
    case used_variable_kind::assignment:
    case used_variable_kind::use:
      QLJS_ASSERT(!global_scope.declared_variables.find_runtime(var.name));
      break;
    case used_variable_kind::type:
      QLJS_ASSERT(!global_scope.declared_variables.find_type(var.name));
      break;
    }

    // TODO(#690): This should not affect type uses.
    return is_variable_declared_by_typeof(var);
  };

  auto check_if_variable_is_undeclared =
      [&](const used_variable &used_var) -> void {
    if (!is_variable_declared(used_var)) {
      switch (used_var.kind) {
      case used_variable_kind::assignment:
        this->diag_reporter_->report(
            diag_assignment_to_undeclared_variable{used_var.name});
        break;
      case used_variable_kind::_delete:
        // TODO(strager): Report a warning if the global variable is not
        // deletable.
        break;
      case used_variable_kind::type:
        this->diag_reporter_->report(
            diag_use_of_undeclared_type{used_var.name});
        break;
      case used_variable_kind::_export:
      case used_variable_kind::use:
        this->diag_reporter_->report(
            diag_use_of_undeclared_variable{used_var.name});
        break;
      case used_variable_kind::_typeof:
        // 'typeof foo' is often used to detect if the variable 'foo' is
        // declared. Do not report that the variable is undeclared.
        break;
      }
    }
  };
  for (const used_variable &used_var : global_scope.variables_used) {
    check_if_variable_is_undeclared(used_var);
  }
  for (const used_variable &used_var :
       global_scope.variables_used_in_descendant_scope) {
    check_if_variable_is_undeclared(used_var);
  }
}

void variable_analyzer::propagate_variable_uses_to_parent_scope(
    bool allow_variable_use_before_declaration, bool consume_arguments) {
  this->propagate_variable_uses_to_parent_scope(
      /*parent_scope=*/this->parent_scope(),
      /*allow_variable_use_before_declaration=*/
      allow_variable_use_before_declaration,
      /*consume_arguments=*/consume_arguments);
}

template <class Scope>
void variable_analyzer::propagate_variable_uses_to_parent_scope(
    Scope &parent_scope, bool allow_variable_use_before_declaration,
    bool consume_arguments) {
  // found_variable_type is either declared_variable* or
  // std::optional<global_declared_variable>.
  using found_variable_type = typename std::decay_t<decltype(
      Scope::declared_variables)>::found_variable_type;

  constexpr bool parent_scope_is_global_scope =
      std::is_same_v<Scope, global_scope>;

  scope &current_scope = this->current_scope();

  auto is_current_scope_function_name = [&](const used_variable &var) {
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
    for (const used_variable &used_var : current_scope.variables_used) {
      found_variable_type var = {};
      switch (used_var.kind) {
      case used_variable_kind::_export:
        QLJS_ASSERT(!current_scope.declared_variables.find(used_var.name));
        var = parent_scope.declared_variables.find(used_var.name);
        break;
      case used_variable_kind::_delete:
      case used_variable_kind::_typeof:
      case used_variable_kind::assignment:
      case used_variable_kind::use:
        QLJS_ASSERT(
            !current_scope.declared_variables.find_runtime(used_var.name));
        var = parent_scope.declared_variables.find_runtime(used_var.name);
        break;
      case used_variable_kind::type:
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

    for (const used_variable &used_var :
         current_scope.variables_used_in_descendant_scope) {
      found_variable_type var = {};
      switch (used_var.kind) {
      case used_variable_kind::_export:
        var = parent_scope.declared_variables.find(used_var.name);
        break;
      case used_variable_kind::_delete:
      case used_variable_kind::_typeof:
      case used_variable_kind::assignment:
      case used_variable_kind::use:
        var = parent_scope.declared_variables.find_runtime(used_var.name);
        break;
      case used_variable_kind::type:
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

void variable_analyzer::propagate_variable_declarations_to_parent_scope() {
  scope &current_scope = this->current_scope();
  scope &parent_scope = this->parent_scope();

  for (const declared_variable &var : current_scope.declared_variables) {
    if (var.kind == variable_kind::_function ||
        var.kind == variable_kind::_var) {
      this->declare_variable(
          /*scope=*/parent_scope,
          /*name=*/var.declaration,
          /*kind=*/var.kind,
          /*declared_scope=*/
          declared_variable_scope::declared_in_descendant_scope,
          /*declaration_possibly_looks_like_assignment=*/
          var.declaration_possibly_looks_like_assignment);
    }

    if (var.declaration_possibly_looks_like_assignment && !var.is_used &&
        (var.kind == variable_kind::_const ||
         var.kind == variable_kind::_let) &&
        !(current_scope.used_eval_in_this_scope ||
          current_scope.used_eval_in_descendant_scope)) {
      // TODO(strager): NOTE[unused-var-shadows-nested-block]: Check multiple
      // parent scopes, not just the immediate parent.
      const declared_variable *already_declared_variable =
          parent_scope.declared_variables.find(var.declaration);
      if (already_declared_variable &&
          (already_declared_variable->kind == variable_kind::_const ||
           already_declared_variable->kind == variable_kind::_let ||
           already_declared_variable->kind == variable_kind::_var)) {
        this->diag_reporter_->report(diag_unused_variable_shadows{
            .shadowing_declaration = var.declaration,
            .shadowed_declaration = already_declared_variable->declaration,
        });
      }
    }
  }
}

void variable_analyzer::report_error_if_assignment_is_illegal(
    const declared_variable *var, const identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(*var, assignment,
                                              is_assigned_before_declaration);
}

void variable_analyzer::report_error_if_assignment_is_illegal(
    const declared_variable &var, const identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(
      /*kind=*/var.kind,
      /*is_global_variable=*/false,
      /*declaration=*/&var.declaration,
      /*assignment=*/assignment,
      /*is_assigned_before_declaration=*/is_assigned_before_declaration);
}

void variable_analyzer::report_error_if_assignment_is_illegal(
    const global_declared_variable &var, const identifier &assignment,
    bool is_assigned_before_declaration) const {
  this->report_error_if_assignment_is_illegal(
      /*kind=*/var.kind(),
      /*is_global_variable=*/true,
      /*declaration=*/nullptr,
      /*assignment=*/assignment,
      /*is_assigned_before_declaration=*/is_assigned_before_declaration);
}

void variable_analyzer::report_error_if_assignment_is_illegal(
    variable_kind kind, bool is_global_variable, const identifier *declaration,
    const identifier &assignment, bool is_assigned_before_declaration) const {
  if (is_global_variable) {
    QLJS_ASSERT(!declaration);
  } else {
    QLJS_ASSERT(declaration);
  }

  switch (kind) {
  case variable_kind::_const:
    if (is_global_variable) {
      this->diag_reporter_->report(
          diag_assignment_to_const_global_variable{assignment});
    } else {
      if (is_assigned_before_declaration) {
        this->diag_reporter_->report(
            diag_assignment_to_const_variable_before_its_declaration{
                *declaration, assignment, kind});
      } else {
        this->diag_reporter_->report(
            diag_assignment_to_const_variable{*declaration, assignment, kind});
      }
    }
    break;
  case variable_kind::_import:
    // Avoid false positive when building GCC 8 Release
    QLJS_WARNING_PUSH
    QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")

    this->diag_reporter_->report(
        diag_assignment_to_imported_variable{*declaration, assignment, kind});

    QLJS_WARNING_POP
    break;
  case variable_kind::_arrow_parameter:
  case variable_kind::_catch:
  case variable_kind::_class:
  case variable_kind::_function:
  case variable_kind::_function_parameter:
  case variable_kind::_function_type_parameter:
  // FIXME(strager): Is _index_signature_parameter correct here?
  case variable_kind::_index_signature_parameter:
  case variable_kind::_let:
  case variable_kind::_var:
    if (is_assigned_before_declaration) {
      QLJS_WARNING_PUSH
      QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")

      this->diag_reporter_->report(diag_assignment_before_variable_declaration{
          .assignment = assignment, .declaration = *declaration});

      QLJS_WARNING_POP
    }
    break;
  case variable_kind::_interface:
    // Interfaces can't be assigned to.
    QLJS_UNREACHABLE();
    break;
  case variable_kind::_enum:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_generic_parameter:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_import_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_import_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_infer_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_namespace:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case variable_kind::_type_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  }
}

template <class DeclaredVariableType>
void variable_analyzer::report_errors_for_variable_use(
    const used_variable &used_var, const DeclaredVariableType &declared,
    bool use_is_before_declaration) const {
  constexpr bool declared_in_global_scope =
      std::is_same_v<DeclaredVariableType, global_declared_variable>;

  if (used_var.kind == used_variable_kind::assignment) {
    this->report_error_if_assignment_is_illegal(
        declared, used_var.name,
        /*is_assigned_before_declaration=*/use_is_before_declaration);
  }

  if (!declared_in_global_scope &&
      used_var.kind == used_variable_kind::_delete) {
    // TODO(strager): What if the variable was parenthesized? We should
    // include the closing parenthesis.
    this->diag_reporter_->report(diag_redundant_delete_statement_on_variable{
        .delete_expression = source_code_span(used_var.delete_keyword_begin,
                                              used_var.name.span().end()),
    });
  }
}

void variable_analyzer::report_error_if_variable_declaration_conflicts_in_scope(
    const variable_analyzer::scope &scope, identifier name, variable_kind kind,
    variable_analyzer::declared_variable_scope declaration_scope) const {
  const declared_variable *already_declared_variable =
      scope.declared_variables.find(name);
  if (already_declared_variable) {
    this->report_error_if_variable_declaration_conflicts(
        /*already_declared=*/&already_declared_variable->declaration,
        /*already_declared_kind=*/already_declared_variable->kind,
        /*already_declared_declaration_scope=*/
        already_declared_variable->declaration_scope,
        /*already_declared_is_global_variable=*/false,
        /*newly_declared_name=*/name,
        /*newly_declared_kind=*/kind,
        /*newly_declared_declaration_scope=*/declaration_scope);
  }
}

void variable_analyzer::report_error_if_variable_declaration_conflicts_in_scope(
    const global_scope &scope, const declared_variable &var) const {
  std::optional<global_declared_variable> already_declared_variable =
      scope.declared_variables.find(var.declaration);
  if (already_declared_variable) {
    if (!already_declared_variable->is_shadowable) {
      this->report_error_if_variable_declaration_conflicts(
          /*already_declared=*/nullptr,
          /*already_declared_kind=*/already_declared_variable->kind(),
          /*already_declared_declaration_scope=*/
          declared_variable_scope::declared_in_current_scope,
          /*already_declared_is_global_variable=*/true,
          /*newly_declared_name=*/var.declaration,
          /*newly_declared_kind=*/var.kind,
          /*newly_declared_declaration_scope=*/var.declaration_scope);
    }
  }
}

void variable_analyzer::report_error_if_variable_declaration_conflicts(
    const identifier *already_declared, variable_kind already_declared_kind,
    declared_variable_scope already_declared_declaration_scope,
    bool already_declared_is_global_variable, identifier newly_declared_name,
    variable_kind newly_declared_kind,
    declared_variable_scope newly_declared_declaration_scope) const {
  using vk = variable_kind;
  vk kind = newly_declared_kind;
  vk other_kind = already_declared_kind;

  switch (other_kind) {
  case vk::_catch:
    QLJS_ASSERT(kind != vk::_arrow_parameter);
    QLJS_ASSERT(kind != vk::_function_parameter);
    QLJS_ASSERT(kind != vk::_function_type_parameter);
    QLJS_ASSERT(kind != vk::_import);
    // FIXME(strager): Is _index_signature_parameter correct here?
    QLJS_ASSERT(kind != vk::_index_signature_parameter);
    break;
  case vk::_class:
  case vk::_const:
  case vk::_function:
  case vk::_let:
  case vk::_var:
    QLJS_ASSERT(kind != vk::_arrow_parameter);
    QLJS_ASSERT(kind != vk::_catch);
    QLJS_ASSERT(kind != vk::_function_parameter);
    QLJS_ASSERT(kind != vk::_function_type_parameter);
    // FIXME(strager): Is _index_signature_parameter correct here?
    QLJS_ASSERT(kind != vk::_index_signature_parameter);
    break;
  case vk::_arrow_parameter:
  case vk::_function_parameter:
  case vk::_function_type_parameter:
  // FIXME(strager): Is _index_signature_parameter correct here?
  case vk::_index_signature_parameter:
    QLJS_ASSERT(kind != vk::_catch);
    QLJS_ASSERT(kind != vk::_import);
    break;
  case vk::_enum:
  case vk::_generic_parameter:
  case vk::_import:
  case vk::_interface:
    break;
  case vk::_import_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case vk::_import_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case vk::_infer_type:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case vk::_namespace:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  case vk::_type_alias:
    QLJS_UNIMPLEMENTED();  // TODO(#690)
    break;
  }

  // FIXME(strager): Is _function_type_parameter correct here?
  // FIXME(strager): Is _index_signature_parameter correct here?
  auto is_parameter = [](vk k) {
    return k == vk::_arrow_parameter || k == vk::_function_parameter ||
           k == vk::_function_type_parameter ||
           k == vk::_index_signature_parameter;
  };
  bool kind_is_parameter = is_parameter(kind);
  bool other_kind_is_parameter = is_parameter(other_kind);

  bool redeclaration_ok =
      (other_kind == vk::_function && kind_is_parameter) ||
      (other_kind == vk::_function && kind == vk::_function) ||
      (other_kind_is_parameter && kind == vk::_function) ||
      (other_kind == vk::_var && kind == vk::_function) ||
      (other_kind_is_parameter && kind_is_parameter) ||
      (other_kind == vk::_catch && kind == vk::_enum) ||
      (other_kind == vk::_catch && kind == vk::_var) ||
      (other_kind == vk::_function && kind == vk::_var) ||
      (other_kind_is_parameter && kind == vk::_var) ||
      (other_kind == vk::_var && kind == vk::_var) ||
      (other_kind == vk::_function &&
       already_declared_declaration_scope ==
           declared_variable_scope::declared_in_descendant_scope) ||
      (kind == vk::_function &&
       newly_declared_declaration_scope ==
           declared_variable_scope::declared_in_descendant_scope) ||
      (other_kind == vk::_interface && kind == vk::_interface) ||
      (other_kind == vk::_interface && kind == vk::_class) ||
      (other_kind == vk::_class && kind == vk::_interface) ||
      (other_kind == vk::_interface && kind == vk::_import) ||
      (other_kind == vk::_import && kind == vk::_interface) ||
      (other_kind == vk::_interface && !is_type(kind)) ||
      (!is_type(other_kind) && kind == vk::_interface) ||
      (other_kind == vk::_enum && kind == vk::_enum);
  if (!redeclaration_ok) {
    if (already_declared_is_global_variable) {
      this->diag_reporter_->report(
          diag_redeclaration_of_global_variable{newly_declared_name});
    } else {
      this->diag_reporter_->report(diag_redeclaration_of_variable{
          newly_declared_name, *already_declared});
    }
  }
}

bool variable_analyzer::declared_variable::is_runtime() const noexcept {
  return quick_lint_js::is_runtime(kind);
}

bool variable_analyzer::declared_variable::is_type() const noexcept {
  return quick_lint_js::is_type(this->kind);
}

bool variable_analyzer::used_variable::is_runtime() const noexcept {
  switch (this->kind) {
  case used_variable_kind::_delete:
  case used_variable_kind::_export:
  case used_variable_kind::_typeof:
  case used_variable_kind::assignment:
  case used_variable_kind::use:
    return true;
  case used_variable_kind::type:
    return false;
  }
  QLJS_UNREACHABLE();
}

bool variable_analyzer::used_variable::is_type() const noexcept {
  switch (this->kind) {
  case used_variable_kind::_export:
  case used_variable_kind::type:
    return true;
  case used_variable_kind::_delete:
  case used_variable_kind::_typeof:
  case used_variable_kind::assignment:
  case used_variable_kind::use:
    return false;
  }
  QLJS_UNREACHABLE();
}

variable_analyzer::declared_variable *
variable_analyzer::declared_variable_set::add_variable_declaration(
    identifier name, variable_kind kind, declared_variable_scope declared_scope,
    bool declaration_possibly_looks_like_assignment) {
  this->variables_.emplace_back(declared_variable{
      .declaration = name,
      .kind = kind,
      .declaration_scope = declared_scope,
      .is_used = false,
      .declaration_possibly_looks_like_assignment =
          declaration_possibly_looks_like_assignment,
  });
  return &this->variables_.back();
}

const variable_analyzer::declared_variable *
variable_analyzer::declared_variable_set::find(identifier name) const noexcept {
  return const_cast<declared_variable_set *>(this)->find(name);
}

variable_analyzer::declared_variable *
variable_analyzer::declared_variable_set::find(identifier name) noexcept {
  string8_view name_view = name.normalized_name();
  for (declared_variable &var : this->variables_) {
    if (var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

variable_analyzer::declared_variable *
variable_analyzer::declared_variable_set::find_runtime(
    identifier name) noexcept {
  string8_view name_view = name.normalized_name();
  for (declared_variable &var : this->variables_) {
    if (var.is_runtime() && var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

variable_analyzer::declared_variable *
variable_analyzer::declared_variable_set::find_type(identifier name) noexcept {
  string8_view name_view = name.normalized_name();
  for (declared_variable &var : this->variables_) {
    if (var.is_type() && var.declaration.normalized_name() == name_view) {
      return &var;
    }
  }
  return nullptr;
}

void variable_analyzer::declared_variable_set::clear() noexcept {
  this->variables_.clear();
}

bool variable_analyzer::declared_variable_set::empty() const noexcept {
  return this->variables_.empty();
}

std::vector<variable_analyzer::declared_variable>::const_iterator
variable_analyzer::declared_variable_set::begin() const noexcept {
  return this->variables_.cbegin();
}

std::vector<variable_analyzer::declared_variable>::const_iterator
variable_analyzer::declared_variable_set::end() const noexcept {
  return this->variables_.cend();
}

void variable_analyzer::scope::clear() {
  this->declared_variables.clear();
  this->variables_used.clear();
  this->variables_used_in_descendant_scope.clear();
  this->function_expression_declaration.reset();
  this->used_eval_in_this_scope = false;
  this->used_eval_in_descendant_scope = false;
}

variable_analyzer::scopes::scopes() {
  this->push();  // module_scope
}

variable_analyzer::scope &variable_analyzer::scopes::module_scope() noexcept {
  return this->scopes_[0];
}

variable_analyzer::scope &variable_analyzer::scopes::current_scope() noexcept {
  QLJS_ASSERT(!this->empty());
  return this->scopes_[narrow_cast<std::size_t>(this->size()) - 1];
}

variable_analyzer::scope &variable_analyzer::scopes::parent_scope() noexcept {
  QLJS_ASSERT(this->size() >= 2);
  return this->scopes_[narrow_cast<std::size_t>(this->size()) - 2];
}

variable_analyzer::scope &variable_analyzer::scopes::push() {
  bool full = this->scope_count_ == narrow_cast<int>(this->scopes_.size());
  scope *s;
  if (full) {
    s = &this->scopes_.emplace_back();
  } else {
    s = &this->scopes_[narrow_cast<std::size_t>(this->scope_count_)];
    s->clear();
  }
  this->scope_count_ += 1;
  return *s;
}

void variable_analyzer::scopes::pop() {
  QLJS_ASSERT(!this->empty());
  this->scope_count_ -= 1;
}

bool variable_analyzer::scopes::empty() const noexcept {
  return this->scope_count_ == 0;
}

int variable_analyzer::scopes::size() const noexcept {
  return this->scope_count_;
}

namespace {
bool is_runtime(variable_kind kind) noexcept {
  switch (kind) {
  case variable_kind::_arrow_parameter:
  case variable_kind::_catch:
  case variable_kind::_class:
  case variable_kind::_const:
  case variable_kind::_enum:
  case variable_kind::_function:
  case variable_kind::_function_parameter:
  case variable_kind::_function_type_parameter:
  case variable_kind::_import:
  case variable_kind::_import_alias:
  case variable_kind::_index_signature_parameter:
  case variable_kind::_let:
  case variable_kind::_namespace:
  case variable_kind::_var:
    return true;
  case variable_kind::_generic_parameter:
  case variable_kind::_import_type:
  case variable_kind::_infer_type:
  case variable_kind::_interface:
  case variable_kind::_type_alias:
    return false;
  }
  QLJS_UNREACHABLE();
}

bool is_type(variable_kind kind) noexcept {
  switch (kind) {
  case variable_kind::_class:
  case variable_kind::_enum:
  case variable_kind::_generic_parameter:
  case variable_kind::_import:
  case variable_kind::_import_alias:
  case variable_kind::_import_type:
  case variable_kind::_infer_type:
  case variable_kind::_interface:
  case variable_kind::_namespace:
  case variable_kind::_type_alias:
    return true;
  case variable_kind::_arrow_parameter:
  case variable_kind::_catch:
  case variable_kind::_const:
  case variable_kind::_function:
  case variable_kind::_function_parameter:
  case variable_kind::_function_type_parameter:
  case variable_kind::_index_signature_parameter:
  case variable_kind::_let:
  case variable_kind::_var:
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
