// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_LINT_H
#define QUICK_LINT_JS_LINT_H

#include <algorithm>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class linter {
 public:
  explicit linter(error_reporter *error_reporter);

  void visit_enter_block_scope() { this->scopes_.emplace_back(); }

  void visit_enter_class_scope() {}

  void visit_enter_for_scope() { this->scopes_.emplace_back(); }

  void visit_enter_function_scope() { this->scopes_.emplace_back(); }

  void visit_enter_named_function_scope(identifier) {
    // TODO(strager): Declare the given identifier.
    this->scopes_.emplace_back();
  }

  void visit_exit_block_scope() {
    assert(!this->scopes_.empty());
    this->propagate_variable_uses_to_parent_scope(
        /*allow_variable_use_before_declaration=*/false);
    this->propagate_variable_declarations_to_parent_scope();
    this->scopes_.pop_back();
  }

  void visit_exit_class_scope() {}

  void visit_exit_for_scope() {
    QLJS_ASSERT(!this->scopes_.empty());
    this->propagate_variable_uses_to_parent_scope(
        /*allow_variable_use_before_declaration=*/false);
    this->propagate_variable_declarations_to_parent_scope();
    this->scopes_.pop_back();
  }

  void visit_exit_function_scope() {
    QLJS_ASSERT(!this->scopes_.empty());
    this->propagate_variable_uses_to_parent_scope(
        /*allow_variable_use_before_declaration=*/true);
    this->scopes_.pop_back();
  }

  void visit_property_declaration(identifier) {}

  void visit_variable_declaration(identifier name, variable_kind kind) {
    scope &current_scope = this->scopes_.back();

    this->report_error_if_variable_declaration_conflicts_in_scope(
        current_scope, name, kind,
        declared_variable_scope::declared_in_current_scope);

    current_scope.declared_variables.emplace_back(
        declared_variable{string8(name.string_view()), kind, name,
                          declared_variable_scope::declared_in_current_scope});

    auto erase_if = [](auto &variables, auto predicate) {
      variables.erase(
          std::remove_if(variables.begin(), variables.end(), predicate),
          variables.end());
    };
    erase_if(current_scope.variables_used, [&](const used_variable &used_var) {
      if (name.string_view() == used_var.name.string_view()) {
        if (kind == variable_kind::_class || kind == variable_kind::_const ||
            kind == variable_kind::_let) {
          switch (used_var.kind) {
            case used_variable_kind::assignment:
              // TODO(strager): Should we also report an error when assigning to
              // a const variable?
              this->error_reporter_
                  ->report_error_assignment_to_undeclared_variable(
                      used_var.name);
              break;
            case used_variable_kind::use:
              this->error_reporter_
                  ->report_error_variable_used_before_declaration(used_var.name,
                                                                  name);
              break;
          }
        }
        return true;
      } else {
        return false;
      }
    });
    erase_if(current_scope.variables_used_in_descendant_scope,
             [&](const used_variable &used_var) {
               // TODO(strager): Should we check used_var.kind?
               return name.string_view() == used_var.name.string_view();
             });
  }

  void visit_variable_assignment(identifier name) {
    QLJS_ASSERT(!this->scopes_.empty());
    scope &current_scope = this->scopes_.back();
    const declared_variable *var = current_scope.find_declared_variable(name);
    if (var) {
      switch (var->kind) {
        case variable_kind::_const:
        case variable_kind::_import:
          if (var->declaration.has_value()) {
            this->error_reporter_->report_error_assignment_to_const_variable(
                *var->declaration, name, var->kind);
          } else {
            this->error_reporter_
                ->report_error_assignment_to_const_global_variable(name);
          }
          break;
        case variable_kind::_catch:
        case variable_kind::_class:
        case variable_kind::_function:
        case variable_kind::_let:
        case variable_kind::_parameter:
        case variable_kind::_var:
          break;
      }
    } else {
      current_scope.variables_used.emplace_back(name,
                                                used_variable_kind::assignment);
    }
  }

  void visit_variable_use(identifier name) {
    QLJS_ASSERT(!this->scopes_.empty());
    scope &current_scope = this->scopes_.back();
    bool variable_is_declared =
        current_scope.find_declared_variable(name) != nullptr;
    if (!variable_is_declared) {
      current_scope.variables_used.emplace_back(name, used_variable_kind::use);
    }
  }

  void visit_end_of_module() {
    for (const used_variable &used_var : this->scopes_.back().variables_used) {
      const declared_variable *var =
          this->find_declared_variable(used_var.name);
      if (!var) {
        switch (used_var.kind) {
          case used_variable_kind::assignment:
            this->error_reporter_
                ->report_error_assignment_to_undeclared_variable(used_var.name);
            break;
          case used_variable_kind::use:
            this->error_reporter_->report_error_use_of_undeclared_variable(
                used_var.name);
            break;
        }
      }
    }
    for (const used_variable &used_var :
         this->scopes_.back().variables_used_in_descendant_scope) {
      const declared_variable *var =
          this->find_declared_variable(used_var.name);
      if (!var) {
        // TODO(strager): Should we check used_var.kind?
        this->error_reporter_->report_error_use_of_undeclared_variable(
            used_var.name);
      }
    }
  }

 private:
  enum class declared_variable_scope {
    declared_in_current_scope,
    declared_in_descendant_scope,
  };

  struct declared_variable {
    string8 name;
    variable_kind kind;
    std::optional<identifier> declaration;
    declared_variable_scope declaration_scope;
  };

  enum class used_variable_kind {
    assignment,
    use,
  };

  struct used_variable {
    explicit used_variable(identifier name, used_variable_kind kind) noexcept
        : name(name), kind(kind) {}

    identifier name;
    used_variable_kind kind;
  };

  struct scope {
    std::vector<declared_variable> declared_variables;
    std::vector<used_variable> variables_used;
    std::vector<used_variable> variables_used_in_descendant_scope;

    const declared_variable *find_declared_variable(identifier name) const
        noexcept {
      for (const declared_variable &var : this->declared_variables) {
        if (var.name == name.string_view()) {
          return &var;
        }
      }
      return nullptr;
    }
  };

  const declared_variable *find_declared_variable(identifier name) const
      noexcept {
    for (auto scope_it = this->scopes_.rbegin();
         scope_it != this->scopes_.rend(); ++scope_it) {
      const declared_variable *var = scope_it->find_declared_variable(name);
      if (var) {
        return var;
      }
    }
    return nullptr;
  }

  void propagate_variable_uses_to_parent_scope(
      bool allow_variable_use_before_declaration) {
    QLJS_ASSERT(this->scopes_.size() >= 2);
    scope &current_scope = this->scopes_[this->scopes_.size() - 1];
    scope &parent_scope = this->scopes_[this->scopes_.size() - 2];

    for (const used_variable &used_var : current_scope.variables_used) {
      QLJS_ASSERT(!current_scope.find_declared_variable(used_var.name));
      const declared_variable *var =
          this->find_declared_variable(used_var.name);
      if (!var) {
        (allow_variable_use_before_declaration
             ? parent_scope.variables_used_in_descendant_scope
             : parent_scope.variables_used)
            .emplace_back(used_var);
      }
    }
    for (const used_variable &used_var :
         current_scope.variables_used_in_descendant_scope) {
      QLJS_ASSERT(!this->find_declared_variable(used_var.name));
      parent_scope.variables_used_in_descendant_scope.emplace_back(used_var);
    }
  }

  void propagate_variable_declarations_to_parent_scope() {
    QLJS_ASSERT(this->scopes_.size() >= 2);
    scope &current_scope = this->scopes_[this->scopes_.size() - 1];
    scope &parent_scope = this->scopes_[this->scopes_.size() - 2];

    for (const declared_variable &var : current_scope.declared_variables) {
      if (var.kind == variable_kind::_function ||
          var.kind == variable_kind::_var) {
        declared_variable parent_var = var;
        parent_var.declaration_scope =
            declared_variable_scope::declared_in_descendant_scope;
        QLJS_ASSERT(parent_var.declaration.has_value());
        this->report_error_if_variable_declaration_conflicts_in_scope(
            parent_scope, *parent_var.declaration, parent_var.kind,
            parent_var.declaration_scope);
        parent_scope.declared_variables.emplace_back(parent_var);
      }
    }
  }

  void report_error_if_variable_declaration_conflicts_in_scope(
      const scope &scope, identifier name, variable_kind kind,
      declared_variable_scope declaration_scope) const {
    const declared_variable *already_declared_variable =
        scope.find_declared_variable(name);
    if (already_declared_variable) {
      using vk = variable_kind;
      vk other_kind = already_declared_variable->kind;

      switch (other_kind) {
        case vk::_catch:
          QLJS_ASSERT(kind != vk::_catch);
          QLJS_ASSERT(kind != vk::_import);
          QLJS_ASSERT(kind != vk::_parameter);
          break;
        case vk::_class:
        case vk::_const:
        case vk::_function:
        case vk::_let:
        case vk::_var:
          QLJS_ASSERT(kind != vk::_catch);
          QLJS_ASSERT(kind != vk::_parameter);
          break;
        case vk::_parameter:
          QLJS_ASSERT(kind != vk::_catch);
          QLJS_ASSERT(kind != vk::_import);
          break;
        case vk::_import:
          break;
      }

      bool redeclaration_ok =
          (other_kind == vk::_function && kind == vk::_function) ||
          (other_kind == vk::_parameter && kind == vk::_function) ||
          (other_kind == vk::_var && kind == vk::_function) ||
          (other_kind == vk::_parameter && kind == vk::_parameter) ||
          (other_kind == vk::_catch && kind == vk::_var) ||
          (other_kind == vk::_function && kind == vk::_var) ||
          (other_kind == vk::_parameter && kind == vk::_var) ||
          (other_kind == vk::_var && kind == vk::_var) ||
          (other_kind == vk::_function &&
           already_declared_variable->declaration_scope ==
               declared_variable_scope::declared_in_descendant_scope) ||
          (kind == vk::_function &&
           declaration_scope ==
               declared_variable_scope::declared_in_descendant_scope);
      if (!redeclaration_ok) {
        assert(already_declared_variable->declaration.has_value());
        this->error_reporter_->report_error_redeclaration_of_variable(
            name, *already_declared_variable->declaration);
      }
    }
  }

  std::vector<scope> scopes_;
  error_reporter *error_reporter_;
};
}

#endif
