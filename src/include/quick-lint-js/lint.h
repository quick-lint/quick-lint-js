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

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <string>
#include <vector>

namespace quick_lint_js {
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
  void visit_property_declaration(identifier);
  void visit_variable_declaration(identifier name, variable_kind kind);
  void visit_variable_assignment(identifier name);
  void visit_variable_typeof_use(identifier name);
  void visit_variable_use(identifier name);
  void visit_end_of_module();

 private:
  enum class declared_variable_scope {
    declared_in_current_scope,
    declared_in_descendant_scope,
  };

  struct declared_variable {
    string8_view name;
    bool is_global_variable;
    variable_kind kind;
    declared_variable_scope declaration_scope;

    identifier declaration() const noexcept {
      QLJS_ASSERT(!this->is_global_variable);
      return identifier(
          source_code_span(name.data(), name.data() + name.size()));
    }
  };

  enum class used_variable_kind {
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

  struct scope {
    std::vector<declared_variable> declared_variables;
    std::vector<used_variable> variables_used;
    std::vector<used_variable> variables_used_in_descendant_scope;
    std::optional<declared_variable> function_expression_declaration;

    const declared_variable *add_variable_declaration(identifier name,
                                                      variable_kind,
                                                      declared_variable_scope);
    void add_predefined_variable_declaration(const char8 *name, variable_kind);

    const declared_variable *find_declared_variable(identifier name) const
        noexcept;
  };

  void declare_variable(scope &, identifier name, variable_kind kind,
                        declared_variable_scope variable_scope);
  void visit_variable_use(identifier name, used_variable_kind);

  void propagate_variable_uses_to_parent_scope(
      bool allow_variable_use_before_declaration, bool consume_arguments);

  void propagate_variable_declarations_to_parent_scope();

  void report_error_if_assignment_is_illegal(
      const declared_variable *var, const identifier &assignment) const;
  void report_error_if_variable_declaration_conflicts_in_scope(
      const scope &scope, identifier name, variable_kind kind,
      declared_variable_scope declaration_scope) const;

  std::vector<scope> scopes_;
  error_reporter *error_reporter_;
};
}

#endif
