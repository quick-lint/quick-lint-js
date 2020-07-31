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

#ifndef QUICK_LINT_JS_ERROR_COLLECTOR_H
#define QUICK_LINT_JS_ERROR_COLLECTOR_H

#include <iosfwd>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <type_traits>
#include <vector>

namespace quick_lint_js {
struct error_collector : public error_reporter {
  void report_error_assignment_to_const_global_variable(
      identifier assignment) override {
    this->errors.emplace_back(error_assignment_to_const_global_variable,
                              assignment.span());
  }

  void report_error_assignment_to_const_variable(
      identifier declaration, identifier assignment,
      variable_kind var_kind) override {
    this->errors.emplace_back(error{error_assignment_to_const_variable,
                                    assignment.span(), declaration.span(),
                                    var_kind});
  }

  void report_error_assignment_to_undeclared_variable(
      identifier assignment) override {
    this->errors.emplace_back(error_assignment_to_undeclared_variable,
                              assignment.span());
  }

  void report_error_invalid_binding_in_let_statement(
      source_code_span where) override {
    this->errors.emplace_back(
        error{error_invalid_binding_in_let_statement, where});
  }

  void report_error_invalid_expression_left_of_assignment(
      source_code_span where) override {
    this->errors.emplace_back(
        error{error_invalid_expression_left_of_assignment, where});
  }

  void report_error_let_with_no_bindings(source_code_span where) override {
    this->errors.emplace_back(error{error_let_with_no_bindings, where});
  }

  void report_error_missing_operand_for_operator(
      source_code_span where) override {
    this->errors.emplace_back(error{error_missing_operand_for_operator, where});
  }

  void report_error_missing_semicolon_after_expression(
      source_code_span where) override {
    this->errors.emplace_back(
        error{error_missing_semicolon_after_expression, where});
  }

  void report_error_stray_comma_in_let_statement(
      source_code_span where) override {
    this->errors.emplace_back(error{error_stray_comma_in_let_statement, where});
  }

  void report_error_unclosed_block_comment(
      source_code_span comment_open) override {
    this->errors.emplace_back(
        error{error_unclosed_block_comment, comment_open});
  }

  void report_error_unclosed_string_literal(
      source_code_span string_literal) override {
    this->errors.emplace_back(
        error{error_unclosed_string_literal, string_literal});
  }

  void report_error_unclosed_template(
      source_code_span incomplete_template) override {
    this->errors.emplace_back(
        error{error_unclosed_template, incomplete_template});
  }

  void report_error_unexpected_identifier(source_code_span where) override {
    this->errors.emplace_back(error{error_unexpected_identifier, where});
  }

  void report_error_unmatched_parenthesis(source_code_span where) override {
    this->errors.emplace_back(error{error_unmatched_parenthesis, where});
  }

  void report_error_variable_used_before_declaration(identifier name) override {
    this->errors.emplace_back(
        error{error_variable_used_before_declaration, name.span()});
  }

  enum error_kind {
    error_assignment_to_const_global_variable,
    error_assignment_to_const_variable,
    error_assignment_to_undeclared_variable,
    error_invalid_binding_in_let_statement,
    error_invalid_expression_left_of_assignment,
    error_let_with_no_bindings,
    error_missing_operand_for_operator,
    error_missing_semicolon_after_expression,
    error_stray_comma_in_let_statement,
    error_unclosed_block_comment,
    error_unclosed_string_literal,
    error_unclosed_template,
    error_unexpected_identifier,
    error_unmatched_parenthesis,
    error_variable_used_before_declaration,
  };
  struct error {
    explicit error(error_kind kind, source_code_span where) noexcept
        : kind(kind), where(where) {}

    explicit error(error_kind kind, source_code_span where,
                   source_code_span other_where,
                   variable_kind var_kind) noexcept
        : kind(kind),
          where(where),
          other_where(other_where),
          var_kind(var_kind) {}

    error_kind kind;
    source_code_span where;
    union {
      source_code_span other_where;
      static_assert(std::is_trivially_destructible_v<source_code_span>);
    };
    variable_kind var_kind;
  };
  std::vector<error> errors;
};

void PrintTo(const error_collector::error &, std::ostream *);
}  // namespace quick_lint_js

#endif
