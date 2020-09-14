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
#include <quick-lint-js/warning.h>
#include <type_traits>
#include <vector>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_MSVC(26495)  // Variable is uninitialized.
QLJS_WARNING_IGNORE_MSVC(26812)  // Prefer 'enum class' over 'enum'.

namespace quick_lint_js {
struct error_collector : public error_reporter {
  void report(error_assignment_before_variable_declaration e) override {
    this->errors.emplace_back(error_assignment_before_variable_declaration,
                              e.assignment.span(), e.declaration.span());
  }

  void report(error_assignment_to_const_global_variable e) override {
    this->errors.emplace_back(error_assignment_to_const_global_variable,
                              e.assignment.span());
  }

  void report(error_assignment_to_const_variable e) override {
    this->errors.emplace_back(error{error_assignment_to_const_variable,
                                    e.assignment.span(), e.declaration.span(),
                                    e.var_kind});
  }

  void report(error_assignment_to_undeclared_variable e) override {
    this->errors.emplace_back(error_assignment_to_undeclared_variable,
                              e.assignment.span());
  }

  void report(error_big_int_literal_contains_decimal_point e) override {
    this->errors.emplace_back(error_big_int_literal_contains_decimal_point,
                              e.where);
  }

  void report(error_big_int_literal_contains_exponent e) override {
    this->errors.emplace_back(error_big_int_literal_contains_exponent, e.where);
  }

  void report(error_big_int_literal_contains_leading_zero e) override {
    this->errors.emplace_back(error_big_int_literal_contains_leading_zero,
                              e.where);
  }

  void report(error_invalid_binding_in_let_statement e) override {
    this->errors.emplace_back(
        error{error_invalid_binding_in_let_statement, e.where});
  }

  void report(error_invalid_expression_left_of_assignment e) override {
    this->errors.emplace_back(
        error{error_invalid_expression_left_of_assignment, e.where});
  }

  void report(error_let_with_no_bindings e) override {
    this->errors.emplace_back(error{error_let_with_no_bindings, e.where});
  }

  void report(error_missing_comma_between_object_literal_entries e) override {
    this->errors.emplace_back(
        error(error_missing_comma_between_object_literal_entries, e.where));
  }

  void report(error_missing_operand_for_operator e) override {
    this->errors.emplace_back(
        error{error_missing_operand_for_operator, e.where});
  }

  void report(error_missing_semicolon_after_expression e) override {
    this->errors.emplace_back(
        error{error_missing_semicolon_after_expression, e.where});
  }

  void report(error_redeclaration_of_global_variable e) override {
    this->errors.emplace_back(error_redeclaration_of_global_variable,
                              e.redeclaration.span());
  }

  void report(error_redeclaration_of_variable e) override {
    this->errors.emplace_back(error(error_redeclaration_of_variable,
                                    e.redeclaration.span(),
                                    e.original_declaration.span()));
  }

  void report(error_stray_comma_in_let_statement e) override {
    this->errors.emplace_back(
        error{error_stray_comma_in_let_statement, e.where});
  }

  void report(error_unclosed_block_comment e) override {
    this->errors.emplace_back(
        error{error_unclosed_block_comment, e.comment_open});
  }

  void report(error_unclosed_regexp_literal e) override {
    this->errors.emplace_back(
        error{error_unclosed_regexp_literal, e.regexp_literal});
  }

  void report(error_unclosed_string_literal e) override {
    this->errors.emplace_back(
        error{error_unclosed_string_literal, e.string_literal});
  }

  void report(error_unclosed_template e) override {
    this->errors.emplace_back(
        error{error_unclosed_template, e.incomplete_template});
  }

  void report(error_unexpected_characters_in_number e) override {
    this->errors.emplace_back(error_unexpected_characters_in_number,
                              e.characters);
  }

  void report(error_unexpected_hash_character e) override {
    this->errors.emplace_back(error_unexpected_hash_character, e.where);
  }

  void report(error_unexpected_identifier e) override {
    this->errors.emplace_back(error{error_unexpected_identifier, e.where});
  }

  void report(error_unmatched_parenthesis e) override {
    this->errors.emplace_back(error{error_unmatched_parenthesis, e.where});
  }

  void report(error_use_of_undeclared_variable e) override {
    this->errors.emplace_back(
        error{error_use_of_undeclared_variable, e.name.span()});
  }

  void report(error_variable_used_before_declaration e) override {
    this->errors.emplace_back(error(error_variable_used_before_declaration,
                                    e.use.span(), e.declaration.span()));
  }

  void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) override;
  void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) override;

  enum error_kind {
    error_assignment_before_variable_declaration,
    error_assignment_to_const_global_variable,
    error_assignment_to_const_variable,
    error_assignment_to_undeclared_variable,
    error_big_int_literal_contains_decimal_point,
    error_big_int_literal_contains_exponent,
    error_big_int_literal_contains_leading_zero,
    error_invalid_binding_in_let_statement,
    error_invalid_expression_left_of_assignment,
    error_let_with_no_bindings,
    error_missing_comma_between_object_literal_entries,
    error_missing_operand_for_operator,
    error_missing_semicolon_after_expression,
    error_redeclaration_of_global_variable,
    error_redeclaration_of_variable,
    error_stray_comma_in_let_statement,
    error_unclosed_block_comment,
    error_unclosed_regexp_literal,
    error_unclosed_string_literal,
    error_unclosed_template,
    error_unexpected_characters_in_number,
    error_unexpected_hash_character,
    error_unexpected_identifier,
    error_unmatched_parenthesis,
    error_use_of_undeclared_variable,
    error_variable_used_before_declaration,
  };
  struct error {
    explicit error(error_kind kind, source_code_span where) noexcept
        : kind(kind), where(where) {}

    explicit error(error_kind kind, source_code_span where,
                   source_code_span other_where) noexcept
        : kind(kind), where(where), other_where(other_where) {}

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
}

QLJS_WARNING_POP

#endif
