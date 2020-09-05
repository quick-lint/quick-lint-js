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

#ifndef QUICK_LINT_JS_ERROR_H
#define QUICK_LINT_JS_ERROR_H

#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
class error_reporter {
 public:
  error_reporter() noexcept = default;

  error_reporter(const error_reporter &) noexcept = default;
  error_reporter &operator=(const error_reporter &) noexcept = default;

  error_reporter(error_reporter &&) noexcept = default;
  error_reporter &operator=(error_reporter &&) noexcept = default;

  virtual ~error_reporter() = default;

  virtual void report_error_assignment_to_const_global_variable(
      identifier assignment) = 0;
  virtual void report_error_assignment_to_const_variable(
      identifier declaration, identifier assignment,
      variable_kind var_kind) = 0;
  virtual void report_error_assignment_to_undeclared_variable(
      identifier assignment) = 0;
  virtual void report_error_invalid_binding_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_invalid_expression_left_of_assignment(
      source_code_span where) = 0;
  virtual void report_error_let_with_no_bindings(source_code_span where) = 0;
  virtual void report_error_missing_comma_between_object_literal_entries(
      source_code_span where) = 0;
  virtual void report_error_missing_operand_for_operator(
      source_code_span where) = 0;
  virtual void report_error_missing_semicolon_after_expression(
      source_code_span where) = 0;
  virtual void report_error_redeclaration_of_variable(
      identifier redeclaration, identifier original_declaration) = 0;
  virtual void report_error_stray_comma_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_unclosed_block_comment(
      source_code_span comment_open) = 0;
  virtual void report_error_unclosed_regexp_literal(
      source_code_span regexp_literal) = 0;
  virtual void report_error_unclosed_string_literal(
      source_code_span string_literal) = 0;
  virtual void report_error_unclosed_template(
      source_code_span incomplete_template) = 0;
  virtual void report_error_unexpected_identifier(source_code_span where) = 0;
  virtual void report_error_unmatched_parenthesis(source_code_span where) = 0;
  virtual void report_error_use_of_undeclared_variable(identifier name) = 0;
  virtual void report_error_variable_used_before_declaration(
      identifier use, identifier declaration) = 0;

  virtual void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) = 0;

  static void write_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin, const locator *, std::ostream &);
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

  void report_error_assignment_to_const_global_variable(identifier) override {}
  void report_error_assignment_to_const_variable(identifier, identifier,
                                                 variable_kind) override {}
  void report_error_assignment_to_undeclared_variable(identifier) override {}
  void report_error_invalid_binding_in_let_statement(
      source_code_span) override {}
  void report_error_invalid_expression_left_of_assignment(
      source_code_span) override {}
  void report_error_let_with_no_bindings(source_code_span) override {}
  void report_error_missing_comma_between_object_literal_entries(
      source_code_span) override {}
  void report_error_missing_operand_for_operator(source_code_span) override {}
  void report_error_missing_semicolon_after_expression(
      source_code_span) override {}
  void report_error_redeclaration_of_variable(identifier, identifier) override {
  }
  void report_error_stray_comma_in_let_statement(source_code_span) override {}
  void report_error_unclosed_block_comment(source_code_span) override {}
  void report_error_unclosed_regexp_literal(source_code_span) override {}
  void report_error_unclosed_string_literal(source_code_span) override {}
  void report_error_unclosed_template(source_code_span) override {}
  void report_error_unexpected_identifier(source_code_span) override {}
  void report_error_unmatched_parenthesis(source_code_span) override {}
  void report_error_use_of_undeclared_variable(identifier) override {}
  void report_error_variable_used_before_declaration(identifier,
                                                     identifier) override {}

  void report_fatal_error_unimplemented_token(const char *, int, const char *,
                                              token_type,
                                              const char8 *) override {}
};
inline null_error_reporter null_error_reporter::instance;
}

#endif
