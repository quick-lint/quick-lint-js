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

#ifndef QUICK_LINT_JS_TEXT_ERROR_REPORTER_H
#define QUICK_LINT_JS_TEXT_ERROR_REPORTER_H

#include <iosfwd>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
class text_error_reporter : public error_reporter {
 public:
  explicit text_error_reporter(std::ostream &output, const char *input,
                               const char *file_path);

  void report_error_assignment_to_const_global_variable(
      identifier assignment) override;
  void report_error_assignment_to_const_variable(
      identifier declaration, identifier assignment,
      variable_kind var_kind) override;
  void report_error_assignment_to_undeclared_variable(
      identifier assignment) override;
  void report_error_invalid_binding_in_let_statement(
      source_code_span where) override;
  void report_error_invalid_expression_left_of_assignment(
      source_code_span where) override;
  void report_error_let_with_no_bindings(source_code_span where) override;
  void report_error_missing_comma_between_object_literal_entries(
      source_code_span where) override;
  void report_error_missing_operand_for_operator(
      source_code_span where) override;
  void report_error_missing_semicolon_after_expression(
      source_code_span where) override;
  void report_error_stray_comma_in_let_statement(
      source_code_span where) override;
  void report_error_unclosed_block_comment(
      source_code_span comment_open) override;
  void report_error_unclosed_regexp_literal(
      source_code_span regexp_literal) override;
  void report_error_unclosed_string_literal(
      source_code_span string_literal) override;
  void report_error_unclosed_template(
      source_code_span incomplete_template) override;
  void report_error_unexpected_identifier(source_code_span where) override;
  void report_error_unmatched_parenthesis(source_code_span where) override;
  void report_error_use_of_undeclared_variable(identifier name) override;
  void report_error_variable_used_before_declaration(
      identifier use, identifier declaration) override;

 private:
  void log_location(identifier) const;
  void log_location(source_code_span) const;

  std::ostream &output_;
  locator locator_;
  const char *file_path_;
};
}  // namespace quick_lint_js

#endif
