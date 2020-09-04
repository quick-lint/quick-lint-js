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

#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>

namespace quick_lint_js {
text_error_reporter::text_error_reporter(std::ostream &output)
    : output_(output) {}

void text_error_reporter::set_source(padded_string_view input,
                                     const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void text_error_reporter::report_error_assignment_to_const_global_variable(
    identifier assignment) {
  log_location(assignment);
  this->output_ << "error: assignment to const global variable\n";
}

void text_error_reporter::report_error_assignment_to_const_variable(
    identifier declaration, identifier assignment, variable_kind) {
  log_location(assignment);
  this->output_ << "error: assignment to const variable\n";
  log_location(declaration);
  this->output_ << "note: const variable declared here\n";
}

void text_error_reporter::report_error_assignment_to_undeclared_variable(
    identifier assignment) {
  log_location(assignment);
  this->output_ << "error: assignment to undeclared variable\n";
}

void text_error_reporter::report_error_invalid_binding_in_let_statement(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: invalid binding in let statement\n";
}

void text_error_reporter::report_error_invalid_expression_left_of_assignment(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: invalid expression left of assignment\n";
}

void text_error_reporter::report_error_let_with_no_bindings(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: let with no bindings\n";
}

void text_error_reporter::
    report_error_missing_comma_between_object_literal_entries(
        source_code_span where) {
  log_location(where);
  this->output_ << "error: missing comma between object literal entries\n";
}

void text_error_reporter::report_error_missing_operand_for_operator(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: missing operand for operator\n";
}

void text_error_reporter::report_error_missing_semicolon_after_expression(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: missing semicolon after expression\n";
}

void text_error_reporter::report_error_redeclaration_of_variable(
    identifier redeclaration, identifier original_declaration) {
  log_location(redeclaration);
  this->output_ << "error: redeclaration of variable: "
                << out_string8(redeclaration.string_view()) << '\n';
  log_location(original_declaration);
  this->output_ << "note: variable already declared here\n";
}

void text_error_reporter::report_error_stray_comma_in_let_statement(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: stray comma in let statement\n";
}

void text_error_reporter::report_error_unclosed_block_comment(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unclosed block comment\n";
}

void text_error_reporter::report_error_unclosed_regexp_literal(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unclosed regexp literal\n";
}

void text_error_reporter::report_error_unclosed_string_literal(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unclosed string literal\n";
}

void text_error_reporter::report_error_unclosed_template(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unclosed template\n";
}

void text_error_reporter::report_error_unexpected_identifier(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unexpected identifier\n";
}

void text_error_reporter::report_error_unmatched_parenthesis(
    source_code_span where) {
  log_location(where);
  this->output_ << "error: unmatched parenthesis\n";
}

void text_error_reporter::report_error_use_of_undeclared_variable(
    identifier name) {
  log_location(name);
  this->output_ << "error: use of undeclared variable: "
                << out_string8(name.string_view()) << '\n';
}

void text_error_reporter::report_error_variable_used_before_declaration(
    identifier use, identifier declaration) {
  log_location(use);
  this->output_ << "error: variable used before declaration: "
                << out_string8(use.string_view()) << '\n';
  log_location(declaration);
  this->output_ << "note: variable declared here\n";
}

void text_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/get(this->locator_),
      /*out=*/this->output_);
}

void text_error_reporter::log_location(identifier i) const {
  log_location(i.span());
}

void text_error_reporter::log_location(source_code_span span) const {
  source_range r = this->locator_->range(span);
  source_position p = r.begin();
  this->output_ << this->file_path_ << ":" << p.line_number << ":"
                << p.column_number << ": ";
}
}
