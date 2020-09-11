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
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>

namespace quick_lint_js {
vim_qflist_json_error_reporter::vim_qflist_json_error_reporter(
    std::ostream &output)
    : output_(output) {
  this->output_ << "{\"qflist\": [";
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name,
                                                int vim_bufnr) {
  this->set_source(input, /*file_name=*/file_name,
                   /*vim_bufnr=*/std::optional<int>(vim_bufnr));
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name,
                                                std::optional<int> vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_ = file_name;
  this->bufnr_ = vim_bufnr.has_value() ? std::to_string(*vim_bufnr) : "";
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name) {
  this->set_source(input, /*file_name=*/file_name, /*vim_bufnr=*/std::nullopt);
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                int vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_.clear();
  this->bufnr_ = std::to_string(vim_bufnr);
}

void vim_qflist_json_error_reporter::finish() { this->output_ << "]}"; }

void vim_qflist_json_error_reporter::
    report_error_assignment_before_variable_declaration(identifier assignment,
                                                        identifier) {
  this->write_qflist_entry_header(assignment);
  this->output_ << ", \"text\": \"variable assigned before its declaration\"}";
}

void vim_qflist_json_error_reporter::
    report_error_assignment_to_const_global_variable(identifier assignment) {
  this->write_qflist_entry_header(assignment);
  this->output_ << ", \"text\": \"assignment to const global variable\"}";
}

void vim_qflist_json_error_reporter::report_error_assignment_to_const_variable(
    identifier, identifier assignment, variable_kind) {
  this->write_qflist_entry_header(assignment);
  this->output_ << ", \"text\": \"assignment to const variable\"}";
}

void vim_qflist_json_error_reporter::
    report_error_assignment_to_undeclared_variable(identifier assignment) {
  this->write_qflist_entry_header(assignment);
  this->output_ << ", \"text\": \"assignment to undeclared variable\"}";
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_decimal_point(
        source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"BigInt literal contains decimal point\"}";
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_exponent(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"BigInt literal contains exponent\"}";
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_leading_zero(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"BigInt literal has a leading 0 digit\"}";
}

void vim_qflist_json_error_reporter::
    report_error_invalid_binding_in_let_statement(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"invalid binding in let statement\"}";
}

void vim_qflist_json_error_reporter::
    report_error_invalid_expression_left_of_assignment(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"invalid expression left of assignment\"}";
}

void vim_qflist_json_error_reporter::report_error_let_with_no_bindings(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"let with no bindings\"}";
}

void vim_qflist_json_error_reporter::
    report_error_missing_comma_between_object_literal_entries(
        source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_
      << ", \"text\": \"missing comma between object literal entries\"}";
}

void vim_qflist_json_error_reporter::report_error_missing_operand_for_operator(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"missing operand for operator\"}";
}

void vim_qflist_json_error_reporter::
    report_error_missing_semicolon_after_expression(source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"missing semicolon after expression\"}";
}

void vim_qflist_json_error_reporter::
    report_error_redeclaration_of_global_variable(identifier redeclaration) {
  this->write_qflist_entry_header(redeclaration);
  this->output_ << ", \"text\": \"redeclaration of global variable\"}";
}

void vim_qflist_json_error_reporter::report_error_redeclaration_of_variable(
    identifier redeclaration, identifier) {
  this->write_qflist_entry_header(redeclaration);
  this->output_ << ", \"text\": \"redeclaration of variable\"}";
}

void vim_qflist_json_error_reporter::report_error_stray_comma_in_let_statement(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"stray comma in let statement\"}";
}

void vim_qflist_json_error_reporter::report_error_unclosed_block_comment(
    source_code_span comment_open) {
  this->write_qflist_entry_header(comment_open);
  this->output_ << ", \"text\": \"unclosed block comment\"}";
}

void vim_qflist_json_error_reporter::report_error_unclosed_regexp_literal(
    source_code_span regexp_literal) {
  this->write_qflist_entry_header(regexp_literal);
  this->output_ << ", \"text\": \"unclosed regexp literal\"}";
}

void vim_qflist_json_error_reporter::report_error_unclosed_string_literal(
    source_code_span string_literal) {
  this->write_qflist_entry_header(string_literal);
  this->output_ << ", \"text\": \"unclosed string literal\"}";
}

void vim_qflist_json_error_reporter::report_error_unclosed_template(
    source_code_span incomplete_template) {
  this->write_qflist_entry_header(incomplete_template);
  this->output_ << ", \"text\": \"unclosed template\"}";
}

void vim_qflist_json_error_reporter::
    report_error_unexpected_characters_in_number(source_code_span characters) {
  this->write_qflist_entry_header(characters);
  this->output_ << ", \"text\": \"unexpected characters in number literal\"}";
}

void vim_qflist_json_error_reporter::report_error_unexpected_hash_character(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"unexpected '#'\"}";
}

void vim_qflist_json_error_reporter::report_error_unexpected_identifier(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"unexpected identifier\"}";
}

void vim_qflist_json_error_reporter::report_error_unmatched_parenthesis(
    source_code_span where) {
  this->write_qflist_entry_header(where);
  this->output_ << ", \"text\": \"unmatched parenthesis\"}";
}

void vim_qflist_json_error_reporter::report_error_use_of_undeclared_variable(
    identifier name) {
  this->write_qflist_entry_header(name);
  this->output_ << ", \"text\": \"use of undeclared variable\"}";
}

void vim_qflist_json_error_reporter::
    report_error_variable_used_before_declaration(identifier use, identifier) {
  this->write_qflist_entry_header(use);
  this->output_ << ", \"text\": \"variable used before declaration\"}";
}

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/get(this->locator_),
      /*out=*/this->output_);
}

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_token(
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

void vim_qflist_json_error_reporter::write_qflist_entry_header(
    identifier name) {
  this->write_qflist_entry_header(name.span());
}

void vim_qflist_json_error_reporter::write_qflist_entry_header(
    source_code_span span) {
  source_range r = this->locator_->range(span);
  if (this->need_comma_) {
    this->output_ << ",\n";
  }
  auto end_column_number = span.begin() == span.end()
                               ? r.begin().column_number
                               : (r.end().column_number - 1);
  this->output_ << "{\"col\": " << r.begin().column_number
                << ", \"lnum\": " << r.begin().line_number
                << ", \"end_col\": " << end_column_number
                << ", \"end_lnum\": " << r.end().line_number << ", \"vcol\": 0";
  if (!this->bufnr_.empty()) {
    this->output_ << ", \"bufnr\": " << this->bufnr_;
  }
  if (!this->file_name_.empty()) {
    this->output_ << ", \"filename\": \"";
    this->write_escaped_string(this->file_name_);
    this->output_ << '"';
  }
  this->need_comma_ = true;
}

void vim_qflist_json_error_reporter::write_escaped_string(
    std::string_view string) {
  for (;;) {
    auto special_character_index = string.find_first_of("\\\"");
    if (special_character_index == string.npos) {
      break;
    }
    this->output_ << string.substr(0, special_character_index);
    this->output_ << '\\' << string[special_character_index];
    string = string.substr(special_character_index + 1);
  }
  this->output_ << string;
}
}
