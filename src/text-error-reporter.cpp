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
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/unreachable.h>

namespace quick_lint_js {
text_error_reporter::text_error_reporter(std::ostream &output)
    : output_(output) {}

void text_error_reporter::set_source(padded_string_view input,
                                     const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void text_error_reporter::report_error_assignment_before_variable_declaration(
    identifier assignment, identifier declaration) {
  this->format()
      .error(u8"variable assigned before its declaration", assignment)
      .note(u8"variable declared here", declaration)
      .end();
}

void text_error_reporter::report_error_assignment_to_const_global_variable(
    identifier assignment) {
  this->format()
      .error(u8"assignment to const global variable", assignment)
      .end();
}

void text_error_reporter::report_error_assignment_to_const_variable(
    identifier declaration, identifier assignment, variable_kind) {
  this->format()
      .error(u8"assignment to const variable", assignment)
      .note(u8"const variable declared here", declaration)
      .end();
}

void text_error_reporter::report_error_assignment_to_undeclared_variable(
    identifier assignment) {
  this->format().error(u8"assignment to undeclared variable", assignment).end();
}

void text_error_reporter::report_error_big_int_literal_contains_decimal_point(
    source_code_span where) {
  this->format().error(u8"BigInt literal contains decimal point", where).end();
}

void text_error_reporter::report_error_big_int_literal_contains_exponent(
    source_code_span where) {
  this->format().error(u8"BigInt literal contains exponent", where).end();
}

void text_error_reporter::report_error_big_int_literal_contains_leading_zero(
    source_code_span where) {
  this->format().error(u8"BigInt literal has a leading 0 digit", where).end();
}

void text_error_reporter::report_error_invalid_binding_in_let_statement(
    source_code_span where) {
  this->format().error(u8"invalid binding in let statement", where).end();
}

void text_error_reporter::report_error_invalid_expression_left_of_assignment(
    source_code_span where) {
  this->format().error(u8"invalid expression left of assignment", where).end();
}

void text_error_reporter::report_error_let_with_no_bindings(
    source_code_span where) {
  this->format().error(u8"let with no bindings", where).end();
}

void text_error_reporter::
    report_error_missing_comma_between_object_literal_entries(
        source_code_span where) {
  this->format()
      .error(u8"missing comma between object literal entries", where)
      .end();
}

void text_error_reporter::report_error_missing_operand_for_operator(
    source_code_span where) {
  this->format().error(u8"missing operand for operator", where).end();
}

void text_error_reporter::report_error_missing_semicolon_after_expression(
    source_code_span where) {
  this->format().error(u8"missing semicolon after expression", where).end();
}

void text_error_reporter::report_error_redeclaration_of_global_variable(
    identifier redeclaration) {
  this->format()
      .error(u8"redeclaration of global variable", redeclaration)
      .end();
}

void text_error_reporter::report_error_redeclaration_of_variable(
    identifier redeclaration, identifier original_declaration) {
  this->format()
      .error(u8"redeclaration of variable: {0}", redeclaration)
      .note(u8"variable already declared here", original_declaration)
      .end();
}

void text_error_reporter::report_error_stray_comma_in_let_statement(
    source_code_span where) {
  this->format().error(u8"stray comma in let statement", where).end();
}

void text_error_reporter::report_error_unclosed_block_comment(
    source_code_span where) {
  this->format().error(u8"unclosed block comment", where).end();
}

void text_error_reporter::report_error_unclosed_regexp_literal(
    source_code_span where) {
  this->format().error(u8"unclosed regexp literal", where).end();
}

void text_error_reporter::report_error_unclosed_string_literal(
    source_code_span where) {
  this->format().error(u8"unclosed string literal", where).end();
}

void text_error_reporter::report_error_unclosed_template(
    source_code_span where) {
  this->format().error(u8"unclosed template", where).end();
}

void text_error_reporter::report_error_unexpected_characters_in_number(
    source_code_span characters) {
  this->format()
      .error(u8"unexpected characters in number literal", characters)
      .end();
}

void text_error_reporter::report_error_unexpected_hash_character(
    source_code_span where) {
  this->format().error(u8"unexpected '#'", where).end();
}

void text_error_reporter::report_error_unexpected_identifier(
    source_code_span where) {
  this->format().error(u8"unexpected identifier", where).end();
}

void text_error_reporter::report_error_unmatched_parenthesis(
    source_code_span where) {
  this->format().error(u8"unmatched parenthesis", where).end();
}

void text_error_reporter::report_error_use_of_undeclared_variable(
    identifier name) {
  this->format().error(u8"use of undeclared variable: {0}", name).end();
}

void text_error_reporter::report_error_variable_used_before_declaration(
    identifier use, identifier declaration) {
  this->format()
      .error(u8"variable used before declaration: {0}", use)
      .note(u8"variable declared here", declaration)
      .end();
}

void text_error_reporter::report_fatal_error_unimplemented_character(
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

text_error_formatter text_error_reporter::format() {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  return text_error_formatter(/*output=*/this->output_,
                              /*file_path=*/this->file_path_,
                              /*locator=*/*this->locator_);
}

text_error_formatter::text_error_formatter(std::ostream &output,
                                           const char *file_path,
                                           quick_lint_js::locator &locator)
    : output_(output), file_path_(file_path), locator_(locator) {}

void text_error_formatter::write_before_message(
    severity sev, const source_code_span &origin) {
  source_range r = this->locator_.range(origin);
  source_position p = r.begin();
  this->output_ << this->file_path_ << ":" << p.line_number << ":"
                << p.column_number << ": ";
  switch (sev) {
    case severity::error:
      this->output_ << "error: ";
      break;
    case severity::note:
      this->output_ << "note: ";
      break;
  }
}

void text_error_formatter::write_message_part(severity, string8_view message) {
  this->output_ << out_string8(message);
}

void text_error_formatter::write_after_message(severity,
                                               const source_code_span &) {
  this->output_ << '\n';
}

void text_error_formatter::add(
    severity sev, const char8 *message,
    std::initializer_list<source_code_span> parameters) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;
  QLJS_ASSERT(message);
  QLJS_ASSERT(!std::empty(parameters));

  const source_code_span &origin_span = *parameters.begin();
  this->write_before_message(sev, origin_span);

  string8_view remaining_message(message);
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    this->write_message_part(sev,
                             remaining_message.substr(0, left_curly_index));

    string8_pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    string8_view curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));
    std::size_t index;
    if (curly_content == u8"0") {
      index = 0;
    } else if (curly_content == u8"1") {
      index = 1;
    } else if (curly_content == u8"2") {
      index = 2;
    } else {
      QLJS_ASSERT(false && "invalid message format: unrecognized placeholder");
      QLJS_UNREACHABLE();
    }

    this->write_message_part(sev, (parameters.begin() + index)->string_view());
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  this->write_message_part(sev, remaining_message);

  this->write_after_message(sev, origin_span);
}

void text_error_formatter::end() {}
}
