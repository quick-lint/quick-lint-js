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
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>

namespace quick_lint_js {
namespace {
template <class Char>
void write_escaped_string(std::ostream &output,
                          std::basic_string_view<Char> string) {
  auto write_string = [&](std::basic_string_view<Char> s) {
    if constexpr (std::is_same_v<Char, char>) {
      output << s;
    } else {
      output << out_string8(s);
    }
  };

  for (;;) {
    auto special_character_index =
        string.find_first_of(reinterpret_cast<const Char *>(u8"\\\""));
    if (special_character_index == string.npos) {
      break;
    }
    write_string(string.substr(0, special_character_index));
    output << '\\' << static_cast<char>(string[special_character_index]);
    string = string.substr(special_character_index + 1);
  }
  write_string(string);
}
}

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
  this->begin_error();
  this->format()
      .error(u8"variable assigned before its declaration", assignment)
      .end();
}

void vim_qflist_json_error_reporter::
    report_error_assignment_to_const_global_variable(identifier assignment) {
  this->begin_error();
  this->format()
      .error(u8"assignment to const global variable", assignment)
      .end();
}

void vim_qflist_json_error_reporter::report_error_assignment_to_const_variable(
    identifier, identifier assignment, variable_kind) {
  this->begin_error();
  this->format().error(u8"assignment to const variable", assignment).end();
}

void vim_qflist_json_error_reporter::
    report_error_assignment_to_undeclared_variable(identifier assignment) {
  this->begin_error();
  this->format().error(u8"assignment to undeclared variable", assignment).end();
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_decimal_point(
        source_code_span where) {
  this->begin_error();
  this->format().error(u8"BigInt literal contains decimal point", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_exponent(source_code_span where) {
  this->begin_error();
  this->format().error(u8"BigInt literal contains exponent", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_big_int_literal_contains_leading_zero(source_code_span where) {
  this->begin_error();
  this->format().error(u8"BigInt literal has a leading 0 digit", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_invalid_binding_in_let_statement(source_code_span where) {
  this->begin_error();
  this->format().error(u8"invalid binding in let statement", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_invalid_expression_left_of_assignment(source_code_span where) {
  this->begin_error();
  this->format().error(u8"invalid expression left of assignment", where).end();
}

void vim_qflist_json_error_reporter::report_error_let_with_no_bindings(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"let with no bindings", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_missing_comma_between_object_literal_entries(
        source_code_span where) {
  this->begin_error();
  this->format()
      .error(u8"missing comma between object literal entries", where)
      .end();
}

void vim_qflist_json_error_reporter::report_error_missing_operand_for_operator(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"missing operand for operator", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_missing_semicolon_after_expression(source_code_span where) {
  this->begin_error();
  this->format().error(u8"missing semicolon after expression", where).end();
}

void vim_qflist_json_error_reporter::
    report_error_redeclaration_of_global_variable(identifier redeclaration) {
  this->begin_error();
  this->format()
      .error(u8"redeclaration of global variable", redeclaration)
      .end();
}

void vim_qflist_json_error_reporter::report_error_redeclaration_of_variable(
    identifier redeclaration, identifier) {
  this->begin_error();
  this->format().error(u8"redeclaration of variable", redeclaration).end();
}

void vim_qflist_json_error_reporter::report_error_stray_comma_in_let_statement(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"stray comma in let statement", where).end();
}

void vim_qflist_json_error_reporter::report_error_unclosed_block_comment(
    source_code_span comment_open) {
  this->begin_error();
  this->format().error(u8"unclosed block comment", comment_open).end();
}

void vim_qflist_json_error_reporter::report_error_unclosed_regexp_literal(
    source_code_span regexp_literal) {
  this->begin_error();
  this->format().error(u8"unclosed regexp literal", regexp_literal).end();
}

void vim_qflist_json_error_reporter::report_error_unclosed_string_literal(
    source_code_span string_literal) {
  this->begin_error();
  this->format().error(u8"unclosed string literal", string_literal).end();
}

void vim_qflist_json_error_reporter::report_error_unclosed_template(
    source_code_span incomplete_template) {
  this->begin_error();
  this->format().error(u8"unclosed template", incomplete_template).end();
}

void vim_qflist_json_error_reporter::
    report_error_unexpected_characters_in_number(source_code_span characters) {
  this->begin_error();
  this->format()
      .error(u8"unexpected characters in number literal", characters)
      .end();
}

void vim_qflist_json_error_reporter::report_error_unexpected_hash_character(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"unexpected '#'", where).end();
}

void vim_qflist_json_error_reporter::report_error_unexpected_identifier(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"unexpected identifier", where).end();
}

void vim_qflist_json_error_reporter::report_error_unmatched_parenthesis(
    source_code_span where) {
  this->begin_error();
  this->format().error(u8"unmatched parenthesis", where).end();
}

void vim_qflist_json_error_reporter::report_error_use_of_undeclared_variable(
    identifier name) {
  this->begin_error();
  this->format().error(u8"use of undeclared variable", name).end();
}

void vim_qflist_json_error_reporter::
    report_error_variable_used_before_declaration(identifier use, identifier) {
  this->begin_error();
  this->format().error(u8"variable used before declaration", use).end();
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

void vim_qflist_json_error_reporter::begin_error() {
  if (this->need_comma_) {
    this->output_ << ",\n";
  }
  this->need_comma_ = true;
}

vim_qflist_json_error_formatter vim_qflist_json_error_reporter::format() {
  QLJS_ASSERT(this->locator_.has_value());
  return vim_qflist_json_error_formatter(/*output=*/this->output_,
                                         /*locator=*/*this->locator_,
                                         /*file_name=*/this->file_name_,
                                         /*bufnr=*/this->bufnr_);
}

vim_qflist_json_error_formatter::vim_qflist_json_error_formatter(
    std::ostream &output, quick_lint_js::locator &locator,
    std::string_view file_name, std::string_view bufnr)
    : output_(output),
      locator_(locator),
      file_name_(file_name),
      bufnr_(bufnr) {}

void vim_qflist_json_error_formatter::add(
    severity sev, const char8 *message,
    std::initializer_list<source_code_span> parameters) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;
  QLJS_ASSERT(message);
  QLJS_ASSERT(!std::empty(parameters));

  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  const source_code_span &origin_span = *parameters.begin();
  source_range r = this->locator_.range(origin_span);
  auto end_column_number = origin_span.begin() == origin_span.end()
                               ? r.begin().column_number
                               : (r.end().column_number - 1);
  this->output_ << "{\"col\": " << r.begin().column_number
                << ", \"lnum\": " << r.begin().line_number
                << ", \"end_col\": " << end_column_number
                << ", \"end_lnum\": " << r.end().line_number
                << ", \"vcol\": 0, \"text\": \"";

  string8_view remaining_message(message);
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    write_escaped_string(this->output_,
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

    write_escaped_string(this->output_,
                         (parameters.begin() + index)->string_view());
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  write_escaped_string(this->output_, remaining_message);

  this->output_ << '\"';
  if (!this->bufnr_.empty()) {
    this->output_ << ", \"bufnr\": " << this->bufnr_;
  }
  if (!this->file_name_.empty()) {
    this->output_ << ", \"filename\": \"";
    write_escaped_string(this->output_, this->file_name_);
    this->output_ << '"';
  }
  this->output_ << '}';
}

void vim_qflist_json_error_formatter::end() {}
}
