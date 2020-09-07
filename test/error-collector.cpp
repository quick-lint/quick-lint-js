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

#include <iostream>
#include <quick-lint-js/error-collector.h>

namespace quick_lint_js {
void error_collector::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void error_collector::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void PrintTo(const error_collector::error &x, std::ostream *out) {
#define QLJS_CASE(k)       \
  case error_collector::k: \
    *out << #k;            \
    break;
  switch (x.kind) {
    QLJS_CASE(error_assignment_to_const_global_variable)
    QLJS_CASE(error_assignment_to_const_variable)
    QLJS_CASE(error_assignment_to_undeclared_variable)
    QLJS_CASE(error_big_int_literal_contains_decimal_point)
    QLJS_CASE(error_big_int_literal_contains_exponent)
    QLJS_CASE(error_big_int_literal_contains_leading_zero)
    QLJS_CASE(error_invalid_binding_in_let_statement)
    QLJS_CASE(error_invalid_expression_left_of_assignment)
    QLJS_CASE(error_let_with_no_bindings)
    QLJS_CASE(error_missing_comma_between_object_literal_entries)
    QLJS_CASE(error_missing_operand_for_operator)
    QLJS_CASE(error_missing_semicolon_after_expression)
    QLJS_CASE(error_redeclaration_of_variable)
    QLJS_CASE(error_stray_comma_in_let_statement)
    QLJS_CASE(error_unclosed_block_comment)
    QLJS_CASE(error_unclosed_regexp_literal)
    QLJS_CASE(error_unclosed_string_literal)
    QLJS_CASE(error_unclosed_template)
    QLJS_CASE(error_unexpected_characters_in_number)
    QLJS_CASE(error_unexpected_hash_character)
    QLJS_CASE(error_unexpected_identifier)
    QLJS_CASE(error_unmatched_parenthesis)
    QLJS_CASE(error_use_of_undeclared_variable)
    QLJS_CASE(error_variable_used_before_declaration)
  }
#undef QLJS_CASE
}
}
