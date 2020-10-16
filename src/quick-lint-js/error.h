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

#define QLJS_X_ERROR_TYPES                                                     \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_before_variable_declaration,                            \
      {                                                                        \
        identifier assignment;                                                 \
        identifier declaration;                                                \
      },                                                                       \
      .error(u8"variable assigned before its declaration", assignment)         \
          .note(u8"variable declared here", declaration))                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_global_variable, { identifier assignment; },   \
      .error(u8"assignment to const global variable", assignment))             \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable,                                      \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(u8"assignment to const variable", assignment)                     \
          .note(u8"const variable declared here", declaration))                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable_before_its_declaration,               \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(u8"assignment to const variable before its declaration",          \
             assignment)                                                       \
          .note(u8"const variable declared here", declaration))                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_undeclared_variable, { identifier assignment; },     \
      .error(u8"assignment to undeclared variable", assignment))               \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_decimal_point,                            \
      { source_code_span where; },                                             \
      .error(u8"BigInt literal contains decimal point", where))                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_exponent, { source_code_span where; },    \
      .error(u8"BigInt literal contains exponent", where))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_leading_zero,                             \
      { source_code_span where; },                                             \
      .error(u8"BigInt literal has a leading 0 digit", where))                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_class_named_let, { source_code_span name; },        \
      .error(u8"classes cannot be named 'let'", name))                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_variable_named_let_with_let,                        \
      { source_code_span name; },                                              \
      .error(u8"let statement cannot declare variables named 'let'", name))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_export_let, { source_code_span export_name; },              \
      .error(u8"cannot export variable named 'let'", export_name))             \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_import_let, { source_code_span import_name; },              \
      .error(u8"cannot import 'let'", import_name))                            \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_character_disallowed_in_identifiers,                       \
      { source_code_span escape_sequence; },                                   \
      .error(u8"escaped character is not allowed in identifiers",              \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_code_point_in_identifier_out_of_range,                     \
      { source_code_span escape_sequence; },                                   \
      .error(u8"code point out of range", escape_sequence))                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_newline, { source_code_span where; },   \
      .error(u8"expected expression before newline", where))                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_semicolon, { source_code_span where; }, \
      .error(u8"expected expression before semicolon", where))                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_hex_digits_in_unicode_escape,                             \
      { source_code_span escape_sequence; },                                   \
      .error(u8"expected hexadecimal digits in Unicode escape sequence",       \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_binding_in_let_statement, { source_code_span where; },     \
      .error(u8"invalid binding in let statement", where))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_expression_left_of_assignment,                             \
      { source_code_span where; },                                             \
      .error(u8"invalid expression left of assignment", where))                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_keywords_cannot_contain_escape_sequences,                          \
      { source_code_span escape_sequence; },                                   \
      .error(u8"keywords cannot contain escape sequences", escape_sequence))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_let_with_no_bindings, { source_code_span where; },                 \
      .error(u8"let with no bindings", where))                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_comma_between_object_literal_entries,                      \
      { source_code_span where; },                                             \
      .error(u8"missing comma between object literal entries", where))         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_operand_for_operator, { source_code_span where; },         \
      .error(u8"missing operand for operator", where))                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_semicolon_after_expression, { source_code_span where; },   \
      .error(u8"missing semicolon after expression", where))                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_consecutive_underscores,                   \
      { source_code_span underscores; },                                       \
      .error(u8"number literal contains consecutive underscores",              \
             underscores))                                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_trailing_underscores,                      \
      { source_code_span underscores; },                                       \
      .error(u8"number literal contains trailing underscore(s)", underscores)) \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_global_variable, { identifier redeclaration; },   \
      .error(u8"redeclaration of global variable", redeclaration))             \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_variable,                                         \
      {                                                                        \
        identifier redeclaration;                                              \
        identifier original_declaration;                                       \
      },                                                                       \
      .error(u8"redeclaration of variable: {0}", redeclaration)                \
          .note(u8"variable already declared here", original_declaration))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_regexp_literal_flags_cannot_contain_unicode_escapes,               \
      { source_code_span escape_sequence; },                                   \
      .error(u8"RegExp literal cannot contain Unicode escapes",                \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_stray_comma_in_let_statement, { source_code_span where; },         \
      .error(u8"stray comma in let statement", where))                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_block_comment, { source_code_span comment_open; },        \
      .error(u8"unclosed block comment", comment_open))                        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_identifier_escape_sequence,                               \
      { source_code_span escape_sequence; },                                   \
      .error(u8"unclosed identifier escape sequence", escape_sequence))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_regexp_literal, { source_code_span regexp_literal; },     \
      .error(u8"unclosed regexp literal", regexp_literal))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_string_literal, { source_code_span string_literal; },     \
      .error(u8"unclosed string literal", string_literal))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_template, { source_code_span incomplete_template; },      \
      .error(u8"unclosed template", incomplete_template))                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_at_character, { source_code_span character; },          \
      .error(u8"unexpected '@'", character))                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_backslash_in_identifier,                                \
      { source_code_span backslash; },                                         \
      .error(u8"unexpected '\\' in identifier", backslash))                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_number, { source_code_span characters; }, \
      .error(u8"unexpected characters in number literal", characters))         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_control_character, { source_code_span character; },     \
      .error(u8"unexpected control character", character))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_hash_character, { source_code_span where; },            \
      .error(u8"unexpected '#'", where))                                       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_identifier, { source_code_span where; },                \
      .error(u8"unexpected identifier", where))                                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unmatched_parenthesis, { source_code_span where; },                \
      .error(u8"unmatched parenthesis", where))                                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_use_of_undeclared_variable, { identifier name; },                  \
      .error(u8"use of undeclared variable: {0}", name))                       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_variable_used_before_declaration,                                  \
      {                                                                        \
        identifier use;                                                        \
        identifier declaration;                                                \
      },                                                                       \
      .error(u8"variable used before declaration: {0}", use)                   \
          .note(u8"variable declared here", declaration))                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      invalid_lone_literal_in_object_literal, { source_code_span where; },     \
      .error(u8"invalid lone literal in object literal", where))               \
  /* END */

namespace quick_lint_js {
#define QLJS_ERROR_TYPE(name, struct_body, format_call) struct name struct_body;
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

class error_reporter {
 public:
  error_reporter() noexcept = default;

  error_reporter(const error_reporter &) noexcept = default;
  error_reporter &operator=(const error_reporter &) noexcept = default;

  error_reporter(error_reporter &&) noexcept = default;
  error_reporter &operator=(error_reporter &&) noexcept = default;

  virtual ~error_reporter() = default;

#define QLJS_ERROR_TYPE(name, struct_body, format) \
  virtual void report(name) = 0;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  virtual void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) = 0;
  virtual void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) = 0;

  static void write_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character, const locator *, std::ostream &);
  static void write_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin, const locator *, std::ostream &);
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

#define QLJS_ERROR_TYPE(name, struct_body, format) \
  void report(name) override {}
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  void report_fatal_error_unimplemented_character(const char *, int,
                                                  const char *,
                                                  const char8 *) override {}
  void report_fatal_error_unimplemented_token(const char *, int, const char *,
                                              token_type,
                                              const char8 *) override {}
};
inline null_error_reporter null_error_reporter::instance;
}

#endif
