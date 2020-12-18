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
#include <quick-lint-js/translation.h>

#define QLJS_X_ERROR_TYPES                                                     \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_before_variable_declaration,                            \
      {                                                                        \
        identifier assignment;                                                 \
        identifier declaration;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATE("variable assigned before its declaration"),       \
             assignment)                                                       \
          .note(QLJS_TRANSLATE("variable declared here"), declaration))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_global_variable, { identifier assignment; },   \
      .error(QLJS_TRANSLATE("assignment to const global variable"),            \
             assignment))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable,                                      \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATE("assignment to const variable"), assignment)       \
          .note(QLJS_TRANSLATE("const variable declared here"), declaration))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable_before_its_declaration,               \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATE(                                                   \
                 "assignment to const variable before its declaration"),       \
             assignment)                                                       \
          .note(QLJS_TRANSLATE("const variable declared here"), declaration))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_undeclared_variable, { identifier assignment; },     \
      .error(QLJS_TRANSLATE("assignment to undeclared variable"), assignment)) \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_decimal_point,                            \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATE("BigInt literal contains decimal point"), where))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_exponent, { source_code_span where; },    \
      .error(QLJS_TRANSLATE("BigInt literal contains exponent"), where))       \
                                                                               \
  /* TODO(mc2), remove */                                                      \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_leading_zero,                             \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATE("BigInt literal has a leading 0 digit"), where))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_class_named_let, { source_code_span name; },        \
      .error(QLJS_TRANSLATE("classes cannot be named 'let'"), name))           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_variable_named_let_with_let,                        \
      { source_code_span name; },                                              \
      .error(QLJS_TRANSLATE(                                                   \
                 "let statement cannot declare variables named 'let'"),        \
             name))                                                            \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_export_let, { source_code_span export_name; },              \
      .error(QLJS_TRANSLATE("cannot export variable named 'let'"),             \
             export_name))                                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_import_let, { source_code_span import_name; },              \
      .error(QLJS_TRANSLATE("cannot import 'let'"), import_name))              \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_character_disallowed_in_identifiers,                               \
      { source_code_span character; },                                         \
      .error(QLJS_TRANSLATE("character is not allowed in identifiers"),        \
             character))                                                       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_character_disallowed_in_identifiers,                       \
      { source_code_span escape_sequence; },                                   \
      .error(                                                                  \
          QLJS_TRANSLATE("escaped character is not allowed in identifiers"),   \
          escape_sequence))                                                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_code_point_in_identifier_out_of_range,                     \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATE("code point out of range"), escape_sequence))      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_newline, { source_code_span where; },   \
      .error(QLJS_TRANSLATE("expected expression before newline"), where))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_semicolon, { source_code_span where; }, \
      .error(QLJS_TRANSLATE("expected expression before semicolon"), where))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_hex_digits_in_unicode_escape,                             \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATE(                                                   \
                 "expected hexadecimal digits in Unicode escape sequence"),    \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_binding_in_let_statement, { source_code_span where; },     \
      .error(QLJS_TRANSLATE("invalid binding in let statement"), where))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_expression_left_of_assignment,                             \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATE("invalid expression left of assignment"), where))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_lone_literal_in_object_literal,                            \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATE("invalid lone literal in object literal"), where)) \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_utf_8_sequence, { source_code_span sequence; },            \
      .error(QLJS_TRANSLATE("invalid UTF-8 sequence"), sequence))              \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_keywords_cannot_contain_escape_sequences,                          \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATE("keywords cannot contain escape sequences"),       \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_let_with_no_bindings, { source_code_span where; },                 \
      .error(QLJS_TRANSLATE("let with no bindings"), where))                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_comma_between_object_literal_entries,                      \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATE("missing comma between object literal entries"),   \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_operand_for_operator, { source_code_span where; },         \
      .error(QLJS_TRANSLATE("missing operand for operator"), where))           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_semicolon_after_expression, { source_code_span where; },   \
      .error(QLJS_TRANSLATE("missing semicolon after expression"), where))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_consecutive_underscores,                   \
      { source_code_span underscores; },                                       \
      .error(                                                                  \
          QLJS_TRANSLATE("number literal contains consecutive underscores"),   \
          underscores))                                                        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_trailing_underscores,                      \
      { source_code_span underscores; },                                       \
      .error(u8"number literal contains trailing underscore(s)", underscores)) \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_have_exponent,                               \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("octal literal may not have exponent"),            \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_have_decimal,                                \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("octal literal may not have decimal"),             \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_be_big_int,                                  \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("octal literal may not be BigInt"), characters))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_global_variable, { identifier redeclaration; },   \
      .error(QLJS_TRANSLATE("redeclaration of global variable"),               \
             redeclaration))                                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_variable,                                         \
      {                                                                        \
        identifier redeclaration;                                              \
        identifier original_declaration;                                       \
      },                                                                       \
      .error(QLJS_TRANSLATE("redeclaration of variable: {0}"), redeclaration)  \
          .note(QLJS_TRANSLATE("variable already declared here"),              \
                original_declaration))                                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_regexp_literal_flags_cannot_contain_unicode_escapes,               \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATE("RegExp literal cannot contain Unicode escapes"),  \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_stray_comma_in_let_statement, { source_code_span where; },         \
      .error(QLJS_TRANSLATE("stray comma in let statement"), where))           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_block_comment, { source_code_span comment_open; },        \
      .error(QLJS_TRANSLATE("unclosed block comment"), comment_open))          \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_identifier_escape_sequence,                               \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATE("unclosed identifier escape sequence"),            \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_regexp_literal, { source_code_span regexp_literal; },     \
      .error(QLJS_TRANSLATE("unclosed regexp literal"), regexp_literal))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_string_literal, { source_code_span string_literal; },     \
      .error(QLJS_TRANSLATE("unclosed string literal"), string_literal))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_template, { source_code_span incomplete_template; },      \
      .error(QLJS_TRANSLATE("unclosed template"), incomplete_template))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_at_character, { source_code_span character; },          \
      .error(QLJS_TRANSLATE("unexpected '@'"), character))                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_backslash_in_identifier,                                \
      { source_code_span backslash; },                                         \
      .error(QLJS_TRANSLATE("unexpected '\\' in identifier"), backslash))      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_number, { source_code_span characters; }, \
      .error(QLJS_TRANSLATE("unexpected characters in number literal"),        \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_control_character, { source_code_span character; },     \
      .error(QLJS_TRANSLATE("unexpected control character"), character))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_binary_number,                            \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("unexpected characters in binary literal"),        \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_octal_number,                             \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("unexpected characters in octal literal"),         \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_hex_number,                               \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATE("unexpected characters in hex literal"),           \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_binary_number, { source_code_span characters; },      \
      .error(QLJS_TRANSLATE("binary number literal has no digits"),            \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_hex_number, { source_code_span characters; },         \
      .error(QLJS_TRANSLATE("hex number literal has no digits"), characters))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_octal_number, { source_code_span characters; },       \
      .error(QLJS_TRANSLATE("octal number literal has no digits"),             \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_hash_character, { source_code_span where; },            \
      .error(QLJS_TRANSLATE("unexpected '#'"), where))                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_identifier, { source_code_span where; },                \
      .error(QLJS_TRANSLATE("unexpected identifier"), where))                  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unmatched_indexing_bracket, { source_code_span left_square; },     \
      .error(QLJS_TRANSLATE("unmatched indexing bracket"), left_square))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unmatched_parenthesis, { source_code_span where; },                \
      .error(QLJS_TRANSLATE("unmatched parenthesis"), where))                  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_use_of_undeclared_variable, { identifier name; },                  \
      .error(QLJS_TRANSLATE("use of undeclared variable: {0}"), name))         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_variable_used_before_declaration,                                  \
      {                                                                        \
        identifier use;                                                        \
        identifier declaration;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATE("variable used before declaration: {0}"), use)     \
          .note(QLJS_TRANSLATE("variable declared here"), declaration))        \
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
