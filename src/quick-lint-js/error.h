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

#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/translation.h>

#define QLJS_X_ERROR_TYPES                                                     \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_before_variable_declaration, "E001",                    \
      {                                                                        \
        identifier assignment;                                                 \
        identifier declaration;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("variable assigned before its declaration"),    \
             assignment)                                                       \
          .note(QLJS_TRANSLATABLE("variable declared here"), declaration))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_global_variable, "E002",                       \
      { identifier assignment; },                                              \
                                                                               \
      .error(QLJS_TRANSLATABLE("assignment to const global variable"),         \
             assignment))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable, "E003",                              \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("assignment to const variable"), assignment)    \
          .note(QLJS_TRANSLATABLE("const variable declared here"),             \
                declaration))                                                  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_const_variable_before_its_declaration, "E004",       \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATABLE(                                                \
                 "assignment to const variable before its declaration"),       \
             assignment)                                                       \
          .note(QLJS_TRANSLATABLE("const variable declared here"),             \
                declaration))                                                  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_assignment_to_undeclared_variable, "E059",                         \
      { identifier assignment; },                                              \
      .warning(QLJS_TRANSLATABLE("assignment to undeclared variable"),         \
               assignment))                                                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_decimal_point, "E005",                    \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("BigInt literal contains decimal point"),       \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_big_int_literal_contains_exponent, "E006",                         \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("BigInt literal contains exponent"), where))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_class_named_let, "E007",                            \
      { source_code_span name; },                                              \
      .error(QLJS_TRANSLATABLE("classes cannot be named 'let'"), name))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_variable_named_let_with_let, "E008",                \
      { source_code_span name; },                                              \
      .error(QLJS_TRANSLATABLE(                                                \
                 "let statement cannot declare variables named 'let'"),        \
             name))                                                            \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_export_let, "E009", { source_code_span export_name; },      \
      .error(QLJS_TRANSLATABLE("cannot export variable named 'let'"),          \
             export_name))                                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_import_let, "E010", { source_code_span import_name; },      \
      .error(QLJS_TRANSLATABLE("cannot import 'let'"), import_name))           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_character_disallowed_in_identifiers, "E011",                       \
      { source_code_span character; },                                         \
      .error(QLJS_TRANSLATABLE("character is not allowed in identifiers"),     \
             character))                                                       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_character_disallowed_in_identifiers, "E012",               \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE(                                                \
                 "escaped character is not allowed in identifiers"),           \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_escaped_code_point_in_identifier_out_of_range, "E013",             \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE("code point out of range"), escape_sequence))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_newline, "E014",                        \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("expected expression before newline"), where))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_expression_before_semicolon, "E015",                      \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("expected expression before semicolon"),        \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_hex_digits_in_unicode_escape, "E016",                     \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE(                                                \
                 "expected hexadecimal digits in Unicode escape sequence"),    \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_parentheses_around_if_condition, "E017",                  \
      { source_code_span condition; },                                         \
      .error(QLJS_TRANSLATABLE(                                                \
                 "if statement needs parentheses around condition"),           \
             condition))                                                       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_expected_parenthesis_around_if_condition, "E018",                  \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      .error(                                                                  \
          QLJS_TRANSLATABLE("if statement is missing '{1}' around condition"), \
          where, string8_view(&token, 1)))                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_binding_in_let_statement, "E019",                          \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("invalid binding in let statement"), where))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_expression_left_of_assignment, "E020",                     \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("invalid expression left of assignment"),       \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_lone_literal_in_object_literal, "E021",                    \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("invalid lone literal in object literal"),      \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_utf_8_sequence, "E022", { source_code_span sequence; },    \
      .error(QLJS_TRANSLATABLE("invalid UTF-8 sequence"), sequence))           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_keywords_cannot_contain_escape_sequences, "E023",                  \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE("keywords cannot contain escape sequences"),    \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_let_with_no_bindings, "E024", { source_code_span where; },         \
      .error(QLJS_TRANSLATABLE("let with no bindings"), where))                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_comma_between_object_literal_entries, "E025",              \
      { source_code_span where; },                                             \
      .error(                                                                  \
          QLJS_TRANSLATABLE("missing comma between object literal entries"),   \
          where))                                                              \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_operand_for_operator, "E026", { source_code_span where; }, \
      .error(QLJS_TRANSLATABLE("missing operand for operator"), where))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_semicolon_after_expression, "E027",                        \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("missing semicolon after expression"), where))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_consecutive_underscores, "E028",           \
      { source_code_span underscores; },                                       \
      .error(QLJS_TRANSLATABLE(                                                \
                 "number literal contains consecutive underscores"),           \
             underscores))                                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_number_literal_contains_trailing_underscores, "E029",              \
      { source_code_span underscores; },                                       \
      .error(                                                                  \
          QLJS_TRANSLATABLE("number literal contains trailing underscore(s)"), \
          underscores))                                                        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_have_exponent, "E030",                       \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("octal literal may not have exponent"),         \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_have_decimal, "E031",                        \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("octal literal may not have decimal"),          \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_octal_literal_may_not_be_big_int, "E032",                          \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("octal literal may not be BigInt"),             \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_global_variable, "E033",                          \
      { identifier redeclaration; },                                           \
      .error(QLJS_TRANSLATABLE("redeclaration of global variable"),            \
             redeclaration))                                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_redeclaration_of_variable, "E034",                                 \
      {                                                                        \
        identifier redeclaration;                                              \
        identifier original_declaration;                                       \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("redeclaration of variable: {0}"),              \
             redeclaration)                                                    \
          .note(QLJS_TRANSLATABLE("variable already declared here"),           \
                original_declaration))                                         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_regexp_literal_flags_cannot_contain_unicode_escapes, "E035",       \
      { source_code_span escape_sequence; },                                   \
      .error(                                                                  \
          QLJS_TRANSLATABLE("RegExp literal cannot contain Unicode escapes"),  \
          escape_sequence))                                                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_stray_comma_in_let_statement, "E036", { source_code_span where; }, \
      .error(QLJS_TRANSLATABLE("stray comma in let statement"), where))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_block_comment, "E037",                                    \
      { source_code_span comment_open; },                                      \
      .error(QLJS_TRANSLATABLE("unclosed block comment"), comment_open))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_identifier_escape_sequence, "E038",                       \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE("unclosed identifier escape sequence"),         \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_regexp_literal, "E039",                                   \
      { source_code_span regexp_literal; },                                    \
      .error(QLJS_TRANSLATABLE("unclosed regexp literal"), regexp_literal))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_string_literal, "E040",                                   \
      { source_code_span string_literal; },                                    \
      .error(QLJS_TRANSLATABLE("unclosed string literal"), string_literal))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unclosed_template, "E041",                                         \
      { source_code_span incomplete_template; },                               \
      .error(QLJS_TRANSLATABLE("unclosed template"), incomplete_template))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_at_character, "E042", { source_code_span character; },  \
      .error(QLJS_TRANSLATABLE("unexpected '@'"), character))                  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_backslash_in_identifier, "E043",                        \
      { source_code_span backslash; },                                         \
      .error(QLJS_TRANSLATABLE("unexpected '\\' in identifier"), backslash))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_number, "E044",                           \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("unexpected characters in number literal"),     \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_control_character, "E045",                              \
      { source_code_span character; },                                         \
      .error(QLJS_TRANSLATABLE("unexpected control character"), character))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_binary_number, "E046",                    \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("unexpected characters in binary literal"),     \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_octal_number, "E047",                     \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("unexpected characters in octal literal"),      \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_characters_in_hex_number, "E048",                       \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("unexpected characters in hex literal"),        \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_binary_number, "E049",                                \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("binary number literal has no digits"),         \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_hex_number, "E050", { source_code_span characters; }, \
      .error(QLJS_TRANSLATABLE("hex number literal has no digits"),            \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_no_digits_in_octal_number, "E051",                                 \
      { source_code_span characters; },                                        \
      .error(QLJS_TRANSLATABLE("octal number literal has no digits"),          \
             characters))                                                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_hash_character, "E052", { source_code_span where; },    \
      .error(QLJS_TRANSLATABLE("unexpected '#'"), where))                      \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_identifier, "E053", { source_code_span where; },        \
      .error(QLJS_TRANSLATABLE("unexpected identifier"), where))               \
                                                                               \
  /* NOTE(strager): Try not to use this error. Find or make a more descriptive \
     and helpful error instead. */                                             \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_token, "E054", { source_code_span token; },             \
      .error(QLJS_TRANSLATABLE("unexpected token"), token))                    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unmatched_indexing_bracket, "E055",                                \
      { source_code_span left_square; },                                       \
      .error(QLJS_TRANSLATABLE("unmatched indexing bracket"), left_square))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unmatched_parenthesis, "E056", { source_code_span where; },        \
      .error(QLJS_TRANSLATABLE("unmatched parenthesis"), where))               \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_use_of_undeclared_variable, "E057", { identifier name; },          \
      .warning(QLJS_TRANSLATABLE("use of undeclared variable: {0}"), name))    \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_variable_used_before_declaration, "E058",                          \
      {                                                                        \
        identifier use;                                                        \
        identifier declaration;                                                \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("variable used before declaration: {0}"), use)  \
          .note(QLJS_TRANSLATABLE("variable declared here"), declaration))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_unexpected_template_string, "E061", { source_code_span where; },   \
      .error(QLJS_TRANSLATABLE("cannot use template string as key in object"), \
             where))                                                           \
                                                                               \
  /* END */

namespace quick_lint_js {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  struct name struct_body;
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

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
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
      const char8 *character, const cli_locator *);
  static void write_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin, const cli_locator *);
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
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
