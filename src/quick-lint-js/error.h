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
      error_cannot_assign_to_variable_named_async_in_for_of_loop, "E082",      \
      { identifier async_identifier; },                                        \
      .error(                                                                  \
          QLJS_TRANSLATABLE(                                                   \
              "assigning to 'async' in a for-of loop requires parentheses"),   \
          async_identifier))                                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_declare_await_in_async_function, "E069",                    \
      { identifier name; },                                                    \
      .error(                                                                  \
          QLJS_TRANSLATABLE("cannot declare 'await' inside async function"),   \
          name))                                                               \
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
      error_cannot_declare_yield_in_generator_function, "E071",                \
      { identifier name; },                                                    \
      .error(QLJS_TRANSLATABLE(                                                \
                 "cannot declare 'yield' inside generator function"),          \
             name))                                                            \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_cannot_export_default_variable, "E076",                            \
      { source_code_span declaring_token; },                                   \
      .error(QLJS_TRANSLATABLE(                                                \
                 "cannot declare and export variable with 'export default'"),  \
             declaring_token))                                                 \
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
      error_comma_not_allowed_after_spread_parameter, "E070",                  \
      {                                                                        \
        source_code_span comma;                                                \
        source_code_span spread;                                               \
      },                                                                       \
      .error(                                                                  \
          QLJS_TRANSLATABLE("commas are not allowed after spread parameter"),  \
          comma))                                                              \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_else_has_no_if, "E065", { source_code_span else_token; },          \
      .error(QLJS_TRANSLATABLE("'else' has no corresponding 'if'"),            \
             else_token))                                                      \
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
      error_extra_comma_not_allowed_between_arguments, "E068",                 \
      { source_code_span comma; },                                             \
      .error(QLJS_TRANSLATABLE(                                                \
                 "extra ',' is not allowed between function call arguments"),  \
             comma))                                                           \
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
      error_exporting_requires_default, "E067",                                \
      { source_code_span expression; },                                        \
      .error(QLJS_TRANSLATABLE("exporting requires 'default'"), expression))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_exporting_requires_curlies, "E066", { source_code_span names; },   \
      .error(QLJS_TRANSLATABLE("exporting requires '{{' and '}'"), names))     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_indexing_requires_expression, "E075",                              \
      { source_code_span squares; },                                           \
      .error(QLJS_TRANSLATABLE("indexing requires an expression"), squares))   \
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
      error_invalid_hex_escape_sequence, "E060",                               \
      { source_code_span escape_sequence; },                                   \
      .error(QLJS_TRANSLATABLE("invalid hex escape sequence: {0}"),            \
             escape_sequence))                                                 \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_lone_literal_in_object_literal, "E021",                    \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("invalid lone literal in object literal"),      \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_invalid_rhs_for_dot_operator, "E074", { source_code_span dot; },   \
      .error(QLJS_TRANSLATABLE(                                                \
                 "'.' operator needs a key name; use + to concatenate "        \
                 "strings; use [] to access with a dynamic key"),              \
             dot))                                                             \
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
      error_methods_should_not_use_function_keyword, "E072",                   \
      { source_code_span function_token; },                                    \
      .error(                                                                  \
          QLJS_TRANSLATABLE("methods should not use the 'function' keyword"),  \
          function_token))                                                     \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_body_for_if_statement, "E064",                             \
      { source_code_span if_and_condition; },                                  \
      .error(QLJS_TRANSLATABLE("missing body for 'if' statement"),             \
             if_and_condition))                                                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_comma_between_object_literal_entries, "E025",              \
      { source_code_span where; },                                             \
      .error(                                                                  \
          QLJS_TRANSLATABLE("missing comma between object literal entries"),   \
          where))                                                              \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_expression_between_parentheses, "E078",                    \
      {                                                                        \
        source_code_span left_paren;                                           \
        source_code_span right_paren;                                          \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("missing expression between parentheses"),      \
             source_code_span(left_paren.begin(), right_paren.end())))         \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_function_parameter_list, "E073",                           \
      { source_code_span function_name; },                                     \
      .error(QLJS_TRANSLATABLE("missing function parameter list"),             \
             function_name))                                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_name_in_function_statement, "E061",                        \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("missing name in function statement"), where))  \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_name_in_class_statement, "E080",                           \
      { source_code_span class_keyword; },                                     \
      .error(QLJS_TRANSLATABLE("missing name of class"), class_keyword))       \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_name_of_exported_class, "E081",                            \
      { source_code_span class_keyword; },                                     \
      .error(QLJS_TRANSLATABLE("missing name of exported class"),              \
             class_keyword))                                                   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_name_of_exported_function, "E079",                         \
      { source_code_span function_keyword; },                                  \
      .error(QLJS_TRANSLATABLE("missing name of exported function"),           \
             function_keyword))                                                \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_name_or_parentheses_for_function, "E062",                  \
      {                                                                        \
        source_code_span where;                                                \
        source_code_span function;                                             \
      },                                                                       \
      .error(QLJS_TRANSLATABLE("missing name or parentheses for function"),    \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_operand_for_operator, "E026", { source_code_span where; }, \
      .error(QLJS_TRANSLATABLE("missing operand for operator"), where))        \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_operator_between_expression_and_arrow_function, "E063",    \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE(                                                \
                 "missing operator between expression and arrow function"),    \
             where))                                                           \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_semicolon_after_statement, "E027",                         \
      { source_code_span where; },                                             \
      .error(QLJS_TRANSLATABLE("missing semicolon after statement"), where))   \
                                                                               \
  QLJS_ERROR_TYPE(                                                             \
      error_missing_value_for_object_literal_entry, "E083",                    \
      { source_code_span key; },                                               \
      .error(QLJS_TRANSLATABLE("missing value for object property"), key))     \
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
