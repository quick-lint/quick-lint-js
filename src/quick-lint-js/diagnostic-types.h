// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAGNOSTIC_TYPES_H
#define QUICK_LINT_JS_DIAGNOSTIC_TYPES_H

#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/identifier.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/translation.h>

// QLJS_DIAG_TYPE should have the following signature:
//
// #define QLJS_DIAG_TYPE(error_name, error_code, severity, struct_body,
// format) ...
//
// * error_name: identifier
// * error_code: string literal
// * severity: diagnostic_severity value
// * struct_body: class member list, wrapped in { }
// * format: member function calls
//
// A class named *error_name* is created in the quick_lint_js namespace.
// *struct_body* is the body of the class.
//
// *format* should look like the following:
//
//    MESSAGE(QLJS_TRANSLATABLE("format string"), source_location)
//
// Within *format*:
//
// * MESSAGE's first argument must be QLJS_TRANSLATABLE(...)
// * MESSAGE's second argument must be a member variable of the *error_name*
//   class (i.e. listed in *struct_body*)
// * MESSAGE's second argument must have type *identifier* or *source_code_span*
#define QLJS_X_DIAG_TYPES                                                      \
  QLJS_DIAG_TYPE(                                                              \
      diag_adjacent_jsx_without_parent, "E0189", diagnostic_severity::error,   \
      {                                                                        \
        source_code_span begin;                                                \
        source_code_span begin_of_second_element;                              \
        source_code_span end;                                                  \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing '<>' and '</>' to enclose multiple children"),      \
              begin) MESSAGE(QLJS_TRANSLATABLE("children end here"), end))     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_before_variable_declaration, "E0001",                    \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier assignment;                                                 \
        identifier declaration;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("variable assigned before its declaration"),   \
              assignment)                                                      \
          MESSAGE(QLJS_TRANSLATABLE("variable declared here"), declaration))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_makes_condition_constant, "E0188",                       \
      diagnostic_severity::warning, { source_code_span assignment_operator; }, \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "'=' changes variables; to compare, use '===' instead"),     \
              assignment_operator))                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_to_const_global_variable, "E0002",                       \
      diagnostic_severity::error, { identifier assignment; },                  \
      MESSAGE(QLJS_TRANSLATABLE("assignment to const global variable"),        \
              assignment))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_to_const_variable, "E0003", diagnostic_severity::error,  \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("assignment to const variable"), assignment)   \
          MESSAGE(QLJS_TRANSLATABLE("const variable declared here"),           \
                  declaration))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_to_imported_variable, "E0185",                           \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("assignment to imported variable"),            \
              assignment)                                                      \
          MESSAGE(QLJS_TRANSLATABLE("imported variable declared here"),        \
                  declaration))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_to_const_variable_before_its_declaration, "E0004",       \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier declaration;                                                \
        identifier assignment;                                                 \
        variable_kind var_kind;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "assignment to const variable before its declaration"),      \
              assignment)                                                      \
          MESSAGE(QLJS_TRANSLATABLE("const variable declared here"),           \
                  declaration))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_assignment_to_undeclared_variable, "E0059",                         \
      diagnostic_severity::warning, { identifier assignment; },                \
      MESSAGE(QLJS_TRANSLATABLE("assignment to undeclared variable"),          \
              assignment))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_await_operator_outside_async, "E0162", diagnostic_severity::error,  \
      { source_code_span await_operator; },                                    \
      MESSAGE(QLJS_TRANSLATABLE("'await' is only allowed in async functions"), \
              await_operator))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_await_followed_by_arrow_function, "E0178",                          \
      diagnostic_severity::error, { source_code_span await_operator; },        \
      MESSAGE(QLJS_TRANSLATABLE("'await' cannot be followed by an arrow "      \
                                "function; use 'async' instead"),              \
              await_operator))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_async_static_method, "E0269", diagnostic_severity::error,           \
      { source_code_span async_static; },                                      \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "'async static' is not allowed; write 'static async' instead"),  \
          async_static))                                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_big_int_literal_contains_decimal_point, "E0005",                    \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("BigInt literal contains decimal point"),      \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_big_int_literal_contains_exponent, "E0006",                         \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("BigInt literal contains exponent"), where))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_c_style_for_loop_is_missing_third_component, "E0093",               \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span expected_last_component;                              \
        source_code_span existing_semicolon;                                   \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "C-style for loop is missing its third component"),          \
              expected_last_component))                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop, "E0173",       \
      diagnostic_severity::error, { source_code_span equal_token; },           \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "cannot assign to loop variable in for of/in loop"),         \
              equal_token))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_access_private_identifier_outside_class, "E0208",            \
      diagnostic_severity::error, { identifier private_identifier; },          \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("cannot access private identifier outside class"), \
          private_identifier))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_assign_to_variable_named_async_in_for_of_loop, "E0082",      \
      diagnostic_severity::error, { identifier async_identifier; },            \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "assigning to 'async' in a for-of loop requires parentheses"),   \
          async_identifier))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_await_in_async_function, "E0069",                    \
      diagnostic_severity::error, { identifier name; },                        \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("cannot declare 'await' inside async function"),   \
          name))                                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_class_named_let, "E0007",                            \
      diagnostic_severity::error, { source_code_span name; },                  \
      MESSAGE(QLJS_TRANSLATABLE("classes cannot be named 'let'"), name))       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_class_named_await_in_async_function, "E0707",        \
      diagnostic_severity::error, { source_code_span name; },                  \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "classes cannot be named 'await' in async function"),        \
              name))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_interface_named_await_in_async_function, "E0222",    \
      diagnostic_severity::error, { source_code_span name; },                  \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript interfaces cannot be named "       \
                                "'await' in async function"),                  \
              name))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_variable_named_let_with_let, "E0008",                \
      diagnostic_severity::error, { source_code_span name; },                  \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "let statement cannot declare variables named 'let'"),       \
              name))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_variable_with_keyword_name, "E0124",                 \
      diagnostic_severity::error, { source_code_span keyword; },               \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("cannot declare variable named keyword '{0}'"),    \
          keyword))                                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_declare_yield_in_generator_function, "E0071",                \
      diagnostic_severity::error, { identifier name; },                        \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "cannot declare 'yield' inside generator function"),         \
              name))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_export_default_variable, "E0076",                            \
      diagnostic_severity::error, { source_code_span declaring_token; },       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "cannot declare and export variable with 'export default'"), \
              declaring_token))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_export_let, "E0009", diagnostic_severity::error,             \
      { source_code_span export_name; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("cannot export variable named 'let'"),         \
              export_name))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_export_variable_named_keyword, "E0144",                      \
      diagnostic_severity::error, { identifier export_name; },                 \
      MESSAGE(QLJS_TRANSLATABLE("cannot export variable named keyword '{0}'"), \
              export_name))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_import_let, "E0010", diagnostic_severity::error,             \
      { source_code_span import_name; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("cannot import 'let'"), import_name))          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_import_variable_named_keyword, "E0145",                      \
      diagnostic_severity::error, { identifier import_name; },                 \
      MESSAGE(QLJS_TRANSLATABLE("cannot import variable named keyword '{0}'"), \
              import_name))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_refer_to_private_variable_without_object, "E0155",           \
      diagnostic_severity::error, { identifier private_identifier; },          \
      MESSAGE(QLJS_TRANSLATABLE("cannot reference private variables without "  \
                                "object; use 'this.'"),                        \
              private_identifier))                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_cannot_update_variable_during_declaration, "E0136",                 \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span declaring_token;                                      \
        source_code_span updating_operator;                                    \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "cannot update variable with '{0}' while declaring it"),     \
              updating_operator)                                               \
          MESSAGE(QLJS_TRANSLATABLE(                                           \
                      "remove '{0}' to update an existing variable"),          \
                  declaring_token))                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_catch_without_try, "E0117", diagnostic_severity::error,             \
      { source_code_span catch_token; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("unexpected 'catch' without 'try'"),           \
              catch_token))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_class_statement_not_allowed_in_body, "E0149",                       \
      diagnostic_severity::error,                                              \
      {                                                                        \
        statement_kind kind_of_statement;                                      \
        source_code_span expected_body;                                        \
        source_code_span class_keyword;                                        \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing body for {1:headlinese}"),            \
              expected_body, kind_of_statement)                                \
          MESSAGE(QLJS_TRANSLATABLE("a class statement is not allowed as the " \
                                    "body of {1:singular}"),                   \
                  class_keyword, kind_of_statement))                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_character_disallowed_in_identifiers, "E0011",                       \
      diagnostic_severity::error, { source_code_span character; },             \
      MESSAGE(QLJS_TRANSLATABLE("character is not allowed in identifiers"),    \
              character))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_comma_not_allowed_after_spread_parameter, "E0070",                  \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span comma;                                                \
        source_code_span spread;                                               \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("commas are not allowed after spread parameter"),  \
          comma))                                                              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_comma_not_allowed_between_class_methods, "E0209",                   \
      diagnostic_severity::error, { source_code_span unexpected_comma; },      \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("commas are not allowed between class methods"),   \
          unexpected_comma))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_json_syntax_error, "E0164", diagnostic_severity::error,      \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("JSON syntax error"), where))                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_global_groups_group_type_mismatch, "E0170",                  \
      diagnostic_severity::error, { source_code_span group; },                 \
      MESSAGE(QLJS_TRANSLATABLE("\"global-groups\" entries must be strings"),  \
              group))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_global_groups_type_mismatch, "E0169",                        \
      diagnostic_severity::error, { source_code_span value; },                 \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "\"global-groups\" must be a boolean or an array"),          \
              value))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_globals_descriptor_type_mismatch, "E0171",                   \
      diagnostic_severity::error, { source_code_span descriptor; },            \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "\"globals\" descriptor must be a boolean or an object"),    \
              descriptor))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_globals_descriptor_shadowable_type_mismatch, "E0166",        \
      diagnostic_severity::error, { source_code_span value; },                 \
      MESSAGE(QLJS_TRANSLATABLE("\"globals\" descriptor \"shadowable\" "       \
                                "property must be a boolean"),                 \
              value))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_globals_descriptor_writable_type_mismatch, "E0167",          \
      diagnostic_severity::error, { source_code_span value; },                 \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("\"globals\" descriptor \"writable\" property "    \
                            "must be a boolean"),                              \
          value))                                                              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_config_globals_type_mismatch, "E0168", diagnostic_severity::error,  \
      { source_code_span value; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("\"globals\" must be an object"), value))      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_depth_limit_exceeded, "E0203", diagnostic_severity::error,          \
      { source_code_span token; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("depth limit exceeded"), token))               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_dot_dot_is_not_an_operator, "E0053", diagnostic_severity::error,    \
      { source_code_span dots; },                                              \
      MESSAGE(QLJS_TRANSLATABLE("missing property name between '.' and '.'"),  \
              dots))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_else_has_no_if, "E0065", diagnostic_severity::error,                \
      { source_code_span else_token; },                                        \
      MESSAGE(QLJS_TRANSLATABLE("'else' has no corresponding 'if'"),           \
              else_token))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_equals_does_not_distribute_over_or, "E0190",                        \
      diagnostic_severity::warning,                                            \
      {                                                                        \
        source_code_span or_operator;                                          \
        source_code_span equals_operator;                                      \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing comparison; '{1}' does not extend "   \
                                "to the right side of '{0}'"),                 \
              or_operator, equals_operator)                                    \
          MESSAGE(QLJS_TRANSLATABLE("'{0}' found here"), equals_operator))     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_escaped_character_disallowed_in_identifiers, "E0012",               \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "escaped character is not allowed in identifiers"),          \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_escaped_code_point_in_identifier_out_of_range, "E0013",             \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE("code point out of range"), escape_sequence))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_escaped_code_point_in_unicode_out_of_range, "E0207",                \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE("code point in Unicode escape sequence must "  \
                                "not be greater than U+10FFFF"),               \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_escaped_hyphen_not_allowed_in_jsx_tag, "E0019",                     \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "escaping '-' is not allowed in tag names; write '-' instead"),  \
          escape_sequence))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_extra_comma_not_allowed_between_arguments, "E0068",                 \
      diagnostic_severity::error, { source_code_span comma; },                 \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "extra ',' is not allowed between function call arguments"), \
              comma))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_as_before_imported_namespace_alias, "E0126",               \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span star_through_alias_token;                             \
        source_code_span alias;                                                \
        source_code_span star_token;                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("expected 'as' between '{1}' and '{2}'"),      \
              star_through_alias_token, star_token, alias))                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_comma_to_separate_object_literal_entries, "E0131",         \
      diagnostic_severity::error, { source_code_span unexpected_token; },      \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("expected ',' between object literal entries"),    \
          unexpected_token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_expression_before_newline, "E0014",                        \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("expected expression before newline"), where)) \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_expression_for_switch_case, "E0140",                       \
      diagnostic_severity::error, { source_code_span case_token; },            \
      MESSAGE(QLJS_TRANSLATABLE("expected expression after 'case'"),           \
              case_token))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_expression_before_semicolon, "E0015",                      \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("expected expression before semicolon"),       \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_from_and_module_specifier, "E0129",                        \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("expected 'from \"name_of_module.mjs\"'"),     \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_from_before_module_specifier, "E0128",                     \
      diagnostic_severity::error, { source_code_span module_specifier; },      \
      MESSAGE(QLJS_TRANSLATABLE("expected 'from' before module specifier"),    \
              module_specifier))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_hex_digits_in_unicode_escape, "E0016",                     \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "expected hexadecimal digits in Unicode escape sequence"),   \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_left_curly, "E0107", diagnostic_severity::error,           \
      { source_code_span expected_left_curly; },                               \
      MESSAGE(QLJS_TRANSLATABLE("expected '{{'"), expected_left_curly))        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_right_paren_for_function_call, "E0141",                    \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span expected_right_paren;                                 \
        source_code_span left_paren;                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("expected ')' to close function call"),        \
              expected_right_paren)                                            \
          MESSAGE(QLJS_TRANSLATABLE("function call started here"),             \
                  left_paren))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parentheses_around_do_while_condition, "E0084",            \
      diagnostic_severity::error, { source_code_span condition; },             \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "do-while loop needs parentheses around condition"),         \
              condition))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parenthesis_around_do_while_condition, "E0085",            \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "do-while loop is missing '{1}' around condition"),          \
              where, token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parentheses_around_if_condition, "E0017",                  \
      diagnostic_severity::error, { source_code_span condition; },             \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "if statement needs parentheses around condition"),          \
              condition))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parenthesis_around_if_condition, "E0018",                  \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("if statement is missing '{1}' around condition"), \
          where, token))                                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parentheses_around_switch_condition, "E0091",              \
      diagnostic_severity::error, { source_code_span condition; },             \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "switch statement needs parentheses around condition"),      \
              condition))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parenthesis_around_switch_condition, "E0092",              \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "switch statement is missing '{1}' around condition"),       \
              where, token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parentheses_around_while_condition, "E0087",               \
      diagnostic_severity::error, { source_code_span condition; },             \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("while loop needs parentheses around condition"),  \
          condition))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parenthesis_around_while_condition, "E0088",               \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("while loop is missing '{1}' around condition"),   \
          where, token))                                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parentheses_around_with_expression, "E0089",               \
      diagnostic_severity::error, { source_code_span expression; },            \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "with statement needs parentheses around expression"),       \
              expression))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_parenthesis_around_with_expression, "E0090",               \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        char8 token;                                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "with statement is missing '{1}' around expression"),        \
              where, token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_variable_name_for_catch, "E0135",                          \
      diagnostic_severity::error, { source_code_span unexpected_token; },      \
      MESSAGE(QLJS_TRANSLATABLE("expected variable name for 'catch'"),         \
              unexpected_token))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_expected_variable_name_for_import_as, "E0175",                      \
      diagnostic_severity::error, { source_code_span unexpected_token; },      \
      MESSAGE(QLJS_TRANSLATABLE("expected variable name for 'import'-'as'"),   \
              unexpected_token))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_exporting_requires_default, "E0067", diagnostic_severity::error,    \
      { source_code_span expression; },                                        \
      MESSAGE(QLJS_TRANSLATABLE("exporting requires 'default'"), expression))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_exporting_requires_curlies, "E0066", diagnostic_severity::error,    \
      { source_code_span names; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("exporting requires '{{' and '}'"), names))    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_exporting_string_name_only_allowed_for_export_from, "E0153",        \
      diagnostic_severity::error, { source_code_span export_name; },           \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "forwarding exports are only allowed in export-from"),       \
              export_name))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_finally_without_try, "E0118", diagnostic_severity::error,           \
      { source_code_span finally_token; },                                     \
      MESSAGE(QLJS_TRANSLATABLE("unexpected 'finally' without 'try'"),         \
              finally_token))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_function_statement_not_allowed_in_body, "E0148",                    \
      diagnostic_severity::error,                                              \
      {                                                                        \
        statement_kind kind_of_statement;                                      \
        source_code_span expected_body;                                        \
        source_code_span function_keywords;                                    \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing body for {1:headlinese}"),            \
              expected_body, kind_of_statement)                                \
          MESSAGE(                                                             \
              QLJS_TRANSLATABLE("a function statement is not allowed as the "  \
                                "body of {1:singular}"),                       \
              function_keywords, kind_of_statement))                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_generator_function_star_belongs_after_keyword_function, "E0204",    \
      diagnostic_severity::error, { source_code_span star; },                  \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "generator function '*' belongs after keyword function"),    \
              star))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_generator_function_star_belongs_before_name, "E0133",               \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span function_name;                                        \
        source_code_span star;                                                 \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "generator function '*' belongs before function name"),      \
              star))                                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_in_disallowed_in_c_style_for_loop, "E0108",                         \
      diagnostic_severity::error, { source_code_span in_token; },              \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "'in' disallowed in C-style for loop initializer"),          \
              in_token))                                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_indexing_requires_expression, "E0075", diagnostic_severity::error,  \
      { source_code_span squares; },                                           \
      MESSAGE(QLJS_TRANSLATABLE("indexing requires an expression"), squares))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_expression_left_of_assignment, "E0020",                     \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("invalid expression left of assignment"),      \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_hex_escape_sequence, "E0060", diagnostic_severity::error,   \
      { source_code_span escape_sequence; },                                   \
      MESSAGE(QLJS_TRANSLATABLE("invalid hex escape sequence: {0}"),           \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_lone_literal_in_object_literal, "E0021",                    \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("invalid lone literal in object literal"),     \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_parameter, "E0151", diagnostic_severity::error,             \
      { source_code_span parameter; },                                         \
      MESSAGE(QLJS_TRANSLATABLE("invalid function parameter"), parameter))     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_quotes_around_string_literal, "E0197",                      \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span opening_quote;                                        \
        char8 suggested_quote;                                                 \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "'{0}' is not allowed for strings; use {1} instead"),        \
              opening_quote, suggested_quote))                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_rhs_for_dot_operator, "E0074", diagnostic_severity::error,  \
      { source_code_span dot; },                                               \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "'.' operator needs a key name; use + to concatenate "       \
                  "strings; use [] to access with a dynamic key"),             \
              dot))                                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_utf_8_sequence, "E0022", diagnostic_severity::error,        \
      { source_code_span sequence; },                                          \
      MESSAGE(QLJS_TRANSLATABLE("invalid UTF-8 sequence"), sequence))          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_jsx_attribute_has_wrong_capitalization, "E0192",                    \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier attribute_name;                                             \
        string8_view expected_attribute_name;                                  \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "attribute has wrong capitalization; write '{1}' instead"),  \
              attribute_name, expected_attribute_name))                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_jsx_attribute_renamed_by_react, "E0193",                            \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier attribute_name;                                             \
        string8_view react_attribute_name;                                     \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "misspelled React attribute; write '{1}' instead"),          \
              attribute_name, react_attribute_name))                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_jsx_event_attribute_should_be_camel_case, "E0191",                  \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier attribute_name;                                             \
        string8_view expected_attribute_name;                                  \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("event attributes must be camelCase: '{1}'"),  \
              attribute_name, expected_attribute_name))                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_jsx_not_yet_implemented, "E0177", diagnostic_severity::error,       \
      { source_code_span jsx_start; },                                         \
      MESSAGE(QLJS_TRANSLATABLE("React/JSX is not yet implemented"),           \
              jsx_start))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_keywords_cannot_contain_escape_sequences, "E0023",                  \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE("keywords cannot contain escape sequences"),   \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_label_named_await_not_allowed_in_async_function, "E0206",           \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span await;                                                \
        source_code_span colon;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "label named 'await' not allowed in async function"),        \
              await))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_legacy_octal_literal_may_not_be_big_int, "E0032",                   \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("legacy octal literal may not be BigInt"),     \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_legacy_octal_literal_may_not_contain_underscores, "E0152",          \
      diagnostic_severity::error, { source_code_span underscores; },           \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "legacy octal literals may not contain underscores"),        \
              underscores))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_let_with_no_bindings, "E0024", diagnostic_severity::error,          \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("{0} with no bindings"), where))               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_lexical_declaration_not_allowed_in_body, "E0150",                   \
      diagnostic_severity::error,                                              \
      {                                                                        \
        statement_kind kind_of_statement;                                      \
        source_code_span expected_body;                                        \
        source_code_span declaring_keyword;                                    \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing body for {1:headlinese}"),            \
              expected_body, kind_of_statement)                                \
          MESSAGE(                                                             \
              QLJS_TRANSLATABLE("a lexical declaration is not allowed as the " \
                                "body of {1:singular}"),                       \
              declaring_keyword, kind_of_statement))                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_functions_or_methods_should_not_have_arrow_operator, "E0174",       \
      diagnostic_severity::error, { source_code_span arrow_operator; },        \
      MESSAGE(QLJS_TRANSLATABLE("functions/methods should not have '=>'"),     \
              arrow_operator))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_methods_should_not_use_function_keyword, "E0072",                   \
      diagnostic_severity::error, { source_code_span function_token; },        \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("methods should not use the 'function' keyword"),  \
          function_token))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_mismatched_jsx_tags, "E0187", diagnostic_severity::error,           \
      {                                                                        \
        source_code_span opening_tag_name;                                     \
        source_code_span closing_tag_name;                                     \
        string8_view opening_tag_name_pretty;                                  \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("mismatched JSX tags; expected '</{1}>'"),     \
              closing_tag_name, opening_tag_name_pretty)                       \
          MESSAGE(QLJS_TRANSLATABLE("opening '<{1}>' tag here"),               \
                  opening_tag_name, opening_tag_name_pretty))                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_array_close, "E0157", diagnostic_severity::error,           \
      {                                                                        \
        source_code_span left_square;                                          \
        source_code_span expected_right_square;                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing end of array; expected ']'"),         \
              expected_right_square)                                           \
          MESSAGE(QLJS_TRANSLATABLE("array started here"), left_square))       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_arrow_operator_in_arrow_function, "E0176",                  \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("missing arrow operator for arrow function"),  \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_arrow_function_parameter_list, "E0105",                     \
      diagnostic_severity::error, { source_code_span arrow; },                 \
      MESSAGE(QLJS_TRANSLATABLE("missing parameters for arrow function"),      \
              arrow))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_catch_clause, "E0119", diagnostic_severity::error, \
      { source_code_span catch_token; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing body for catch clause"),              \
              catch_token))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_class, "E0111", diagnostic_severity::error,        \
      { source_code_span class_keyword_and_name_and_heritage; },               \
      MESSAGE(QLJS_TRANSLATABLE("missing body for class"),                     \
              class_keyword_and_name_and_heritage))                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_do_while_statement, "E0101",                       \
      diagnostic_severity::error, { source_code_span do_token; },              \
      MESSAGE(QLJS_TRANSLATABLE("missing body for do-while loop"), do_token))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_finally_clause, "E0121",                           \
      diagnostic_severity::error, { source_code_span finally_token; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing body for finally clause"),            \
              finally_token))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_for_statement, "E0094",                            \
      diagnostic_severity::error, { source_code_span for_and_header; },        \
      MESSAGE(QLJS_TRANSLATABLE("missing body for 'for' loop"),                \
              for_and_header))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_if_statement, "E0064", diagnostic_severity::error, \
      { source_code_span expected_body; },                                     \
      MESSAGE(QLJS_TRANSLATABLE("missing body for 'if' statement"),            \
              expected_body))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_switch_statement, "E0106",                         \
      diagnostic_severity::error, { source_code_span switch_and_condition; },  \
      MESSAGE(QLJS_TRANSLATABLE("missing body for 'switch' statement"),        \
              switch_and_condition))                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_try_statement, "E0120",                            \
      diagnostic_severity::error, { source_code_span try_token; },             \
      MESSAGE(QLJS_TRANSLATABLE("missing body for try statement"), try_token)) \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_body_for_while_statement, "E0104",                          \
      diagnostic_severity::error, { source_code_span while_and_condition; },   \
      MESSAGE(QLJS_TRANSLATABLE("missing body for while loop"),                \
              while_and_condition))                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_catch_or_finally_for_try_statement, "E0122",                \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span expected_catch_or_finally;                            \
        source_code_span try_token;                                            \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing catch or finally clause for try statement"),        \
              expected_catch_or_finally)                                       \
          MESSAGE(QLJS_TRANSLATABLE("try statement starts here"), try_token))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_catch_variable_between_parentheses, "E0130",                \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span left_paren_to_right_paren;                            \
        source_code_span left_paren;                                           \
        source_code_span right_paren;                                          \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing catch variable name between parentheses"),          \
              left_paren_to_right_paren))                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_comma_between_object_literal_entries, "E0025",              \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("missing comma between object literal entries"),   \
          where))                                                              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_comma_between_variable_declarations, "E0132",               \
      diagnostic_severity::error, { source_code_span expected_comma; },        \
      MESSAGE(QLJS_TRANSLATABLE("missing ',' between variable declarations"),  \
              expected_comma))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_colon_in_conditional_expression, "E0146",                   \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span expected_colon;                                       \
        source_code_span question;                                             \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing ':' in conditional expression"),      \
              expected_colon)                                                  \
          MESSAGE(QLJS_TRANSLATABLE("'?' creates a conditional expression"),   \
                  question))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_condition_for_if_statement, "E0138",                        \
      diagnostic_severity::error, { source_code_span if_keyword; },            \
      MESSAGE(QLJS_TRANSLATABLE("missing condition for if statement"),         \
              if_keyword))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_condition_for_while_statement, "E0139",                     \
      diagnostic_severity::error, { source_code_span while_keyword; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing condition for while statement"),      \
              while_keyword))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_condition_for_switch_statement, "E0137",                    \
      diagnostic_severity::error, { source_code_span switch_keyword; },        \
      MESSAGE(QLJS_TRANSLATABLE("missing condition for switch statement"),     \
              switch_keyword))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_dots_for_attribute_spread, "E0186",                         \
      diagnostic_severity::error, { source_code_span expected_dots; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing '...' in JSX attribute spread"),      \
              expected_dots))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_equal_after_variable, "E0202", diagnostic_severity::error,  \
      { source_code_span expected_equal; },                                    \
      MESSAGE(QLJS_TRANSLATABLE("missing '=' after variable"),                 \
              expected_equal))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_expression_between_parentheses, "E0078",                    \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span left_paren_to_right_paren;                            \
        source_code_span left_paren;                                           \
        source_code_span right_paren;                                          \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing expression between parentheses"),     \
              left_paren_to_right_paren))                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_for_loop_header, "E0125", diagnostic_severity::error,       \
      { source_code_span for_token; },                                         \
      MESSAGE(QLJS_TRANSLATABLE("missing header and body for 'for' loop"),     \
              for_token))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_for_loop_rhs_or_components_after_expression, "E0097",       \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span header;                                               \
        source_code_span for_token;                                            \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "for loop needs an iterable, or condition and update clauses"),  \
          header)                                                              \
          MESSAGE(                                                             \
              QLJS_TRANSLATABLE(                                               \
                  "use 'while' instead to loop until a condition is false"),   \
              for_token))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_for_loop_rhs_or_components_after_declaration, "E0098",      \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span header;                                               \
        source_code_span for_token;                                            \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "for loop needs an iterable, or condition and update clauses"),  \
          header))                                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_function_parameter_list, "E0073",                           \
      diagnostic_severity::error,                                              \
      { source_code_span expected_parameter_list; },                           \
      MESSAGE(QLJS_TRANSLATABLE("missing function parameter list"),            \
              expected_parameter_list))                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_function_body, "E0172", diagnostic_severity::error,         \
      { source_code_span expected_body; },                                     \
      MESSAGE(QLJS_TRANSLATABLE("missing body for function"), expected_body))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_header_of_for_loop, "E0096", diagnostic_severity::error,    \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("missing for loop header"), where))            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_initializer_in_const_declaration, "E0205",                  \
      diagnostic_severity::error, { source_code_span variable_name; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing initializer in const declaration"),   \
              variable_name))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_key_for_object_entry, "E0154", diagnostic_severity::error,  \
      { source_code_span expression; },                                        \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "unexpected expression; missing key for object entry"),      \
              expression))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_class_method_name, "E0229", diagnostic_severity::error,     \
      { source_code_span expected_name; },                                     \
      MESSAGE(QLJS_TRANSLATABLE("missing name for class method"),              \
              expected_name))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_name_in_function_statement, "E0061",                        \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("missing name in function statement"), where)) \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_name_in_class_statement, "E0080",                           \
      diagnostic_severity::error, { source_code_span class_keyword; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing name of class"), class_keyword))      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_name_of_exported_class, "E0081",                            \
      diagnostic_severity::error, { source_code_span class_keyword; },         \
      MESSAGE(QLJS_TRANSLATABLE("missing name of exported class"),             \
              class_keyword))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_name_of_exported_function, "E0079",                         \
      diagnostic_severity::error, { source_code_span function_keyword; },      \
      MESSAGE(QLJS_TRANSLATABLE("missing name of exported function"),          \
              function_keyword))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_name_or_parentheses_for_function, "E0062",                  \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span where;                                                \
        source_code_span function;                                             \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing name or parentheses for function"),   \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_operand_for_operator, "E0026", diagnostic_severity::error,  \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("missing operand for operator"), where))       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_redundant_delete_statement_on_variable, "E0086",                    \
      diagnostic_severity::warning, { source_code_span delete_expression; },   \
      MESSAGE(QLJS_TRANSLATABLE("redundant delete statement on variable"),     \
              delete_expression))                                              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_if_after_else, "E0184", diagnostic_severity::error,         \
      { source_code_span expected_if; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("missing 'if' after 'else'"), expected_if))    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_operator_between_expression_and_arrow_function, "E0063",    \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing operator between expression and arrow function"),   \
              where))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_parentheses_around_exponent_with_unary_lhs, "E0195",        \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span exponent_expression;                                  \
        source_code_span unary_operator;                                       \
      },                                                                       \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("missing parentheses around operand of '{0}'"),    \
          exponent_expression)                                                 \
          MESSAGE(                                                             \
              QLJS_TRANSLATABLE("'{0}' operator cannot be used before '**' "   \
                                "without parentheses"),                        \
              unary_operator))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_parentheses_around_self_invoked_function, "E0211",          \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span invocation;                                           \
        source_code_span func_start;                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing parentheses around self-invoked function"),         \
              invocation)                                                      \
          MESSAGE(QLJS_TRANSLATABLE("function starts here"), func_start))      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_parentheses_around_unary_lhs_of_exponent, "E0194",          \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span unary_expression;                                     \
        source_code_span exponent_operator;                                    \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing parentheses around left-hand side of '**'"),        \
              unary_expression)                                                \
          MESSAGE(                                                             \
              QLJS_TRANSLATABLE("'**' operator cannot be used after unary "    \
                                "'{1}' without parentheses"),                  \
              exponent_operator, unary_expression))                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_property_name_for_dot_operator, "E0142",                    \
      diagnostic_severity::error, { source_code_span dot; },                   \
      MESSAGE(QLJS_TRANSLATABLE("missing property name after '.' operator"),   \
              dot))                                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_semicolon_after_statement, "E0027",                         \
      diagnostic_severity::error, { source_code_span where; },                 \
      MESSAGE(QLJS_TRANSLATABLE("missing semicolon after statement"), where))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_semicolon_after_field, "E0223", diagnostic_severity::error, \
      { source_code_span expected_semicolon; },                                \
      MESSAGE(QLJS_TRANSLATABLE("missing semicolon after field"),              \
              expected_semicolon))                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_semicolon_after_index_signature, "E0226",                   \
      diagnostic_severity::error, { source_code_span expected_semicolon; },    \
      MESSAGE(QLJS_TRANSLATABLE("missing semicolon after index signature"),    \
              expected_semicolon))                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_semicolon_between_for_loop_condition_and_update, "E0100",   \
      diagnostic_severity::error, { source_code_span expected_semicolon; },    \
      MESSAGE(QLJS_TRANSLATABLE("missing semicolon between condition and "     \
                                "update parts of for loop"),                   \
              expected_semicolon))                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_semicolon_between_for_loop_init_and_condition, "E0099",     \
      diagnostic_severity::error, { source_code_span expected_semicolon; },    \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("missing semicolon between init and condition "    \
                            "parts of for loop"),                              \
          expected_semicolon))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_token_after_export, "E0113", diagnostic_severity::error,    \
      { source_code_span export_token; },                                      \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "incomplete export; expected 'export default ...' or "       \
                  "'export {{name}' or 'export * from ...' or 'export class' " \
                  "or 'export function' or 'export let'"),                     \
              export_token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_value_for_object_literal_entry, "E0083",                    \
      diagnostic_severity::error, { source_code_span key; },                   \
      MESSAGE(QLJS_TRANSLATABLE("missing value for object property"), key))    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_variable_name_in_declaration, "E0123",                      \
      diagnostic_severity::error, { source_code_span equal_token; },           \
      MESSAGE(QLJS_TRANSLATABLE("missing variable name"), equal_token))        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_missing_while_and_condition_for_do_while_statement, "E0103",        \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span do_token;                                             \
        source_code_span expected_while;                                       \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "missing 'while (condition)' for do-while statement"),       \
              expected_while)                                                  \
          MESSAGE(QLJS_TRANSLATABLE("do-while statement starts here"),         \
                  do_token))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_newline_not_allowed_between_async_and_parameter_list, "E0163",      \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span async;                                                \
        source_code_span arrow;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("newline is not allowed between 'async' and "  \
                                "arrow function parameter list"),              \
              async) MESSAGE(QLJS_TRANSLATABLE("arrow is here"), arrow))       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_number_literal_contains_consecutive_underscores, "E0028",           \
      diagnostic_severity::error, { source_code_span underscores; },           \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "number literal contains consecutive underscores"),          \
              underscores))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_number_literal_contains_trailing_underscores, "E0029",              \
      diagnostic_severity::error, { source_code_span underscores; },           \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("number literal contains trailing underscore(s)"), \
          underscores))                                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_octal_literal_may_not_have_exponent, "E0030",                       \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("octal literal may not have exponent"),        \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_octal_literal_may_not_have_decimal, "E0031",                        \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("octal literal may not have decimal"),         \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_integer_literal_will_lose_precision, "E0212",                       \
      diagnostic_severity::warning,                                            \
      {                                                                        \
        source_code_span characters;                                           \
        string8_view rounded_val;                                              \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("integer cannot be represented and will be "   \
                                "rounded to '{1}'"),                           \
              characters, rounded_val))                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_private_properties_are_not_allowed_in_object_literals, "E0156",     \
      diagnostic_severity::error, { identifier private_identifier; },          \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "private properties are not allowed in object literals"),    \
              private_identifier))                                             \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_readonly_static_field, "E0232", diagnostic_severity::error,         \
      { source_code_span readonly_static; },                                   \
      MESSAGE(QLJS_TRANSLATABLE("'readonly static' is not allowed; write "     \
                                "'static readonly' instead"),                  \
              readonly_static))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_redeclaration_of_global_variable, "E0033",                          \
      diagnostic_severity::error, { identifier redeclaration; },               \
      MESSAGE(QLJS_TRANSLATABLE("redeclaration of global variable"),           \
              redeclaration))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_redeclaration_of_variable, "E0034", diagnostic_severity::error,     \
      {                                                                        \
        identifier redeclaration;                                              \
        identifier original_declaration;                                       \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("redeclaration of variable: {0}"),             \
              redeclaration)                                                   \
          MESSAGE(QLJS_TRANSLATABLE("variable already declared here"),         \
                  original_declaration))                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_regexp_literal_flags_cannot_contain_unicode_escapes, "E0035",       \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "RegExp literal flags cannot contain Unicode escapes"),      \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_return_statement_returns_nothing, "E0179",                          \
      diagnostic_severity::warning, { source_code_span return_keyword; },      \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("return statement returns nothing (undefined)"),   \
          return_keyword))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_statement_before_first_switch_case, "E0198",                        \
      diagnostic_severity::error, { source_code_span unexpected_statement; },  \
      MESSAGE(QLJS_TRANSLATABLE("unexpected statement before first switch "    \
                                "case, expected 'case' or 'default'"),         \
              unexpected_statement))                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_stray_comma_in_let_statement, "E0036", diagnostic_severity::error,  \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("stray comma in let statement"), where))       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_stray_comma_in_parameter, "E0180", diagnostic_severity::error,      \
      { source_code_span comma; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("stray comma in function parameter"), comma))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_enum_not_implemented, "E0127",                           \
      diagnostic_severity::error, { source_code_span enum_keyword; },          \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript's 'enum' feature is not yet "      \
                                "implemented by quick-lint-js"),               \
              enum_keyword))                                                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_index_signature_cannot_be_method, "E0227",               \
      diagnostic_severity::error, { source_code_span left_paren; },            \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("index signature must be a field, not a method"),  \
          left_paren))                                                         \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_index_signature_needs_type, "E0225",                     \
      diagnostic_severity::error, { source_code_span expected_type; },         \
      MESSAGE(QLJS_TRANSLATABLE("index signatures require a value type"),      \
              expected_type))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_interfaces_not_allowed_in_javascript, "E0213",           \
      diagnostic_severity::error, { source_code_span interface_keyword; },     \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript's 'interface' feature is not "     \
                                "allowed in JavaScript code"),                 \
              interface_keyword))                                              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_optional_properties_not_allowed_in_javascript, "E0228",  \
      diagnostic_severity::error, { source_code_span question; },              \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript optional properties are not "      \
                                "allowed in JavaScript code"),                 \
              question))                                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_readonly_fields_not_allowed_in_javascript, "E0230",      \
      diagnostic_severity::error, { source_code_span readonly_keyword; },      \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript's 'readonly' feature is not "      \
                                "allowed in JavaScript code"),                 \
              readonly_keyword))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_readonly_method, "E0231", diagnostic_severity::error,    \
      { source_code_span readonly_keyword; },                                  \
      MESSAGE(QLJS_TRANSLATABLE("methods cannot be readonly"),                 \
              readonly_keyword))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_type_annotations_not_allowed_in_javascript, "E0224",     \
      diagnostic_severity::error, { source_code_span type_colon; },            \
      MESSAGE(QLJS_TRANSLATABLE("TypeScript type annotations are not "         \
                                "allowed in JavaScript code"),                 \
              type_colon))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_typescript_style_const_field, "E0165", diagnostic_severity::error,  \
      { source_code_span const_token; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("const fields within classes are only "        \
                                "allowed in TypeScript, not JavaScript"),      \
              const_token))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_block_comment, "E0037", diagnostic_severity::error,        \
      { source_code_span comment_open; },                                      \
      MESSAGE(QLJS_TRANSLATABLE("unclosed block comment"), comment_open))      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_class_block, "E0199", diagnostic_severity::error,          \
      { source_code_span block_open; },                                        \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("unclosed class; expected '}' by end of file"),    \
          block_open))                                                         \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_code_block, "E0134", diagnostic_severity::error,           \
      { source_code_span block_open; },                                        \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "unclosed code block; expected '}' by end of file"),         \
              block_open))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_interface_block, "E0215", diagnostic_severity::error,      \
      { source_code_span block_open; },                                        \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "unclosed interface; expected '}' by end of file"),          \
              block_open))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_identifier_escape_sequence, "E0038",                       \
      diagnostic_severity::error, { source_code_span escape_sequence; },       \
      MESSAGE(QLJS_TRANSLATABLE("unclosed identifier escape sequence"),        \
              escape_sequence))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_object_literal, "E0161", diagnostic_severity::error,       \
      {                                                                        \
        source_code_span object_open;                                          \
        source_code_span expected_object_close;                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("unclosed object literal; expected '}'"),      \
              expected_object_close)                                           \
          MESSAGE(QLJS_TRANSLATABLE("object literal started here"),            \
                  object_open))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_regexp_literal, "E0039", diagnostic_severity::error,       \
      { source_code_span regexp_literal; },                                    \
      MESSAGE(QLJS_TRANSLATABLE("unclosed regexp literal"), regexp_literal))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_string_literal, "E0040", diagnostic_severity::error,       \
      { source_code_span string_literal; },                                    \
      MESSAGE(QLJS_TRANSLATABLE("unclosed string literal"), string_literal))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_jsx_string_literal, "E0181", diagnostic_severity::error,   \
      { source_code_span string_literal_begin; },                              \
      MESSAGE(QLJS_TRANSLATABLE("unclosed string literal"),                    \
              string_literal_begin))                                           \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unclosed_template, "E0041", diagnostic_severity::error,             \
      { source_code_span incomplete_template; },                               \
      MESSAGE(QLJS_TRANSLATABLE("unclosed template"), incomplete_template))    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_at_character, "E0042", diagnostic_severity::error,       \
      { source_code_span character; },                                         \
      MESSAGE(QLJS_TRANSLATABLE("unexpected '@'"), character))                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_arrow_after_expression, "E0160",                         \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span arrow;                                                \
        source_code_span expression;                                           \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("unexpected '{0}'"), arrow)                    \
          MESSAGE(QLJS_TRANSLATABLE("expected parameter for arrow function, "  \
                                    "but got an expression instead"),          \
                  expression))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_arrow_after_literal, "E0158",                            \
      diagnostic_severity::error,                                              \
      {                                                                        \
        source_code_span arrow;                                                \
        source_code_span literal_parameter;                                    \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("unexpected '{0}'"), arrow)                    \
          MESSAGE(QLJS_TRANSLATABLE("expected parameter for arrow function, "  \
                                    "but got a literal instead"),              \
                  literal_parameter))                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_backslash_in_identifier, "E0043",                        \
      diagnostic_severity::error, { source_code_span backslash; },             \
      MESSAGE(QLJS_TRANSLATABLE("unexpected '\\' in identifier"), backslash))  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_case_outside_switch_statement, "E0115",                  \
      diagnostic_severity::error, { source_code_span case_token; },            \
      MESSAGE(QLJS_TRANSLATABLE("unexpected 'case' outside switch statement"), \
              case_token))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_characters_in_number, "E0044",                           \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("unexpected characters in number literal"),    \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_control_character, "E0045", diagnostic_severity::error,  \
      { source_code_span character; },                                         \
      MESSAGE(QLJS_TRANSLATABLE("unexpected control character"), character))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_characters_in_binary_number, "E0046",                    \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("unexpected characters in binary literal"),    \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_characters_in_octal_number, "E0047",                     \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("unexpected characters in octal literal"),     \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_characters_in_hex_number, "E0048",                       \
      diagnostic_severity::error, { source_code_span characters; },            \
      MESSAGE(QLJS_TRANSLATABLE("unexpected characters in hex literal"),       \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_default_outside_switch_statement, "E0116",               \
      diagnostic_severity::error, { source_code_span default_token; },         \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("unexpected 'default' outside switch statement"),  \
          default_token))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_greater_in_jsx_text, "E0182",                            \
      diagnostic_severity::error, { source_code_span greater; },               \
      MESSAGE(QLJS_TRANSLATABLE("'>' is not allowed directly in JSX text; "    \
                                "write {{'>'} or &gt; instead"),               \
              greater))                                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_literal_in_parameter_list, "E0159",                      \
      diagnostic_severity::error, { source_code_span literal; },               \
      MESSAGE(QLJS_TRANSLATABLE("unexpected literal in parameter list; "       \
                                "expected parameter name"),                    \
              literal))                                                        \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_right_curly_in_jsx_text, "E0183",                        \
      diagnostic_severity::error, { source_code_span right_curly; },           \
      MESSAGE(QLJS_TRANSLATABLE("'}' is not allowed directly in JSX text; "    \
                                "write {{'}'} instead"),                       \
              right_curly))                                                    \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_semicolon_in_c_style_for_loop, "E0102",                  \
      diagnostic_severity::error, { source_code_span semicolon; },             \
      MESSAGE(QLJS_TRANSLATABLE("C-style for loops have only three "           \
                                "semicolon-separated components"),             \
              semicolon))                                                      \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_semicolon_in_for_in_loop, "E0110",                       \
      diagnostic_severity::error, { source_code_span semicolon; },             \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("for-in loop expression cannot have semicolons"),  \
          semicolon))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_semicolon_in_for_of_loop, "E0109",                       \
      diagnostic_severity::error, { source_code_span semicolon; },             \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE("for-of loop expression cannot have semicolons"),  \
          semicolon))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unopened_block_comment, "E0210", diagnostic_severity::error,        \
      { source_code_span comment_close; },                                     \
      MESSAGE(QLJS_TRANSLATABLE("unopened block comment"), comment_close))     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unused_variable_shadows, "E0196", diagnostic_severity::warning,     \
      {                                                                        \
        identifier shadowing_declaration;                                      \
        identifier shadowed_declaration;                                       \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("new variable shadows existing variable"),     \
              shadowing_declaration)                                           \
          MESSAGE(QLJS_TRANSLATABLE("existing variable declared here"),        \
                  shadowed_declaration))                                       \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_no_digits_in_binary_number, "E0049", diagnostic_severity::error,    \
      { source_code_span characters; },                                        \
      MESSAGE(QLJS_TRANSLATABLE("binary number literal has no digits"),        \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_no_digits_in_hex_number, "E0050", diagnostic_severity::error,       \
      { source_code_span characters; },                                        \
      MESSAGE(QLJS_TRANSLATABLE("hex number literal has no digits"),           \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_no_digits_in_octal_number, "E0051", diagnostic_severity::error,     \
      { source_code_span characters; },                                        \
      MESSAGE(QLJS_TRANSLATABLE("octal number literal has no digits"),         \
              characters))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_hash_character, "E0052", diagnostic_severity::error,     \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("unexpected '#'"), where))                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_bom_before_shebang, "E0095", diagnostic_severity::error, \
      { source_code_span bom; },                                               \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "unicode byte order mark (BOM) cannot appear before #! "     \
                  "at beginning of script"),                                   \
              bom))                                                            \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_identifier_in_expression, "E0147",                       \
      diagnostic_severity::error, { identifier unexpected; },                  \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "unexpected identifier in expression; missing operator before"), \
          unexpected))                                                         \
                                                                               \
  /* NOTE(strager): Try not to use this error. Find or make a more descriptive \
     and helpful error instead. */                                             \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_token, "E0054", diagnostic_severity::error,              \
      { source_code_span token; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("unexpected token"), token))                   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_token_after_export, "E0112", diagnostic_severity::error, \
      { source_code_span unexpected_token; },                                  \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "unexpected token in export; expected 'export default ...' " \
                  "or 'export {{name}' or 'export * from ...' or 'export "     \
                  "class' or 'export function' or 'export let'"),              \
              unexpected_token))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unexpected_token_in_variable_declaration, "E0114",                  \
      diagnostic_severity::error, { source_code_span unexpected_token; },      \
      MESSAGE(QLJS_TRANSLATABLE("unexpected token in variable declaration; "   \
                                "expected variable name"),                     \
              unexpected_token))                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unmatched_indexing_bracket, "E0055", diagnostic_severity::error,    \
      { source_code_span left_square; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("unmatched indexing bracket"), left_square))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unmatched_parenthesis, "E0056", diagnostic_severity::error,         \
      { source_code_span where; },                                             \
      MESSAGE(QLJS_TRANSLATABLE("unmatched parenthesis"), where))              \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_unmatched_right_curly, "E0143", diagnostic_severity::error,         \
      { source_code_span right_curly; },                                       \
      MESSAGE(QLJS_TRANSLATABLE("unmatched '}'"), right_curly))                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_use_of_undeclared_type, "E0214", diagnostic_severity::warning,      \
      { identifier name; },                                                    \
      MESSAGE(QLJS_TRANSLATABLE("use of undeclared type: {0}"), name))         \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_use_of_undeclared_variable, "E0057", diagnostic_severity::warning,  \
      { identifier name; },                                                    \
      MESSAGE(QLJS_TRANSLATABLE("use of undeclared variable: {0}"), name))     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_variable_used_before_declaration, "E0058",                          \
      diagnostic_severity::error,                                              \
      {                                                                        \
        identifier use;                                                        \
        identifier declaration;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE("variable used before declaration: {0}"), use) \
          MESSAGE(QLJS_TRANSLATABLE("variable declared here"), declaration))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_function_call_before_declaration_in_block_scope, "E0077",           \
      diagnostic_severity::warning,                                            \
      {                                                                        \
        identifier use;                                                        \
        identifier declaration;                                                \
      },                                                                       \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "function called before declaration in block scope: {0}"),   \
              use)                                                             \
          MESSAGE(QLJS_TRANSLATABLE("function declared here"), declaration))   \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_fields_cannot_have_initializers, "E0221",                 \
      diagnostic_severity::error, { source_code_span equal; },                 \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "TypeScript interface fields cannot be initalized"),         \
              equal))                                                          \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_methods_cannot_be_async, "E0217",                         \
      diagnostic_severity::error, { source_code_span async_keyword; },         \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "TypeScript interface methods cannot be marked 'async'"),    \
              async_keyword))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_methods_cannot_be_generators, "E0218",                    \
      diagnostic_severity::error, { source_code_span star; },                  \
      MESSAGE(                                                                 \
          QLJS_TRANSLATABLE(                                                   \
              "TypeScript interface methods cannot be marked as a generator"), \
          star))                                                               \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_methods_cannot_contain_bodies, "E0220",                   \
      diagnostic_severity::error, { source_code_span body_start; },            \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "TypeScript interface methods cannot contain a body"),       \
              body_start))                                                     \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_properties_cannot_be_private, "E0219",                    \
      diagnostic_severity::error, { identifier property_name; },               \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "TypeScript interface properties cannot be private"),        \
              property_name))                                                  \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_interface_properties_cannot_be_static, "E0216",                     \
      diagnostic_severity::error, { source_code_span static_keyword; },        \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "TypeScript interface properties cannot be 'static'"),       \
              static_keyword))                                                 \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_break, "E0200", diagnostic_severity::error,                 \
      { source_code_span break_statement; },                                   \
      MESSAGE(QLJS_TRANSLATABLE(                                               \
                  "break can only be used inside of a loop or switch"),        \
              break_statement))                                                \
                                                                               \
  QLJS_DIAG_TYPE(                                                              \
      diag_invalid_continue, "E0201", diagnostic_severity::error,              \
      { source_code_span continue_statement; },                                \
      MESSAGE(QLJS_TRANSLATABLE("continue can only be used inside of a loop"), \
              continue_statement))                                             \
                                                                               \
  /* END */

namespace quick_lint_js {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  struct name struct_body;
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

enum class diag_type {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) name,
  QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
};

std::ostream& operator<<(std::ostream&, diag_type);

template <class Error>
struct diag_type_from_type_detail;

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  template <>                                                          \
  struct diag_type_from_type_detail<name> {                            \
    static constexpr diag_type type = diag_type::name;                 \
  };
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

template <class Error>
inline constexpr diag_type diag_type_from_type =
    diag_type_from_type_detail<Error>::type;

inline constexpr int diag_type_count = 0
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) +1
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
    ;
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
