// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
class test_parse_typescript_class : public test_parse_expression {};

TEST_F(test_parse_typescript_class,
       field_with_type_is_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { fieldName: FieldType; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"fieldName"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"FieldType"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"class C { fieldName"), u8":")));
  }
}

TEST_F(test_parse_typescript_class, field_with_type_is_allowed_in_typescript) {
  {
    test_parser p(u8"class C { fieldName: FieldType; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  //
                            "visit_variable_type_use",       // FieldType
                            "visit_property_declaration",    // fieldName
                            "visit_exit_class_scope",        // C
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"fieldName"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"FieldType"));
  }
}

TEST_F(test_parse_typescript_class,
       class_index_signature_is_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { [key: KeyType]: ValueType; }"_sv, capture_diags);
    p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_unexpected_token,  //
                              token, strlen(u8"class C { [key"), u8":")));
  }
}

TEST_F(test_parse_typescript_class,
       class_index_signature_is_allowed_in_typescript) {
  {
    test_parser p(u8"class C { [key: KeyType]: ValueType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",            // C
                                      "visit_enter_class_scope_body",       //
                                      "visit_enter_index_signature_scope",  //
                                      "visit_variable_type_use",     // KeyType
                                      "visit_variable_declaration",  // key
                                      "visit_variable_type_use",  // ValueType
                                      "visit_exit_index_signature_scope",  //
                                      "visit_exit_class_scope",            // C
                                      "visit_variable_declaration"));      // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"KeyType", u8"ValueType"));
    // TODO(strager): We probably should create a new kind of variable instead
    // of 'parameter'.
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(param_decl(u8"key"), class_decl(u8"C")));
  }
}

TEST_F(test_parse_typescript_class,
       optional_properties_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { field1?; field2? = init; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field1
                                      "visit_variable_use",            // init
                                      "visit_property_declaration",    // field2
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_optional_properties_not_allowed_in_javascript,  //
                question, strlen(u8"class C { field1"), u8"?"),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_optional_properties_not_allowed_in_javascript,  //
                question, strlen(u8"class C { field1?; field2"), u8"?")));
  }
}

TEST_F(test_parse_typescript_class,
       optional_methods_are_disallowed_in_classes) {
  for (parser_options options : {parser_options(), typescript_options}) {
    SCOPED_TRACE(options.typescript ? "typescript" : "javascript");
    test_parser p(u8"class C { method?() {} }"_sv, options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_typescript_optional_class_method,  //
                              question, strlen(u8"class C { method"), u8"?")));
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_fields_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { field1!; field2! = init; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field1
                                      "visit_variable_use",            // init
                                      "visit_property_declaration",    // field2
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_javascript,  //
                bang, strlen(u8"class C { field1"), u8"!"),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_javascript,  //
                bang, strlen(u8"class C { field1?; field2"), u8"!")));
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_fields_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { field1!; field2! = init; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field1
                                      "visit_variable_use",            // init
                                      "visit_property_declaration",    // field2
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_methods_are_not_allowed) {
  {
    test_parser p(u8"class C { method!() {} }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_typescript_assignment_asserted_method,  //
                              bang, strlen(u8"class C { method"), u8"!")));
  }
}

TEST_F(test_parse_typescript_class,
       readonly_fields_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { readonly field; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_readonly_fields_not_allowed_in_javascript,  //
            readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }

  {
    test_parser p(u8"class C { readonly field = null; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_readonly_fields_not_allowed_in_javascript,  //
            readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }

  {
    test_parser p(u8"class C { readonly field\nmethod() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_readonly_fields_not_allowed_in_javascript,  //
            readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }

  {
    test_parser p(u8"class C { readonly field\n[methodName]() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_readonly_fields_not_allowed_in_javascript,  //
            readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }

  {
    test_parser p(u8"class C { readonly async\nmethod() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // async
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_readonly_fields_not_allowed_in_javascript,  //
            readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }

  {
    test_parser p(u8"class C { readonly field? method() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE(diag_missing_semicolon_after_field),
            DIAG_TYPE(
                diag_typescript_optional_properties_not_allowed_in_javascript),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }
}

TEST_F(test_parse_typescript_class, readonly_fields_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { readonly field; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
  }

  {
    test_parser p(u8"class C { static readonly field; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // field
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
  }

  {
    test_parser p(u8"class C { readonly #field; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // #field
                                      "visit_exit_class_scope",        // C
                                      "visit_variable_declaration"));  // C
  }
}

TEST_F(test_parse_typescript_class, readonly_methods_are_invalid) {
  {
    test_parser p(u8"class C { readonly method() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_typescript_readonly_method,  //
                    readonly_keyword, strlen(u8"class C { "), u8"readonly")));
  }
}

TEST_F(test_parse_typescript_class, readonly_static_field_is_disallowed) {
  {
    test_parser p(u8"class C { readonly static field; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  //
                            "visit_property_declaration",    // field
                            "visit_exit_class_scope",        // C
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field"));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_readonly_static_field,  //
                              readonly_static, strlen(u8"class C { "),
                              u8"readonly static")));
  }
}

TEST_F(test_parse_typescript_class,
       generic_classes_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C<T> { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // {
                            "visit_variable_declaration",    // T
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T"), class_decl(u8"C")));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_typescript_generics_not_allowed_in_javascript,  //
                    opening_less, strlen(u8"class C"), u8"<")));
  }
}

TEST_F(test_parse_typescript_class, generic_classes_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C<T> { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // {
                            "visit_variable_declaration",    // T
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T"), class_decl(u8"C")));
  }
}

TEST_F(test_parse_typescript_class,
       generic_methods_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { method<T>() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // T
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"method"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_typescript_generics_not_allowed_in_javascript,  //
                    opening_less, strlen(u8"class C { method"), u8"<")));
  }
}

TEST_F(test_parse_typescript_class, generic_methods_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { method<T>() {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // T
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"method"));
  }
}

TEST_F(test_parse_typescript_class,
       call_signatures_are_disallowed_in_typescript_classes) {
  {
    test_parser p(u8"class C { () {} }"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.visits,
        ElementsAre("visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_property_declaration",       // (call signature)
                    "visit_enter_function_scope",       // (call signature)
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration"));     // C
    EXPECT_THAT(p.property_declarations, ElementsAre(std::nullopt));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_missing_class_method_name,  //
                              expected_name, strlen(u8"class C { "), u8"")));
  }

  {
    test_parser p(u8"class C { <T>() {} }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.visits,
        ElementsAre("visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_property_declaration",       // (call signature)
                    "visit_enter_function_scope",       // (call signature)
                    "visit_variable_declaration",       // T
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration"));     // C
    EXPECT_THAT(p.property_declarations, ElementsAre(std::nullopt));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_typescript_call_signatures_not_allowed_in_classes,  //
                    expected_method_name, strlen(u8"class C { "), u8"")));
  }
}

TEST_F(test_parse_typescript_class,
       access_specifiers_are_disallowed_in_javascript) {
  for (string8 specifier : {u8"public", u8"protected", u8"private"}) {
    {
      string8 code = u8"class C { " + specifier + u8" method() {} }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration"));     // C
      EXPECT_THAT(p.property_declarations, ElementsAre(u8"method"));
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code = u8"class C { " + specifier + u8" field }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code = u8"class C { " + specifier + u8" field = init; }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code = u8"class C { " + specifier + u8" field\nmethod() {} }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code =
          u8"class C { " + specifier + u8" field\n[methodName]() {} }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code = u8"class C { " + specifier + u8" field? method() {} }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          UnorderedElementsAre(
              DIAG_TYPE(
                  diag_typescript_optional_properties_not_allowed_in_javascript),
              DIAG_TYPE(diag_missing_semicolon_after_field),
              DIAG_TYPE_OFFSETS(
                  p.code,
                  diag_typescript_access_specifiers_not_allowed_in_javascript,  //
                  specifier, strlen(u8"class C { "), specifier)));
    }

    {
      string8 code = u8"class C { " + specifier +
                     u8" async\nmethod() { const await = null; } }";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_access_specifiers_not_allowed_in_javascript,  //
              specifier, strlen(u8"class C { "), specifier)));
    }
  }
}

TEST_F(test_parse_typescript_class,
       access_specifiers_are_allowed_in_typescript) {
  for (string8 specifier : {u8"public", u8"protected", u8"private"}) {
    padded_string code(u8"class C { " + specifier + u8" method() {} }");
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // C
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"method"));
  }
}

TEST_F(test_parse_typescript_class,
       static_blocks_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { static #private; static { C.#private; } }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  //
                            "visit_property_declaration",    // #private
                            "visit_enter_block_scope",       // static
                            "visit_variable_use",            // C
                            "visit_exit_block_scope",        // static
                            "visit_exit_class_scope",        // C
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"#private"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_typescript_static_blocks_not_allowed_in_javascript,  //
                    static_token, strlen(u8"class C { static #private; "),
                    u8"static")));
  }
}

TEST_F(test_parse_typescript_class, static_blocks_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { static #private; static { C.#private; } }",
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  //
                            "visit_property_declaration",    // #private
                            "visit_enter_block_scope",       // static
                            "visit_variable_use",            // C
                            "visit_exit_block_scope",        // static
                            "visit_exit_class_scope",        // C
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_class,
       method_return_type_annotations_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { method(): T { } }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // {
                            "visit_enter_class_scope_body",     // C
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_type_use",          // T
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // }
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"class C { method()"), u8":")));
  }
}

TEST_F(test_parse_typescript_class,
       method_return_type_annotations_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { method(): T { } }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // {
                            "visit_enter_class_scope_body",     // C
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_type_use",          // T
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope",           // }
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }
}

TEST_F(test_parse_typescript_class,
       abstract_classes_are_disallowed_in_javascript) {
  {
    test_parser p(u8"abstract class C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration"));  // C
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_abstract_class_not_allowed_in_javascript,  //
            abstract_keyword, strlen(u8""), u8"abstract")));
  }
}

TEST_F(test_parse_typescript_class,
       abstract_classes_are_allowed_in_typescript) {
  {
    test_parser p(u8"abstract class C { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration"));  // C
  }
}

TEST_F(test_parse_typescript_class,
       newline_before_class_causes_abstract_to_be_identifier) {
  {
    test_parser p(u8"abstract\nclass C { }"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_use",            // abstract
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"abstract"));
  }
}

TEST_F(test_parse_typescript_class, implements_is_not_allowed_in_javascript) {
  test_parser p(u8"class C implements Base {}"_sv, javascript_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                    "visit_variable_type_use",       // Base
                                    "visit_enter_class_scope_body",  // C
                                    "visit_exit_class_scope",        // }
                                    "visit_variable_declaration",    // C
                                    "visit_end_of_module"));
  EXPECT_THAT(
      p.errors,
      ElementsAre(DIAG_TYPE_OFFSETS(
          p.code,
          diag_typescript_class_implements_not_allowed_in_javascript,  //
          implements_keyword, strlen(u8"class C "), u8"implements")));
}

TEST_F(test_parse_typescript_class, implements) {
  test_parser p(u8"class C implements Base {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                    "visit_variable_type_use",       // Base
                                    "visit_enter_class_scope_body",  // C
                                    "visit_exit_class_scope",        // }
                                    "visit_variable_declaration",    // C
                                    "visit_end_of_module"));
  EXPECT_THAT(p.variable_uses, ElementsAre(u8"Base"));
}

TEST_F(test_parse_typescript_class, implements_comes_after_extends) {
  {
    test_parser p(u8"class C extends Base implements I {}"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                      "visit_variable_use",            // Base
                                      "visit_variable_type_use",       // I
                                      "visit_enter_class_scope_body",  // C
                                      "visit_exit_class_scope",        // }
                                      "visit_variable_declaration",    // C
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Base", u8"I"));
  }

  {
    test_parser p(u8"class C implements I extends Base {}"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                      "visit_variable_type_use",       // I
                                      "visit_variable_use",            // Base
                                      "visit_enter_class_scope_body",  // C
                                      "visit_exit_class_scope",        // }
                                      "visit_variable_declaration",    // C
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"I", u8"Base"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_implements_must_be_after_extends,          //
            implements_keyword, strlen(u8"class C "), u8"implements",  //
            extends_keyword, strlen(u8"class C implements I "), u8"extends")));
  }
}

TEST_F(test_parse_typescript_class, implements_interface_from_namespace) {
  test_parser p(u8"class C implements ns.A {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                    "visit_variable_namespace_use",  // ns
                                    "visit_enter_class_scope_body",  // C
                                    "visit_exit_class_scope",        // }
                                    "visit_variable_declaration",    // C
                                    "visit_end_of_module"));
  EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns"));
}

TEST_F(test_parse_typescript_class, implement_multiple_things) {
  test_parser p(u8"class C implements Apple, Banana, Carrot {}"_sv,
                typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                    "visit_variable_type_use",       // Apple
                                    "visit_variable_type_use",       // Banana
                                    "visit_variable_type_use",       // Carrot
                                    "visit_enter_class_scope_body",  // C
                                    "visit_exit_class_scope",        // }
                                    "visit_variable_declaration",    // C
                                    "visit_end_of_module"));
  EXPECT_THAT(p.variable_uses, ElementsAre(u8"Apple", u8"Banana", u8"Carrot"));
}
}
}

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
