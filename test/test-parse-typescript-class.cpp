// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
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
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_type_annotations_not_allowed_in_javascript,  //
                type_colon, strlen(u8"class C { fieldName"), u8":"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class, field_with_type_is_allowed_in_typescript) {
  {
    test_parser p(u8"class C { fieldName: FieldType; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_variable_type_use",       // FieldType
                              "visit_property_declaration",    // fieldName
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
  }

  {
    test_parser p(u8"class C { fieldName: FieldType = init; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_variable_type_use",       // FieldType
                              "visit_variable_use",            // init
                              "visit_property_declaration",    // fieldName
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(test_parse_typescript_class,
       class_index_signature_is_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { [key: KeyType]: ValueType; }"_sv, capture_diags);
    p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                              token, strlen(u8"class C { [key"), u8":"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       class_index_signature_is_allowed_in_typescript) {
  {
    test_parser p(u8"class C { [key: KeyType]: ValueType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_class_scope",             // C
                              "visit_variable_declaration",         // C
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"KeyType", u8"ValueType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({index_signature_param_decl(u8"key"_sv),
                                  class_decl(u8"C"_sv)}));
  }
}

TEST_F(test_parse_typescript_class,
       optional_fields_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { field1?; field2? = init; }"_sv,
                  javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field1
                              "visit_variable_use",            // init
                              "visit_property_declaration",    // field2
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_optional_properties_not_allowed_in_javascript,  //
                question, strlen(u8"class C { field1"), u8"?"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_optional_properties_not_allowed_in_javascript,  //
                question, strlen(u8"class C { field1?; field2"), u8"?"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class, optional_fields_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { field1?; field2? = init; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field1
                              "visit_variable_use",            // init
                              "visit_property_declaration",    // field2
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  field1\n"
        u8"  ?;\n"
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field1
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }))
        << "newline should be allowed before '?' (no ASI)";
  }
}

TEST_F(test_parse_typescript_class,
       optional_methods_are_allowed_in_typescript_classes) {
  {
    test_parser p(u8"class C { method?() {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  method?\n"
        u8"  () {}\n"
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }))
        << "newline is allowed after '?'";
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  method\n"
        u8"  ?() {}\n"
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }))
        << "newline is allowed before '?'";
  }
}

TEST_F(test_parse_typescript_class,
       optional_methods_are_not_allowed_in_javascript_classes) {
  test_parser p(u8"class C { method?() {} }"_sv, javascript_options,
                capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_optional_properties_not_allowed_in_javascript,  //
              question, strlen(u8"class C { method"), u8"?"_sv),
      }));
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_fields_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { field!: any; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_javascript,  //
                bang, strlen(u8"class C { field"), u8"!"_sv),
        }));
  }

  {
    test_parser p(u8"class C { field!: any = init; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_javascript,  //
                bang, strlen(u8"class C { field"), u8"!"_sv),
        }))
        << "should not also report "
           "diag_typescript_assignment_asserted_field_cannot_have_initializer";
  }

  {
    test_parser p(u8"class C { field!; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_javascript,  //
                bang, strlen(u8"class C { field"), u8"!"_sv),
        }))
        << "should not also report "
           "diag_typescript_assignment_asserted_field_must_have_a_type";
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_fields_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { field!: any; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(test_parse_typescript_class,
       newline_not_allowed_before_assignment_asserted_field_bang) {
  {
    test_parser p(
        u8"class C {\n"
        u8"  field\n"
        u8"  !: any;\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_newline_not_allowed_before_assignment_assertion_operator,  //
                bang, strlen(u8"class C {\n  field\n  "), u8"!"_sv, field_name,
                strlen(u8"class C {\n  "), u8"field"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_field_must_have_a_type) {
  {
    test_parser p(u8"class C { field!; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_field_must_have_a_type,  //
                bang, strlen(u8"class C { field"), u8"!"_sv),
        }));
  }

  {
    test_parser p(u8"class C { field! = init; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE(
                diag_typescript_assignment_asserted_field_cannot_have_initializer),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_field_must_have_a_type,  //
                bang, strlen(u8"class C { field"), u8"!"_sv)));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  field1!\n"
        u8"  field2;\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"field1", u8"field2"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_field_must_have_a_type,  //
                bang, strlen(u8"class C {\n  field1"), u8"!"_sv),
        }));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  field1!\n"
        u8"  'field2';\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field1", std::nullopt));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_field_must_have_a_type,  //
                bang, strlen(u8"class C {\n  field1"), u8"!"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_field_cannot_have_initializers) {
  {
    test_parser p(u8"class C { field!: any = init; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_field_cannot_have_initializer,  //
                equal, strlen(u8"class C { field!: any "), u8"="_sv, bang,
                strlen(u8"class C { field"), u8"!"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       assignment_asserted_methods_are_not_allowed) {
  {
    test_parser p(u8"class C { method!() {} }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_typescript_assignment_asserted_method,  //
                              bang, strlen(u8"class C { method"), u8"!"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       readonly_fields_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { readonly field; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv),
        }));
  }

  {
    test_parser p(u8"class C { readonly field = null; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv),
        }));
  }

  {
    test_parser p(u8"class C { readonly field\nmethod() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv),
        }));
  }

  {
    test_parser p(u8"class C { readonly field\n[methodName]() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv),
        }));
  }

  {
    test_parser p(u8"class C { readonly async\nmethod() {} }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // async
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv),
        }));
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
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv)));
  }

  {
    test_parser p(u8"class C { readonly field: any; }"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE(
                diag_typescript_type_annotations_not_allowed_in_javascript),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_readonly_fields_not_allowed_in_javascript,  //
                readonly_keyword, strlen(u8"class C { "), u8"readonly"_sv)));
  }
}

TEST_F(test_parse_typescript_class, readonly_fields_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { readonly field; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    test_parser p(u8"class C { static readonly field; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    test_parser p(u8"class C { readonly #field; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // #field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(test_parse_typescript_class, readonly_methods_are_invalid) {
  {
    test_parser p(u8"class C { readonly method() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code,
                                      diag_typescript_readonly_method,  //
                                      readonly_keyword, strlen(u8"class C { "),
                                      u8"readonly"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class, readonly_static_field_is_disallowed) {
  {
    test_parser p(u8"class C { readonly static field; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code,
                                      diag_readonly_static_field,  //
                                      readonly_static, strlen(u8"class C { "),
                                      u8"readonly static"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class,
       generic_classes_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C<T> { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_declaration",    // T
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"_sv), class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_generics_not_allowed_in_javascript,  //
                        opening_less, strlen(u8"class C"), u8"<"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class, generic_classes_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C<T> { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_declaration",    // T
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(u8"class C<T> extends Base<T> { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_declaration",    // T
                              "visit_variable_type_use",       // T
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"_sv), class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv, u8"Base"_sv}));
  }
}

TEST_F(test_parse_typescript_class,
       generic_methods_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { method<T>() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_generics_not_allowed_in_javascript,  //
                        opening_less, strlen(u8"class C { method"), u8"<"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class, generic_methods_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { method<T>() {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(test_parse_typescript_class,
       call_signatures_are_disallowed_in_typescript_classes) {
  {
    test_parser p(u8"class C { () {} }"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_property_declaration",       // (call signature)
                    "visit_enter_function_scope",       // (call signature)
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration",       // C
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_class_method_name,  //
                              expected_name, strlen(u8"class C { "), u8""_sv),
        }));
  }

  {
    test_parser p(u8"class C { <T>() {} }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_property_declaration",       // (call signature)
                    "visit_enter_function_scope",       // (call signature)
                    "visit_variable_declaration",       // T
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration",       // C
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_class_method_name,  //
                              expected_name, strlen(u8"class C { "), u8""_sv),
        }));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  field!\n"
        u8"  (param) {}\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field", std::nullopt));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_class_method_name,  //
                              expected_name,
                              strlen(u8"class C {\n  field!\n  "), u8""_sv),
        }));
  }

  {
    test_parser p(
        u8"class C {\n"
        u8"  field!\n"
        u8"  <T>(param) {}\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field", std::nullopt));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_class_method_name,  //
                              expected_name,
                              strlen(u8"class C {\n  field!\n  "), u8""_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       access_specifiers_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { public method() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_public_not_allowed_in_javascript,  //
                        specifier, strlen(u8"class C { "), u8"public"_sv),
                }));
  }

  {
    test_parser p(u8"class C { protected method() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_protected_not_allowed_in_javascript,  //
                        specifier, strlen(u8"class C { "), u8"protected"_sv),
                }));
  }

  {
    test_parser p(u8"class C { private method() {} }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_private_not_allowed_in_javascript,  //
                        specifier, strlen(u8"class C { "), u8"private"_sv),
                }));
  }

  for (string8 specifier : {u8"public", u8"protected", u8"private"}) {
#define MATCH_ACCESS_SPECIFIER_ERROR(code_before)                            \
  ::testing::AnyOf(                                                          \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        diag_typescript_private_not_allowed_in_javascript,   \
                        specifier, strlen(code_before), u8"private"_sv),     \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        diag_typescript_protected_not_allowed_in_javascript, \
                        specifier, strlen(code_before), u8"protected"_sv),   \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        diag_typescript_public_not_allowed_in_javascript,    \
                        specifier, strlen(code_before), u8"public"_sv))

    {
      test_parser p(
          concat(u8"class C { "_sv, specifier, u8" field = init; }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "),
                            }));
    }

    {
      test_parser p(
          concat(u8"class C { "_sv, specifier, u8" field\nmethod() {} }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "),
                            }));
    }

    {
      test_parser p(concat(u8"class C { "_sv, specifier,
                           u8" field\n[methodName]() {} }"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "),
                            }));
    }

    {
      test_parser p(
          concat(u8"class C { "_sv, specifier, u8" field? method() {} }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          UnorderedElementsAre(
              DIAG_TYPE(
                  diag_typescript_optional_properties_not_allowed_in_javascript),
              DIAG_TYPE(diag_missing_semicolon_after_field),
              MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { ")));
    }

    {
      test_parser p(concat(u8"class C { "_sv, specifier,
                           u8" async\nmethod() { const await = null; } }"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "),
                            }));
    }
#undef MATCH_ACCESS_SPECIFIER_ERROR
  }
}

TEST_F(test_parse_typescript_class,
       access_specifiers_are_allowed_in_typescript) {
  for (string8 specifier : {u8"public", u8"protected", u8"private"}) {
    padded_string code(
        concat(u8"class C { "_sv, specifier, u8" method() {} }"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(test_parse_typescript_class,
       method_return_type_annotations_are_disallowed_in_javascript) {
  {
    test_parser p(u8"class C { method(): T { } }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_variable_type_use",          // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_type_annotations_not_allowed_in_javascript,  //
                type_colon, strlen(u8"class C { method()"), u8":"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       method_return_type_annotations_are_allowed_in_typescript) {
  {
    test_parser p(u8"class C { method(): T { } }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_variable_type_use",          // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(test_parse_typescript_class,
       abstract_classes_are_disallowed_in_javascript) {
  {
    test_parser p(u8"abstract class C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_abstract_class_not_allowed_in_javascript,  //
                abstract_keyword, strlen(u8""), u8"abstract"_sv),
        }));
  }

  {
    test_parser p(u8"export abstract class C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_abstract_class_not_allowed_in_javascript,  //
                abstract_keyword, strlen(u8"export "), u8"abstract"_sv),
        }));
  }

  {
    test_parser p(u8"export default abstract class C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_abstract_class_not_allowed_in_javascript,  //
                abstract_keyword, strlen(u8"export default "), u8"abstract"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class, abstract_class) {
  {
    test_parser p(u8"abstract class C { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(test_parse_typescript_class, abstract_class_method) {
  {
    test_parser p(u8"abstract class C { abstract myMethod(param); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myMethod
                              "visit_enter_function_scope",    // myMethod
                              "visit_variable_declaration",    // param
                              "visit_exit_function_scope",     // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myMethod"}));
  }

  {
    test_parser p(
        u8"abstract class C {\n"
        u8"  abstract f()\n"      // ASI
        u8"  abstract g() }"_sv,  // ASI
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // f
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_property_declaration",    // g
                              "visit_enter_function_scope",    // g
                              "visit_exit_function_scope",     // g
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }
}

TEST_F(test_parse_typescript_class, abstract_class_method_requires_semicolon) {
  {
    test_parser p(u8"abstract class C { abstract f() abstract g(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // f
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_property_declaration",    // g
                              "visit_enter_function_scope",    // g
                              "visit_exit_function_scope",     // g
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_missing_semicolon_after_abstract_method,  //
                        expected_semicolon,
                        strlen(u8"abstract class C { abstract f()"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class, abstract_methods_cannot_have_bodies) {
  {
    test_parser p(u8"abstract class C { abstract f() { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_abstract_methods_cannot_contain_bodies,  //
                        body_start,
                        strlen(u8"abstract class C { abstract f() "), u8"{"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class,
       abstract_method_prohibits_newline_after_abstract_keyword) {
  {
    test_parser p(u8"abstract class C { abstract\n m() { } }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"abstract", u8"m"}));
  }
}

TEST_F(test_parse_typescript_class, abstract_methods_cannot_be_async) {
  {
    test_parser p(u8"abstract class C { abstract async method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // method
                              "visit_enter_function_scope",    // method
                              "visit_exit_function_scope",     // method
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_abstract_methods_cannot_be_async,  //
                        async_keyword, strlen(u8"abstract class C { abstract "),
                        u8"async"_sv, abstract_keyword,
                        strlen(u8"abstract class C { "), u8"abstract"_sv),
                }));
  }

  {
    test_parser p(u8"abstract class C { async abstract method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // method
                              "visit_enter_function_scope",    // method
                              "visit_exit_function_scope",     // method
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_abstract_methods_cannot_be_async,  //
                        async_keyword, strlen(u8"abstract class C { "),
                        u8"async"_sv, abstract_keyword,
                        strlen(u8"abstract class C { async "), u8"abstract"_sv),
                }));
  }

  {
    // ASI activates after 'async'.
    test_parser p(u8"abstract class C { async\n abstract f(); }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"async", u8"f"}));
  }

  {
    // ASI activates after 'abstract'.
    test_parser p(u8"abstract class C { abstract\n async f() {} }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"abstract", u8"f"}));
  }
}

TEST_F(test_parse_typescript_class, abstract_methods_cannot_be_generators) {
  test_parser p(u8"abstract class C { abstract *method(); }"_sv,
                typescript_options, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  // {
                            "visit_property_declaration",    // method
                            "visit_enter_function_scope",    // method
                            "visit_exit_function_scope",     // method
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                        }));
  EXPECT_THAT(p.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_OFFSETS(
                      p.code, diag_abstract_methods_cannot_be_generators,  //
                      star, strlen(u8"abstract class C { abstract "), u8"*"_sv,
                      abstract_keyword, strlen(u8"abstract class C { "),
                      u8"abstract"_sv),
              }));
}

TEST_F(test_parse_typescript_class, abstract_field) {
  {
    test_parser p(u8"abstract class C { abstract myField: string; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField;
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }
}

TEST_F(test_parse_typescript_class, abstract_fields_cannot_have_initializers) {
  {
    test_parser p(
        u8"abstract class C { abstract myField: string = 'hello'; }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_abstract_field_cannot_have_initializer,  //
                equal, strlen(u8"abstract class C { abstract myField: string "),
                u8"="_sv, abstract_keyword, strlen(u8"abstract class C { "),
                u8"abstract"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       abstract_properties_are_not_allowed_in_non_abstract_classes) {
  {
    test_parser p(u8"class C { abstract myField; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_non_abstract_class,  //
                abstract_keyword, strlen(u8"class C { "), u8"abstract"_sv,
                class_keyword, 0, u8"class"_sv),
        }));
  }

  {
    test_parser p(u8"class C { abstract myMethod(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myMethod
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_non_abstract_class,  //
                abstract_keyword, strlen(u8"class C { "), u8"abstract"_sv,
                class_keyword, 0, u8"class"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class,
       newline_before_class_causes_abstract_to_be_identifier) {
  {
    test_parser p(u8"abstract\nclass C { }"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // abstract
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"abstract"}));
  }
}

TEST_F(test_parse_typescript_class, implements_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"class C implements Base {}"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_class_implements_not_allowed_in_javascript,  //
                implements_keyword, strlen(u8"class C "), u8"implements"_sv),
        }));
  }

  {
    test_parser p(u8"class C extends Base implements I {}"_sv,
                  javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_variable_type_use",       // I
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_class_implements_not_allowed_in_javascript,  //
                implements_keyword, strlen(u8"class C extends Base "),
                u8"implements"_sv),
        }));
  }

  {
    test_parser p(u8"class C implements I extends Base {}"_sv,
                  javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // I
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_class_implements_not_allowed_in_javascript,  //
                implements_keyword, strlen(u8"class C "), u8"implements"_sv),
        }))
        << "should not report diag_typescript_implements_must_be_after_extends";
  }
}

TEST_F(test_parse_typescript_class, implements) {
  test_parser p(u8"class C implements Base {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_variable_type_use",       // Base
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Base"}));
}

TEST_F(test_parse_typescript_class, implements_comes_after_extends) {
  {
    test_parser p(u8"class C extends Base implements I {}"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_variable_type_use",       // I
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Base", u8"I"}));
  }

  {
    test_parser p(u8"class C implements I extends Base {}"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // I
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"I", u8"Base"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_typescript_implements_must_be_after_extends,  //
                        implements_keyword, strlen(u8"class C "),
                        u8"implements"_sv, extends_keyword,
                        strlen(u8"class C implements I "), u8"extends"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class, implements_interface_from_namespace) {
  test_parser p(u8"class C implements ns.A {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_variable_namespace_use",  // ns
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
}

TEST_F(test_parse_typescript_class, implement_multiple_things) {
  test_parser p(u8"class C implements Apple, Banana, Carrot {}"_sv,
                typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_variable_type_use",       // Apple
                            "visit_variable_type_use",       // Banana
                            "visit_variable_type_use",       // Carrot
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses,
              ElementsAreArray({u8"Apple", u8"Banana", u8"Carrot"}));
}

TEST_F(test_parse_typescript_class, parameter_property_in_constructor) {
  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public field) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",       // C
                    "visit_enter_class_scope_body",  // {
                    // TODO(strager): visit_property_declaration for 'field'.
                    "visit_property_declaration",       // constructor
                    "visit_enter_function_scope",       // constructor
                    "visit_variable_declaration",       // field
                    "visit_enter_function_scope_body",  // {
                    "visit_exit_function_scope",        // }
                    "visit_exit_class_scope",           // }
                    "visit_variable_declaration",       // C
                    "visit_end_of_module",
                }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(protected field) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(private field) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public field: FieldType) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public field = defaultValue) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"defaultValue"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(readonly field) {}\n"_sv
        u8"}"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  for (string8_view access_specifier :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    test_parser p(concat(u8"class C {\n"_sv
                         u8"  constructor("_sv,
                         access_specifier,
                         u8" readonly field) {}\n"_sv
                         u8"}"_sv),
                  typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }
}

TEST_F(test_parse_typescript_class, parameter_property_cannot_destructure) {
  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public [field1, field2]) {}\n"_sv
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Assert that visit_property_declaration(field1) and
    // visit_property_declaration(field2) occurred.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"field1"_sv),
                                  func_param_decl(u8"field2"_sv),
                                  class_decl(u8"C"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_parameter_property_cannot_be_destructured,  //
                destructure_token,
                u8"class C {\n  constructor(public "_sv.size(),
                u8"["_sv,  //
                property_keyword, u8"class C {\n  constructor("_sv.size(),
                u8"public"_sv),
        }));
  }

  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public {field1, other: field2}) {}\n"_sv
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Assert that visit_property_declaration(field1) and
    // visit_property_declaration(field2) occurred.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"field1"_sv),
                                  func_param_decl(u8"field2"_sv),
                                  class_decl(u8"C"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_parameter_property_cannot_be_destructured,  //
                destructure_token,
                u8"class C {\n  constructor(public "_sv.size(),
                u8"{"_sv,  //
                property_keyword, u8"class C {\n  constructor("_sv.size(),
                u8"public"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_class, parameter_property_cannot_be_rest) {
  {
    test_parser p(
        u8"class C {\n"_sv
        u8"  constructor(public ...field) {}\n"_sv
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Assert that visit_property_declaration(field) occurred.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_typescript_parameter_property_cannot_be_rest,  //
                        spread, u8"class C {\n  constructor(public "_sv.size(),
                        u8"..."_sv,  //
                        property_keyword,
                        u8"class C {\n  constructor("_sv.size(), u8"public"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_class,
       parameter_property_is_not_allowed_in_javascript) {
  for (string8_view keyword :
       {u8"readonly"_sv, u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      test_parser p(concat(u8"class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" field) {}\n"_sv
                           u8"}"_sv),
                    javascript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  diag_typescript_parameter_property_not_allowed_in_javascript,  //
                  property_keyword, u8"class C {\n  constructor("_sv.size(),
                  keyword),
          }));
    }
  }

  for (string8_view keyword :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      test_parser p(concat(u8"class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" readonly field) {}\n"_sv
                           u8"}"_sv),
                    javascript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  diag_typescript_parameter_property_not_allowed_in_javascript,  //
                  property_keyword, u8"class C {\n  constructor("_sv.size(),
                  keyword),
          }))
          << "only '" << out_string8(keyword)
          << "' should report a diagnostic; 'readonly' should not have its own "
             "diagnostic";
    }
  }
}

TEST_F(test_parse_typescript_class,
       parameter_property_can_be_named_contextual_keyword) {
  // TODO(#73): Disallow names 'private', 'yield', etc. because those are
  // not allowed in strict mode (and classes enforce strict mode).
  for (string8 keyword : contextual_keywords |
                             dirty_set<string8>{u8"await", u8"yield"} |
                             strict_only_reserved_keywords) {
    {
      test_parser p(concat(u8"class C {\n"_sv
                           u8"  constructor(public "_sv,
                           keyword,
                           u8") {}\n"_sv
                           u8"}"_sv),
                    typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }

    {
      test_parser p(concat(u8"class C {\n"_sv
                           u8"  constructor(public readonly "_sv,
                           keyword,
                           u8") {}\n"_sv
                           u8"}"_sv),
                    typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }

    {
      test_parser p(concat(u8"class C {\n"_sv
                           u8"  constructor(readonly "_sv,
                           keyword,
                           u8") {}\n"_sv
                           u8"}"_sv),
                    typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }
  }
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
