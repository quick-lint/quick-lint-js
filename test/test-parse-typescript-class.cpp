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
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Class : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Class,
       field_with_type_is_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { fieldName: FieldType; }"_sv,  //
        u8"                   ^ Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, field_with_type_is_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { fieldName: FieldType; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_enter_type_scope",        // :
                              "visit_variable_type_use",       // FieldType
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // fieldName
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { fieldName: FieldType = init; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // FieldType
                              "visit_exit_type_scope",              //
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // fieldName
                              "visit_exit_class_scope",             // C
                              "visit_variable_declaration",         // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       class_index_signature_is_disallowed_in_javascript) {
  {
    Test_Parser p(u8"class C { [key: KeyType]: ValueType; }"_sv, capture_diags);
    p.parse_and_visit_module_catching_fatal_parse_errors();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"              ^ Diag_Unexpected_Token"_diag,
                       });
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       class_index_signature_is_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { [key: KeyType]: ValueType; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_type_scope",              //
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

TEST_F(Test_Parse_TypeScript_Class,
       optional_fields_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field1?; field2? = init; }"_sv,  //
        u8"                         ^ Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript"_diag,  //
        u8"                ^ Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript"_diag,  //

        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_property_declaration",         // field1
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // field2
                              "visit_exit_class_scope",             // C
                              "visit_variable_declaration",         // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, optional_fields_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field1?; field2? = init; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_property_declaration",         // field1
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // field2
                              "visit_exit_class_scope",             // C
                              "visit_variable_declaration",         // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"
        u8"  field1\n"
        u8"  ?;\n"
        u8"}"_sv,
        no_diags, typescript_options);
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

TEST_F(Test_Parse_TypeScript_Class, optional_accessors_are_not_allowed) {
  test_parse_and_visit_statement(
      u8"class C { accessor myField?; }"_sv,
      u8"                          ^ Diag_TypeScript_Accessor_Cannot_Be_Optional.optional_question\n"_diag
      u8"          ^^^^^^^^ .accessor_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class,
       optional_methods_are_allowed_in_typescript_classes) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method?() {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"
        u8"  method?\n"
        u8"  () {}\n"
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }))
        << "newline is allowed after '?'";
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"
        u8"  method\n"
        u8"  ?() {}\n"
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }))
        << "newline is allowed before '?'";
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       optional_methods_are_not_allowed_in_javascript_classes) {
  test_parse_and_visit_statement(
      u8"class C { method?() {} }"_sv,  //
      u8"                ^ Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Class,
       assignment_asserted_fields_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!: any; }"_sv,  //
        u8"               ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }

  {
    // Should not also report
    // Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!: any = init; }"_sv,  //
        u8"               ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }

  {
    // Should not also report
    // Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!; }"_sv,  //
        u8"               ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       assignment_asserted_fields_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!: any; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor field!: any; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       newline_not_allowed_before_assignment_asserted_field_bang) {
  {
    Test_Parser p(
        u8"class C {\n"
        u8"  field\n"
        u8"  !: any;\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                      ^ Diag_Newline_Not_Allowed_Before_Assignment_Assertion_Operator.bang\n"_diag
            u8"             ^^^^^ .field_name"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       assignment_asserted_field_must_have_a_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!; }"_sv,  //
        u8"               ^ Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field! = init; }"_sv,  //
        u8"               ^ Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type"_diag,  //
        u8"Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }

  {
    Test_Parser p(
        u8"class C {\n"
        u8"  field1!\n"
        u8"  field2;\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"field1", u8"field2"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                   ^ Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type"_diag,
        });
  }

  {
    Test_Parser p(
        u8"class C {\n"
        u8"  field1!\n"
        u8"  'field2';\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field1", std::nullopt));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                   ^ Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       assignment_asserted_field_cannot_have_initializers) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { field!: any = init; }"_sv,  //
        u8"                      ^ Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer.equal\n"_diag
        u8"               ^ .bang"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       assignment_asserted_methods_are_not_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method!() {} }"_sv,  //
        u8"                ^ Diag_TypeScript_Assignment_Asserted_Method"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       readonly_fields_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly field; }"_sv,  //
        u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"class C { readonly field = null; }"_sv,  //
      u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag);

  test_parse_and_visit_statement(
      u8"class C { readonly field\nmethod() {} }"_sv,  //
      u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag);

  test_parse_and_visit_statement(
      u8"class C { readonly field\n[methodName]() {} }"_sv,  //
      u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly async\nmethod() {} }"_sv,  //
        u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // async
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"class C { readonly field? method() {} }"_sv,  //
      u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag,  //
      u8"Diag_Missing_Semicolon_After_Field"_diag,  //
      u8"Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript"_diag);

  test_parse_and_visit_statement(
      u8"class C { readonly field: any; }"_sv,  //
      u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript"_diag,  //
      u8"Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Class, readonly_fields_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly field; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static readonly field; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly #field; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // #field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, newline_after_readonly_is_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly\n myField; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"readonly"_sv, u8"myField"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, readonly_methods_are_invalid) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly method() {} }"_sv,  //
        u8"          ^^^^^^^^ Diag_TypeScript_Readonly_Method"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, readonly_static_field_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { readonly static field; }"_sv,                      //
        u8"          ^^^^^^^^^^^^^^^ Diag_Readonly_Static_Field"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       declare_fields_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { declare field; }"_sv,  //
        u8"          ^^^^^^^ Diag_TypeScript_Declare_Field_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, declare_fields_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { declare field; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static declare field; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { declare static field; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, declare_fields_cannot_have_private_name) {
  test_parse_and_visit_statement(
      u8"class C { declare #field; }"_sv,  //
      u8"                  ^ Diag_TypeScript_Declare_Field_Cannot_Use_Private_Identifier.private_identifier_hash\n"_diag
      u8"          ^^^^^^^ .declare_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class, newline_after_declare_is_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { declare\n myField; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"declare"_sv, u8"myField"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, declare_methods_are_invalid) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { declare method() {} }"_sv,  //
        u8"          ^^^^^^^ Diag_TypeScript_Declare_Method"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       declare_field_cannot_be_assignment_asserted) {
  test_parse_and_visit_statement(
      u8"class C { declare myField!: number; }"_sv,  //
      u8"                         ^ Diag_TypeScript_Declare_Field_Cannot_Be_Assignment_Asserted.bang\n"_diag
      u8"          ^^^^^^^ .declare_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class,
       generic_classes_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C<T> { }"_sv,  //
        u8"       ^ Diag_TypeScript_Generics_Not_Allowed_In_JavaScript"_diag);
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
}

TEST_F(Test_Parse_TypeScript_Class, generic_classes_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C<T> { }"_sv, no_diags, typescript_options);
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
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C<T> extends Base<T> { }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_declaration",    // T
                              "visit_enter_type_scope",        // <
                              "visit_variable_type_use",       // T
                              "visit_exit_type_scope",         // >
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

TEST_F(Test_Parse_TypeScript_Class,
       generic_methods_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method<T>() {} }"_sv,  //
        u8"                ^ Diag_TypeScript_Generics_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, generic_methods_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method<T>() {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       call_signatures_are_disallowed_in_typescript_classes) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { () {} }"_sv,                             //
        u8"          ` Diag_Missing_Class_Method_Name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_enter_function_scope",       // (call signature)
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_property_declaration",       // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration",       // C
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { <T>() {} }"_sv,                          //
        u8"          ` Diag_Missing_Class_Method_Name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",          // C
                    "visit_enter_class_scope_body",     //
                    "visit_enter_function_scope",       // (call signature)
                    "visit_variable_declaration",       // T
                    "visit_enter_function_scope_body",  // (call signature)
                    "visit_exit_function_scope",        // (call signature)
                    "visit_property_declaration",       // (call signature)
                    "visit_exit_class_scope",           // C
                    "visit_variable_declaration",       // C
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Test_Parser p(
        u8"class C {\n"
        u8"  field!\n"
        u8"  (param) {}\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field", std::nullopt));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                       ` Diag_Missing_Class_Method_Name"_diag,
        });
  }

  {
    Test_Parser p(
        u8"class C {\n"
        u8"  field!\n"
        u8"  <T>(param) {}\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAre(u8"field", std::nullopt));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                       ` Diag_Missing_Class_Method_Name"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       access_specifiers_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { public method() {} }"_sv,  //
        u8"          ^^^^^^ Diag_TypeScript_Public_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { protected method() {} }"_sv,  //
        u8"          ^^^^^^^^^ Diag_TypeScript_Protected_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { private method() {} }"_sv,  //
        u8"          ^^^^^^^ Diag_TypeScript_Private_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  for (String8 specifier : {u8"public", u8"protected", u8"private"}) {
#define MATCH_ACCESS_SPECIFIER_ERROR(code_before)                            \
  ::testing::AnyOf(                                                          \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        Diag_TypeScript_Private_Not_Allowed_In_JavaScript,   \
                        specifier, code_before.size(), u8"private"_sv),      \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        Diag_TypeScript_Protected_Not_Allowed_In_JavaScript, \
                        specifier, code_before.size(), u8"protected"_sv),    \
      DIAG_TYPE_OFFSETS(p.code,                                              \
                        Diag_TypeScript_Public_Not_Allowed_In_JavaScript,    \
                        specifier, code_before.size(), u8"public"_sv))

    {
      Test_Parser p(
          concat(u8"class C { "_sv, specifier, u8" field = init; }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "_sv),
                            }));
    }

    {
      Test_Parser p(
          concat(u8"class C { "_sv, specifier, u8" field\nmethod() {} }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "_sv),
                            }));
    }

    {
      Test_Parser p(concat(u8"class C { "_sv, specifier,
                           u8" field\n[methodName]() {} }"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "_sv),
                            }));
    }

    {
      Test_Parser p(
          concat(u8"class C { "_sv, specifier, u8" field? method() {} }"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          UnorderedElementsAre(
              DIAG_TYPE(
                  Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript),
              DIAG_TYPE(Diag_Missing_Semicolon_After_Field),
              MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "_sv)));
    }

    {
      Test_Parser p(concat(u8"class C { "_sv, specifier,
                           u8" async\nmethod() { const await = null; } }"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.errors, ElementsAreArray({
                                MATCH_ACCESS_SPECIFIER_ERROR(u8"class C { "_sv),
                            }));
    }
#undef MATCH_ACCESS_SPECIFIER_ERROR
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       access_specifiers_are_allowed_in_typescript) {
  for (String8 specifier : {u8"public", u8"protected", u8"private"}) {
    {
      Padded_String code(
          concat(u8"class C { "_sv, specifier, u8" method() {} }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",          // C
                                "visit_enter_class_scope_body",     //
                                "visit_enter_function_scope",       // method
                                "visit_enter_function_scope_body",  // method
                                "visit_exit_function_scope",        // method
                                "visit_property_declaration",       // method
                                "visit_exit_class_scope",           // C
                                "visit_variable_declaration",       // C
                            }));
      EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    }

    {
      Padded_String code(
          concat(u8"class C { "_sv, specifier, u8" field; }"_sv));
      SCOPED_TRACE(code);
      test_parse_and_visit_statement(code.string_view(), no_diags,
                                     typescript_options);
    }

    {
      Padded_String code(
          concat(u8"class C { "_sv, specifier, u8" accessor field; }"_sv));
      SCOPED_TRACE(code);
      test_parse_and_visit_statement(code.string_view(), no_diags,
                                     typescript_options);
    }
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       access_specifiers_must_precede_other_modifiers_in_typescript) {
  for (String8 access_specifier : {u8"public", u8"protected", u8"private"}) {
    for (String8 other_modifier : {u8"static", u8"readonly", u8"accessor"}) {
      Padded_String code(concat(u8"class C { "_sv, other_modifier, u8" "_sv,
                                access_specifier, u8" public; }"_sv));
      SCOPED_TRACE(code);
      Test_Parser p(code.string_view(), typescript_options, capture_diags);
      p.parse_and_visit_statement();

      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_2_OFFSETS(
                  p.code, Diag_Access_Specifier_Must_Precede_Other_Modifiers,
                  second_modifier,
                  concat(u8"class C { "_sv, other_modifier, u8" "_sv).size(),
                  access_specifier,  //
                  first_modifier, u8"class C { "_sv.size(), other_modifier),
          }));
    }
  }

  for (String8 access_specifier : {u8"public", u8"protected", u8"private"}) {
    for (String8 other_modifier : {u8"static", u8"async"}) {
      Padded_String code(concat(u8"class C { "_sv, other_modifier, u8" "_sv,
                                access_specifier, u8" method() {}; }"_sv));
      SCOPED_TRACE(code);
      Test_Parser p(code.string_view(), typescript_options, capture_diags);
      p.parse_and_visit_statement();

      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_2_OFFSETS(
                  p.code, Diag_Access_Specifier_Must_Precede_Other_Modifiers,
                  second_modifier,
                  concat(u8"class C { "_sv, other_modifier, u8" "_sv).size(),
                  access_specifier,  //
                  first_modifier, u8"class C { "_sv.size(), other_modifier),
          }));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Class, newline_after_access_specifier_is_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { public\n myField; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"public"_sv, u8"myField"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { protected\n myField; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"protected"_sv, u8"myField"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { private\n myField; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"private"_sv, u8"myField"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       method_return_type_annotations_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(): T { } }"_sv,  //
        u8"                  ^ Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // method
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // T
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       method_return_type_annotations_are_allowed_in_typescript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(): T { } }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // method
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // T
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       abstract_classes_are_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { }"_sv,  //
        u8"^^^^^^^^ Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"export abstract class C { }"_sv,  //
      u8"       ^^^^^^^^ Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript"_diag);

  test_parse_and_visit_statement(
      u8"export default abstract class C { }"_sv,  //
      u8"               ^^^^^^^^ Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript"_diag);
}

TEST_F(Test_Parse_TypeScript_Class, abstract_class) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_class_method) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract myMethod(param); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // myMethod
                              "visit_variable_declaration",    // param
                              "visit_exit_function_scope",     // myMethod
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myMethod"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C {\n"
        u8"  abstract f()\n"      // ASI
        u8"  abstract g() }"_sv,  // ASI
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_property_declaration",    // f
                              "visit_enter_function_scope",    // g
                              "visit_exit_function_scope",     // g
                              "visit_property_declaration",    // g
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_class_method_requires_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract f() abstract g(); }"_sv,  //
        u8"                               ` Diag_Missing_Semicolon_After_Abstract_Method"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_property_declaration",    // f
                              "visit_enter_function_scope",    // g
                              "visit_exit_function_scope",     // g
                              "visit_property_declaration",    // g
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_methods_cannot_have_bodies) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract f() { } }"_sv,  //
        u8"                                ^ Diag_Abstract_Methods_Cannot_Contain_Bodies"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       abstract_method_prohibits_newline_after_abstract_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract\n m() { } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"abstract", u8"m"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_methods_cannot_be_async) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract async method(); }"_sv,  //
        u8"                            ^^^^^ Diag_Abstract_Methods_Cannot_Be_Async.async_keyword\n"_diag
        u8"                   ^^^^^^^^ .abstract_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // method
                              "visit_exit_function_scope",     // method
                              "visit_property_declaration",    // method
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { async abstract method(); }"_sv,  //
        u8"                   ^^^^^ Diag_Abstract_Methods_Cannot_Be_Async.async_keyword\n"_diag
        u8"                         ^^^^^^^^ .abstract_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // method
                              "visit_exit_function_scope",     // method
                              "visit_property_declaration",    // method
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    // ASI activates after 'async'.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"abstract class C { async\n abstract f(); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"async", u8"f"}));
  }

  {
    // ASI activates after 'abstract'.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"abstract class C { abstract\n async f() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"abstract", u8"f"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_methods_cannot_be_generators) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"abstract class C { abstract *method(); }"_sv,  //
      u8"                            ^ Diag_Abstract_Methods_Cannot_Be_Generators.star\n"_diag
      u8"                   ^^^^^^^^ .abstract_keyword"_diag,  //
      typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // C
                            "visit_enter_class_scope_body",  // {
                            "visit_enter_function_scope",    // method
                            "visit_exit_function_scope",     // method
                            "visit_property_declaration",    // method
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                        }));
}

TEST_F(Test_Parse_TypeScript_Class, abstract_field) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract myField: string; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // myField;
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract accessor myField: string; }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // myField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }

  test_parse_and_visit_statement(
      u8"abstract class C { accessor abstract myField: string; }"_sv,
      u8"                            ^^^^^^^^ Diag_Class_Modifier_Must_Preceed_Other_Modifier.expected_first_modifier\n"_diag
      u8"                   ^^^^^^^^ .expected_second_modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class, abstract_fields_cannot_have_initializers) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract myField: string = 'hello'; }"_sv,  //
        u8"                                            ^ Diag_Abstract_Field_Cannot_Have_Initializer.equal\n"_diag
        u8"                   ^^^^^^^^ .abstract_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // {
                              "visit_enter_class_scope_body",       // C
                              "visit_enter_type_scope",             // :
                              "visit_exit_type_scope",              //
                              "visit_enter_class_construct_scope",  // 'hello'
                              "visit_exit_class_construct_scope",   // 'hello'
                              "visit_property_declaration",         // myField
                              "visit_exit_class_scope",             // }
                              "visit_variable_declaration",         // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       abstract_properties_are_not_allowed_in_non_abstract_classes) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { abstract myField; }"_sv,  //
        u8"          ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class.abstract_keyword\n"_diag
        u8"^^^^^ .class_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { abstract myMethod(); }"_sv,  //
        u8"          ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class.abstract_keyword\n"_diag
        u8"^^^^^ .class_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, abstract_properties_cannot_be_static) {
  test_parse_and_visit_statement(
      u8"abstract class C { static abstract method(); }"_sv,  //
      u8"                          ^^^^^^^^ Diag_TypeScript_Abstract_Static_Property.abstract_keyword\n"_diag
      u8"                   ^^^^^^ .static_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class,
       newline_before_class_causes_abstract_to_be_identifier) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"abstract\nclass C { }"_sv,
                                                no_diags, typescript_options);
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

TEST_F(Test_Parse_TypeScript_Class, override_property) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends B { override method() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_variable_use",               // B
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends B { override field; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_variable_use",            // B
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends B { override accessor prop; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_variable_use",            // B
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // prop
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  test_parse_and_visit_module(
      u8"class C extends B { static override method() {} }"_sv, no_diags,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class, override_keyword_order) {
  test_parse_and_visit_module(
      u8"class C extends B { override static method() {} }"_sv,  //
      u8"                             ^^^^^^ Diag_Class_Modifier_Must_Preceed_Other_Modifier.expected_first_modifier\n"_diag  //
      u8"                    ^^^^^^^^ .expected_second_modifier"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"class C extends B { accessor override prop; }"_sv,  //
      u8"                             ^^^^^^^^ Diag_Class_Modifier_Must_Preceed_Other_Modifier.expected_first_modifier\n"_diag  //
      u8"                    ^^^^^^^^ .expected_second_modifier"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"class C extends B { override public method() {} }"_sv,  //
      u8"                             ^^^^^^ Diag_Access_Specifier_Must_Precede_Other_Modifiers.second_modifier\n"_diag  //
      u8"                    ^^^^^^^^ .first_modifier"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"class C extends B { override public field; }"_sv,  //
      u8"                             ^^^^^^ Diag_Access_Specifier_Must_Precede_Other_Modifiers.second_modifier\n"_diag  //
      u8"                    ^^^^^^^^ .first_modifier"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"abstract class C extends B { override abstract field; }"_sv,  //
      u8"                                      ^^^^^^^^ Diag_Class_Modifier_Must_Preceed_Other_Modifier.expected_first_modifier\n"_diag  //
      u8"                             ^^^^^^^^ .expected_second_modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class,
       override_method_prohibits_newline_after_override_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { override\n m() { } }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"override", u8"m"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, accessor_field) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor f: MyType; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_type_scope",        // :
                              "visit_variable_type_use",       // MyType
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // f
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyType"_sv}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, accessor_conflicts_with_readonly) {
  test_parse_and_visit_statement(
      u8"class C { accessor readonly f: MyType; }"_sv,  //
      u8"                   ^^^^^^^^ Diag_Class_Conflicting_Modifiers.second_modifier\n"_diag
      u8"          ^^^^^^^^ .first_modifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { readonly accessor f: MyType; }"_sv,  //
      u8"                   ^^^^^^^^ Diag_Class_Conflicting_Modifiers.second_modifier\n"_diag
      u8"          ^^^^^^^^ .first_modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class, implements_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C implements Base {}"_sv,  //
        u8"        ^^^^^^^^^^ Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends Base implements I {}"_sv,  //
        u8"                     ^^^^^^^^^^ Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript"_diag,  //

        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_variable_type_use",       // I
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    // Should not report Diag_TypeScript_Implements_Must_Be_After_Extends.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C implements I extends Base {}"_sv,  //
        u8"        ^^^^^^^^^^ Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // I
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, implements) {
  Spy_Visitor p = test_parse_and_visit_module(u8"class C implements Base {}"_sv,
                                              no_diags, typescript_options);
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

TEST_F(Test_Parse_TypeScript_Class, implements_comes_after_extends) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"class C extends Base implements I {}"_sv,
                                    no_diags, typescript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C implements I extends Base {}"_sv,  //
        u8"        ^^^^^^^^^^ Diag_TypeScript_Implements_Must_Be_After_Extends.implements_keyword\n"_diag
        u8"                     ^^^^^^^ .extends_keyword"_diag,  //
        typescript_options);
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
  }
}

TEST_F(Test_Parse_TypeScript_Class, implements_interface_from_namespace) {
  Spy_Visitor p = test_parse_and_visit_module(u8"class C implements ns.A {}"_sv,
                                              no_diags, typescript_options);
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

TEST_F(Test_Parse_TypeScript_Class, implement_multiple_things) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"class C implements Apple, Banana, Carrot {}"_sv, no_diags,
      typescript_options);
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

TEST_F(Test_Parse_TypeScript_Class,
       implements_generic_with_arrow_type_requires_space) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C implements I<<T>() => ReturnType<T>> {}"_sv,  //
        u8"                     ` Diag_TypeScript_Generic_Less_Less_Not_Split.expected_space"_diag
        u8"{.context=Statement_Kind::class_implements_clause}"_diag,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), class_decl(u8"C")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"I", u8"ReturnType", u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C implements I< <T>() => ReturnType<T>> {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), class_decl(u8"C")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"I", u8"ReturnType", u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       extends_generic_with_arrow_type_requires_space) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export class C extends I<<T>() => ReturnType<T>> {}"_sv,  //
        u8"                         ` Diag_TypeScript_Generic_Less_Less_Not_Split.expected_space"_diag
        u8"{.context=Statement_Kind::class_extends_clause}"_diag,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), class_decl(u8"C")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ReturnType", u8"T", u8"I"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export class C extends I< <T>() => ReturnType<T>> {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), class_decl(u8"C")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ReturnType", u8"T", u8"I"}));
  }
}

TEST_F(Test_Parse_TypeScript_Class, class_name_can_be_omitted_in_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"class {}"_sv, no_diags,
                                                    typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"class extends Base {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"class implements I {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_type_use",       // I
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Class, parameter_property_in_constructor) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(public field) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",       // C
                    "visit_enter_class_scope_body",  // {
                    // TODO(strager): visit_property_declaration for 'field'.
                    "visit_enter_function_scope",       // constructor
                    "visit_variable_declaration",       // field
                    "visit_enter_function_scope_body",  // {
                    "visit_exit_function_scope",        // }
                    "visit_property_declaration",       // constructor
                    "visit_exit_class_scope",           // }
                    "visit_variable_declaration",       // C
                    "visit_end_of_module",
                }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(protected field) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(private field) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(public field: FieldType) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(public field = defaultValue) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"defaultValue"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C {\n"_sv
        u8"  constructor(readonly field) {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
  }

  for (String8_View access_specifier :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    Test_Parser p(concat(u8"class C {\n"_sv
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

  for (String8_View access_specifier :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    Test_Parser p(concat(u8"class C {\n"_sv
                         u8"  constructor(readonly "_sv,
                         access_specifier,
                         u8" public) {}\n"_sv
                         u8"}"_sv),
                  typescript_options, capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();

    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, Diag_Access_Specifier_Must_Precede_Other_Modifiers,
                second_modifier,
                u8"class C {\n  constructor(readonly "_sv.size(),
                access_specifier,  //
                first_modifier, u8"class C {\n  constructor("_sv.size(),
                u8"readonly"),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       parameter_property_in_constructor_named_with_escapes) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"class C {\n"_sv
      u8"  \\u{63}onstructo\\u{72}(public readonly field) {}"_sv
      u8"}"_sv,
      no_diags, typescript_options);
  EXPECT_THAT(
      p.variable_declarations,
      ElementsAreArray({func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Class, parameter_property_cannot_destructure) {
  {
    Test_Parser p(
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
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                                ^ Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured.destructure_token\n"_diag
            u8"                         ^^^^^^ .property_keyword"_diag,
        });
  }

  {
    Test_Parser p(
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
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                                ^ Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured.destructure_token\n"_diag
            u8"                         ^^^^^^ .property_keyword"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Class, parameter_property_cannot_be_rest) {
  {
    Test_Parser p(
        u8"class C {\n"_sv
        u8"  constructor(public ...field) {}\n"_sv
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Assert that visit_property_declaration(field) occurred.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                                ^^^ Diag_TypeScript_Parameter_Property_Cannot_Be_Rest.spread\n"_diag
            u8"                         ^^^^^^ .property_keyword"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       parameter_property_is_not_allowed_in_javascript) {
  for (String8_View keyword :
       {u8"readonly"_sv, u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      Test_Parser p(concat(u8"class C {\n"_sv
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
                  Diag_TypeScript_Parameter_Property_Not_Allowed_In_JavaScript,  //
                  property_keyword, u8"class C {\n  constructor("_sv.size(),
                  keyword),
          }));
    }
  }

  for (String8_View keyword :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      Test_Parser p(concat(u8"class C {\n"_sv
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
                  Diag_TypeScript_Parameter_Property_Not_Allowed_In_JavaScript,  //
                  property_keyword, u8"class C {\n  constructor("_sv.size(),
                  keyword),
          }))
          << "only '" << out_string8(keyword)
          << "' should report a diagnostic; 'readonly' should not have its own "
             "diagnostic";
    }
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       parameter_property_can_be_named_contextual_keyword) {
  // TODO(#73): Disallow names 'private', 'yield', etc. because those are
  // not allowed in strict mode (and classes enforce strict mode).
  for (String8 keyword : contextual_keywords |
                             Dirty_Set<String8>{u8"await", u8"yield"} |
                             strict_only_reserved_keywords) {
    {
      Spy_Visitor p =
          test_parse_and_visit_module(concat(u8"class C {\n"_sv
                                             u8"  constructor(public "_sv,
                                             keyword,
                                             u8") {}\n"_sv
                                             u8"}"_sv),
                                      no_diags, typescript_options);
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_module(
          concat(u8"class C {\n"_sv
                 u8"  constructor(public readonly "_sv,
                 keyword,
                 u8") {}\n"_sv
                 u8"}"_sv),
          no_diags, typescript_options);
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }

    {
      Spy_Visitor p =
          test_parse_and_visit_module(concat(u8"class C {\n"_sv
                                             u8"  constructor(readonly "_sv,
                                             keyword,
                                             u8") {}\n"_sv
                                             u8"}"_sv),
                                      no_diags, typescript_options);
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({func_param_decl(keyword), class_decl(u8"C"_sv)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       parameter_property_not_allowed_on_normal_method) {
  {
    Test_Parser p(
        u8"class C {\n"_sv
        u8"  notAConstructor(public field) {}\n"_sv
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Assert that visit_property_declaration(field) occurred.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                             ^^^^^^ Diag_TypeScript_Parameter_Property_Only_Allowed_In_Class_Constructor"_diag,
        });
  }
}

// NOTE[typescript-constructor-escape]: The TypeScript compiler complains if
// 'constructor' in a class contains an identifier escape sequence, despite
// escapes being allowed in vanilla JavaScript. TypeScript does not complain if
// there is more than one sequence, though.
TEST_F(Test_Parse_TypeScript_Class, constructor_keyword_with_escape_sequence) {
  Test_Parser p(
      u8"class C {\n"_sv
      u8"  \\u{63}onstructor() {}"_sv  // equivalent to: constructor() {}
      u8"}"_sv,
      typescript_options, capture_diags);
  p.parse_and_visit_statement();

  assert_diagnostics(
      p.code, p.errors,
      {
          u8"             ^^^^^^^^^^^^^^^^^ Diag_Keyword_Contains_Escape_Characters"_diag,
      });
}

TEST_F(Test_Parse_TypeScript_Class, no_diag_for_more_than_one_escape) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"class C {\n"_sv
      u8"  \\u{63}onstructo\\u{72}() {}"_sv  // equivalent to: constructor() {}
      u8"}"_sv,
      no_diags, typescript_options);

  EXPECT_THAT(p.visits,
              ElementsAreArray({
                  "visit_enter_class_scope",          //
                  "visit_enter_class_scope_body",     //
                  "visit_enter_function_scope",       //
                  "visit_enter_function_scope_body",  //
                  "visit_exit_function_scope",        //
                  "visit_property_declaration",       //
                  "visit_exit_class_scope",           //
                  "visit_variable_declaration",       //
              }));
}

TEST_F(
    Test_Parse_TypeScript_Class,
    escape_in_contextual_keyword_property_name_is_allowed_in_typescript_except_constructor) {
  for (String8_View keyword :
       contextual_keywords - Dirty_Set<String8>{u8"constructor"}) {
    Test_Parser p(
        concat(u8"class C { "_sv, escape_first_character_in_keyword(keyword),
               u8"() {} }"_sv),
        typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
  }
}

TEST_F(Test_Parse_TypeScript_Class,
       constructor_keyword_with_escape_sequence_legal_in_javascript) {
  test_parse_and_visit_statement(
      u8"class C {\n"_sv
      u8"  \\u{63}onstructor() {}"_sv  // equivalent to: constructor() {}
      u8"}"_sv,
      no_diags, javascript_options);
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
