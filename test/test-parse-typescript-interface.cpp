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
#include <quick-lint-js/dirty-set.h>
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

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Interface : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Interface, not_supported_in_vanilla_javascript) {
  Parser_Options options;
  options.typescript = false;
  Test_Parser p(u8"interface I {}"_sv, options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  assert_diagnostics(
      p.code, p.errors,
      {
          u8"^^^^^^^^^ Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript"_diag,
      });
}

TEST_F(Test_Parse_TypeScript_Interface, empty_interface) {
  Spy_Visitor p = test_parse_and_visit_module(u8"interface I {}"_sv, no_diags,
                                              typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({interface_decl(u8"I"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Interface, interface_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I"_sv,                                               //
        u8"^^^^^^^^^^^ Diag_Missing_Body_For_TypeScript_Interface"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I extends Other"_sv,  //
        u8"^^^^^^^^^^^^^^^^^^^^^^^^^ Diag_Missing_Body_For_TypeScript_Interface"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_type_use",      // J
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, extends) {
  Spy_Visitor p = test_parse_and_visit_module(u8"interface I extends A {}"_sv,
                                              no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_variable_type_use",      // A
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
}

TEST_F(Test_Parse_TypeScript_Interface, extends_interface_from_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I extends ns.A {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // I
                              "visit_enter_interface_scope",   // I
                              "visit_variable_namespace_use",  // ns
                              "visit_exit_interface_scope",    // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I extends ns.subns.A {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // I
                              "visit_enter_interface_scope",   // I
                              "visit_variable_namespace_use",  // ns
                              "visit_exit_interface_scope",    // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, extends_multiple_things) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"interface I extends A, B, C {}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_variable_type_use",      // A
                            "visit_variable_type_use",      // B
                            "visit_variable_type_use",      // C
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
}

TEST_F(Test_Parse_TypeScript_Interface, extends_generic) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"interface I extends A<B> {}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_variable_type_use",      // A
                            "visit_enter_type_scope",       // extends
                            "visit_variable_type_use",      // B
                            "visit_exit_type_scope",        // >
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
}

TEST_F(Test_Parse_TypeScript_Interface,
       extends_generic_with_arrow_type_requires_space) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I extends Base<<T>() => ReturnType<T>> {}"_sv,  //
        u8"                         ` Diag_TypeScript_Generic_Less_Less_Not_Split.expected_space"_diag
        u8"{.context=Statement_Kind::interface_extends_clause}"_diag,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({interface_decl(u8"I"), generic_param_decl(u8"T")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Base", u8"ReturnType", u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I extends Base< <T>() => ReturnType<T>> {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({interface_decl(u8"I"), generic_param_decl(u8"T")}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Base", u8"ReturnType", u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, unclosed_interface_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { "_sv,                                 //
        u8"            ^ Diag_Unclosed_Interface_Block"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { property "_sv,                        //
        u8"            ^ Diag_Unclosed_Interface_Block"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { method() "_sv,                        //
        u8"            ^ Diag_Unclosed_Interface_Block"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_enter_function_scope",   // method
                              "visit_exit_function_scope",    // method
                              "visit_property_declaration",   // method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       interface_can_be_named_contextual_keyword) {
  for (String8 name : contextual_keywords - typescript_builtin_type_keywords -
                          typescript_special_type_keywords -
                          Dirty_Set<String8>{
                              u8"let",
                              u8"static",
                              u8"yield",
                          }) {
    Padded_String code(concat(u8"interface "_sv, name, u8" {}"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_statement(code.string_view(), no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // (name)
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       interface_cannot_have_newline_after_interface_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface\nI {}"_sv,                                               //
        u8"^^^^^^^^^ Diag_Newline_Not_Allowed_After_Interface_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",
                          }));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface\nI<T> {}"_sv,                                            //
        u8"^^^^^^^^^ Diag_Newline_Not_Allowed_After_Interface_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_declaration",   // T
                              "visit_exit_interface_scope",
                          }));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface\nI<T>\n{}"_sv,                                           //
        u8"^^^^^^^^^ Diag_Newline_Not_Allowed_After_Interface_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_declaration",   // T
                              "visit_exit_interface_scope",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       interface_keyword_with_following_newline_is_variable_name) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"interface\nI\n{}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // interface
                              "visit_variable_use",       // I
                              "visit_enter_block_scope",  // {
                              "visit_exit_block_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"interface", u8"I"}));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    Spy_Visitor p = test_parse_and_visit_module(u8"interface\nI<T> {}"_sv,
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // interface
                              "visit_variable_use",  // I
                              "visit_variable_use",  // T
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, property_without_type) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"interface I { a;b\nc }"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // a
                              "visit_property_declaration",   // b
                              "visit_property_declaration",   // c
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"a", u8"b", u8"c"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { 'fieldName'; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // 'fieldName'
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { 3.14; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // 3.14
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [x + y]; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_variable_use",           // x
                              "visit_variable_use",           // y
                              "visit_property_declaration",   // (x + y)
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, optional_property) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName?; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // fieldName
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
  }

  {
    // Semicolon is required.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { fieldName? otherField }"_sv,  //
        u8"                        ` Diag_Missing_Semicolon_After_Field"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName?\notherField }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [2 + 2]?; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { 'prop'?; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { method?(param); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_enter_function_scope",   // method
                              "visit_variable_declaration",   // param
                              "visit_exit_function_scope",    // method
                              "visit_property_declaration",   // method
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    // should parse optional field but not complain about it
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { field?; }"_sv,                                 //
        u8"Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       field_requires_comma_or_semicolon_or_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { myField1; myField2: any }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"myField1"_sv, u8"myField2"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { myField1, myField2: any, }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"myField1"_sv, u8"myField2"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { myField }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { myField1\n myField2: any\n }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"myField1"_sv, u8"myField2"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       assignment_asserted_field_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName!: any; }"_sv,  //
        u8"                       ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
  }

  {
    // Missing type annotation should not report two errors.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName!; }"_sv,  //
        u8"                       ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
  }

  {
    // Initializer should not report two errors.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName!: any = init; }"_sv,  //
        u8"                       ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, field_with_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName: FieldType; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_enter_type_scope",       // :
                              "visit_variable_type_use",      // FieldType
                              "visit_exit_type_scope",        //
                              "visit_property_declaration",   // fieldName
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
  }

  {
    // Semicolon is required.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { fieldName: FieldType otherField }"_sv,  //
        u8"                                  ` Diag_Missing_Semicolon_After_Field"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { fieldName: FieldType\notherField }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, interface_with_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface Monster { eatMuffins(muffinCount); }"_sv, no_diags,
        typescript_options);
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"Monster");
    EXPECT_EQ(p.variable_declarations[1].name, u8"muffinCount");

    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"eatMuffins"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Monster
                              "visit_enter_interface_scope",  //
                              "visit_enter_function_scope",   //
                              "visit_variable_declaration",   // muffinCount
                              "visit_exit_function_scope",    //
                              "visit_property_declaration",   // eatMuffins
                              "visit_exit_interface_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { get length(); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { set length(value); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { a(); b(); c(); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"a", u8"b", u8"c"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { \"stringKey\"(); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [x + y](); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface Getter<T> { get(): T; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Getter
                              "visit_enter_interface_scope",  // {
                              "visit_variable_declaration",   // T
                              "visit_enter_function_scope",   //
                              "visit_enter_type_scope",       // :
                              "visit_variable_type_use",      // T
                              "visit_exit_type_scope",        //
                              "visit_exit_function_scope",    //
                              "visit_property_declaration",   // get
                              "visit_exit_interface_scope",   // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, interface_with_index_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType]: ValueType; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_type_scope",              //
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"KeyType", u8"ValueType"}));
    // TODO(strager): We probably should create a new kind of variable instead
    // of 'parameter'.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv),
                                  index_signature_param_decl(u8"key"_sv)}));
  }

  {
    // should parse index signature and not complain about it
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType]: ValueType; }"_sv,              //
        u8"Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_type_scope",              //
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       index_signature_variable_can_be_named_contextual_keyword) {
  for (String8_View keyword : contextual_keywords) {
    Spy_Visitor p =
        test_parse_and_visit_statement(concat(u8"interface I { ["_sv, keyword,
                                              u8": KeyType]: ValueType; }"_sv),
                                       no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv),
                                  index_signature_param_decl(keyword)}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, index_signature_requires_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType]; }"_sv,  //
        u8"                            ` Diag_TypeScript_Index_Signature_Needs_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType], method(); }"_sv,  //
        u8"                            ` Diag_TypeScript_Index_Signature_Needs_Type"_diag,  //
        typescript_options);
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType]\n  method(); }"_sv,  //
        u8"                            ` Diag_TypeScript_Index_Signature_Needs_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_exit_index_signature_scope",   //
                              "visit_enter_function_scope",         // method
                              "visit_exit_function_scope",          // method
                              "visit_property_declaration",         // method
                              "visit_exit_interface_scope",         // I
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, index_signature_cannot_be_a_method) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType](param); }"_sv,  //
        u8"                            ^ Diag_TypeScript_Index_Signature_Cannot_Be_Method"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_variable_declaration",         // I
                    "visit_enter_interface_scope",        // I
                    "visit_enter_index_signature_scope",  //
                    "visit_enter_type_scope",             // :
                    "visit_variable_type_use",            // KeyType
                    "visit_exit_type_scope",              //
                    "visit_variable_declaration",         // key
                    // TODO(strager): Don't emit visit_property_declaration.
                    "visit_enter_function_scope",        //
                    "visit_variable_declaration",        // param
                    "visit_exit_function_scope",         //
                    "visit_property_declaration",        //
                    "visit_exit_index_signature_scope",  //
                    "visit_exit_interface_scope",        // I
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       index_signature_requires_semicolon_or_comma_or_asi) {
  test_parse_and_visit_statement(
      u8"interface I { [key: KeyType]: ValueType; method(); }"_sv, no_diags,
      typescript_options);
  test_parse_and_visit_statement(
      u8"interface I { [key: KeyType]: ValueType, method(); }"_sv, no_diags,
      typescript_options);
  test_parse_and_visit_statement(
      u8"interface I { [key: KeyType]: ValueType }"_sv,  // ASI
      no_diags, typescript_options);
  test_parse_and_visit_statement(
      u8"interface I {\n"_sv
      u8"  [key: KeyType]: ValueType\n"_sv  // ASI
      u8"  method();\n"_sv
      u8"}"_sv,
      no_diags, typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { [key: KeyType]: ValueType method(); }"_sv,  //
        u8"                                       ` Diag_Missing_Semicolon_After_Index_Signature"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // KeyType
                              "visit_exit_type_scope",              //
                              "visit_variable_declaration",         // key
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_type_scope",              //
                              "visit_exit_index_signature_scope",   //
                              "visit_enter_function_scope",         // method
                              "visit_exit_function_scope",          // method
                              "visit_property_declaration",         // method
                              "visit_exit_interface_scope",         // I
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, interface_methods_cannot_have_bodies) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { method() { x } }"_sv,  //
        u8"                       ^ Diag_Interface_Methods_Cannot_Contain_Bodies"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // I
                              "visit_enter_interface_scope",      //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_interface_scope",       //
                              "visit_end_of_module",
                          }));
  }

  test_parse_and_visit_module(
      u8"interface I { method() => { x } }"_sv,  //
      u8"                          ^ Diag_Interface_Methods_Cannot_Contain_Bodies"_diag,  //
      u8"Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, interface_with_keyword_property) {
  for (String8_View suffix : {u8""_sv, u8"?"_sv}) {
    for (String8_View keyword : keywords) {
      {
        Test_Parser p(
            concat(u8"interface I { "_sv, keyword, suffix, u8"(); }"_sv),
            typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      for (String8_View prefix : {u8"get"_sv, u8"set"_sv}) {
        Test_Parser p(concat(u8"interface I { "_sv, prefix, u8" "_sv, keyword,
                             suffix, u8"(); }"_sv),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      {
        Test_Parser p(concat(u8"interface I { "_sv, keyword, suffix, u8" }"_sv),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      {
        Test_Parser p(
            concat(u8"interface I { "_sv, keyword, suffix, u8"; }"_sv),
            typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      {
        Test_Parser p(
            concat(u8"interface I { "_sv, keyword, suffix, u8": any; }"_sv),
            typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }
    }

    for (String8_View keyword : strict_reserved_keywords) {
      String8 property = escape_first_character_in_keyword(keyword);
      for (String8_View prefix : {u8""_sv, u8"get"_sv, u8"set"_sv}) {
        Padded_String code(concat(u8"interface I { "_sv, prefix, u8" "_sv,
                                  property, suffix, u8"(); }"_sv));
        SCOPED_TRACE(code);
        Spy_Visitor p = test_parse_and_visit_statement(
            code.string_view(), no_diags, typescript_options);
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }
    }
  }
}

TEST_F(Test_Parse_TypeScript_Interface, interface_with_number_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface Wat { 42.0(); }"_sv, no_diags, typescript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Wat
                              "visit_enter_interface_scope",  //
                              "visit_enter_function_scope",   //
                              "visit_exit_function_scope",    //
                              "visit_property_declaration",   // 42.0
                              "visit_exit_interface_scope",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, interface_allows_stray_semicolons) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"interface I{ ; f() ; ; }"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"}));
}

TEST_F(Test_Parse_TypeScript_Interface, private_properties_are_not_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { #method(); }"_sv,  //
        u8"              ^^^^^^^ Diag_Interface_Properties_Cannot_Be_Private"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_enter_function_scope",   // #method
                              "visit_exit_function_scope",    // #method
                              "visit_property_declaration",   // #method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { #field; }"_sv,  //
        u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Private"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #field
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { async static #method(); }"_sv,  //
        u8"                           ^^^^^^^ Diag_Interface_Properties_Cannot_Be_Private"_diag,  //
        u8"Diag_Interface_Methods_Cannot_Be_Async"_diag,      //
        u8"Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_enter_function_scope",   // #method
                              "visit_exit_function_scope",    // #method
                              "visit_property_declaration",   // #method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I { readonly static #field; }"_sv,  //
        u8"                              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Private"_diag,  //
        u8"Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #field
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, static_properties_are_not_allowed) {
  for (String8 property_name : Dirty_Set<String8>{u8"myProperty"} | keywords) {
    SCOPED_TRACE(out_string8(property_name));

    {
      Test_Parser p(
          concat(u8"interface I { static "_sv, property_name, u8"(); }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_enter_function_scope",   // property
                                "visit_exit_function_scope",    // property
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"interface I { static get "_sv, property_name, u8"(); }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_enter_function_scope",   // property
                                "visit_exit_function_scope",    // property
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"interface I { static set "_sv, property_name,
                           u8"(value); }"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_enter_function_scope",   // property
                                "visit_variable_declaration",   // value
                                "visit_exit_function_scope",    // property
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"interface I { static "_sv, property_name, u8"; }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    // TODO(#736): Fix 'static readonly static'.
    if (property_name != u8"static") {
      Test_Parser p(concat(u8"interface I { static readonly "_sv, property_name,
                           u8"; }"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"interface I { static async\n "_sv, property_name,
                           u8"(); }"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      Test_Parser p(
          concat(u8"interface I { static\n"_sv, property_name, u8"(); }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({property_name}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      Test_Parser p(
          concat(u8"interface I { static\n"_sv, property_name, u8"; }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({property_name}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,
          });
    }
  }

  test_parse_and_visit_module(
      u8"interface I { static field\n method(); }"_sv,  //
      u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"interface I { static field\n ['methodName'](); }"_sv,  //
      u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"interface I { static field? method(); }"_sv,  //
      u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      u8"Diag_Missing_Semicolon_After_Field"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, accessor_fields_are_not_allowed) {
  test_parse_and_visit_module(
      u8"interface I { accessor field; }"_sv,  //
      u8"              ^^^^^^^^ Diag_Interface_Field_Cannot_Be_Accessor"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, declare_fields_are_not_allowed) {
  test_parse_and_visit_module(
      u8"interface I { declare field; }"_sv,  //
      u8"              ^^^^^^^ Diag_Interface_Field_Cannot_Be_Declare"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, async_methods_are_not_allowed) {
  for (String8 method_name : Dirty_Set<String8>{u8"method"} | keywords) {
    SCOPED_TRACE(out_string8(method_name));

    {
      Test_Parser p(
          concat(u8"interface I { async "_sv, method_name, u8"(); }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_enter_function_scope",   // method
                                "visit_exit_function_scope",    // method
                                "visit_property_declaration",   // method
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"              ^^^^^ Diag_Interface_Methods_Cannot_Be_Async"_diag,
          });
    }

    {
      // ASI activates after 'async'.
      Test_Parser p(
          concat(u8"interface I { async\n"_sv, method_name, u8"(); }"_sv),
          typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAre(u8"async", method_name));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Interface, generator_methods_are_not_allowed) {
  for (String8 method_name : Dirty_Set<String8>{u8"method"} | keywords) {
    SCOPED_TRACE(out_string8(method_name));

    {
      // clang-format off
      Spy_Visitor p = test_parse_and_visit_module(
          concat(u8"interface I { *"_sv, method_name, u8"(); }"_sv), //
          /*  */ u8"              ^ Diag_Interface_Methods_Cannot_Be_Generators"_diag, //
          typescript_options);
      // clang-format on
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_enter_function_scope",   // method
                                "visit_exit_function_scope",    // method
                                "visit_property_declaration",   // method
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
    }

    // clang-format off
    test_parse_and_visit_module(
        concat(u8"interface I { static *"_sv, method_name, u8"(); }"_sv),  //
        /*  */ u8"                     ^ Diag_Interface_Methods_Cannot_Be_Generators"_diag,  //
        /*  */ u8"Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
        typescript_options);
    // clang-format on

    // clang-format off
    test_parse_and_visit_module(
        concat(u8"interface I { async *"_sv, method_name, u8"(); }"_sv),  //
        /*  */ u8"                    ^ Diag_Interface_Methods_Cannot_Be_Generators"_diag,  //
        /*  */ u8"Diag_Interface_Methods_Cannot_Be_Async"_diag,  //
        typescript_options);
    // clang-format on
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       static_async_methods_are_definitely_not_allowed) {
  test_parse_and_visit_module(
      u8"interface I { static async method(); }"_sv,  //
      u8"                     ^^^^^ Diag_Interface_Methods_Cannot_Be_Async"_diag,  //
      u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"interface I { async static method(); }"_sv,  //
      u8"                    ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      u8"              ^^^^^ Diag_Interface_Methods_Cannot_Be_Async"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"interface I { async static *method(); }"_sv,  //
      u8"                           ^ Diag_Interface_Methods_Cannot_Be_Generators"_diag,  //
      u8"                    ^^^^^^ Diag_Interface_Properties_Cannot_Be_Static"_diag,  //
      u8"              ^^^^^ Diag_Interface_Methods_Cannot_Be_Async"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, field_initializers_are_not_allowed) {
  for (String8 field_name : Dirty_Set<String8>{u8"field"} | keywords) {
    SCOPED_TRACE(out_string8(field_name));

    {
      Test_Parser p(concat(u8"interface I { "_sv, field_name, u8" = y; }"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",         // I
                                "visit_enter_interface_scope",        //
                                "visit_enter_class_construct_scope",  //
                                "visit_variable_use",                 // y
                                "visit_exit_class_construct_scope",   //
                                "visit_property_declaration",  // field_name
                                "visit_exit_interface_scope",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Interface_Fields_Cannot_Have_Initializers,  //
                  equal, (u8"interface I { " + field_name + u8" ").size(),
                  u8"="_sv),
          }));
    }

    {
      Test_Parser p(
          concat(u8"interface I { static "_sv, field_name, u8" = y; }"_sv),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ::testing::UnorderedElementsAreArray({
              DIAG_TYPE(Diag_Interface_Properties_Cannot_Be_Static),
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Interface_Fields_Cannot_Have_Initializers,  //
                  equal,
                  (u8"interface I { static " + field_name + u8" ").size(),
                  u8"="_sv),
          }));
    }
  }

  test_parse_and_visit_module(
      u8"interface I { 'fieldName' = init; }"_sv,  //
      u8"                          ^ Diag_Interface_Fields_Cannot_Have_Initializers"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"interface I { fieldName: typeName = init; }"_sv,  //
      u8"                                  ^ Diag_Interface_Fields_Cannot_Have_Initializers"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface,
       interface_named_await_in_async_function) {
  test_parse_and_visit_statement(u8"interface await {}"_sv, no_diags,
                                 typescript_options);

  test_parse_and_visit_statement(
      u8"function f() {"
      u8"interface await {}"
      u8"}"_sv,
      no_diags, typescript_options);

  test_parse_and_visit_module(
      u8"async function g() { interface await {} }"_sv,  //
      u8"                               ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Interface, call_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { (param); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_property_declaration",  // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       call_signature_after_invalid_field_with_newline) {
  {
    Test_Parser p(
        u8"interface I {\n"
        u8"  field!\n"
        u8"  (param);\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // field
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_property_declaration",  // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                      ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       call_signature_cannot_have_generator_star) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { *(param); }"_sv,  //
        u8"              ^ Diag_Interface_Methods_Cannot_Be_Generators"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_property_declaration",  // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, generic_call_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { <T>(param); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_property_declaration",  // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv),
                                  generic_param_decl(u8"T"_sv),
                                  func_param_decl(u8"param"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, generic_interface) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I<T> { field: T; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_declaration",   // T
                              "visit_enter_type_scope",       // :
                              "visit_variable_type_use",      // T
                              "visit_exit_type_scope",        //
                              "visit_property_declaration",   // field
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {interface_decl(u8"I"_sv), generic_param_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I<T> extends T {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_declaration",   // T
                              "visit_variable_type_use",      // T
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {interface_decl(u8"I"_sv), generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, access_specifiers_are_not_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { public method(); }"_sv,  //
        u8"              ^^^^^^ Diag_Interface_Properties_Cannot_Be_Explicitly_Public"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { protected method(); }"_sv,  //
        u8"              ^^^^^^^^^ Diag_Interface_Properties_Cannot_Be_Protected"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { private method(); }"_sv,  //
        u8"              ^^^^^^^ Diag_Interface_Properties_Cannot_Be_Private"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, static_blocks_are_not_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { static { console.log('hello'); } }"_sv,  //
        u8"              ^^^^^^ Diag_TypeScript_Interfaces_Cannot_Contain_Static_Blocks"_diag,  //
        typescript_options);
    EXPECT_THAT(p.property_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       type_annotations_dont_add_extra_diagnostic_in_javascript) {
  test_parse_and_visit_statement(
      u8"interface I<T> { method(): Type; }"_sv,                      //
      u8"Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Interface,
       method_requires_comma_or_semicolon_or_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { f(); g(); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { f(), g(), }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I {\n"
        u8"  f()\n"      // ASI
        u8"  g() }"_sv,  // ASI
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_property_declaration",   // f
                              "visit_enter_function_scope",   // g
                              "visit_exit_function_scope",    // g
                              "visit_property_declaration",   // g
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { f() g(); }"_sv,  //
        u8"                 ` Diag_Missing_Semicolon_After_Interface_Method"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_property_declaration",   // f
                              "visit_enter_function_scope",   // g
                              "visit_exit_function_scope",    // g
                              "visit_property_declaration",   // g
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       abstract_properties_are_not_allowed_in_interfaces) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { abstract myField; }"_sv,  //
        u8"              ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Interface"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // myField
                              "visit_exit_interface_scope",   // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { abstract myMethod(); }"_sv,  //
        u8"              ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Interface"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_enter_function_scope",   // myMethod
                              "visit_exit_function_scope",    // myMethod
                              "visit_property_declaration",   // myMethod
                              "visit_exit_interface_scope",   // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface,
       interface_keyword_with_escape_sequence) {
  test_parse_and_visit_statement(
      u8"interface A {\n"_sv
      u8"  \\u{63}onstructor();}"_sv,
      no_diags, typescript_options);
}

// Regression test for https://github.com/quick-lint/quick-lint-js/issues/1108
TEST_F(Test_Parse_TypeScript_Interface,
       parser_is_left_in_consistent_state_after_parsing_interface) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"interface I {}\nlet a = \"",
        u8"                        ^^ Diag_Unclosed_String_Literal"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_variable_declaration",   // a
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Interface, override_is_not_allowed) {
  test_parse_and_visit_statement(
      u8"interface I { override method(); }"_sv,
      u8"              ^^^^^^^^ Diag_Override_Property_Not_Allowed_In_Interface"_diag,
      typescript_options);
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
