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

using ::testing::ElementsAreArray;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Class : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_is_not_allowed_in_javascript) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare class C {}"_sv,                                      //
      u8"^^^^^^^ Diag_Declare_Class_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_abstract_class_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare abstract class C {}"_sv,  //
        u8"^^^^^^^ Diag_Declare_Abstract_Class_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class, declare_empty_class) {
  Spy_Visitor p = test_parse_and_visit_module(u8"declare class C {}"_sv,
                                              no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class, declare_empty_abstract_class) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare abstract class C {}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_before_class_keyword_triggers_asi) {
  Spy_Visitor p = test_parse_and_visit_module(u8"declare\nclass C {}"_sv,
                                              no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",            // declare
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_before_abstract_keyword_triggers_asi) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare\nabstract class C {}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",            // declare
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(
    Test_Parse_TypeScript_Declare_Class,
    newline_is_not_allowed_between_abstract_and_class_keyword_in_declare_abstract_class) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare abstract\nclass C {}"_sv,  //
      u8"        ^^^^^^^^ Diag_Newline_Not_Allowed_After_Abstract_Keyword"_diag,  //
      typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_can_extend_and_implement) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare class C extends B implements I {}"_sv, no_diags,
      typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_variable_use",            // B
                            "visit_variable_type_use",       // I
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"B"_sv, u8"I"_sv}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class, declare_class_can_be_generic) {
  Spy_Visitor p = test_parse_and_visit_module(u8"declare class C<T> {}"_sv,
                                              no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_variable_declaration",    // T
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(
      p.variable_declarations,
      ElementsAreArray({generic_param_decl(u8"T"_sv), class_decl(u8"C"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_can_contain_empty_static_block) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare class C { static { } }"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",     //
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_enter_block_scope",       // static {
                            "visit_exit_block_scope",        // }
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_exit_declare_scope",      //
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_cannot_contain_statements_in_static_block) {
  for (String8_View static_block_body : {
           u8"console.log('hello');",
           u8"{}",
       }) {
    Padded_String code(concat(u8"declare class C { static { "_sv,
                              static_block_body, u8" } }"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_module();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                  ^^^^^^ Diag_TypeScript_Declare_Class_Cannot_Contain_Static_Block_Statement"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_can_have_method_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare class C { myMethod(); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_property_declaration",    //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare class C { get myProperty(): number; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // myProperty
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_exit_function_scope",     // myProperty
                              "visit_property_declaration",    //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare class C { set myProperty(value: number): void; }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_enter_function_scope",    // myProperty
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_variable_declaration",    // value
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_exit_function_scope",     // myProperty
                              "visit_property_declaration",    //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_methods_cannot_contain_bodies) {
  test_parse_and_visit_module(
      u8"declare class C { myMethod() {} }"_sv,  //
      u8"                             ^ Diag_Declare_Class_Methods_Cannot_Contain_Bodies"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_methods_must_be_semicolon_terminated) {
  test_parse_and_visit_module(
      u8"declare class C { myMethod() myOtherMethod() }"_sv,  //
      u8"                            ` Diag_Missing_Semicolon_After_Declare_Class_Method"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       non_abstract_declare_class_properties_cannot_be_abstract) {
  {
    Test_Parser p(u8"declare class C { abstract myMethod(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Should this be a different message? They can just drop the
    // 'abstract' keyword.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                  ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class.abstract_keyword\n"_diag
            u8"        ^^^^^ .class_keyword"_diag,
        });
  }

  {
    Test_Parser p(u8"declare class C { abstract myField: any; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Should this be a different message? They can just drop the
    // 'abstract' keyword.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                  ^^^^^^^^ Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class.abstract_keyword\n"_diag
            u8"        ^^^^^ .class_keyword"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       abstract_declare_class_properties_can_be_abstract) {
  test_parse_and_visit_module(
      u8"declare abstract class C { abstract myMethod(); }"_sv, no_diags,
      typescript_options);

  test_parse_and_visit_module(
      u8"declare abstract class C { abstract myField: any; }"_sv, no_diags,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_can_have_field_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare class C { myField; myOtherField: any; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField
                              "visit_enter_type_scope",        // :
                              "visit_exit_type_scope",         //
                              "visit_property_declaration",    // myOtherField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }

  test_parse_and_visit_module(u8"declare class C { 'myField'; }"_sv, no_diags,
                              typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_cannot_have_field_with_initializer) {
  test_parse_and_visit_module(
      u8"declare class C { myField = 42; }"_sv,  //
      u8"                          ^ Diag_Declare_Class_Fields_Cannot_Have_Initializers"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_cannot_have_assignment_asserted_field) {
  test_parse_and_visit_module(
      u8"declare class C { myField!: any; }"_sv,  //
      u8"                         ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class"_diag,  //
      typescript_options);

  // Diag_Declare_Class_Fields_Cannot_Have_Initializers should not also be
  // reported.
  test_parse_and_visit_module(
      u8"declare class C { myField!: any = init; }"_sv,  //
      u8"                         ^ Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_properties_can_have_access_specifiers) {
  for (String8_View keyword :
       {u8"private"_sv, u8"protected"_sv, u8"public"_sv}) {
    test_parse_and_visit_module(
        concat(u8"declare class C { "_sv, keyword, u8" myField; }"_sv),
        no_diags, typescript_options);

    test_parse_and_visit_module(
        concat(u8"declare class C { "_sv, keyword, u8" myMethod(); }"_sv),
        no_diags, typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_properties_can_be_optional) {
  test_parse_and_visit_module(u8"declare class C { myField?: any; }"_sv,
                              no_diags, typescript_options);

  test_parse_and_visit_module(u8"declare class C { myMethod?(); }"_sv, no_diags,
                              typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_properties_can_be_private) {
  test_parse_and_visit_module(u8"declare class C { #myField; }"_sv, no_diags,
                              typescript_options);

  test_parse_and_visit_module(u8"declare class C { #myMethod(); }"_sv, no_diags,
                              typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_properties_can_be_static) {
  test_parse_and_visit_module(u8"declare class C { static myField; }"_sv,
                              no_diags, typescript_options);

  test_parse_and_visit_module(u8"declare class C { static myMethod(); }"_sv,
                              no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_methods_cannot_be_async_or_generator) {
  test_parse_and_visit_module(
      u8"declare class C { async myMethod(); }"_sv,  //
      u8"                  ^^^^^ Diag_Declare_Class_Methods_Cannot_Be_Async"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare class C { *myMethod(); }"_sv,  //
      u8"                  ^ Diag_Declare_Class_Methods_Cannot_Be_Generators"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare class C { async *myMethod(); }"_sv,             //
      u8"Diag_Declare_Class_Methods_Cannot_Be_Generators"_diag,  //
      u8"Diag_Declare_Class_Methods_Cannot_Be_Async"_diag,       //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       declare_class_can_have_index_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare class C { [key: KeyType]: ValueType; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",          //
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
                              "visit_exit_declare_scope",           //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"KeyType", u8"ValueType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({index_signature_param_decl(u8"key"_sv),
                                  class_decl(u8"C"_sv)}));
  }

  test_parse_and_visit_statement(
      u8"declare class C { static [key: KeyType]: ValueType; }"_sv, no_diags,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       call_signatures_are_disallowed_in_declare_classes) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare class C { (): any; }"_sv,                          //
        u8"                  ` Diag_Missing_Class_Method_Name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_declare_scope",     //
                    "visit_enter_class_scope",       // C
                    "visit_enter_class_scope_body",  //
                    "visit_enter_function_scope",    // (call signature)
                    "visit_enter_type_scope",        // :
                    "visit_exit_type_scope",         //
                    "visit_exit_function_scope",     // (call signature)
                    "visit_property_declaration",    // (call signature)
                    "visit_exit_class_scope",        // C
                    "visit_variable_declaration",    // C
                    "visit_exit_declare_scope",      //
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Class,
       parameter_property_is_not_allowed_in_declare_class) {
  for (String8_View keyword :
       {u8"readonly"_sv, u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      Test_Parser p(concat(u8"declare class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" field);\n"_sv
                           u8"}"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.legacy_errors(),
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  Diag_TypeScript_Parameter_Property_Not_Allowed_In_Declare_Class,  //
                  property_keyword,
                  u8"declare class C {\n  constructor("_sv.size(), keyword),
          }));
    }
  }

  for (String8_View keyword :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      Test_Parser p(concat(u8"declare class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" readonly field);\n"_sv
                           u8"}"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.legacy_errors(),
          ElementsAreArray({
              DIAG_TYPE_2_OFFSETS(
                  p.code,
                  Diag_TypeScript_Parameter_Property_Not_Allowed_In_Declare_Class,  //
                  property_keyword,
                  u8"declare class C {\n  constructor("_sv.size(), keyword,  //
                  declare_keyword, 0, u8"declare"_sv),
          }))
          << "only '" << out_string8(keyword)
          << "' should report a diagnostic; 'readonly' should not have its own "
             "diagnostic";
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
