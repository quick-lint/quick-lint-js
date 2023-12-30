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
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Function : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Function,
       parameter_type_annotation_is_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p: T) { }"_sv,  //
        u8"            ^ Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       return_type_annotation_is_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(): C { }"_sv,  //
        u8"            ^ Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, function_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(): C { }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, arrow_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((param): C => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((): C => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (param): C => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (): C => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

// If an arrow function has a return type, and that return type is an arrow
// type, then the return type may be parenthesized or unparenthesized.
TEST_F(Test_Parse_TypeScript_Function,
       arrow_with_function_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((): ((returnParam) => ReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((param): ((returnParam) => ReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((param): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(<T>(param): ((returnParam) => ReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(<T>(param): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    // NOTE(strager): There's a different code path for generic arrow functions
    // for TS and TSX modes.
    Test_Parser p(u8"(<T,>(param): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_jsx_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    // NOTE(strager): There's a different code path for generic arrow functions
    // for TS and TSX modes.
    Test_Parser p(u8"(<T,>(param): (returnParam) => ReturnType => {})"_sv,
                  typescript_jsx_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (): ((returnParam) => ReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (param): ((returnParam) => ReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async (param): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(
        u8"(async <T>(param): ((returnParam) => ReturnType) => {})"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async <T>(param): (returnParam) => ReturnType => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_with_parameter_type_annotation_is_disallowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(p: T) => {}"_sv,  //
        u8"  ^ Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_with_complex_return_type_annotation_including_arrow) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((): (() => ReturnType) | Other => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // =>
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_type_scope",            //
                              "visit_exit_function_scope",        //
                              "visit_variable_type_use",          // Other
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType", u8"Other"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((): (() => ReturnType)[] => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           //
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // =>
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_type_scope",            //
                              "visit_exit_function_scope",        //
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(
    Test_Parse_TypeScript_Function,
    arrow_cannot_have_parenthesized_return_type_annotation_which_might_be_a_parameter_list) {
  test_parse_and_visit_statement(
      u8"((param): (number) => {})"_sv,
      u8"Diag_TypeScript_Type_Annotation_In_Expression"_diag,
      typescript_options);
}

TEST_F(
    Test_Parse_TypeScript_Function,
    arrow_can_have_parenthesized_return_type_annotation_which_cannot_be_a_parameter_list) {
  {
    Spy_Visitor v = test_parse_and_visit_statement(
        u8"((param): (42) => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // =>
                              "visit_exit_function_scope",        //
                          }));
  }

  {
    Spy_Visitor v = test_parse_and_visit_statement(
        u8"((param): (number[]) => {})"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // =>
                              "visit_exit_function_scope",        //
                          }));
  }
}

// If a variable or function or method has a type annotation, and that type is
// an arrow type, then the type may be parenthesized or unparenthesized.
TEST_F(Test_Parse_TypeScript_Function,
       variable_or_function_with_function_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((param: (innerParam) => ParamReturnType) => {})"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"innerParam"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ParamReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param): (returnParam) => ReturnType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv),
                                  function_decl(u8"f"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, arrow_with_arrow_body) {
  {
    // This used to confuse the quick-lint-js parser.
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"() => (): ReturnType => myVariable"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // myVariable
                              "visit_exit_function_scope",        //
                              "visit_exit_function_scope",        //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ReturnType", u8"myVariable"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, object_method_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ method(param): C {} })"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, class_method_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(param): C {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       interface_method_return_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { method(param): C; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_enter_function_scope",   // method
                              "visit_variable_declaration",   // param
                              "visit_enter_type_scope",       // :
                              "visit_variable_type_use",      // C
                              "visit_exit_type_scope",        //
                              "visit_exit_function_scope",    // method
                              "visit_property_declaration",   // method
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       generic_arrow_function_expression_body_can_use_in_operator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<T,>() => x in y"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       non_null_assertion_in_parameter_list_is_an_error) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param!) {}"_sv,  //
        u8"                ^ Diag_Non_Null_Assertion_Not_Allowed_In_Parameter"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param!) => {}"_sv,                                              //
        u8"      ^ Diag_Non_Null_Assertion_Not_Allowed_In_Parameter"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       function_parameter_can_have_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p1: A, p2: B = init) {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // A
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // p1
                              "visit_variable_use",               // init
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // B
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // p2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"init", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f([a, b]: C) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // C
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       method_parameter_can_have_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(param: Type) {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ method(param: Type) {} });"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_parameter_can_have_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((param: Type) => {});"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((p1: T1, {p2}: T2 = init, [p3]: T3) => {});"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // T1
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // p1
                              "visit_variable_use",               // init
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // T2
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // p2
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // T3
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // p3
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_parameter_without_parens_cannot_have_type_annotation) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param: Type => {});"_sv,  //
        u8" ^^^^^^^^^^^ Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses.parameter_and_annotation\n"_diag
        u8"      ^ .type_colon"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async param: Type => {});"_sv,  //
        u8"       ^^^^^^^^^^^ Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses.parameter_and_annotation\n"_diag
        u8"            ^ .type_colon"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, optional_expression) {
  {
    Test_Parser p(u8"(x?)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "paren(optional(var x))");
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(x?)"_sv,                                          //
        u8"  ^ Diag_Unexpected_Question_In_Expression"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }

  {
    Test_Parser p(u8"(x?: Type)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_THAT(summarize(ast), "paren(typed(optional(var x)))");

    auto* type_annotated =
        expression_cast<Expression::Type_Annotated*>(ast->without_paren());
    Spy_Visitor v;
    type_annotated->visit_type_annotation(v);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // :
                              "visit_variable_type_use",  // Type
                              "visit_exit_type_scope",    //
                          }));
  }

  {
    Test_Parser p(u8"(x?, y)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "paren(binary(optional(var x), var y))");
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(x?, other)"_sv,                               //
        u8"Diag_Unexpected_Question_In_Expression"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // other
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, optional_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param1?, param2?) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param1
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param?: ParamType) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ParamType
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param1
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }

  for (String8_View parameter_name : contextual_keywords) {
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(u8"function f("_sv, parameter_name, u8"?) {}"_sv), no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations, ElementsAreArray({
                                             func_param_decl(parameter_name),
                                             function_decl(u8"f"_sv),
                                         }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param?) => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param1?, param2?) => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param1
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param?: ParamType) => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ParamType
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param1?: Param1Type, param2?: Param2Type) => {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Param1Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param1
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Param2Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async (param?: ParamType) => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ParamType
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       required_parameter_followed_by_optional_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1, param2?) {}"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function, optional_parameter_in_function_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(param1?: ParamType, param2?, param3?) => ReturnType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // ParamType
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // param1
                              "visit_variable_declaration",  // param2
                              "visit_variable_declaration",  // param3
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_rest_parameter_is_allowed) {
  test_parse_and_visit_typescript_type_expression(
      u8"(param1?, ...rest) => ReturnType"_sv, no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       parameter_with_default_followed_by_rest_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1 = null, ...rest) {}"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_required) {
  test_parse_and_visit_typescript_type_expression(
      u8"(param1?, param2) => ReturnType"_sv,  //
      u8" ^^^^^^^ Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter.optional_parameter\n"_diag
      u8"          ^^^^^^ .required_parameter"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_required_type_annotated) {
  test_parse_and_visit_typescript_type_expression(
      u8"(param1?: number, param2: number) => ReturnType"_sv,  //
      u8" ^^^^^^^^^^^^^^^ Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter.optional_parameter\n"_diag
      u8"                  ^^^^^^^^^^^^^^ .required_parameter"_diag,  //
      typescript_options);

  test_parse_and_visit_typescript_type_expression(
      u8"(param1?: number, param2: number, param3: number) => ReturnType"_sv,  //
      u8" ^^^^^^^^^^^^^^^ Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter.optional_parameter\n"_diag
      u8"                  ^^^^^^^^^^^^^^ .required_parameter"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       parameter_with_default_followed_by_normal_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1 = null, param2) {}"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_parameter_with_default_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1?, param2 = null) {}"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       parameter_with_default_followed_by_optional_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1 = null, param2?) {}"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameters_are_not_allowed_in_javascript) {
  test_parse_and_visit_statement(
      u8"(param?) => {}"_sv,  //
      u8"      ^ Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);

  test_parse_and_visit_statement(
      u8"function f(param?) {}"_sv,  //
      u8"                ^ Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameters_cannot_have_initializers) {
  test_parse_and_visit_statement(
      u8"(param? = init) => {}"_sv,  //
      u8"        ^ Diag_Optional_Parameter_Cannot_Have_Initializer.equal\n"_diag
      u8"      ^ .question"_diag,  //
      typescript_options);

  test_parse_and_visit_statement(
      u8"function f(param? = init) {}"_sv,  //
      u8"                  ^ Diag_Optional_Parameter_Cannot_Have_Initializer.equal\n"_diag
      u8"                ^ .question"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_arrow_parameter_must_have_parentheses) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"param? => {}"_sv,  //
        u8"^^^^^^ Diag_Optional_Arrow_Parameter_Requires_Parentheses.parameter_and_question\n"_diag
        u8"     ^ .question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async param? => {}"_sv,  //
        u8"      ^^^^^^ Diag_Optional_Arrow_Parameter_Requires_Parentheses.parameter_and_question\n"_diag
        u8"           ^ .question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_arrow_parameter_with_type_must_have_parentheses) {
  {
    // TODO(strager): Don't require surrounding parentheses for this diagnostic.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(param?: Type => {})"_sv,  //
        u8" ^^^^^^^^^^^^ Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses.parameter_and_annotation\n"_diag
        u8"      ^ .question\n"_diag
        u8"       ^ .type_colon"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async param?: Type => {}"_sv,  //
        u8"      ^^^^^^^^^^^^ Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses.parameter_and_annotation\n"_diag
        u8"           ^ .question\n"_diag
        u8"            ^ .type_colon"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Type
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param): param is SomeType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // param
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_exit_type_scope",              //
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                              "visit_variable_declaration",         // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(param): param is SomeType => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // param
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_exit_type_scope",              //
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"<T>(param): param is SomeType => {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // T
                              "visit_variable_declaration",         // param
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_exit_type_scope",              //
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"<T, U>(param): param is SomeType => {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // T
                              "visit_variable_declaration",         // U
                              "visit_variable_declaration",         // param
                              "visit_enter_type_scope",             // :
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_exit_type_scope",              //
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  // TODO(#881): Only allow 'this' within class and interface method signatures.
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(): this is Derived { return true; } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // f
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // Derived
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Derived"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate_on_async_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f(param): param is SomeType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"async <T>(param): param is SomeType => {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate_on_generator_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function *f(param): param is SomeType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       type_predicate_is_only_allowed_as_return_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p, q: p is SomeType) {}"_sv,  //
        u8"                   ^^ Diag_TypeScript_Type_Predicate_Only_Allowed_As_Return_Type"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // p
                              "visit_enter_type_scope",           // :
                              "visit_variable_use",               // p
                              "visit_variable_type_use",          // SomeType
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // q
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p", u8"SomeType"}));
  }

  {
    test_parse_and_visit_statement(
        u8"var x: p is T;"_sv,  //
        u8"         ^^ Diag_TypeScript_Type_Predicate_Only_Allowed_As_Return_Type"_diag,
        typescript_options);
  }

  {
    test_parse_and_visit_statement(
        u8"function f(p): p is p is T {}"_sv,  //
        u8"                      ^^ Diag_TypeScript_Type_Predicate_Only_Allowed_As_Return_Type"_diag,
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate_in_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(param) => param is SomeType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // param
                              "visit_enter_type_scope",             // =>
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_exit_type_scope",              //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       type_predicate_parameter_name_can_be_contextual_keyword) {
  for (String8_View parameter_name :
       keywords - disallowed_binding_identifier_keywords) {
    Test_Parser p(concat(u8"function f("_sv, parameter_name, u8"): "_sv,
                         parameter_name, u8" is SomeType {}"_sv),
                  typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_function_scope",         //
                    "visit_variable_declaration",         // (parameter_name)
                    "visit_enter_type_scope",             // :
                    "visit_variable_type_predicate_use",  // (parameter_name)
                    "visit_variable_type_use",            // SomeType
                    "visit_exit_type_scope",              //
                    "visit_enter_function_scope_body",    // {
                    "visit_exit_function_scope",          // }
                    "visit_variable_declaration",         // f
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       newline_not_allowed_before_is_in_type_predicate) {
  test_parse_and_visit_expression(
      u8"(p): p\nis string => {}"_sv,  //
      u8"        ^^ Diag_Newline_Not_Allowed_Before_Is_In_Assertion_Signature.is_keyword"_diag,
      typescript_options);

  // TODO(strager): Report
  // Diag_Newline_Not_Allowed_Before_Is_In_Assertion_Signature instead.
  test_parse_and_visit_statement(
      u8"function f(param): asserts\nparam {}"_sv,  //
      u8"Diag_Missing_Function_Body"_diag, typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I {\n"_sv
        u8"  f(p): p\n"_sv  // ASI
        u8"  is\n"_sv       // ASI
        u8"  string;\n"_sv
        u8"}\n"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"f"_sv, u8"is"_sv, u8"string"_sv}));
  }
}
TEST_F(Test_Parse_TypeScript_Function, boolean_assertion_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param): asserts param {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_function_scope",              //
                    "visit_variable_declaration",              // param
                    "visit_enter_type_scope",                  // :
                    "visit_variable_assertion_signature_use",  // param
                    "visit_exit_type_scope",                   //
                    "visit_enter_function_scope_body",         // {
                    "visit_exit_function_scope",               // }
                    "visit_variable_declaration",              // f
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(p): asserts p => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",              //
                              "visit_variable_declaration",              // p
                              "visit_enter_type_scope",                  // :
                              "visit_variable_assertion_signature_use",  // p
                              "visit_exit_type_scope",                   //
                              "visit_enter_function_scope_body",         // {
                              "visit_exit_function_scope",               // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, assertion_signature_with_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(param): asserts param is MyType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_function_scope",              //
                    "visit_variable_declaration",              // param
                    "visit_enter_type_scope",                  // :
                    "visit_variable_assertion_signature_use",  // param
                    "visit_variable_type_use",                 // MyType
                    "visit_exit_type_scope",                   //
                    "visit_enter_function_scope_body",         // {
                    "visit_exit_function_scope",               // }
                    "visit_variable_declaration",              // f
                }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"param"_sv, u8"MyType"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p): asserts p is MyType|MyOtherType {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv, u8"MyType"_sv,
                                                   u8"MyOtherType"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p): asserts p is readonly T[] {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv, u8"T"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, assertion_signature_with_funny_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p): asserts p is is {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv, u8"is"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, assertion_signature_can_assert_this) {
  // TODO(#881): Only allow 'this' within class and interface method signatures.
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(): asserts this {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(): asserts this is SomeType {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"SomeType"_sv}));
  }
}

TEST_F(
    Test_Parse_TypeScript_Function,
    assertion_signature_can_assert_parameter_named_contextual_keyword_except_is) {
  for (String8_View keyword :
       contextual_keywords - Dirty_Set<String8>{u8"is"}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { f("_sv, keyword, u8"): asserts "_sv, keyword,
                 u8" {} }"_sv),
          no_diags, typescript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { f("_sv, keyword, u8"): asserts "_sv, keyword,
                 u8" is string {} }"_sv),
          no_diags, typescript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       assertion_signature_is_only_allowed_as_return_types) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p, q: asserts p) {}"_sv,  //
        u8"                 ^^^^^^^ Diag_TypeScript_Assertion_Signature_Only_Allowed_As_Return_Types"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // p
                              "visit_enter_type_scope",           // :
                              "visit_variable_use",               // p
                              "visit_exit_type_scope",            //
                              "visit_variable_declaration",       // q
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let x: asserts x is string;"_sv,  //
        u8"       ^^^^^^^ Diag_TypeScript_Assertion_Signature_Only_Allowed_As_Return_Types"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(p): asserts p is (asserts u) {}"_sv,  //
        u8"                             ^^^^^^^ Diag_TypeScript_Assertion_Signature_Only_Allowed_As_Return_Types"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"p"_sv, u8"u"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       newline_not_allowed_after_asserts_in_assertion_signature) {
  test_parse_and_visit_expression(
      u8"(p): asserts\np => {}"_sv,  //
      u8"     ^^^^^^^ Diag_Newline_Not_Allowed_After_Asserts_In_Assertion_Signature.asserts_keyword"_diag,
      typescript_options);

  // TODO(strager): Report
  // Diag_Newline_Not_Allowed_After_Asserts_In_Assertion_Signature instead.
  test_parse_and_visit_statement(
      u8"function f(param): asserts\nparam {}"_sv,  //
      u8"Diag_Missing_Function_Body"_diag, typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I {\n"_sv
        u8"  f(p): asserts\n"_sv  // ASI
        u8"  p;\n"_sv
        u8"}\n"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"f"_sv, u8"p"_sv}));
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
