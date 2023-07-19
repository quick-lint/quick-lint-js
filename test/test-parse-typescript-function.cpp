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
class Test_Parse_TypeScript_Function : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Function,
       parameter_type_annotation_is_disallowed_in_javascript) {
  {
    Test_Parser p(u8"function f(p: T) { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript,  //
                type_colon, u8"function f(p"_sv.size(), u8":"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       return_type_annotation_is_disallowed_in_javascript) {
  {
    Test_Parser p(u8"function f(): C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript,  //
                type_colon, u8"function f()"_sv.size(), u8":"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, function_return_type_annotation) {
  {
    Test_Parser p(u8"function f(): C { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, arrow_return_type_annotation) {
  {
    Test_Parser p(u8"((param): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Test_Parser p(u8"((): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Test_Parser p(u8"(async (param): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Test_Parser p(u8"(async (): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // C
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
    Test_Parser p(u8"((): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"((): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"((param): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"((param): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"(<T>(param): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"(<T>(param): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
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
    Test_Parser p(u8"(async (): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"(async (): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"(async (param): ((returnParam) => ReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Test_Parser p(u8"(async (param): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
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
    Test_Parser p(u8"(async <T>(param): (returnParam) => ReturnType => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
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
    Test_Parser p(u8"(p: T) => {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript,  //
                type_colon, u8"(p"_sv.size(), u8":"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_with_complex_return_type_annotation_including_arrow) {
  {
    Test_Parser p(u8"((): (() => ReturnType) | Other => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_function_scope",        //
                              "visit_variable_type_use",          // Other
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType", u8"Other"}));
  }

  {
    Test_Parser p(u8"((): (() => ReturnType)[] => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_cannot_have_parenthesized_return_type_annotation) {
  {
    Test_Parser p(u8"((param): (number) => {})"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    // FIXME(strager): The above code is illegal TypeScript.
    //
    // Our parser currently interprets the above code as if '(param)' is a
    // parameter list and '(number) => {}' is the function's return type. It
    // then backtracks, thinking that ':' was bogus (E0254).
    //
    // We should parse intelligently and instead interpret '(param)' as a
    // parameter list, '(number)' as the return type and '{}' as the function's
    // body, and also produce a nice diagnostic.
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_TypeScript_Type_Annotation_In_Expression),
                }));
  }
}

// If a variable or function or method has a type annotation, and that type is
// an arrow type, then the type may be parenthesized or unparenthesized.
TEST_F(Test_Parse_TypeScript_Function,
       variable_or_function_with_function_return_type_annotation) {
  {
    Test_Parser p(u8"((param: (innerParam) => ParamReturnType) => {})"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"innerParam"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ParamReturnType"}));
  }

  {
    Test_Parser p(u8"function f(param): (returnParam) => ReturnType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv),
                                  func_param_decl(u8"param"_sv),
                                  func_type_param_decl(u8"returnParam"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, arrow_with_arrow_body) {
  {
    // This used to confuse the quick-lint-js parser.
    Test_Parser p(u8"() => (): ReturnType => myVariable"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ReturnType
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
    Test_Parser p(u8"({ method(param): C {} })"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // param
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, class_method_return_type_annotation) {
  {
    Test_Parser p(u8"class C { method(param): C {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // param
                              "visit_variable_type_use",          // C
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       interface_method_return_type_annotation) {
  {
    Test_Parser p(u8"interface I { method(param): C; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // method
                              "visit_enter_function_scope",   // method
                              "visit_variable_declaration",   // param
                              "visit_variable_type_use",      // C
                              "visit_exit_function_scope",    // method
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       generic_arrow_function_expression_body_can_use_in_operator) {
  {
    Test_Parser p(u8"<T,>() => x in y"_sv, typescript_options);
    p.parse_and_visit_statement();
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
    Test_Parser p(u8"function f(param!) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_Non_Null_Assertion_Not_Allowed_In_Parameter,  //
                        bang, u8"function f(param"_sv.size(), u8"!"_sv),
                }));
  }

  {
    Test_Parser p(u8"(param!) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_Non_Null_Assertion_Not_Allowed_In_Parameter,  //
                        bang, u8"(param"_sv.size(), u8"!"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       function_parameter_can_have_type_annotation) {
  {
    Test_Parser p(u8"function f(p1: A, p2: B = init) {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // A
                              "visit_variable_declaration",       // p1
                              "visit_variable_use",               // init
                              "visit_variable_type_use",          // B
                              "visit_variable_declaration",       // p2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"init", u8"B"}));
  }

  {
    Test_Parser p(u8"function f([a, b]: C) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // C
                              "visit_variable_declaration",       // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       method_parameter_can_have_type_annotation) {
  {
    Test_Parser p(u8"class C { method(param: Type) {} }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Test_Parser p(u8"({ method(param: Type) {} });"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_parameter_can_have_type_annotation) {
  {
    Test_Parser p(u8"((param: Type) => {});"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"((p1: T1, {p2}: T2 = init, [p3]: T3) => {});"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // T1
                              "visit_variable_declaration",       // p1
                              "visit_variable_use",               // init
                              "visit_variable_type_use",          // T2
                              "visit_variable_declaration",       // p2
                              "visit_variable_type_use",          // T3
                              "visit_variable_declaration",       // p3
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       arrow_parameter_without_parens_cannot_have_type_annotation) {
  {
    Test_Parser p(u8"(param: Type => {});"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses,  //
                parameter_and_annotation, u8"("_sv.size(), u8"param: Type"_sv,
                type_colon, u8"(param"_sv.size(), u8":"_sv),
        }));
  }

  {
    Test_Parser p(u8"(async param: Type => {});"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses,  //
                parameter_and_annotation, u8"(async "_sv.size(),
                u8"param: Type"_sv, type_colon, u8"(async param"_sv.size(),
                u8":"_sv),
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
    Test_Parser p(u8"(x?)"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              Diag_Unexpected_Question_In_Expression,  //
                              question, u8"(x"_sv.size(), u8"?"_sv),
        }));
  }

  {
    Test_Parser p(u8"(x?: Type)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_THAT(summarize(ast), "paren(typed(optional(var x)))");

    auto* type_annotated =
        static_cast<Expression::Type_Annotated*>(ast->without_paren());
    Spy_Visitor v;
    type_annotated->visit_type_annotation(v);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Test_Parser p(u8"(x?, y)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "paren(binary(optional(var x), var y))");
  }

  {
    Test_Parser p(u8"(x?, other)"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // other
                          }));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Unexpected_Question_In_Expression),
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, optional_parameter) {
  {
    Test_Parser p(u8"function f(param1?, param2?) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param1
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"function f(param?: ParamType) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ParamType
                              "visit_variable_declaration",       // param1
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"(param?) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"(param1?, param2?) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param1
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"(param?: ParamType) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ParamType
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"(param1?: Param1Type, param2?: Param2Type) => {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Param1Type
                              "visit_variable_declaration",       // param1
                              "visit_variable_type_use",          // Param2Type
                              "visit_variable_declaration",       // param2
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"async (param?: ParamType) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // ParamType
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, optional_parameter_in_function_type) {
  {
    Test_Parser p(u8"(param1?: ParamType, param2?, param3?) => ReturnType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_type_use",     // ParamType
                              "visit_variable_declaration",  // param1
                              "visit_variable_declaration",  // param2
                              "visit_variable_declaration",  // param3
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_required) {
  {
    Test_Parser p(u8"(param1?, param2) => ReturnType"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter,  //
                optional_parameter, u8"("_sv.size(), u8"param1?",
                required_parameter, u8"(param1?, "_sv.size(), u8"param2"),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameter_followed_by_required_type_annotated) {
  {
    Test_Parser p(u8"(param1?: number, param2: number) => ReturnType"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter,  //
                optional_parameter, u8"("_sv.size(), u8"param1?: number",
                required_parameter, u8"(param1?: number, "_sv.size(),
                u8"param2: number"),
        }));
  }

  {
    Test_Parser p(
        u8"(param1?: number, param2: number, param3: number) => ReturnType"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter,  //
                optional_parameter, u8"("_sv.size(), u8"param1?: number",
                required_parameter, u8"(param1?: number, "_sv.size(),
                u8"param2: number"),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameters_are_not_allowed_in_javascript) {
  {
    Test_Parser p(u8"(param?) => {}"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript,  //
                question, u8"(param"_sv.size(), u8"?"_sv),
        }));
  }

  {
    Test_Parser p(u8"function f(param?) {}"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript,  //
                question, u8"function f(param"_sv.size(), u8"?"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_parameters_cannot_have_initializers) {
  {
    Test_Parser p(u8"(param? = init) => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Optional_Parameter_Cannot_Have_Initializer,  //
                        equal, u8"(param? "_sv.size(), u8"="_sv, question,
                        u8"(param"_sv.size(), u8"?"_sv),
                }));
  }

  {
    Test_Parser p(u8"function f(param? = init) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Optional_Parameter_Cannot_Have_Initializer,  //
                        equal, u8"function f(param? "_sv.size(), u8"="_sv,
                        question, u8"function f(param"_sv.size(), u8"?"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_arrow_parameter_must_have_parentheses) {
  {
    Test_Parser p(u8"param? => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Optional_Arrow_Parameter_Requires_Parentheses,  //
                        parameter_and_question, 0, u8"param?"_sv, question,
                        u8"param"_sv.size(), u8"?"_sv),
                }));
  }

  {
    Test_Parser p(u8"async param? => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Optional_Arrow_Parameter_Requires_Parentheses,  //
                parameter_and_question, u8"async "_sv.size(), u8"param?"_sv,
                question, u8"async param"_sv.size(), u8"?"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       optional_arrow_parameter_with_type_must_have_parentheses) {
  {
    // TODO(strager): Don't require surrounding parentheses for this diagnostic.
    Test_Parser p(u8"(param?: Type => {})"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code,
                Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses,  //
                parameter_and_annotation, u8"("_sv.size(),
                u8"param?: Type"_sv,                       //
                question, u8"(param"_sv.size(), u8"?"_sv,  //
                type_colon, u8"(param?"_sv.size(), u8":"_sv),
        }));
  }

  {
    Test_Parser p(u8"async param?: Type => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_type_use",          // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code,
                Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses,  //
                parameter_and_annotation, u8"async "_sv.size(),
                u8"param?: Type"_sv,                            //
                question, u8"async param"_sv.size(), u8"?"_sv,  //
                type_colon, u8"async param?"_sv.size(), u8":"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate) {
  {
    Test_Parser p(u8"function f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // f
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // param
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  {
    Test_Parser p(u8"(param): param is SomeType => {}"_sv, typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // param
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  {
    Test_Parser p(u8"<T>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // T
                              "visit_variable_declaration",         // param
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  {
    Test_Parser p(u8"<T, U>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",         //
                              "visit_variable_declaration",         // T
                              "visit_variable_declaration",         // U
                              "visit_variable_declaration",         // param
                              "visit_variable_type_predicate_use",  // param
                              "visit_variable_type_use",            // SomeType
                              "visit_enter_function_scope_body",    // {
                              "visit_exit_function_scope",          // }
                          }));
  }

  // TODO(#881): Only allow 'this' within class and interface method signatures.
  {
    Test_Parser p(u8"class C { f(): this is Derived { return true; } }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // Derived
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Derived"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate_on_async_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    Test_Parser p(u8"async function f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }

  {
    Test_Parser p(u8"async <T>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"param", u8"SomeType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Function, type_predicate_on_generator_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    Test_Parser p(u8"function *f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
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
                    "visit_variable_declaration",         // f
                    "visit_enter_function_scope",         //
                    "visit_variable_declaration",         // (parameter_name)
                    "visit_variable_type_predicate_use",  // (parameter_name)
                    "visit_variable_type_use",            // SomeType
                    "visit_enter_function_scope_body",    // {
                    "visit_exit_function_scope",          // }
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Function, function_overload_signatures) {
  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    // ASI
    Test_Parser p(
        u8"function f()\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function \\u{66}() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  for (String8_View function_name : contextual_keywords) {
    Test_Parser p(concat(u8"function "_sv, function_name, u8"();\n"_sv,
                         u8"function "_sv, function_name, u8"() {}"_sv),
                  typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(function_name)}));
  }

  {
    Test_Parser p(
        u8"function \\u{66}();\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"async function f() { await(myPromise); }"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(
        u8"async function f();\n"_sv
        u8"function f() { await(myPromise); }"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await", u8"myPromise"}))
        << "'async' keyword should not apply to implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function *f() { yield(myValue); }"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}))
        << "'yield' should be a keyword in the implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"async function *f() { yield(myValue); await(myPromise); }"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue", u8"myPromise"}))
        << "both 'await' and 'yield' should be keywords in the implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       function_overload_signature_with_wrong_name) {
  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // g(
                              "visit_enter_function_scope_body",  // ){
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // g
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name, u8"function "_sv.size(), u8"f"_sv, function_name,
                u8"function f();\nfunction "_sv.size(), u8"g"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function h() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // g(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // h(
                              "visit_enter_function_scope_body",  // ){
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // g
                              "visit_variable_declaration",       // h
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        UnorderedElementsAre(function_decl(u8"f"_sv), function_decl(u8"g"_sv),
                             function_decl(u8"h"_sv)));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name, u8"function "_sv.size(), u8"f"_sv, function_name,
                u8"function f();\nfunction g();\nfunction "_sv.size(),
                u8"h"_sv),
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name, u8"function f();\nfunction "_sv.size(), u8"g"_sv,
                function_name,
                u8"function f();\nfunction g();\nfunction "_sv.size(),
                u8"h"_sv)));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function f() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name, u8"function f();\nfunction "_sv.size(), u8"g"_sv,
                function_name,
                u8"function f();\nfunction g();\nfunction "_sv.size(),
                u8"f"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name, u8"function "_sv.size(), u8"f"_sv, function_name,
                u8"function f();\nfunction g();\nfunction "_sv.size(),
                u8"g"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function f() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name,
                u8"function f();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function "_sv.size(),
                u8"g"_sv, function_name,
                u8"function f();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function g();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function "_sv.size(),
                u8"g"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function f()\n"_sv  // ASI
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // g
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Function_Body, expected_body,
                              u8"function f()"_sv.size(), u8""_sv),
        }))
        << "missing function body is more likely, so don't report "
           "Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name";
  }

  {
    Test_Parser p(
        u8"function f()\n"_sv  // ASI
        u8"async\n"_sv
        u8"function g() { await(myPromise); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // async
                              "visit_variable_declaration",       // g
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // await
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"async", u8"await", u8"myPromise"}))
        << "'async' should be a variable reference, not a keyword";
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Function_Body, expected_body,
                              u8"function f()"_sv.size(), u8""_sv),
        }))
        << "missing function body is more likely, so don't report "
           "Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name";
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       function_overload_signature_with_newline_after_async) {
  {
    // Normally, we would treat 'async' as a variable name here. However, we
    // know that there's a syntax error anyway (missing function body), so
    // Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword seems more
    // helpful.
    Test_Parser p(
        u8"function f()\n"_sv
        u8"async\n"_sv
        u8"function f() { await(myPromise); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword,
                async_keyword, u8"function f()\n"_sv.size(), u8"async"_sv,
                function_keyword, u8"function f()\nasync\n"_sv.size(),
                u8"function"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Function,
       function_overload_signature_declaration_cannot_have_generator_star) {
  {
    Test_Parser p(
        u8"function *f();"_sv
        u8"function *f() { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // myValue
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star,
                generator_star, u8"function "_sv.size(), u8"*"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function *f(a);"_sv
        u8"function *f(a, b);"_sv
        u8"function *f(...args) { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star,
                generator_star, u8"function "_sv.size(), u8"*"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star,
                generator_star, u8"function *f(a);function "_sv.size(),
                u8"*"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function *f(a);"_sv
        u8"function f(a, b);"_sv
        u8"function f(...args) { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield", u8"myValue"}))
        << "'yield' should not be a keyword in the implementation";
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star,
                generator_star, u8"function "_sv.size(), u8"*"_sv),
        }));
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
