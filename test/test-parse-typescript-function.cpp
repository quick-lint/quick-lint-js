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
class test_parse_typescript_function : public test_parse_expression {};

TEST_F(test_parse_typescript_function,
       parameter_type_annotation_is_disallowed_in_javascript) {
  {
    test_parser p(u8"function f(p: T) { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"function f(p"), u8":")));
  }
}

TEST_F(test_parse_typescript_function,
       return_type_annotation_is_disallowed_in_javascript) {
  {
    test_parser p(u8"function f(): C { }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"function f()"), u8":")));
  }
}

TEST_F(test_parse_typescript_function, function_return_type_annotation) {
  {
    test_parser p(u8"function f(): C { }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       // f
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_function, arrow_return_type_annotation) {
  {
    test_parser p(u8"((param): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }

  {
    test_parser p(u8"((): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }

  {
    test_parser p(u8"(async (param): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }

  {
    test_parser p(u8"(async (): C => {})"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_function, object_method_return_type_annotation) {
  {
    test_parser p(u8"({ method(param): C {} })"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_function, class_method_return_type_annotation) {
  {
    test_parser p(u8"class C { method(param): C {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",          // C
                            "visit_enter_class_scope_body",     // {
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // param
                            "visit_variable_type_use",          // C
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope",        // }
                            "visit_exit_class_scope",           // }
                            "visit_variable_declaration"));     // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_function,
       interface_method_return_type_annotation) {
  {
    test_parser p(u8"interface I { method(param): C; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",    // I
                            "visit_enter_interface_scope",   // I
                            "visit_property_declaration",    // method
                            "visit_enter_function_scope",    // method
                            "visit_variable_declaration",    // param
                            "visit_variable_type_use",       // C
                            "visit_exit_function_scope",     // method
                            "visit_exit_interface_scope"));  // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C"));
  }
}

TEST_F(test_parse_typescript_function,
       generic_arrow_function_expression_body_can_use_in_operator) {
  {
    test_parser p(u8"<T,>() => x in y"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // T
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // x
                            "visit_variable_use",               // y
                            "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x", u8"y"));
  }
}

TEST_F(test_parse_typescript_function,
       non_null_assertion_in_parameter_list_is_an_error) {
  {
    test_parser p(u8"function f(param!) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       // f
                            "visit_variable_declaration",       // param
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_non_null_assertion_not_allowed_in_parameter,  //
                    bang, strlen(u8"function f(param"), u8"!")));
  }

  {
    test_parser p(u8"(param!) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       // f
                            "visit_variable_declaration",       // param
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_non_null_assertion_not_allowed_in_parameter,  //
                    bang, strlen(u8"(param"), u8"!")));
  }
}

TEST_F(test_parse_typescript_function,
       function_parameter_can_have_type_annotation) {
  {
    test_parser p(u8"function f(p1: A, p2: B = init) {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_variable_type_use",     // A
                                      "visit_variable_declaration",  // p1
                                      "visit_variable_use",          // init
                                      "visit_variable_type_use",     // B
                                      "visit_variable_declaration",  // p2
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"init", u8"B"));
  }

  {
    test_parser p(u8"function f([a, b]: C) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_variable_type_use",          // C
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST_F(test_parse_typescript_function,
       method_parameter_can_have_type_annotation) {
  {
    test_parser p(u8"class C { method(param: Type) {} }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  // {
                                      "visit_property_declaration",    // f
                                      "visit_enter_function_scope",    // f
                                      "visit_variable_type_use",       // Type
                                      "visit_variable_declaration",    // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope",        // }
                                      "visit_exit_class_scope",           // }
                                      "visit_variable_declaration"));     // C
  }

  {
    test_parser p(u8"({ method(param: Type) {} });"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  // f
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST_F(test_parse_typescript_function,
       arrow_parameter_can_have_type_annotation) {
  {
    test_parser p(u8"((param: Type) => {});"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"((p1: T1, {p2}: T2 = init, [p3]: T3) => {});"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // T1
                                      "visit_variable_declaration",  // p1
                                      "visit_variable_use",          // init
                                      "visit_variable_type_use",     // T2
                                      "visit_variable_declaration",  // p2
                                      "visit_variable_type_use",     // T3
                                      "visit_variable_declaration",  // p3
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST_F(test_parse_typescript_function,
       arrow_parameter_without_parens_cannot_have_type_annotation) {
  {
    test_parser p(u8"(param: Type => {});"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"("), u8"param: Type",        //
            type_colon, strlen(u8"(param"), u8":")));
  }

  {
    test_parser p(u8"(async param: Type => {});"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"(async "), u8"param: Type",  //
            type_colon, strlen(u8"(async param"), u8":")));
  }
}

TEST_F(test_parse_typescript_function, optional_expression) {
  {
    test_parser p(u8"(x?)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "paren(optional(var x))");
  }

  {
    test_parser p(u8"(x?)"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // x
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_unexpected_question_in_expression,  //
                              question, strlen(u8"(x"), u8"?")));
  }

  {
    test_parser p(u8"(x?: Type)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    ASSERT_THAT(summarize(ast), "paren(typed(optional(var x)))");

    auto* type_annotated =
        static_cast<expression::type_annotated*>(ast->without_paren());
    spy_visitor v;
    type_annotated->visit_type_annotation(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use"));  // Type
  }

  {
    test_parser p(u8"(x?, y)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "paren(binary(optional(var x), var y))");
  }

  {
    test_parser p(u8"(x?, other)"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",    // x
                                      "visit_variable_use"));  // other
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(diag_unexpected_question_in_expression)));
  }
}

TEST_F(test_parse_typescript_function, optional_parameter) {
  {
    test_parser p(u8"function f(param1?, param2?) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param1
                                      "visit_variable_declaration",  // param2
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"function f(param?: ParamType) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param1
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"(param?) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"(param1?, param2?) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param1
                                      "visit_variable_declaration",  // param2
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"(param?: ParamType) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"(param1?: Param1Type, param2?: Param2Type) => {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // Param1Type
                                      "visit_variable_declaration",  // param1
                                      "visit_variable_type_use",  // Param2Type
                                      "visit_variable_declaration",  // param2
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"(param1?: ParamType, param2?, param3?) => ReturnType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param1
                                      "visit_variable_declaration",  // param2
                                      "visit_variable_declaration",  // param3
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
  }
}

TEST_F(test_parse_typescript_function,
       optional_parameters_are_not_allowed_in_javascript) {
  {
    test_parser p(u8"(param?) => {}"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_optional_parameters_not_allowed_in_javascript,  //
            question, strlen(u8"(param"), u8"?")));
  }

  {
    test_parser p(u8"function f(param?) {}"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_typescript_optional_parameters_not_allowed_in_javascript,  //
            question, strlen(u8"function f(param"), u8"?")));
  }
}

TEST_F(test_parse_typescript_function,
       optional_parameters_cannot_have_initializers) {
  {
    test_parser p(u8"(param? = init) => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code,
                    diag_optional_parameter_cannot_have_initializer,  //
                    equal, strlen(u8"(param? "), u8"=",               //
                    question, strlen(u8"(param"), u8"?")));
  }

  {
    test_parser p(u8"function f(param? = init) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code,
                    diag_optional_parameter_cannot_have_initializer,  //
                    equal, strlen(u8"function f(param? "), u8"=",     //
                    question, strlen(u8"function f(param"), u8"?")));
  }
}

TEST_F(test_parse_typescript_function,
       optional_arrow_parameter_must_have_parentheses) {
  {
    test_parser p(u8"param? => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code,
                    diag_optional_arrow_parameter_requires_parentheses,  //
                    parameter_and_question, 0, u8"param?",               //
                    question, strlen(u8"param"), u8"?")));
  }

  {
    test_parser p(u8"async param? => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code,
                    diag_optional_arrow_parameter_requires_parentheses,      //
                    parameter_and_question, strlen(u8"async "), u8"param?",  //
                    question, strlen(u8"async param"), u8"?")));
  }
}

TEST_F(test_parse_typescript_function,
       optional_arrow_parameter_with_type_must_have_parentheses) {
  {
    // TODO(strager): Don't require surrounding parentheses for this diagnostic.
    test_parser p(u8"(param?: Type => {})"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_OFFSETS(
            p.code,
            diag_optional_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"("), u8"param?: Type",  //
            question, strlen(u8"(param"), u8"?",                        //
            type_colon, strlen(u8"(param?"), u8":")));
  }

  {
    test_parser p(u8"async param?: Type => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_OFFSETS(
            p.code,
            diag_optional_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"async "), u8"param?: Type",  //
            question, strlen(u8"async param"), u8"?",                        //
            type_colon, strlen(u8"async param?"), u8":")));
  }
}

TEST_F(test_parse_typescript_function, type_predicate) {
  {
    test_parser p(u8"function f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",         // f
                            "visit_enter_function_scope",         //
                            "visit_variable_declaration",         // param
                            "visit_variable_type_predicate_use",  // param
                            "visit_variable_type_use",            // SomeType
                            "visit_enter_function_scope_body",    // {
                            "visit_exit_function_scope"));        // }
  }

  {
    test_parser p(u8"(param): param is SomeType => {}"_sv, typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",         //
                            "visit_variable_declaration",         // param
                            "visit_variable_type_predicate_use",  // param
                            "visit_variable_type_use",            // SomeType
                            "visit_enter_function_scope_body",    // {
                            "visit_exit_function_scope"));        // }
  }

  {
    test_parser p(u8"<T>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",         //
                            "visit_variable_declaration",         // T
                            "visit_variable_declaration",         // param
                            "visit_variable_type_predicate_use",  // param
                            "visit_variable_type_use",            // SomeType
                            "visit_enter_function_scope_body",    // {
                            "visit_exit_function_scope"));        // }
  }

  {
    test_parser p(u8"<T, U>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",         //
                            "visit_variable_declaration",         // T
                            "visit_variable_declaration",         // U
                            "visit_variable_declaration",         // param
                            "visit_variable_type_predicate_use",  // param
                            "visit_variable_type_use",            // SomeType
                            "visit_enter_function_scope_body",    // {
                            "visit_exit_function_scope"));        // }
  }
}

TEST_F(test_parse_typescript_function, type_predicate_on_async_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    test_parser p(u8"async function f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"param", u8"SomeType"));
  }

  {
    test_parser p(u8"async <T>(param): param is SomeType => {}"_sv,
                  typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"param", u8"SomeType"));
  }
}

TEST_F(test_parse_typescript_function, type_predicate_on_generator_function) {
  // TODO(#856): Report a diagnostic for these cases.

  {
    test_parser p(u8"function *f(param): param is SomeType {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"param", u8"SomeType"));
  }
}

TEST_F(test_parse_typescript_function,
       type_predicate_parameter_name_can_be_contextual_keyword) {
  for (string8 parameter_name :
       keywords - disallowed_binding_identifier_keywords) {
    string8 code = u8"function f(" + parameter_name + u8"): " + parameter_name +
                   u8" is SomeType {}";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.visits,
        ElementsAre("visit_variable_declaration",         // f
                    "visit_enter_function_scope",         //
                    "visit_variable_declaration",         // (parameter_name)
                    "visit_variable_type_predicate_use",  // (parameter_name)
                    "visit_variable_type_use",            // SomeType
                    "visit_enter_function_scope_body",    // {
                    "visit_exit_function_scope"));        // }
  }
}

TEST_F(test_parse_typescript_function, function_overload_signatures) {
  {
    test_parser p(
        u8"function f();\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_exit_function_scope",        //
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"f")));
  }

  {
    // ASI
    test_parser p(
        u8"function f()\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_exit_function_scope",        //
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"f")));
  }

  {
    test_parser p(
        u8"function f();\n"_sv
        u8"function \\u{66}() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"f")));
  }

  {
    test_parser p(
        u8"function \\u{66}();\n"_sv
        u8"function f() {}"_sv,
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"f")));
  }
}

TEST_F(test_parse_typescript_function,
       function_overload_signature_with_wrong_name) {
  {
    test_parser p(
        u8"function f();\n"_sv
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_exit_function_scope",        //
                            "visit_variable_declaration",       // g
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope",        // }
                            "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"f"), function_decl(u8"g")));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_OFFSETS(
            p.code,
            diag_typescript_function_overload_signature_must_have_same_name,
            first_name, strlen(u8"function "), u8"f",                  //
            second_name, strlen(u8"function f();\nfunction "), u8"g",  //
            first_semicolon, strlen(u8"function f()"), u8";")));
  }

  {
    test_parser p(
        u8"function f()\n"_sv  // ASI
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_exit_function_scope",        //
                            "visit_variable_declaration",       // g
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope",        // }
                            "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"f"), function_decl(u8"g")));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body, expected_body,
                              strlen(u8"function f()"), u8"")))
        << "missing function body is more likely, so don't report "
           "diag_typescript_function_overload_signature_must_have_same_name";
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
