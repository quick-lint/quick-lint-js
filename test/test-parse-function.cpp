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
class Test_Parse_Function : public Test_Parse_Expression {};

TEST_F(Test_Parse_Function,
       parse_function_parameters_with_object_destructuring) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f({x, y, z}) {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 4);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"z");
    EXPECT_EQ(p.variable_declarations[3].name, u8"f");
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"({x, y, z}) => {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"z");
  }
}

TEST_F(Test_Parse_Function, parse_function_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function foo() {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"foo"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function sin(theta) {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"theta"_sv),
                                  function_decl(u8"sin"_sv)}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // theta
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // sin
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function pow(base, exponent) {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"base");
    EXPECT_EQ(p.variable_declarations[1].name, u8"exponent");
    EXPECT_EQ(p.variable_declarations[2].name, u8"pow");

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // base
                              "visit_variable_declaration",       // exponent
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // pow
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(x, y = x) {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"f");

    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_variable_use",               // x
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f() { return x; }"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");

    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function g(first, ...args) {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"first"_sv),
                                  func_param_decl(u8"args"_sv),
                                  function_decl(u8"g"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function foo() { return x++,y }"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"foo");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }
}

TEST_F(Test_Parse_Function,
       parameter_with_default_followed_by_rest_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1 = null, ...rest) {}"_sv,
                                 no_diags, javascript_options);
}

TEST_F(Test_Parse_Function,
       parameter_with_default_followed_by_normal_parameter_is_allowed) {
  test_parse_and_visit_statement(u8"function f(param1 = null, param2) {}"_sv,
                                 no_diags, javascript_options);
}

TEST_F(Test_Parse_Function, function_with_arrow_operator) {
  test_parse_and_visit_statement(
      u8"function f() => {}"_sv,  //
      u8"             ^^ Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator"_diag);
}

TEST_F(Test_Parse_Function, function_statement_with_no_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function() {x;}"_sv,  //
        u8"        ` Diag_Missing_Name_In_Function_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function  () {x;}"_sv,  //
        u8"                ` Diag_Missing_Name_In_Function_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function(x) {y;}(z)"_sv,  //
        u8"      ^^^^^^^^^ Diag_Missing_Name_Or_Parentheses_For_Function.where\n"_diag
        u8"^^^^^^^^^^^^^^^^^^^^^^ .function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // z
                          }));
  }
}

TEST_F(Test_Parse_Function, async_function_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() {}"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() { await null; }"_sv, no_diags,
        javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
  }
}

TEST_F(Test_Parse_Function, async_keyword_order_diagnostic) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export async function f() { await myPromise; };"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async export function f() { await myPromise; };"_sv,  //
        u8"^^^^^^^^^^^^ Diag_Async_Export_Function"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() {};"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
  }

  test_parse_and_visit_statement(u8"function async f() {};"_sv,  //
                                 u8"Diag_Function_Async_Function"_diag);
}

TEST_F(Test_Parse_Function,
       async_function_cannot_have_newline_after_async_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"async\nfunction f() { await myPromise; }"_sv,  //
        u8"Diag_Await_Operator_Outside_Async"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // async
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"async", u8"myPromise"}));
  }
}

TEST_F(Test_Parse_Function, let_async_async_newline_export_is_valid) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let async;\nasync\nexport function f() { }"_sv, no_diags);
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_variable_declaration",       // let async
                    "visit_variable_use",               // async
                    "visit_enter_function_scope",       //
                    "visit_enter_function_scope_body",  //
                    "visit_exit_function_scope",        //
                    "visit_variable_declaration",       // export function f
                    "visit_end_of_module",
                }));
  }
}

TEST_F(Test_Parse_Function, generator_function_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function* f() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_Function, await_in_async_function) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() { await myPromise; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async () => { await myPromise; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(async function() { await myPromise; })"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ async f() { await myPromise; } })"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ async *f() { await myPromise; } })"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async f() { await myPromise; } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async *f() { await myPromise; } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() {\n"
        u8"  function g() {}\n"
        u8"  await myPromise;\n"
        u8"}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }
}

TEST_F(Test_Parse_Function, await_asi_in_async_function) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f() { await a\nawait b }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"a",  //
                                                   u8"b"}));
  }
}

TEST_F(Test_Parse_Function, yield_in_generator_function) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function *f() { yield myValue; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function*() { yield myValue; })"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ *f() { yield myValue; } })"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"({ async *f() { yield myValue; } })"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *f() { yield myValue; } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function* f() {\n"
        u8"  function g() {}\n"
        u8"  yield myValue;\n"
        u8"}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }
}

TEST_F(Test_Parse_Function, parse_function_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function() {});"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function(x, y) {});"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function() {let x = y;});"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_variable_declaration",       // x
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(a, function(b) {c;}(d));"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // c
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // a
                              "visit_variable_use",               // d
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"b"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"c", u8"a", u8"d"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function recur() { recur(); })();"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // recur
                              "visit_enter_function_scope_body",   //
                              "visit_variable_use",                // recur
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAreArray({u8"recur"}));
  }
}

TEST_F(Test_Parse_Function, arrow_function_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"(() => x);"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"(x => y);"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((x = y) => z);"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_use",               // y
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // z
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y", u8"z"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async (x) => y;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async (x) => y, z;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                              "visit_variable_use",  // z
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async x => y;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_Function, arrow_function_expression_with_statements) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(() => { x; });"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(x => { y; });"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }
}

TEST_F(Test_Parse_Function, nested_arrow_function) {
  for (String8_View code : {
           u8"(x => y => (x, y));"_sv,
           u8"(x => { (y => (x, y)); });"_sv,
           u8"(x => y => { x; y; });"_sv,
           u8"(x => { (y => { x; y; }); });"_sv,
       }) {
    Spy_Visitor p =
        test_parse_and_visit_statement(code, no_diags, javascript_options);
    SCOPED_TRACE(out_string8(code));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {arrow_param_decl(u8"x"_sv), arrow_param_decl(u8"y"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(a => ((b = a) => {}));"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_enter_function_scope_body",  //
                              "visit_enter_function_scope",       //
                              "visit_variable_use",               // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_Function, empty_parens_parameter_is_an_error) {
  test_parse_and_visit_module(
      u8"function f(()) {}"_sv,  //
      u8"           ^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren"_diag);

  test_parse_and_visit_module(
      u8"let f = (()) => {};"_sv,  //
      u8"         ^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren"_diag);
}

TEST_F(Test_Parse_Function,
       function_statements_allow_trailing_commas_in_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(x,) { y; });"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }
}

TEST_F(Test_Parse_Function,
       arrow_functions_allow_trailing_commas_in_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"((x,) => { y; });"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_Function,
       function_statement_without_name_or_parameter_list_or_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"{ function } x = y;"_sv,  //
        u8"  ^^^^^^^^ Diag_Missing_Name_In_Function_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",    //
                              "visit_exit_block_scope",     //
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, function_statement_without_parameter_list_or_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"{ function f } x = y;"_sv,  //
        u8"            ` Diag_Missing_Function_Parameter_List"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_declaration",  // f
                              "visit_exit_block_scope",      //
                              "visit_variable_use",          // y
                              "visit_variable_assignment",   // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f\n3 * x;"_sv,  //
        u8"          ` Diag_Missing_Function_Parameter_List"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_declaration",  // f
                              "visit_variable_use",          // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f, x;"_sv,                                      //
        u8"          ` Diag_Missing_Function_Parameter_List"_diag,  //
        u8"          ^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_declaration",  // f
                              "visit_variable_use",          // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f.x() {}"_sv,                                         //
        u8"              ` Diag_Missing_Semicolon_After_Statement"_diag,  //
        u8"          ` Diag_Missing_Function_Parameter_List"_diag,        //
        u8"          ^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_declaration",  // f
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, named_function_statement_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f()\nf;"_sv,  //
        u8"            ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(x)"_sv,  //
        u8"             ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // x
                              "visit_exit_function_scope",   //
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    // This should not be interpreted as a TypeScript overloaded function.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f(x) function f(y) {}"_sv,               //
        u8"             ` Diag_Missing_Function_Body"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class f { m() }"_sv,  //
        u8"             ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_property_declaration",    // m
                              "visit_exit_class_scope",        //
                              "visit_variable_declaration",    // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class f { m(x) }"_sv,  //
        u8"              ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_enter_function_scope",    //
                              "visit_variable_declaration",    // x
                              "visit_exit_function_scope",     //
                              "visit_property_declaration",    // m
                              "visit_exit_class_scope",        //
                              "visit_variable_declaration",    // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default function f()"_sv,  //
        u8"                           ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function* f()"_sv,  //
        u8"             ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                              "visit_variable_declaration",  // f
                          }));
  }
}

TEST_F(Test_Parse_Function, unnamed_function_statement_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function* ()"_sv,                                //
        u8"            ` Diag_Missing_Function_Body"_diag,  //
        u8"          ` Diag_Missing_Name_In_Function_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function()"_sv,                                //
        u8"          ` Diag_Missing_Function_Body"_diag,  //
        u8"        ` Diag_Missing_Name_In_Function_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default function()"_sv,  //
        u8"                         ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                          }));
  }
}

TEST_F(Test_Parse_Function, named_function_expression_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function f())"_sv,  //
        u8"             ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  //
                              "visit_exit_function_scope",         //
                          }));
  }
}

TEST_F(Test_Parse_Function, unnamed_function_expression_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(function())"_sv,  //
        u8"           ` Diag_Missing_Function_Body"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",   //
                          }));
  }
}

TEST_F(Test_Parse_Function, arrow_function_invoked_with_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"(() => {})()"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, async_arrow_function_invoked_with_parens) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"(async () => {})()"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, arrow_function_invoked_no_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"() => {}()"_sv,  //
        u8"` Diag_Missing_Parentheses_Around_Self_Invoked_Function.func_start\n"_diag
        u8"        ^ .invocation"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, async_arrow_function_invoked_no_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"async () => {}()"_sv,  //
        u8"` Diag_Missing_Parentheses_Around_Self_Invoked_Function.func_start\n"_diag
        u8"              ^ .invocation"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function,
       arrow_function_with_parens_on_separate_line_triggers_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"() => {} \n (x);"_sv,  //
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // =>
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // x
                              "visit_end_of_module",              //
                          }));
  }
}

TEST_F(Test_Parse_Function, arrow_function_without_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"=> x + y"_sv,  //
        u8"^^ Diag_Missing_Arrow_Function_Parameter_List"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, function_with_invalid_parameters) {
  for (String8_View parameter_list : {
           u8"x << y"_sv,
           u8"x.prop"_sv,
           u8"html`<strong>hello</strong>`"_sv,
       }) {
    test_parse_and_visit_statement(
        concat(u8"function f("_sv, parameter_list, u8") {}"_sv),
        u8"Diag_Invalid_Parameter"_diag);
  }

  test_parse_and_visit_statement(
      u8"function f(42) {}"_sv,  //
      u8"Diag_Unexpected_Literal_In_Parameter_List"_diag);
}

TEST_F(Test_Parse_Function, arrow_function_with_invalid_parameters) {
  for (String8_View parameter_list : {
           u8"(new C())"_sv,
           u8"(class{})"_sv,
           u8"(typeof x)"_sv,
           u8"(() => {})"_sv,
           u8"(() => null)"_sv,
           u8"(x ?\x3f= y)"_sv,
           u8"(function f() {})"_sv,
           u8"(function() {})"_sv,
           u8"(x[y])"_sv,
           u8"(x++)"_sv,
           u8"(++x)"_sv,
           u8"(~x)"_sv,
           u8"(x?y:z)"_sv,
           u8"(new.target)"_sv,
           u8"(yield x)"_sv,
           u8"(yield* x)"_sv,
           u8"(x -= y)"_sv,
           u8"(super)"_sv,
           u8"([super])"_sv,
           u8"([import])"_sv,
           u8"(<jsx />)"_sv,
           u8"(<jsx.Component />)"_sv,
           u8"(<namespace:jsx />)"_sv,
           u8"(<>JSX fragment</>)"_sv,

           // TODO(strager): We should report
           // Diag_Unexpected_Arrow_After_Literal for these:
           u8"(`<strong>${hello}</strong>`)"_sv,
           u8"(html`<strong>hello</strong>`)"_sv,
           u8"(html`<strong>${hello}</strong>`)"_sv,
       }) {
    Test_Parser p(concat(u8"("_sv, parameter_list, u8" => {});"_sv),
                  jsx_options, capture_diags);
    SCOPED_TRACE(p.code);
    auto guard = p.enter_function(Function_Attributes::async_generator);
    p.parse_and_visit_statement();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Invalid_Parameter"_diag,
                       });
  }

  test_parse_and_visit_statement(
      u8"((`<strong>hello</strong>`) => {});"_sv,  //
      u8"                            ^^ Diag_Unexpected_Arrow_After_Literal.arrow"_diag);

  {
    Test_Parser p(u8"([(x,)] => {});"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^ Diag_Stray_Comma_In_Parameter"_diag,  //
            u8"  ^^^^ Diag_Unexpected_Function_Parameter_Is_Parenthesized"_diag,
        });
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Test_Parser p(u8"((yield) => {});"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"Diag_Cannot_Declare_Yield_In_Generator_Function"_diag,
        });
  }

  test_parse_and_visit_statement(
      u8"((#priv) => {});"_sv,  //
      u8"Diag_Cannot_Refer_To_Private_Variable_Without_Object"_diag);

  test_parse_and_visit_statement(
      u8"((42,) => {});"_sv,  //
      u8"Diag_Unexpected_Literal_In_Parameter_List"_diag);

  test_parse_and_visit_statement(u8"((:) => {});"_sv,  //
                                 u8"Diag_Unexpected_Token"_diag);
}

TEST_F(Test_Parse_Function, arrow_function_expression_without_arrow_operator) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(() {});"_sv,  //
        u8"    ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(async () {});"_sv,  //
        u8"          ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"(()\n{});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"((a) {});"_sv,  //
        u8"     ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(async (a) {});"_sv,  //
        u8"           ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"((a, b) {});"_sv,  //
        u8"        ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(async (a, b) {});"_sv,  //
        u8"              ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(async param {});"_sv,  //
        u8"             ^ Diag_Missing_Arrow_Operator_In_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
  }

  // TODO(strager): u8"(async (a, b)\n{});"_sv should report
  // Diag_Missing_Arrow_Operator_In_Arrow_Function.
}

TEST_F(Test_Parse_Function,
       not_arrow_function_expression_without_arrow_operator) {
  // These aren't arrow expressions, but might look like arrow expressions to a
  // bad error-recovering parser.

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"(a, b)\n{}"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"foo() {}"_sv,  //
        u8"Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // foo
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  if ((false)) {  // TODO(strager): Treat '+' differently from ','.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(a+b) {}"_sv,  //
        u8"Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"async(a, b)\n{}"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // async
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, generator_function_with_misplaced_star) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f*(x) { yield x; }"_sv,  //
        u8"         ^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
        u8"          ^ .star"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"*function f(x) { yield x; }\nf(10);"_sv,  //
        u8"          ^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
        u8"^ .star"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // f
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"*async function f(x) { yield x; }\nf(10);"_sv,  //
        u8"                ^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
        u8"^ .star"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // x
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // f
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"*function"_sv,                                            //
        u8" ^^^^^^^^ Diag_Missing_Name_In_Function_Statement"_diag,  //
        u8"^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"*async function"_sv,                                            //
        u8"       ^^^^^^^^ Diag_Missing_Name_In_Function_Statement"_diag,  //
        u8"^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x = *function(y) { yield y; }"_sv,  //
        u8"        ^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x = *function f(y) { yield y; }"_sv,  //
        u8"                  ^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
        u8"        ^ .star"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_variable_declaration",        // y
                              "visit_enter_function_scope_body",   //
                              "visit_variable_use",                // y
                              "visit_exit_function_scope",         //
                              "visit_variable_declaration",        // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x = *async function(y) { yield y; }"_sv,  //
        u8"        ^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",       // y
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x = *async function f(y) { yield y; }"_sv,  //
        u8"                        ^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
        u8"        ^ .star"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_variable_declaration",        // y
                              "visit_enter_function_scope_body",   //
                              "visit_variable_use",                // y
                              "visit_exit_function_scope",         //
                              "visit_variable_declaration",        // x
                              "visit_end_of_module",
                          }));
  }

  test_parse_and_visit_module(
      u8"let x = *function* f(y) { yield y; }"_sv,  //
      u8"        ^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);

  test_parse_and_visit_module(
      u8"let x = *async function* f(y) { yield y; }"_sv,  //
      u8"        ^ Diag_Generator_Function_Star_Belongs_After_Keyword_Function"_diag);
}

TEST_F(Test_Parse_Function,
       star_before_async_or_function_is_not_generator_star) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"*\nfunction f() {}"_sv,  //
        u8"^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"*\nasync function f() {}"_sv,  //
        u8"^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"async *function f() {}"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_variable_use",                // async
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"console.log('hi')\n*function f() {}\nconsole.log('hi')"_sv,
        no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_variable_use",                // console
                              "visit_variable_use",                // console
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, incomplete_function_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f() { a; "_sv,  //
        u8"             ^ Diag_Unclosed_Code_Block"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_variable_use",               // a
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(Test_Parse_Function,
       function_as_if_body_is_allowed_and_creates_implicit_block_scope) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) function f() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_block_scope",          //
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) async function f() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_block_scope",          //
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) body; else function f() {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_variable_use",               // body
                              "visit_enter_block_scope",          //
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) body; else async function f() {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_variable_use",               // body
                              "visit_enter_block_scope",          //
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Function, function_as_do_while_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"do function f() {} while (cond);"_sv,  //
        u8"  ` Diag_Function_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^^^^^^ .function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::do_while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // cond
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"do async function f() {} while (cond);"_sv,  //
        u8"   ^^^^^^^^^^^^^^ Diag_Function_Statement_Not_Allowed_In_Body.function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::do_while_loop}");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // cond
                          }));
  }
}

TEST_F(Test_Parse_Function, function_as_for_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"for (;cond;) function f() {}"_sv,  //
        u8"            ` Diag_Function_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^^^^ .function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::for_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"for (;cond;) async function f() {}"_sv,  //
        u8"             ^^^^^^^^^^^^^^ Diag_Function_Statement_Not_Allowed_In_Body.function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::for_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(Test_Parse_Function, function_as_while_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while (cond) function f() {}"_sv,  //
        u8"            ` Diag_Function_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^^^^ .function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while (cond) async function f() {}"_sv,  //
        u8"             ^^^^^^^^^^^^^^ Diag_Function_Statement_Not_Allowed_In_Body.function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // cond
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(Test_Parse_Function, function_as_with_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (obj) function f() {}"_sv,  //
        u8"          ` Diag_Function_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"           ^^^^^^^^ .function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::with_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // obj
                              "visit_enter_with_scope",           // with
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_with_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (obj) async function f() {}"_sv,  //
        u8"           ^^^^^^^^^^^^^^ Diag_Function_Statement_Not_Allowed_In_Body.function_keywords"_diag
        u8"{.kind_of_statement=Statement_Kind::with_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // obj
                              "visit_enter_with_scope",           // with
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // f
                              "visit_exit_function_scope",        // f
                              "visit_variable_declaration",       // f
                              "visit_exit_with_scope",
                          }));
  }
}

TEST_F(Test_Parse_Function, function_as_label_body_is_allowed) {
  Spy_Visitor p = test_parse_and_visit_statement(u8"l: function f() {}"_sv,
                                                 no_diags, javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_function_scope",
                            "visit_enter_function_scope_body",
                            "visit_exit_function_scope",
                            "visit_variable_declaration",
                        }));
}

TEST_F(Test_Parse_Function, invalid_function_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f(g(), p) {}"_sv,  //
        u8"           ^^^ Diag_Invalid_Parameter"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // p
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({func_param_decl(u8"p"_sv), function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"(g(), p) => {}"_sv,  //
                                    u8" ^^^ Diag_Invalid_Parameter"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // p
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"p"_sv)}));
  }

  {
    // TODO(strager): Is Diag_Unexpected_Arrow_After_Literal appropriate here?
    // Maybe we should recover in a different way.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"g(42) => {}"_sv,                                       //
        u8"  ^^ Diag_Unexpected_Literal_In_Parameter_List"_diag,  //
        u8"Diag_Missing_Operator_Between_Expression_And_Arrow_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // g
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let g = ((x)) => { }"_sv,  //
        u8"         ^^^ Diag_Unexpected_Function_Parameter_Is_Parenthesized"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let f = function ((x)) { }"_sv,  //
        u8"                  ^^^ Diag_Unexpected_Function_Parameter_Is_Parenthesized"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Function, function_body_is_visited_first_in_expression) {
  for (String8_View function : {u8"function(){b;}"_sv, u8"()=>{b;}"_sv}) {
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(u8"[a, "_sv, function, u8", c];"_sv), no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // b
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // a
                              "visit_variable_use",               // c
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"b", u8"a", u8"c"}));
  }

  for (String8_View function : {u8"function(){b;}"_sv, u8"()=>{b;}"_sv}) {
    Test_Parser p(
        concat(u8"[a, ("_sv, function, u8")().prop, c] = [1, 2, 3];"_sv));
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // b
                              "visit_exit_function_scope",        //
                              "visit_variable_assignment",        // a
                              "visit_variable_assignment",        // c
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"b"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"a", u8"c"}));
  }
}

TEST_F(Test_Parse_Function, return_with_comma_operator_missing_arguments) {
  test_parse_and_visit_statement(
      u8"function f() { return 4, }"_sv,  //
      u8"                       ^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_statement(
      u8"function f() { return 1,2, }"_sv,  //
      u8"                         ^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_statement(
      u8"function f() { return ,-5 }"_sv,  //
      u8"                      ^ Diag_Missing_Operand_For_Operator"_diag);
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
