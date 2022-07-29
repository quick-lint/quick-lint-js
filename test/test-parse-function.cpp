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
class test_parse_function : public test_parse_expression {};

TEST_F(test_parse_function,
       parse_function_parameters_with_object_destructuring) {
  {
    test_parser p(u8"function f({x, y, z}) {}"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 4);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
    EXPECT_EQ(p.variable_declarations[1].name, u8"x");
    EXPECT_EQ(p.variable_declarations[2].name, u8"y");
    EXPECT_EQ(p.variable_declarations[3].name, u8"z");
  }

  {
    test_parser p(u8"({x, y, z}) => {}"_sv);
    p.parse_and_visit_expression();
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"z");
  }
}

TEST_F(test_parse_function, parse_function_statement) {
  {
    test_parser p(u8"function foo() {}"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"foo")));
  }

  {
    test_parser p(u8"function sin(theta) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"sin"), param_decl(u8"theta")));
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // sin
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // theta
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"function pow(base, exponent) {}"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"pow");
    EXPECT_EQ(p.variable_declarations[1].name, u8"base");
    EXPECT_EQ(p.variable_declarations[2].name, u8"exponent");

    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // pow
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // base
                                      "visit_variable_declaration",  // exponent
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"function f(x, y = x) {}"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
    EXPECT_EQ(p.variable_declarations[1].name, u8"x");
    EXPECT_EQ(p.variable_declarations[2].name, u8"y");

    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));

    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_variable_use",               // x
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"function f() { return x; }"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");

    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));

    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"function g(first, ...args) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"g"), param_decl(u8"first"),
                            param_decl(u8"args")));
  }
}

TEST_F(test_parse_function, function_with_arrow_operator) {
  {
    test_parser p(u8"function f() => {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_functions_or_methods_should_not_have_arrow_operator,  //
            arrow_operator, strlen(u8"function f() "), u8"=>")));
  }
}

TEST_F(test_parse_function, function_statement_with_no_name) {
  {
    test_parser p(u8"function() {x;}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_name_in_function_statement,  //
                    where, 0, u8"function(")));
  }

  {
    test_parser p(u8"async function() {x;}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_name_in_function_statement,  //
                    where, strlen(u8"async "), u8"function(")));
  }

  {
    test_parser p(u8"async function(x) {y;}(z)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use"));             // z
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_name_or_parentheses_for_function,  //
                    where, strlen(u8"async "), u8"function(",               //
                    function, 0, u8"async function(x) {y;}")));
  }
}

TEST_F(test_parse_function, async_function_statement) {
  {
    test_parser p(u8"async function f() {}"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
  }

  {
    test_parser p(u8"async function f() { await null; }"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"f");
  }
}

TEST_F(test_parse_function,
       async_function_cannot_have_newline_after_async_keyword) {
  {
    test_parser p(u8"async\nfunction f() { await myPromise; }"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // async
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"async", u8"myPromise"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(diag_await_operator_outside_async)));
  }
}

TEST_F(test_parse_function, generator_function_statement) {
  {
    test_parser p(u8"function* f() {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"f")));
  }
}

TEST_F(test_parse_function, await_in_async_function) {
  {
    test_parser p(u8"async function f() { await myPromise; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"async () => { await myPromise; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"(async function() { await myPromise; })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"({ async f() { await myPromise; } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"({ async *f() { await myPromise; } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"class C { async f() { await myPromise; } }");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(u8"class C { async *f() { await myPromise; } }");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }

  {
    test_parser p(
        u8"async function f() {\n"
        u8"  function g() {}\n"
        u8"  await myPromise;\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
  }
}

TEST_F(test_parse_function, await_asi_in_async_function) {
  {
    test_parser p(u8"async function f() { await a\nawait b }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"a",  //
                            u8"b"));
  }
}

TEST_F(test_parse_function, yield_in_generator_function) {
  {
    test_parser p(u8"function *f() { yield myValue; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }

  {
    test_parser p(u8"(function*() { yield myValue; })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }

  {
    test_parser p(u8"({ *f() { yield myValue; } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }

  {
    test_parser p(u8"({ async *f() { yield myValue; } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }

  {
    test_parser p(u8"class C { *f() { yield myValue; } }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }

  {
    test_parser p(
        u8"function* f() {\n"
        u8"  function g() {}\n"
        u8"  yield myValue;\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"myValue"));
  }
}

TEST_F(test_parse_function, parse_function_expression) {
  {
    test_parser p(u8"(function() {});"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"(function(x, y) {});"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // x
                            "visit_variable_declaration",       // y
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"(function() {let x = y;});"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // y
                            "visit_variable_declaration",       // x
                            "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"(a, function(b) {c;}(d));"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // b
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // c
                            "visit_exit_function_scope",        //
                            "visit_variable_use",               // a
                            "visit_variable_use"));             // d
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"b")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"c", u8"a", u8"d"));
  }

  {
    test_parser p(u8"(function recur() { recur(); })();"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_named_function_scope",  // recur
                            "visit_enter_function_scope_body",   //
                            "visit_variable_use",                // recur
                            "visit_exit_function_scope"));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAre(u8"recur"));
  }
}

TEST_F(test_parse_function, arrow_function_expression) {
  {
    test_parser p(u8"(() => x);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
  }

  {
    test_parser p(u8"(x => y);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"x")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"y"));
  }

  {
    test_parser p(u8"((x = y) => z);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_use",               // y
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // z
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"x")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"y", u8"z"));
  }

  {
    test_parser p(u8"async (x) => y;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"async (x) => y, z;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",
                                      "visit_variable_use"));  // z
  }

  {
    test_parser p(u8"async x => y;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST_F(test_parse_function, arrow_function_expression_with_statements) {
  {
    test_parser p(u8"(() => { x; });"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
  }

  {
    test_parser p(u8"(x => { y; });"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"x")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"y"));
  }
}

TEST_F(test_parse_function, nested_arrow_function) {
  for (string8_view code : {
           u8"(x => y => (x, y));"_sv,
           u8"(x => { (y => (x, y)); });"_sv,
           u8"(x => y => { x; y; });"_sv,
           u8"(x => { (y => { x; y; }); });"_sv,
       }) {
    test_parser p(code);
    p.parse_and_visit_statement();
    SCOPED_TRACE(out_string8(code));
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(param_decl(u8"x"), param_decl(u8"y")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x", u8"y"));
  }

  {
    test_parser p(u8"(a => ((b = a) => {}));"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // a
                                      "visit_enter_function_scope_body",  //
                                      "visit_enter_function_scope",       //
                                      "visit_variable_use",               // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_exit_function_scope"));
  }
}

TEST_F(test_parse_function, empty_parens_parameter_is_an_error) {
  {
    test_parser p(u8"function f(()) {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code, diag_missing_expression_between_parentheses,  //
            left_paren_to_right_paren, strlen(u8"function f("), u8"()")));
  }

  {
    test_parser p(u8"let f = (()) => {};"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_expression_between_parentheses,  //
                    left_paren_to_right_paren, strlen(u8"let f = ("), u8"()")));
  }
}

TEST_F(test_parse_function,
       function_statements_allow_trailing_commas_in_parameter_list) {
  {
    test_parser p(u8"function f(x,) { y; });"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST_F(test_parse_function,
       arrow_functions_allow_trailing_commas_in_parameter_list) {
  {
    test_parser p(u8"((x,) => { y; });"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST_F(test_parse_function,
       function_statement_without_name_or_parameter_list_or_body) {
  {
    test_parser p(u8"{ function } x = y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_block_scope",    //
                                      "visit_exit_block_scope",     //
                                      "visit_variable_use",         // y
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_name_in_function_statement,  //
                    where, strlen(u8"{ "), u8"function")));
  }
}

TEST_F(test_parse_function, function_statement_without_parameter_list_or_body) {
  {
    test_parser p(u8"{ function f } x = y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use",          // y
                                      "visit_variable_assignment",   // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_function_parameter_list,  //
                    expected_parameter_list, strlen(u8"{ function f"), u8"")));
  }

  {
    test_parser p(u8"function f\n3 * x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_function_parameter_list,  //
                    expected_parameter_list, strlen(u8"function f"), u8"")));
  }

  {
    test_parser p(u8"function f, x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_missing_function_parameter_list,  //
                              expected_parameter_list, strlen(u8"function f"),
                              u8""),
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"function f"), u8",")));
  }

  {
    test_parser p(u8"function f.x() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_missing_function_parameter_list,  //
                              expected_parameter_list, strlen(u8"function f"),
                              u8""),
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"function f"), u8"."),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"function f.x()"), u8"")));
  }
}

TEST_F(test_parse_function, named_function_statement_without_body) {
  {
    test_parser p(u8"function f()\nf;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // f
                                      "visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"function f()"), u8"")));
  }

  {
    test_parser p(u8"function f(x)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // f
                                      "visit_enter_function_scope",   //
                                      "visit_variable_declaration",   // x
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"function f(x)"), u8"")));
  }

  {
    test_parser p(u8"class f { m() }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       //
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // m
                                      "visit_enter_function_scope",    //
                                      "visit_exit_function_scope",     //
                                      "visit_exit_class_scope",        //
                                      "visit_variable_declaration"));  // f
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"class f { m()"), u8"")));
  }

  {
    test_parser p(u8"class f { m(x) }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       //
                                      "visit_enter_class_scope_body",  //
                                      "visit_property_declaration",    // m
                                      "visit_enter_function_scope",    //
                                      "visit_variable_declaration",    // x
                                      "visit_exit_function_scope",     //
                                      "visit_exit_class_scope",        //
                                      "visit_variable_declaration"));  // f
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_function_body,  //
                    expected_body, strlen(u8"class f { m(x)"), u8"")));
  }

  {
    test_parser p(u8"export default function f()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // f
                                      "visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body,
                              strlen(u8"export default function f()"), u8"")));
  }

  {
    test_parser p(u8"function* f()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // f
                                      "visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"function* f()"), u8"")));
  }
}

TEST_F(test_parse_function, unnamed_function_statement_without_body) {
  {
    test_parser p(u8"function*()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"function*()"), u8""),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_name_in_function_statement,  //
                              where, strlen(u8""), u8"function*(")));
  }

  {
    test_parser p(u8"function()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"function()"), u8""),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_name_in_function_statement,  //
                              where, strlen(u8""), u8"function(")));
  }

  {
    test_parser p(u8"export default function()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body,
                              strlen(u8"export default function()"), u8"")));
  }
}

TEST_F(test_parse_function, named_function_expression_without_body) {
  {
    test_parser p(u8"(function f())"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  //
                                      "visit_exit_function_scope"));       //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"(function f()"), u8"")));
  }
}

TEST_F(test_parse_function, unnamed_function_expression_without_body) {
  {
    test_parser p(u8"(function())"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   //
                                      "visit_exit_function_scope"));  //
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_function_body,  //
                              expected_body, strlen(u8"(function()"), u8"")));
  }
}

TEST_F(test_parse_function, arrow_function_invoked_with_parens) {
  {
    test_parser p(u8"(() => {})()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_enter_function_scope_body",
                                      "visit_exit_function_scope",
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_function, async_arrow_function_invoked_with_parens) {
  {
    test_parser p(u8"(async () => {})()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_enter_function_scope_body",
                                      "visit_exit_function_scope",
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_function, arrow_function_invoked_no_parens) {
  {
    test_parser p(u8"() => {}()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_enter_function_scope_body",
                                      "visit_exit_function_scope",
                                      "visit_end_of_module"));

    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_missing_parentheses_around_self_invoked_function,  //
            func_start, 0, u8"",                                            //
            invocation, strlen(u8"() => {}"), u8"(")));
  }
}

TEST_F(test_parse_function, async_arrow_function_invoked_no_parens) {
  {
    test_parser p(u8"async () => {}()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_enter_function_scope_body",
                                      "visit_exit_function_scope",
                                      "visit_end_of_module"));

    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_missing_parentheses_around_self_invoked_function,  //
            func_start, 0, u8"",                                            //
            invocation, strlen(u8"async () => {}"), u8"(")));
  }
}

TEST_F(test_parse_function, arrow_function_without_parameter_list) {
  {
    test_parser p(u8"=> x + y"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_function_parameter_list,  //
                    arrow, 0, u8"=>")));
  }
}

TEST_F(test_parse_function, function_with_invalid_parameters) {
  for (string8_view parameter_list : {
           u8"x << y"_sv,
           u8"x.prop"_sv,
           u8"html`<strong>hello</strong>`"_sv,
       }) {
    string8 code = u8"function f(" + string8(parameter_list) + u8") {}";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE(diag_invalid_parameter)));
  }

  {
    test_parser p(u8"function f(42) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE(diag_unexpected_literal_in_parameter_list)));
  }
}

TEST_F(test_parse_function, arrow_function_with_invalid_parameters) {
  for (string8_view parameter_list : {
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
           // diag_unexpected_arrow_after_literal for these:
           u8"(`<strong>${hello}</strong>`)"_sv,
           u8"(html`<strong>hello</strong>`)"_sv,
           u8"(html`<strong>${hello}</strong>`)"_sv,
       }) {
    padded_string code(u8"(" + string8(parameter_list) + u8" => {});");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser_options options;
    options.jsx = true;
    parser p(&code, &v, options);
    auto guard = p.enter_function(function_attributes::async_generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE(diag_invalid_parameter)));
  }

  {
    test_parser p(u8"((`<strong>hello</strong>`) => {});"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_unexpected_arrow_after_literal,  //
                    arrow, strlen(u8"((`<strong>hello</strong>`) "), u8"=>")));
  }

  {
    padded_string code(u8"([(x,)] => {});"_sv);
    spy_visitor v;
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_stray_comma_in_parameter,  //
                              comma, strlen(u8"([(x"), u8",")));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    padded_string code(u8"((yield) => {});"_sv);
    spy_visitor v;
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE(
                    diag_cannot_declare_yield_in_generator_function)));
  }

  {
    test_parser p(u8"((#priv) => {});"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(
                    diag_cannot_refer_to_private_variable_without_object)));
  }

  {
    test_parser p(u8"((42,) => {});"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE(diag_unexpected_literal_in_parameter_list)));
  }

  {
    test_parser p(u8"((:) => {});"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE(diag_unexpected_token)));
  }
}

TEST_F(test_parse_function, arrow_function_expression_without_arrow_operator) {
  {
    test_parser p(u8"(() {});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"(() "), u8"{")));
  }

  {
    test_parser p(u8"(async () {});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"(async () "), u8"{")));
  }

  {
    test_parser p(u8"(()\n{});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"(()\n"), u8"{")));
  }

  {
    test_parser p(u8"((a, b) {});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"((a, b) "), u8"{")));
  }

  {
    test_parser p(u8"(async (a, b) {});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"(async (a, b) "), u8"{")));
  }

  {
    test_parser p(u8"(async param {});"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_arrow_operator_in_arrow_function,  //
                    where, strlen(u8"(async param "), u8"{")));
  }

  // TODO(strager): u8"(async (a, b)\n{});"_sv should report
  // diag_missing_arrow_operator_in_arrow_function.
}

TEST_F(test_parse_function,
       not_arrow_function_expression_without_arrow_operator) {
  // These aren't arrow expressions, but might look like arrow expressions to a
  // bad error-recovering parser.

  {
    test_parser p(u8"(a, b)\n{}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"foo() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",       // foo
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(diag_missing_semicolon_after_statement)));
  }

  if ((false)) {  // TODO(strager): Treat '+' differently from ','.
    test_parser p(u8"(a+b) {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(diag_missing_semicolon_after_statement)));
  }

  {
    test_parser p(u8"async(a, b)\n{}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",       // async
                                      "visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_function, generator_function_with_misplaced_star) {
  {
    test_parser p(u8"function f*(x) { yield x; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_generator_function_star_belongs_before_name,  //
            function_name, strlen(u8"function "), u8"f",               //
            star, strlen(u8"function f"), u8"*")));
  }

  {
    test_parser p(u8"*function f(x) { yield x; }\nf(10);"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_FIELDS(
            diag_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(p.code, strlen(u8"*function "), u8"f"), star,
            offsets_matcher(p.code, 0, u8"*"))));
  }

  {
    test_parser p(u8"*async function f(x) { yield x; }\nf(10);"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_generator_function_star_belongs_before_name,  //
            function_name, strlen(u8"*async function "), u8"f",        //
            star, 0, u8"*")));
  }

  {
    test_parser p(u8"*function"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_generator_function_star_belongs_after_keyword_function,  //
                star, 0, u8"*"),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_name_in_function_statement,  //
                              where, strlen(u8"*"), u8"function")));
  }

  {
    test_parser p(u8"*async function"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_generator_function_star_belongs_after_keyword_function,  //
                star, 0, u8"*"),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_name_in_function_statement,  //
                              where, strlen(u8"*async "), u8"function")));
  }

  {
    test_parser p(u8"let x = *function(y) { yield y; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_generator_function_star_belongs_after_keyword_function,  //
            star, strlen(u8"let x = "), u8"*")));
  }

  {
    test_parser p(u8"let x = *function f(y) { yield y; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_variable_declaration",        // y
                                      "visit_enter_function_scope_body",   //
                                      "visit_variable_use",                // y
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_FIELDS(
            diag_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(p.code, strlen(u8"let x = *function "), u8"f"),
            star, offsets_matcher(p.code, strlen(u8"let x = "), u8"*"))));
  }

  {
    test_parser p(u8"let x = *async function(y) { yield y; }"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_generator_function_star_belongs_after_keyword_function,  //
            star, strlen(u8"let x = "), u8"*")));
  }

  {
    test_parser p(u8"let x = *async function f(y) { yield y; }"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_variable_declaration",        // y
                                      "visit_enter_function_scope_body",   //
                                      "visit_variable_use",                // y
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_FIELDS(
            diag_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(p.code, strlen(u8"let x = *async function "),
                            u8"f"),
            star, offsets_matcher(p.code, strlen(u8"let x = "), u8"*"))));
  }

  {
    test_parser p(u8"let x = *function* f(y) { yield y; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_generator_function_star_belongs_after_keyword_function,  //
            star, strlen(u8"let x = "), u8"*")));
  }

  {
    test_parser p(u8"let x = *async function* f(y) { yield y; }"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code,
            diag_generator_function_star_belongs_after_keyword_function,  //
            star, strlen(u8"let x = "), u8"*")));
  }
}

TEST_F(test_parse_function,
       star_before_async_or_function_is_not_generator_star) {
  {
    test_parser p(u8"*\nfunction f() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"*")));
  }

  {
    test_parser p(u8"*\nasync function f() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"*")));
  }

  {
    test_parser p(u8"async *function f() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_named_function_scope",  // f
                            "visit_enter_function_scope_body",   //
                            "visit_exit_function_scope",         //
                            "visit_variable_use",                // async
                            "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"console.log('hi')\n*function f() {}\nconsole.log('hi')"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_named_function_scope",  // f
                            "visit_enter_function_scope_body",   //
                            "visit_exit_function_scope",         //
                            "visit_variable_use",                // console
                            "visit_variable_use",                // console
                            "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_function, incomplete_function_body) {
  {
    test_parser p(u8"function f() { a; "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_variable_use",               // a
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_unclosed_code_block,  //
                              block_open, strlen(u8"function f() "), u8"{")));
  }
}

TEST_F(test_parse_function,
       function_as_if_body_is_allowed_and_creates_implicit_block_scope) {
  {
    test_parser p(u8"if (cond) function f() {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_block_scope"));
  }

  {
    test_parser p(u8"if (cond) async function f() {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_block_scope"));
  }

  {
    test_parser p(u8"if (cond) body; else function f() {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_use",          // body
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_block_scope"));
  }

  {
    test_parser p(u8"if (cond) body; else async function f() {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_use",          // body
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_block_scope"));
  }
}

TEST_F(test_parse_function, function_as_do_while_loop_body_is_disallowed) {
  {
    test_parser p(u8"do function f() {} while (cond);"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_variable_use"));  // cond
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_function_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::do_while_loop,                                 //
            expected_body, offsets_matcher(p.code, strlen(u8"do"), u8""),  //
            function_keywords,
            offsets_matcher(p.code, strlen(u8"do "), u8"function"))));
  }

  {
    test_parser p(u8"do async function f() {} while (cond);"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_variable_use"));  // cond
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_FIELDS(
            diag_function_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::do_while_loop,  //
            function_keywords,
            offsets_matcher(p.code, strlen(u8"do "), u8"async function"))));
  }
}

TEST_F(test_parse_function, function_as_for_loop_body_is_disallowed) {
  {
    test_parser p(u8"for (;cond;) function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_function_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::for_loop,  //
            expected_body,
            offsets_matcher(p.code, strlen(u8"for (;cond;)"), u8""),  //
            function_keywords,
            offsets_matcher(p.code, strlen(u8"for (;cond;) "), u8"function"))));
  }

  {
    test_parser p(u8"for (;cond;) async function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                              diag_function_statement_not_allowed_in_body,
                              kind_of_statement,
                              statement_kind::for_loop,  //
                              function_keywords,
                              offsets_matcher(p.code, strlen(u8"for (;cond;) "),
                                              u8"async function"))));
  }
}

TEST_F(test_parse_function, function_as_while_loop_body_is_disallowed) {
  {
    test_parser p(u8"while (cond) function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_function_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::while_loop,  //
            expected_body,
            offsets_matcher(p.code, strlen(u8"while (cond)"), u8""),  //
            function_keywords,
            offsets_matcher(p.code, strlen(u8"while (cond) "), u8"function"))));
  }

  {
    test_parser p(u8"while (cond) async function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                              diag_function_statement_not_allowed_in_body,
                              kind_of_statement,
                              statement_kind::while_loop,  //
                              function_keywords,
                              offsets_matcher(p.code, strlen(u8"while (cond) "),
                                              u8"async function"))));
  }
}

TEST_F(test_parse_function, function_as_with_statement_body_is_disallowed) {
  {
    test_parser p(u8"with (obj) function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // obj
                                      "visit_enter_with_scope",      // with
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_with_scope"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_function_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::with_statement,  //
            expected_body,
            offsets_matcher(p.code, strlen(u8"with (obj)"), u8""),  //
            function_keywords,
            offsets_matcher(p.code, strlen(u8"with (obj) "), u8"function"))));
  }

  {
    test_parser p(u8"with (obj) async function f() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // obj
                                      "visit_enter_with_scope",      // with
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_exit_with_scope"));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                              diag_function_statement_not_allowed_in_body,
                              kind_of_statement,
                              statement_kind::with_statement,  //
                              function_keywords,
                              offsets_matcher(p.code, strlen(u8"with (obj) "),
                                              u8"async function"))));
  }
}

TEST_F(test_parse_function, invalid_function_parameter) {
  {
    test_parser p(u8"function f(g(), p) {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // p
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"f"), param_decl(u8"p")));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_invalid_parameter,  //
                              parameter, strlen(u8"function f("), u8"g()")));
  }

  {
    test_parser p(u8"(g(), p) => {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // p
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"p")));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_invalid_parameter,  //
                              parameter, strlen(u8"("), u8"g()")));
  }

  {
    // TODO(strager): Is diag_unexpected_arrow_after_literal appropriate here?
    // Maybe we should recover in a different way.
    test_parser p(u8"g(42) => {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // g
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE(
                diag_missing_operator_between_expression_and_arrow_function),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_literal_in_parameter_list,  //
                              literal, strlen(u8"g("), u8"42")));
  }
}

TEST_F(test_parse_function, function_body_is_visited_first_in_expression) {
  for (string8_view function : {u8"function(){b;}"sv, u8"()=>{b;}"sv}) {
    string8 code = u8"[a, " + string8(function) + u8", c];";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // b
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // a
                                      "visit_variable_use"));             // c
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"b", u8"a", u8"c"));
  }

  for (string8_view function : {u8"function(){b;}"sv, u8"()=>{b;}"sv}) {
    string8 code =
        u8"[a, (" + string8(function) + u8")().prop, c] = [1, 2, 3];";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // b
                                      "visit_exit_function_scope",        //
                                      "visit_variable_assignment",        // a
                                      "visit_variable_assignment"));      // c
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"b"));
    EXPECT_THAT(p.variable_assignments, ElementsAre(u8"a", u8"c"));
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
