// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse, parse_function_parameters_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function f({x, y, z}) {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 4);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
    EXPECT_EQ(v.variable_declarations[1].name, u8"x");
    EXPECT_EQ(v.variable_declarations[2].name, u8"y");
    EXPECT_EQ(v.variable_declarations[3].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"({x, y, z}) => {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[2].name, u8"z");
  }
}

TEST(test_parse, parse_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function foo() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function sin(theta) {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"sin");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, u8"theta");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // sin
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // theta
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"function pow(base, exponent) {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"pow");
    EXPECT_EQ(v.variable_declarations[1].name, u8"base");
    EXPECT_EQ(v.variable_declarations[2].name, u8"exponent");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // pow
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // base
                                      "visit_variable_declaration",  // exponent
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function f(x, y = x) {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
    EXPECT_EQ(v.variable_declarations[1].name, u8"x");
    EXPECT_EQ(v.variable_declarations[2].name, u8"y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_variable_use",               // x
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"function f() { return x; }"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"function g(first, ...args) {}"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"g", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        u8"first", variable_kind::_parameter},
                    spy_visitor::visited_variable_declaration{
                        u8"args", variable_kind::_parameter}));
  }
}

TEST(test_parse, function_statement_with_no_name) {
  {
    padded_string code(u8"function() {x;}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_name_in_function_statement, where,
                              offsets_matcher(&code, 0, u8"function("))));
  }

  {
    padded_string code(u8"async function() {x;}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_name_in_function_statement, where,
            offsets_matcher(&code, strlen(u8"async "), u8"function("))));
  }

  {
    padded_string code(u8"async function(x) {y;}(z)"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use"));             // z
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_name_or_parentheses_for_function,  //
            where,
            offsets_matcher(&code, strlen(u8"async "), u8"function("),  //
            function, offsets_matcher(&code, 0, u8"async function(x) {y;}"))));
  }
}

TEST(test_parse, async_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"async function f() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"async function f() { await null; }"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }
}

TEST(test_parse, generator_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function* f() {}"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"f", variable_kind::_function}));
  }
}

TEST(test_parse, await_in_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() { await myPromise; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"async () => { await myPromise; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() { await myPromise; })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ async f() { await myPromise; } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C { async f() { await myPromise; } }");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() {\n"
        u8"  function g() {}\n"
        u8"  await myPromise;\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }
}

TEST(test_parse, await_asi_in_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() { await a\nawait b }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},  //
                            spy_visitor::visited_variable_use{u8"b"}));
  }
}

TEST(test_parse, yield_in_generator_function) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"function *f() { yield myValue; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function*() { yield myValue; })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ *f() { yield myValue; } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { *f() { yield myValue; } }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function* f() {\n"
        u8"  function g() {}\n"
        u8"  yield myValue;\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }
}

TEST(test_parse, parse_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(function() {});"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(function(x, y) {});"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // x
                            "visit_variable_declaration",       // y
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function() {let x = y;});"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // y
                            "visit_variable_declaration",       // x
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(a, function(b) {c;}(d));"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",               // a
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       // b
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // c
                            "visit_exit_function_scope",        //
                            "visit_variable_use"));             // d
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"b", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"c"},
                            spy_visitor::visited_variable_use{u8"d"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function recur() { recur(); })();"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_named_function_scope",  // recur
                            "visit_enter_function_scope_body",   //
                            "visit_variable_use",                // recur
                            "visit_exit_function_scope"));
    EXPECT_THAT(v.enter_named_function_scopes,
                ElementsAre(spy_visitor::visited_enter_named_function_scope{
                    u8"recur"}));
  }
}

TEST(test_parse, arrow_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(() => x);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => y);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"((x = y) => z);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_use",               // y
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // z
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"},
                            spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async (x) => y;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async (x) => y, z;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",
                                      "visit_variable_use"));  // z
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async x => y;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, arrow_function_expression_with_statements) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(() => { x; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => { y; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
  }
}

TEST(test_parse, function_statements_allow_trailing_commas_in_parameter_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function f(x,) { y; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, arrow_functions_allow_trailing_commas_in_parameter_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"((x,) => { y; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, function_statement_without_name_or_parameter_list_or_body) {
  {
    padded_string code(u8"{ function } x = y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",    //
                                      "visit_exit_block_scope",     //
                                      "visit_variable_use",         // y
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_name_in_function_statement, where,
                    offsets_matcher(&code, strlen(u8"{ "), u8"function"))));
  }
}

TEST(test_parse, function_statement_without_parameter_list_or_body) {
  {
    padded_string code(u8"{ function f } x = y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use",          // y
                                      "visit_variable_assignment",   // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_function_parameter_list, function_name,
                    offsets_matcher(&code, strlen(u8"{ function "), u8"f"))));
  }

  {
    padded_string code(u8"function f\n3 * x;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_function_parameter_list, function_name,
                    offsets_matcher(&code, strlen(u8"function "), u8"f"))));
  }

  {
    padded_string code(u8"function f, x;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_missing_function_parameter_list, function_name,
                offsets_matcher(&code, strlen(u8"function "), u8"f")),
            ERROR_TYPE_FIELD(
                error_missing_operand_for_operator, where,
                offsets_matcher(&code, strlen(u8"function f"), u8","))));
  }

  {
    padded_string code(u8"function f.x() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    // Expected parse:
    //   function f  // no parameter list or body
    //   .x()        // no lhs for '.'; no ';'
    //   {}
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_exit_function_scope",   // f
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_missing_function_parameter_list, function_name,
                offsets_matcher(&code, strlen(u8"function "), u8"f")),
            ERROR_TYPE_FIELD(
                error_missing_operand_for_operator, where,
                offsets_matcher(&code, strlen(u8"function f"), u8".")),
            ERROR_TYPE_FIELD(
                error_missing_semicolon_after_statement, where,
                offsets_matcher(&code, strlen(u8"function f.x()"), u8""))));
  }
}

TEST(test_parse, arrow_function_without_parameter_list) {
  {
    padded_string code(u8"=> x + y"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_arrow_function_parameter_list,
                              arrow, offsets_matcher(&code, 0, u8"=>"))));
  }
}

TEST(test_parse, generator_function_with_misplaced_star) {
  {
    padded_string code(u8"function f*(x) { yield x; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(&code, strlen(u8"function "), u8"f"),  //
            star, offsets_matcher(&code, strlen(u8"function f"), u8"*"))));
  }
}

TEST(test_parse, incomplete_function_body) {
  {
    padded_string code(u8"function f() { a; "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_variable_use",               // a
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unclosed_code_block, block_open,
                    offsets_matcher(&code, strlen(u8"function f() "), u8"{"))));
  }
}

TEST(test_parse, function_as_do_while_loop_body_is_disallowed) {
  {
    padded_string code(u8"do function f() {} while (cond);"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_variable_use"));  // cond
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_function_statement_not_allowed_in_do_while_loop,
                    function_keywords,
                    offsets_matcher(&code, strlen(u8"do "), u8"function"))));
  }

  {
    padded_string code(u8"do async function f() {} while (cond);"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope",        // f
                                      "visit_variable_use"));  // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_function_statement_not_allowed_in_do_while_loop,
            function_keywords,
            offsets_matcher(&code, strlen(u8"do "), u8"async function"))));
  }
}

TEST(test_parse, function_as_for_loop_body_is_disallowed) {
  {
    padded_string code(u8"for (;cond;) function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_function_statement_not_allowed_in_for_loop, function_keywords,
            offsets_matcher(&code, strlen(u8"for (;cond;) "), u8"function"))));
  }

  {
    padded_string code(u8"for (;cond;) async function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_function_statement_not_allowed_in_for_loop,
                              function_keywords,
                              offsets_matcher(&code, strlen(u8"for (;cond;) "),
                                              u8"async function"))));
  }
}

TEST(test_parse, function_as_while_loop_body_is_disallowed) {
  {
    padded_string code(u8"while (cond) function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_function_statement_not_allowed_in_while_loop,
            function_keywords,
            offsets_matcher(&code, strlen(u8"while (cond) "), u8"function"))));
  }

  {
    padded_string code(u8"while (cond) async function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_function_statement_not_allowed_in_while_loop,
                    function_keywords,
                    offsets_matcher(&code, strlen(u8"while (cond) "),
                                    u8"async function"))));
  }
}

TEST(test_parse, function_as_with_statement_body_is_disallowed) {
  {
    padded_string code(u8"with (obj) function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",               // obj
                                      "visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_function_statement_not_allowed_in_with_statement,
            function_keywords,
            offsets_matcher(&code, strlen(u8"with (obj) "), u8"function"))));
  }

  {
    padded_string code(u8"with (obj) async function f() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",               // obj
                                      "visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // f
                                      "visit_exit_function_scope"));      // f
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_function_statement_not_allowed_in_with_statement,
                    function_keywords,
                    offsets_matcher(&code, strlen(u8"with (obj) "),
                                    u8"async function"))));
  }
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
