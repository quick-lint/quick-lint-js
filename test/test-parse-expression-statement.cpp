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
TEST(test_parse, parse_math_expression) {
  for (const char8 *input :
       {u8"2", u8"2+2", u8"2^2", u8"2 + + 2", u8"2 * (3 + 4)", u8"1+1+1+1+1"}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    spy_visitor v = parse_and_visit_expression(input);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"some_var"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }

  {
    spy_visitor v =
        parse_and_visit_expression(u8"some_var + some_other_var"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
    EXPECT_EQ(v.variable_uses[1].name, u8"some_other_var");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"+ v"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"v");
  }
}

TEST(test_parse, parse_invalid_math_expression) {
  {
    spy_visitor v;
    padded_string code(u8"2 +"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, strlen(u8"2 "), u8"+"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"^ 2"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, u8"^"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * * 2"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, strlen(u8"2 "), u8"*"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 & & & 2"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                             offsets_matcher(&code, strlen(u8"2 "), u8"&")),
            ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                             offsets_matcher(&code, strlen(u8"2 & "), u8"&"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"(2 *)"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, strlen(u8"(2 "), u8"*"))));
  }
  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + 4"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(&code, strlen(u8"2 * "), u8"("))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + (4"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            ERROR_TYPE_FIELD(
                error_unmatched_parenthesis, where,
                offsets_matcher(&code, strlen(u8"2 * (3 + "), u8"(")),
            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                             offsets_matcher(&code, strlen(u8"2 * "), u8"("))));
  }
}

TEST(test_parse, stray_right_parenthesis) {
  {
    spy_visitor v;
    padded_string code(u8")"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unmatched_parenthesis, where,
                              offsets_matcher(&code, 0, u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"x))"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                             offsets_matcher(&code, strlen(u8"x"), u8")")),
            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                             offsets_matcher(&code, strlen(u8"x)"), u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"-x))"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                             offsets_matcher(&code, strlen(u8"-x"), u8")")),
            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                             offsets_matcher(&code, strlen(u8"-x)"), u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"await p)"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(&code, strlen(u8"await p"), u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"yield v)"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(&code, strlen(u8"yield p"), u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"return result)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(&code, strlen(u8"return result"), u8")"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw banana)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(&code, strlen(u8"throw banana"), u8")"))));
  }
}

TEST(test_parse, statement_starting_with_binary_only_operator) {
  for (string8_view op : {
           u8"!=", u8"!==", u8"%",  u8"&",          u8"&&", u8"*",
           u8"**", u8",",   u8"<",  u8"<<",         u8"<=", u8"=",
           u8"==", u8"===", u8">",  u8">=",         u8">>", u8">>>",
           u8"??", u8"^",   u8"in", u8"instanceof", u8"|",
       }) {
    padded_string code(string8(op) + u8" x");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, op))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }

  {
    padded_string code(u8".x; y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, strlen(u8".")))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // y
  }
}

TEST(test_parse, invalid_identifier_after_expression) {
  {
    spy_visitor v;
    padded_string code(u8"one two"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_statement, where,
                              offsets_matcher(&code, strlen(u8"one"), u8""))));
  }

  {
    spy_visitor v;
    padded_string code(u8"(one two)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_identifier_in_expression, unexpected,
                    offsets_matcher(&code, strlen(u8"(one "), u8"two"))));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"one"},
                            spy_visitor::visited_variable_use{u8"two"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"f(one two)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_identifier_in_expression, unexpected,
                    offsets_matcher(&code, strlen(u8"f(one "), u8"two"))));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"f"},
                            spy_visitor::visited_variable_use{u8"one"},
                            spy_visitor::visited_variable_use{u8"two"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"xs[one two]"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_identifier_in_expression, unexpected,
                    offsets_matcher(&code, strlen(u8"xs[one "), u8"two"))));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},
                            spy_visitor::visited_variable_use{u8"one"},
                            spy_visitor::visited_variable_use{u8"two"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"(a b c d)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    ERROR_TYPE_FIELD(
                        error_unexpected_identifier_in_expression, unexpected,
                        offsets_matcher(&code, strlen(u8"(a "), u8"b")),
                    ERROR_TYPE_FIELD(
                        error_unexpected_identifier_in_expression, unexpected,
                        offsets_matcher(&code, strlen(u8"(a b "), u8"c")),
                    ERROR_TYPE_FIELD(
                        error_unexpected_identifier_in_expression, unexpected,
                        offsets_matcher(&code, strlen(u8"(a b c "), u8"d"))));
  }
}

TEST(test_parse, function_call_without_right_paren) {
  {
    padded_string code(u8"f(x "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_right_paren_for_function_call, expected_right_paren,
            offsets_matcher(&code, strlen(u8"f(x"), u8""),  //
            left_paren, offsets_matcher(&code, strlen(u8"f"), u8"("))));
  }

  {
    padded_string code(u8"{ f( }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_right_paren_for_function_call, expected_right_paren,
            offsets_matcher(&code, strlen(u8"{ f("), u8""),  //
            left_paren, offsets_matcher(&code, strlen(u8"{ f"), u8"("))));
  }

  {
    padded_string code(u8"f(x; y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // f
                                      "visit_variable_use",  // x
                                      "visit_variable_use",  // y
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_right_paren_for_function_call, expected_right_paren,
            offsets_matcher(&code, strlen(u8"f(x"), u8""),  //
            left_paren, offsets_matcher(&code, strlen(u8"f"), u8"("))));
  }
}

TEST(test_parse, parse_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"(x) = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");

    EXPECT_EQ(v.variable_assignments.size(), 0);
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x = y = z"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"z");

    EXPECT_EQ(v.variable_assignments.size(), 2);
    EXPECT_EQ(v.variable_assignments[0].name, u8"y");
    EXPECT_EQ(v.variable_assignments[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"xs[i] = j"_sv);
    EXPECT_THAT(v.variable_assignments, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"i"},   //
                            spy_visitor::visited_variable_use{u8"j"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{x: y} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[x]: y} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{k1: {k2: x, k3: y}} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"},  //
                            spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, parse_compound_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x += y"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",           // x
                                      "visit_variable_use",           // y
                                      "visit_variable_assignment"));  // x
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p += y"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // x
                                      "visit_variable_use"));  // y
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments, IsEmpty());
  }
}

TEST(test_parse, parse_plusplus_minusminus) {
  {
    spy_visitor v = parse_and_visit_expression(u8"++x"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"y--"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }
}

TEST(test_parse, parse_typeof_with_just_variable) {
  {
    spy_visitor v = parse_and_visit_expression(u8"typeof x"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"typeof(x)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_typeof_with_non_variable) {
  {
    spy_visitor v = parse_and_visit_expression(u8"typeof x.prop"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_array_subscript) {
  {
    spy_visitor v = parse_and_visit_expression(u8"array[index]"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"array"},
                            spy_visitor::visited_variable_use{u8"index"}));
  }
}

TEST(test_parse, array_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"[]"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"[...elements]"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"elements"}));
  }
}

TEST(test_parse, object_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"{key: value}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[key1 + key2]: value}"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // key1
                            "visit_variable_use",    // key2
                            "visit_variable_use"));  // value
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"key1"},
                            spy_visitor::visited_variable_use{u8"key2"},
                            spy_visitor::visited_variable_use{u8"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{...other1, ...other2}"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // other1
                            "visit_variable_use"));  // other2
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other1"},
                            spy_visitor::visited_variable_use{u8"other2"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{func(a, b) { body; }}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",  // body
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, expression_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"console.log('hello');"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"this.x = xPos;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xPos"}));
  }

  for (string8 literal : {u8"null", u8"true", u8"false"}) {
    SCOPED_TRACE(out_string8(literal));
    spy_visitor v = parse_and_visit_statement((literal + u8";").c_str());
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"++x;"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"new C();"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"C"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"delete x;"_sv);
    // TODO(strager): Should this be visit_variable_assignment instead? Or
    // something else?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8R"("use strict";)");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"`hello`"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"`hello ${world}`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // world
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"42"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"import(url).then(); secondStatement;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // url
                            "visit_variable_use"));  // secondStatement
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"typeof x"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"[x, y, z];"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // x
                            "visit_variable_use",    // y
                            "visit_variable_use"));  // z
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/regexp/;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/=regexp/;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  for (string8 op : {u8"void ", u8"!", u8"~", u8"+", u8"-"}) {
    string8 code = op + u8" x;";
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async => rhs;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",  // async
                                      "visit_enter_function_scope_body",
                                      "visit_variable_use",  // rhs
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, asi_plusplus_minusminus) {
  {
    spy_visitor v;
    padded_string code(u8"x\n++\ny;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_use",  //
                            "visit_variable_assignment"));
  }
}

TEST(test_parse, asi_after_let) {
  {
    spy_visitor v = parse_and_visit_module(u8"let\nwhile (x) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // let
                                      "visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, asi_between_identifiers) {
  {
    spy_visitor v = parse_and_visit_module(u8"a\nb"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // a
                                      "visit_variable_use",  // b
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, parse_function_calls) {
  {
    spy_visitor v = parse_and_visit_expression(u8"f(x)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"f(x, y)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"o.f(x, y)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"o");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"console.log('hello', 42)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"console");
  }
}

TEST(test_parse, parse_templates_in_expressions) {
  {
    spy_visitor v = parse_and_visit_expression(u8"`hello`"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`hello${world}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"world");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${one}${two}${three}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"one");
    EXPECT_EQ(v.variable_uses[1].name, u8"two");
    EXPECT_EQ(v.variable_uses[2].name, u8"three");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${2+2, four}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"four");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"tag`${inside}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"tag");
    EXPECT_EQ(v.variable_uses[1].name, u8"inside");
  }
}

TEST(test_parse, DISABLED_parse_invalid_function_calls) {
  {
    spy_visitor v;
    padded_string code(u8"(x)f"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_identifier, where,
                              offsets_matcher(&code, strlen(u8"(x)"), u8"f"))));

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"f");
  }
}

TEST(test_parse, parse_function_call_as_statement) {
  {
    spy_visitor v;
    padded_string code(u8"f(x); g(y);"_sv);
    parser p(&code, &v);

    EXPECT_TRUE(p.parse_and_visit_statement(v));
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");

    EXPECT_TRUE(p.parse_and_visit_statement(v));
    ASSERT_EQ(v.variable_uses.size(), 4);
    EXPECT_EQ(v.variable_uses[2].name, u8"g");
    EXPECT_EQ(v.variable_uses[3].name, u8"y");

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_property_lookup) {
  {
    spy_visitor v = parse_and_visit_expression(u8"some_var.some_property"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }
}

TEST(test_parse, parse_new_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"new Foo()"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"Foo");
  }
}

TEST(test_parse, conditional_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x ? y : z"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, statement_beginning_with_async_or_let) {
  for (string8 name : {u8"async", u8"let"}) {
    SCOPED_TRACE(out_string8(name));

    {
      spy_visitor v = parse_and_visit_statement(name + u8" = other;");
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"other"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8"();");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8".method();");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8";");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    for (const char8 *unary_operator : {u8"++", u8"--"}) {
      string8 code = name + unary_operator;
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",  //
                              "visit_variable_assignment"));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8" ? a : b;");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"a"},
                              spy_visitor::visited_variable_use{u8"b"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8" => {body;};");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",       //
                              "visit_variable_declaration",       // (name)
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // body
                              "visit_exit_function_scope"));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8"`template`;");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use"));  // (name)
    }

    {
      spy_visitor v =
          parse_and_visit_statement(name + u8"`template${variable}`;");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",    // (name)
                              "visit_variable_use"));  // variable
    }

    for (const char8 *binary_operator : {
             u8"%=",
             u8"&&=",
             u8"&=",
             u8"**=",
             u8"*=",
             u8"+=",
             u8"-=",
             u8"/=",
             u8"<<=",
             u8">>=",
             u8">>>=",
             u8"?\x3f=",
             u8"^=",
             u8"|=",
             u8"||=",
         }) {
      string8 code = name + u8" " + binary_operator + u8" other";
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"other"}));
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
    }

    for (const char8 *binary_operator : {
             u8"!=", u8"!==", u8"%",          u8"&",  u8"&&", u8"*",   u8"**",
             u8"+",  u8",",   u8"-",          u8"/",  u8"<",  u8"<<",  u8"<=",
             u8"==", u8"===", u8">",          u8">=", u8">>", u8">>>", u8"??",
             u8"^",  u8"in",  u8"instanceof", u8"|",  u8"||",
         }) {
      string8 code = name + u8" " + binary_operator + u8" other";
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"other"}));
    }

    {
      string8 code = name + u8": while (go());";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code);
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"go"}));
    }
  }
}

TEST(test_parse, trailing_comma_in_comma_expression_is_disallowed) {
  {
    spy_visitor v;
    padded_string code(u8"(a, b, );"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(&code, strlen(u8"(a, b"), u8","))));
  }
}

TEST(test_parse, incomplete_unary_expression_with_following_statement_keyword) {
  for (string8_view statement : {
           u8"for(;x;);"_sv,
           u8"if(x);"_sv,
           u8"return x;"_sv,
           u8"throw x;"_sv,
           u8"while(x);"_sv,
       }) {
    padded_string code(string8(u8"!\n"_sv) + string8(statement));
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, u8"!"))));
  }

  {
    padded_string code(u8"!\nswitch(x){}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, u8"!"))));
  }

  {
    padded_string code(u8"!\nenum"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                                     offsets_matcher(&code, 0, u8"!")),
                    ERROR_TYPE_FIELD(
                        error_typescript_enum_not_implemented, enum_keyword,
                        offsets_matcher(&code, strlen(u8"!\n"), u8"enum"))));
  }
}

TEST(test_parse, let_is_an_identifier_if_escaped) {
  {
    // The following would instead be parsed as a declaration of a variable
    // named 'x':
    //
    //   let[x] = [y];
    //
    // Escaping the 'l' in 'let' forces it to behave as an identifier, not a
    // contextual keyword.
    spy_visitor v = parse_and_visit_expression(u8"\\u{6c}et[x] = [y];"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // let
                                      "visit_variable_use",    // x
                                      "visit_variable_use"));  // y
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"},
                            spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"y"}));
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
