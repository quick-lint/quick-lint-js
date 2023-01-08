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
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
class test_parse_expression_statement : public test_parse_expression {};

TEST_F(test_parse_expression_statement, parse_math_expression) {
  for (const char8* input :
       {u8"2", u8"2+2", u8"2^2", u8"2 + + 2", u8"2 * (3 + 4)", u8"1+1+1+1+1"}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    test_parser p(input);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"some_var"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"some_var"}));
  }

  {
    test_parser p(u8"some_var + some_other_var"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"some_var", u8"some_other_var"}));
  }

  {
    test_parser p(u8"+ v"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"v"}));
  }
}

TEST_F(test_parse_expression_statement, parse_invalid_math_expression) {
  {
    test_parser p(u8"2 +"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 "), u8"+"),
        }));
  }

  {
    test_parser p(u8"^ 2"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"^"),
        }));
  }

  {
    test_parser p(u8"2 * * 2"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 "), u8"*"),
        }));
  }

  {
    test_parser p(u8"2 & & & 2"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 "), u8"&"),
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 & "), u8"&"),
        }));
  }

  {
    test_parser p(u8"(2 *)"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"(2 "), u8"*"),
        }));
  }
  {
    test_parser p(u8"2 * (3 + 4"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * "), u8"("),
                }));
  }

  {
    test_parser p(u8"2 * (3 + (4"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * (3 + "), u8"("),
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * "), u8"("),
                }));
  }

  {
    test_parser p(u8"x += ;"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"x "), u8"+="),
        }));
  }
}

TEST_F(test_parse_expression_statement, stray_right_parenthesis) {
  {
    test_parser p(u8")"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, 0, u8")"),
                }));
  }

  {
    test_parser p(u8"x))"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"x"), u8")"),
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"x)"), u8")"),
                }));
  }

  {
    test_parser p(u8"-x))"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"-x"), u8")"),
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"-x)"), u8")"),
                }));
  }

  {
    test_parser p(u8"await p)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"await p"), u8")"),
                }));
  }

  {
    test_parser p(u8"yield v)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"yield p"), u8")"),
                }));
  }

  {
    test_parser p(u8"return result)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"return result"), u8")"),
                }));
  }

  {
    test_parser p(u8"throw banana)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"throw banana"), u8")"),
                }));
  }
}

TEST_F(test_parse_expression_statement,
       statement_starting_with_binary_only_operator) {
  // '<' omitted. It is used for JSX.
  for (string8_view op : {
           u8"!=",  u8"!==", u8"%",          u8"&",  u8"&&",  u8"*",
           u8"**",  u8",",   u8"<<",         u8"<=", u8"=",   u8"==",
           u8"===", u8">",   u8">=",         u8">>", u8">>>", u8"??",
           u8"^",   u8"in",  u8"instanceof", u8"|",
       }) {
    test_parser p(concat(op, u8" x"), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, op),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }

  {
    test_parser p(u8".x; y;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"."),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // y
                          }));
  }
}

TEST_F(test_parse_expression_statement, invalid_identifier_after_expression) {
  {
    test_parser p(u8"one two"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_semicolon_after_statement,  //
                        where, strlen(u8"one"), u8""),
                }));
  }

  {
    test_parser p(u8"(one two)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"(one "), u8"two"),
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"one", u8"two"}));
  }

  {
    test_parser p(u8"f(one two)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"f(one "), u8"two"),
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"one", u8"two"}));
  }

  {
    test_parser p(u8"xs[one two]"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"xs[one "), u8"two"),
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"one", u8"two"}));
  }

  {
    test_parser p(u8"(a b c d)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"(a "), u8"b"),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"(a b "), u8"c"),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_identifier_in_expression,  //
                        unexpected, strlen(u8"(a b c "), u8"d")));
  }
}

TEST_F(test_parse_expression_statement, function_call_without_right_paren) {
  {
    test_parser p(u8"f(x "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_expected_right_paren_for_function_call,  //
                        expected_right_paren, strlen(u8"f(x"), u8"",          //
                        left_paren, strlen(u8"f"), u8"("),
                }));
  }

  {
    test_parser p(u8"{ f( }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_expected_right_paren_for_function_call,  //
                        expected_right_paren, strlen(u8"{ f("), u8"",         //
                        left_paren, strlen(u8"{ f"), u8"("),
                }));
  }

  {
    test_parser p(u8"f(x; y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // f
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_expected_right_paren_for_function_call,  //
                        expected_right_paren, strlen(u8"f(x"), u8"",          //
                        left_paren, strlen(u8"f"), u8"("),
                }));
  }

  {
    test_parser p(u8"f(x\n--y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // f
                              "visit_variable_use",         // x
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // y
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_expected_right_paren_for_function_call,  //
                        expected_right_paren, strlen(u8"f(x"), u8"",          //
                        left_paren, strlen(u8"f"), u8"("),
                }));
  }
}

TEST_F(test_parse_expression_statement, parse_assignment) {
  {
    test_parser p(u8"x = y"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));

    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
  }

  {
    test_parser p(u8"(x) = y"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));

    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
  }

  {
    test_parser p(u8"x.p = y"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));

    EXPECT_EQ(p.variable_assignments.size(), 0);
  }

  {
    test_parser p(u8"x = y = z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y", u8"x"}));
  }

  {
    test_parser p(u8"xs[i] = j"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_assignments, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"i", u8"j"}));
  }

  {
    test_parser p(u8"{x: y} = z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }

  {
    test_parser p(u8"{[x]: y} = z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"z"}));
  }

  {
    test_parser p(u8"{k1: {k2: x, k3: y}} = z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x",  //
                                                          u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }

  {
    test_parser p(u8"[x, y] = z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x",  //
                                                          u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }
}

TEST_F(test_parse_expression_statement, parse_compound_assignment) {
  {
    test_parser p(u8"x += y"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // x
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"x.p += y"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.variable_assignments, IsEmpty());
  }
}

TEST_F(test_parse_expression_statement, parse_plusplus_minusminus) {
  {
    test_parser p(u8"++x"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
  }

  {
    test_parser p(u8"y--"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
  }
}

TEST_F(test_parse_expression_statement, parse_typeof_with_just_variable) {
  {
    test_parser p(u8"typeof x"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"typeof(x)"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(test_parse_expression_statement, parse_typeof_with_non_variable) {
  {
    test_parser p(u8"typeof x.prop"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(test_parse_expression_statement,
       parse_typeof_with_conditional_operator) {
  {
    test_parser p(u8"typeof x ? 10 : 20"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"typeof x.y ? 10 : 20"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(test_parse_expression_statement, parse_array_subscript) {
  {
    test_parser p(u8"array[index]"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"array", u8"index"}));
  }
}

TEST_F(test_parse_expression_statement, array_literal) {
  {
    test_parser p(u8"[]"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"[...elements]"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"elements"}));
  }
}

TEST_F(test_parse_expression_statement, object_literal) {
  {
    test_parser p(u8"{key: value}"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"value"}));
  }

  {
    test_parser p(u8"{[key1 + key2]: value}"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // key1
                              "visit_variable_use",  // key2
                              "visit_variable_use",  // value
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"key1", u8"key2", u8"value"}));
  }

  {
    test_parser p(u8"{...other1, ...other2}"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // other1
                              "visit_variable_use",  // other2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other1", u8"other2"}));
  }

  {
    test_parser p(u8"{func(a, b) { body; }}"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_variable_declaration",       // b
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(test_parse_expression_statement,
       object_literal_method_with_arrow_operator) {
  {
    test_parser p(u8"let o = {method() => {}}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_functions_or_methods_should_not_have_arrow_operator,  //
                arrow_operator, strlen(u8"let o = {method() "), u8"=>"),
        }));
  }
}

TEST_F(test_parse_expression_statement, expression_statement) {
  {
    test_parser p(u8"console.log('hello');"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
  }

  {
    test_parser p(u8"this.x = xPos;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xPos"}));
  }

  for (string8 literal : {u8"null", u8"true", u8"false"}) {
    SCOPED_TRACE(out_string8(literal));
    test_parser p((literal + u8";").c_str());
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"++x;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"new C();"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    test_parser p(u8R"("use strict";)");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"`hello`"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"`hello ${world}`"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // world
                          }));
  }

  {
    test_parser p(u8"42"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"import(url).then(); secondStatement;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // url
                              "visit_variable_use",  // secondStatement
                          }));
  }

  {
    test_parser p(u8"import.meta; secondStatement;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // secondStatement
                          }));
  }

  {
    test_parser p(u8"typeof x"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
  }

  {
    test_parser p(u8"[x, y, z];"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_variable_use",  // z
                          }));
  }

  {
    test_parser p(u8"/regexp/;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"/=regexp/;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  for (string8 op : {u8"void ", u8"!", u8"~", u8"+", u8"-"}) {
    test_parser p(concat(op, u8" x;"));
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"async => rhs;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // async
                              "visit_enter_function_scope_body",
                              "visit_variable_use",  // rhs
                              "visit_exit_function_scope",
                          }));
  }

  {
    test_parser p(u8"#myArray[index] = rhs;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"index", u8"rhs"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // index
                              "visit_variable_use",  // rhs
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    ::testing::VariantWith<
                        diag_cannot_refer_to_private_variable_without_object>(
                        ::testing::_),
                }));
  }

  {
    test_parser p(u8"<MyJSXComponent />", jsx_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // MyJSXComponent
                          }));
  }
}

TEST_F(test_parse_expression_statement, delete_of_variable) {
  test_parser p(u8"delete x;"_sv, capture_diags);
  p.parse_and_visit_expression();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_delete_use",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_expression_statement, delete_of_expression) {
  test_parser p(u8"delete x.p;"_sv, capture_diags);
  p.parse_and_visit_expression();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
}

TEST_F(test_parse_expression_statement,
       cannot_reference_private_identifier_outside_class) {
  {
    test_parser p(u8"this.#x = 10;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.visits, IsEmpty());

    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_cannot_access_private_identifier_outside_class,  //
                private_identifier, strlen(u8"this."), u8"#x"),
        }));
  }
}

TEST_F(test_parse_expression_statement, asi_plusplus_minusminus) {
  {
    test_parser p(u8"x\n++\ny;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());

    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x",  //
                                                   u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
  }
}

TEST_F(test_parse_expression_statement, asi_after_let) {
  {
    test_parser p(u8"let\nwhile (x) {}"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // let
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_expression_statement, asi_between_identifiers) {
  {
    test_parser p(u8"a\nb"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // b
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_expression_statement, parse_function_calls) {
  {
    test_parser p(u8"f(x)"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));
  }

  {
    test_parser p(u8"f(x, y)"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x", u8"y"}));
  }

  {
    test_parser p(u8"o.f(x, y)"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"o", u8"x", u8"y"}));
  }

  {
    test_parser p(u8"console.log('hello', 42)"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
  }
}

TEST_F(test_parse_expression_statement, parse_templates_in_expressions) {
  {
    test_parser p(u8"`hello`"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"`hello${world}`"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"world"}));
  }

  {
    test_parser p(u8"`${one}${two}${three}`"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"one", u8"two", u8"three"}));
  }

  {
    test_parser p(u8"`${2+2, four}`"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"four"}));
  }

  {
    test_parser p(u8"tag`${inside}`"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"tag", u8"inside"}));
  }
}

TEST_F(test_parse_expression_statement, parse_invalid_function_calls) {
  {
    test_parser p(u8"(x)f"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_semicolon_after_statement,  //
                        where, strlen(u8"(x)"), u8""),
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"f"}));
  }
}

TEST_F(test_parse_expression_statement, parse_function_call_as_statement) {
  {
    test_parser p(u8"f(x); g(y);"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));

    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"f", u8"x", u8"g", u8"y"}));

    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_statement, parse_property_lookup) {
  {
    test_parser p(u8"some_var.some_property"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"some_var"}));
  }
}

TEST_F(test_parse_expression_statement, parse_new_expression) {
  {
    test_parser p(u8"new Foo()"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Foo"}));
  }
}

TEST_F(test_parse_expression_statement, conditional_expression) {
  {
    test_parser p(u8"x ? y : z"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y", u8"z"}));
  }
}

TEST_F(test_parse_expression_statement, statement_beginning_with_async_or_let) {
  for (string8 name : {u8"async", u8"let"}) {
    SCOPED_TRACE(out_string8(name));

    {
      test_parser p(concat(name, u8" = other;"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other"}));
    }

    {
      test_parser p(concat(name, u8"();"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      test_parser p(concat(name, u8".method();"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      test_parser p(concat(name, u8";"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    for (const char8* unary_operator : {u8"++", u8"--"}) {
      test_parser p(concat(name, unary_operator));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  //
                                "visit_variable_assignment",
                            }));
    }

    {
      test_parser p(concat(name, u8" ? a : b;"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"a", u8"b"));
    }

    {
      test_parser p(concat(name, u8" => {body;};"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // (name)
                                "visit_enter_function_scope_body",  //
                                "visit_variable_use",               // body
                                "visit_exit_function_scope",
                            }));
    }

    {
      test_parser p(concat(name, u8"`template`;"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                            }));
    }

    {
      test_parser p(concat(name, u8"`template${variable}`;"));
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                                "visit_variable_use",  // variable
                            }));
    }

    for (const char8* binary_operator : {
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
      test_parser p(concat(name, u8" ", binary_operator, u8" other"));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"other"));
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
    }

    for (const char8* binary_operator : {
             u8"!=", u8"!==", u8"%",          u8"&",  u8"&&", u8"*",   u8"**",
             u8"+",  u8",",   u8"-",          u8"/",  u8"<",  u8"<<",  u8"<=",
             u8"==", u8"===", u8">",          u8">=", u8">>", u8">>>", u8"??",
             u8"^",  u8"in",  u8"instanceof", u8"|",  u8"||",
         }) {
      test_parser p(concat(name, u8" ", binary_operator, u8" other"));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"other"));
    }

    {
      test_parser p(concat(name, u8": while (go());"));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"go"}));
    }
  }
}

TEST_F(test_parse_expression_statement,
       async_followed_by_newline_is_a_variable_reference) {
  {
    test_parser p(u8"x = async\na => b;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // async
                              "visit_variable_assignment",  // x
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // a
                              "visit_enter_function_scope_body",
                              "visit_variable_use",         // b
                              "visit_exit_function_scope",  //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"async", u8"b"}));
  }
}

TEST_F(test_parse_expression_statement,
       async_followed_by_newline_and_arrow_function) {
  {
    test_parser p(u8"x = async\n(a) => b;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // b
                              "visit_exit_function_scope",        //
                              "visit_variable_assignment",        // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_newline_not_allowed_between_async_and_parameter_list,  //
                async, strlen(u8"x = "), u8"async",                         //
                arrow, strlen(u8"x = async\n(x) "), u8"=>"),
        }));
  }
}

TEST_F(test_parse_expression_statement,
       trailing_comma_in_comma_expression_is_disallowed) {
  {
    test_parser p(u8"(a, b, );"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"(a, b"), u8","),
        }));
  }
}

TEST_F(test_parse_expression_statement,
       handle_unexpected_keyword_in_expression) {
  //
  // keywords temporarily omitted because they will crash downstream
  // ---
  // catch    parse_expression_remainder
  // do       parse_and_visit_do_while
  // else     parse_and_visit_statement
  // extends  parse_expression_remainder: kw_extends
  // finally  parse_expression_remainder: kw_finally
  // function parse_and_visit_function_parameters_and_body_no_scope
  // with     internal check failed in narrow_cast
  //
  for (string8_view statement : {
           u8"break"_sv,
           u8"case"_sv,
           u8"const"_sv,
           u8"continue"_sv,
           u8"default"_sv,
           u8"export"_sv,
           u8"try"_sv,
           u8"var"_sv,
       }) {
    test_parser p(concat(u8"*\n"_sv, statement), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, Not(IsEmpty()));
  }
}

TEST_F(test_parse_expression_statement,
       incomplete_unary_expression_with_following_statement_keyword) {
  for (string8_view statement : {
           u8"for(;x;);"_sv,
           u8"if(x);"_sv,
           u8"return x;"_sv,
           u8"throw x;"_sv,
           u8"while(x);"_sv,
       }) {
    test_parser p(concat(u8"!\n"_sv, statement), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"!"),
        }));
  }

  {
    test_parser p(u8"!\nswitch(x){}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"!"),
        }));
  }

  {
    test_parser p(u8"!\nenum E {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"!"),
        }));
  }
}

TEST_F(test_parse_expression_statement, let_is_an_identifier_if_escaped) {
  {
    // The following would instead be parsed as a declaration of a variable
    // named 'x':
    //
    //   let[x] = [y];
    //
    // Escaping the 'l' in 'let' forces it to behave as an identifier, not a
    // contextual keyword.
    test_parser p(u8"\\u{6c}et[x] = [y];"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"x", u8"y"}));
  }
}

TEST_F(test_parse_expression_statement,
       let_expression_as_statement_body_is_allowed) {
  // do-while loops, for loops, if statements, while loops, and with statements
  // all disallow lexical declarations ('let x = y;') as their body. However,
  // they do allow expression statements in their body, including expression
  // statements starting with 'let'.

  {
    test_parser p(u8"do let(); while (cond);");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // cond
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"cond"}));
  }

  {
    test_parser p(u8"for (;cond;) let = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // let
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"let"}));
  }

  {
    test_parser p(u8"if (cond) let++; else let ? a : b;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_variable_use",         // let
                              "visit_variable_assignment",  // let
                              "visit_variable_use",         // let
                              "visit_variable_use",         // a
                              "visit_variable_use",         // b
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"cond", u8"let", u8"let", u8"a", u8"b"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"let"}));
  }
}

TEST_F(test_parse_expression_statement, let_as_statement_body_allows_asi) {
  // do-while loops, for loops, if statements, while loops, and with statements
  // all disallow lexical declarations ('let x = y;') as their body. However,
  // they do allow expression statements in their body, including expression
  // statements starting with 'let'.

  {
    test_parser p(u8"do let\nwhile (cond);");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // cond
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"cond"}));
  }

  {
    test_parser p(u8"for (;cond;) let\nx = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_variable_use",         // let
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"let", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"if (cond) let\nx = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_variable_use",         // let
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"let", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"if (cond) let\nelse {}");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_variable_use",       // let
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"let"}));
  }

  {
    test_parser p(u8"if (cond) {} else let\nx = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_enter_block_scope",    // if
                              "visit_exit_block_scope",     // if
                              "visit_variable_use",         // let
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"let", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"while (cond) let\nx = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // cond
                              "visit_variable_use",         // let
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond", u8"let", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"with (obj) let\nx = y;");
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",      // obj
                              "visit_enter_with_scope",  // with
                              "visit_variable_use",      // let
                              "visit_exit_with_scope",
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"obj", u8"let", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }
}

TEST_F(test_parse_expression_statement,
       disallow_await_parameter_in_async_arrow_function) {
  {
    test_parser p(u8"(async (await) => null)"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_cannot_declare_await_in_async_function,  //
                        name, strlen(u8"(async ("), u8"await"),
                }));
  }

  {
    test_parser p(u8"(async await => null)"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_cannot_declare_await_in_async_function,  //
                        name, strlen(u8"(async "), u8"await"),
                }));
  }

  {
    test_parser p(u8"async(await)"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
  }

  {
    test_parser p(u8"(async (await, await, await) => {})"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_cannot_declare_await_in_async_function,  //
                              name, strlen(u8"(async ("), u8"await"),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_cannot_declare_await_in_async_function,  //
                              name, strlen(u8"(async (await, "), u8"await"),
            DIAG_TYPE_OFFSETS(
                p.code, diag_cannot_declare_await_in_async_function,  //
                name, strlen(u8"(async (await, await, "), u8"await"),
        }));
  }

  {
    test_parser p(u8"(async (await p) => {})"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_cannot_declare_await_in_async_function,  //
                        name, strlen(u8"(async ("), u8"await"),
                }));
    // TODO(strager): We're ignoring 'p'. Should we treat it as a parameter?
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"await")}));
  }
}

TEST_F(test_parse_expression_statement,
       object_property_default_is_not_allowed) {
  {
    test_parser p(u8"({key = value})"_sv, capture_diags);
    p.parse_and_visit_expression();
    // Important: 'key' should be treated as a literal (like in {key: value}),
    // thus shouldn't be visited.
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // value
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"value"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_object_literal_default_in_expression,  //
                        equal, strlen(u8"({eky "), u8"="),
                }));
  }
}

TEST_F(test_parse_expression_statement,
       object_property_with_equals_is_allowed) {
  // {x: y = z} means something special when destructuring. However, in
  // expressions, it's the same as {x: (y = z)}.
  {
    test_parser p(u8"({x: y = z})"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // z
                              "visit_variable_assignment",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
  }

  {
    test_parser p(u8"({x: y[index] = z})"_sv);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // z
                              "visit_variable_use",  // y
                              "visit_variable_use",  // index
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z", u8"y", u8"index"}));
  }
}

TEST_F(test_parse_expression_statement, invalid_parentheses) {
  {
    test_parser p(u8"()"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code, diag_missing_expression_between_parentheses,  //
                        left_paren_to_right_paren, 0, u8"()",                 //
                        left_paren, 0, u8"(",                                 //
                        right_paren, strlen(u8"("), u8")"),
                }));
  }

  {
    test_parser p(u8"x = ()"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code, diag_missing_expression_between_parentheses,  //
                        left_paren_to_right_paren, strlen(u8"x = "), u8"()",  //
                        left_paren, strlen(u8"x = "), u8"(",                  //
                        right_paren, strlen(u8"x = ("), u8")"),
                }));
  }

  {
    test_parser p(u8"() = x"_sv, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code, diag_missing_expression_between_parentheses,  //
                        left_paren_to_right_paren, 0, u8"()",                 //
                        left_paren, 0, u8"(",                                 //
                        right_paren, strlen(u8"("), u8")"),
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
