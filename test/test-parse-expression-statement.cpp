// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
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

namespace quick_lint_js {
namespace {
class Test_Parse_Expression_Statement : public Test_Parse_Expression {};

TEST_F(Test_Parse_Expression_Statement, parse_math_expression) {
  for (String8_View input : {u8"2"_sv, u8"2+2"_sv, u8"3^2"_sv, u8"2 + + 2"_sv,
                             u8"2 * (3 + 4)"_sv, u8"1+1+1+1+1"_sv}) {
    SCOPED_TRACE(out_string8(u8"input = " + String8(input)));
    Test_Parser p(input);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"some_var"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"some_var"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"some_var + some_other_var"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"some_var", u8"some_other_var"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"+ v"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"v"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_invalid_math_expression) {
  test_parse_and_visit_expression(
      u8"2 +"_sv,  //
      u8"  ^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_expression(u8"^ 2"_sv,  //
                                  u8"^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_expression(
      u8"2 * * 2"_sv,  //
      u8"  ^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_expression(
      u8"2 & & & 2"_sv,                                  //
      u8"    ^ Diag_Missing_Operand_For_Operator"_diag,  //
      u8"  ^ Diag_Missing_Operand_For_Operator"_diag);

  test_parse_and_visit_expression(
      u8"(2 *)"_sv,  //
      u8"   ^ Diag_Missing_Operand_For_Operator"_diag);
  test_parse_and_visit_expression(u8"2 * (3 + 4"_sv,  //
                                  u8"    ^ Diag_Unmatched_Parenthesis"_diag);

  test_parse_and_visit_expression(
      u8"2 * (3 + (4"_sv,                              //
      u8"         ^ Diag_Unmatched_Parenthesis"_diag,  //
      u8"    ^ Diag_Unmatched_Parenthesis"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"x += ;"_sv,  //
        u8"  ^^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, stray_right_parenthesis) {
  test_parse_and_visit_statement(u8")"_sv,  //
                                 u8"^ Diag_Unmatched_Parenthesis"_diag);

  test_parse_and_visit_statement(u8"x))"_sv,                               //
                                 u8"  ^ Diag_Unmatched_Parenthesis"_diag,  //
                                 u8" ^ Diag_Unmatched_Parenthesis"_diag);

  test_parse_and_visit_statement(u8"-x))"_sv,                               //
                                 u8"   ^ Diag_Unmatched_Parenthesis"_diag,  //
                                 u8"  ^ Diag_Unmatched_Parenthesis"_diag);

  {
    Test_Parser p(u8"await p)"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::async);
    p.parse_and_visit_statement();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"       ^ Diag_Unmatched_Parenthesis"_diag,
                       });
  }

  {
    Test_Parser p(u8"yield v)"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"       ^ Diag_Unmatched_Parenthesis"_diag,
                       });
  }

  test_parse_and_visit_statement(
      u8"return result)"_sv,  //
      u8"             ^ Diag_Unmatched_Parenthesis"_diag);

  test_parse_and_visit_statement(
      u8"throw banana)"_sv,  //
      u8"            ^ Diag_Unmatched_Parenthesis"_diag);
}

TEST_F(Test_Parse_Expression_Statement,
       statement_starting_with_binary_only_operator) {
  // '<' omitted. It is used for JSX.
  // '/=' omitted. It starts a regular expression.
  for (String8_View op : {
           u8"!="_sv,         u8"!=="_sv, u8"%"_sv,    u8"%="_sv,
           u8"&"_sv,          u8"&&"_sv,  u8"&&="_sv,  u8"&="_sv,
           u8"*"_sv,          u8"**"_sv,  u8"**="_sv,  u8"*="_sv,
           u8"+="_sv,         u8","_sv,   u8"-="_sv,   u8"<<"_sv,
           u8"<<="_sv,        u8"<="_sv,  u8"="_sv,    u8"=="_sv,
           u8"==="_sv,        u8">"_sv,   u8">="_sv,   u8">>"_sv,
           u8">>="_sv,        u8">>>"_sv, u8">>>="_sv, u8"??"_sv,
           u8"?\x3f="_sv,     u8"^"_sv,   u8"^="_sv,   u8"in"_sv,
           u8"instanceof"_sv, u8"|"_sv,   u8"|="_sv,   u8"||="_sv,
       }) {
    Test_Parser p(concat(op, u8" x"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    assert_diagnostics(
        p.code, p.errors,
        {
            DIAGNOSTIC_ASSERTION_SPAN(Diag_Missing_Operand_For_Operator,  //
                                      where, 0, op),
        });
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }

  {
    Test_Parser p(u8".x; y;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Operand_For_Operator"_diag,
                       });
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // y
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, invalid_identifier_after_expression) {
  test_parse_and_visit_module(
      u8"one two"_sv,  //
      u8"   ` Diag_Missing_Semicolon_After_Statement"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(one two)"_sv,  //
        u8"     ^^^ Diag_Unexpected_Identifier_In_Expression"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"one", u8"two"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"f(one two)"_sv,  //
        u8"      ^^^ Diag_Unexpected_Identifier_In_Expression"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"one", u8"two"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"xs[one two]"_sv,  //
        u8"       ^^^ Diag_Unexpected_Identifier_In_Expression"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"one", u8"two"}));
  }

  test_parse_and_visit_statement(
      u8"(a b c d)"_sv,                                            //
      u8"       ^ Diag_Unexpected_Identifier_In_Expression"_diag,  //
      u8"     ^ Diag_Unexpected_Identifier_In_Expression"_diag,    //
      u8"   ^ Diag_Unexpected_Identifier_In_Expression"_diag);
}

TEST_F(Test_Parse_Expression_Statement, function_call_without_right_paren) {
  test_parse_and_visit_statement(
      u8"f(x "_sv,  //
      u8"   ` Diag_Expected_Right_Paren_For_Function_Call.expected_right_paren\n"_diag
      u8" ^ .left_paren"_diag);

  test_parse_and_visit_statement(
      u8"{ f( }"_sv,  //
      u8"    ` Diag_Expected_Right_Paren_For_Function_Call.expected_right_paren\n"_diag
      u8"   ^ .left_paren"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"f(x; y;"_sv,  //
        u8"   ` Diag_Expected_Right_Paren_For_Function_Call.expected_right_paren\n"_diag
        u8" ^ .left_paren"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // f
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"f(x\n--y;"_sv,  //
        u8"   ` Diag_Expected_Right_Paren_For_Function_Call.expected_right_paren\n"_diag
        u8" ^ .left_paren"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // f
                              "visit_variable_use",         // x
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // y
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_assignment) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x = y"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));

    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"(x) = y"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));

    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x.p = y"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));

    EXPECT_EQ(p.variable_assignments.size(), 0);
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x = y = z"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"xs[i] = j"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_assignments, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"i", u8"j"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_destrcturing_assignment) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"{x: y} = z"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{[x]: y} = z"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"z"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{k1: {k2: x, k3: y}} = z"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x",  //
                                                          u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{x = y} = z"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
    // FIXME(strager): This order is technically wrong. z should be evaluated
    // before y.
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y", u8"z"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{x: y = z} = w"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    // FIXME(strager): This order is technically wrong. z should be evaluated
    // before y.
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z", u8"w"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"[x, y] = z"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x",  //
                                                          u8"y"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_compound_assignment) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x += y"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // x
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x.p += y"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.variable_assignments, IsEmpty());
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_plusplus_minusminus) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"++x"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"y--"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_assignment",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_typeof_with_just_variable) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"typeof x"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"typeof(x)"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_typeof_with_non_variable) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"typeof x.prop"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       parse_typeof_with_conditional_operator) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"typeof x ? 10 : 20"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"typeof x.y ? 10 : 20"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_array_subscript) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"array[index]"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"array", u8"index"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, array_literal) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"[]"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"[...elements]"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"elements"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, object_literal) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{key: value}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"value"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{[key1 + key2]: value}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // key1
                              "visit_variable_use",  // key2
                              "visit_variable_use",  // value
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"key1", u8"key2", u8"value"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{...other1, ...other2}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // other1
                              "visit_variable_use",  // other2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other1", u8"other2"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{func(a, b) { body; }}"_sv, no_diags, javascript_options);
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

TEST_F(Test_Parse_Expression_Statement,
       object_literal_method_with_arrow_operator) {
  test_parse_and_visit_statement(
      u8"let o = {method() => {}}"_sv,  //
      u8"                  ^^ Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator"_diag);
}

TEST_F(Test_Parse_Expression_Statement, expression_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"console.log('hello');"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"this.x = xPos;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xPos"}));
  }

  for (String8 literal : {u8"null", u8"true", u8"false"}) {
    SCOPED_TRACE(out_string8(literal));
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(literal, u8";"_sv), no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"++x;"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_assignment",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"new C();"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    Test_Parser p(u8R"("use strict";)"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"`hello`"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"`hello ${world}`"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // world
                          }));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"42"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Test_Parser p(u8"import(url).then(); secondStatement;"_sv);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // url
                              "visit_variable_use",  // secondStatement
                          }));
  }

  {
    Test_Parser p(u8"import.meta; secondStatement;"_sv);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // secondStatement
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"typeof x"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"[x, y, z];"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_variable_use",  // z
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"/regexp/;"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"/=regexp/;"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  for (String8 op : {u8"void ", u8"!", u8"~", u8"+", u8"-"}) {
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(op, u8" x;"_sv), no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async => rhs;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // async
                              "visit_enter_function_scope_body",
                              "visit_variable_use",  // rhs
                              "visit_exit_function_scope",
                          }));
  }

  {
    Test_Parser p(u8"#myArray[index] = rhs;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"index", u8"rhs"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // index
                              "visit_variable_use",  // rhs
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"Diag_Cannot_Refer_To_Private_Variable_Without_Object"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"<MyJSXComponent />"_sv,
                                                   no_diags, jsx_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // MyJSXComponent
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, delete_of_variable) {
  Spy_Visitor p = test_parse_and_visit_expression(u8"delete x;"_sv, no_diags);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_delete_use",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
}

TEST_F(Test_Parse_Expression_Statement, delete_of_expression) {
  Spy_Visitor p = test_parse_and_visit_expression(u8"delete x.p;"_sv, no_diags,
                                                  javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
}

TEST_F(Test_Parse_Expression_Statement,
       cannot_reference_private_identifier_outside_class) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"this.#x = 10;"_sv,  //
        u8"     ^^ Diag_Cannot_Access_Private_Identifier_Outside_Class"_diag);
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_Expression_Statement, asi_plusplus_minusminus) {
  {
    Test_Parser p(u8"x\n++\ny;"_sv);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();

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

TEST_F(Test_Parse_Expression_Statement, asi_after_let) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"let\nwhile (x) {}"_sv,
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // let
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, asi_between_identifiers) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"a\nb"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // b
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_function_calls) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"f(x)"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"f(x, y)"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x", u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"o.f(x, y)"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"o", u8"x", u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"console.log('hello', 42)"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_templates_in_expressions) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"`hello`"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"`hello${world}`"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"world"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"`${one}${two}${three}`"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"one", u8"two", u8"three"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"`${2+2, four}`"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"four"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"tag`${inside}`"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"tag", u8"inside"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_invalid_function_calls) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(x)f"_sv,  //
        u8"   ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"f"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_function_call_as_statement) {
  {
    Test_Parser p(u8"f(x); g(y);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));

    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"f", u8"x", u8"g", u8"y"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_property_lookup) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"some_var.some_property"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"some_var"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, parse_new_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"new Foo()"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Foo"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, conditional_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"x ? y : z"_sv, no_diags,
                                                    javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y", u8"z"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, statement_beginning_with_async_or_let) {
  for (String8 name : {u8"async", u8"let"}) {
    SCOPED_TRACE(out_string8(name));

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8" = other;"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other"}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8"();"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8".method();"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8";"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    for (String8_View unary_operator : {u8"++"_sv, u8"--"_sv}) {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, unary_operator), no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  //
                                "visit_variable_assignment",
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8" ? a : b;"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"a", u8"b"));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8" => {body;};"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // (name)
                                "visit_enter_function_scope_body",  //
                                "visit_variable_use",               // body
                                "visit_exit_function_scope",
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8"`template`;"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8"`template${variable}`;"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                                "visit_variable_use",  // variable
                            }));
    }

    for (String8_View binary_operator : {
             u8"%="_sv,
             u8"&&="_sv,
             u8"&="_sv,
             u8"**="_sv,
             u8"*="_sv,
             u8"+="_sv,
             u8"-="_sv,
             u8"/="_sv,
             u8"<<="_sv,
             u8">>="_sv,
             u8">>>="_sv,
             u8"?\x3f="_sv,
             u8"^="_sv,
             u8"|="_sv,
             u8"||="_sv,
         }) {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8" "_sv, binary_operator, u8" other"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"other"));
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
    }

    for (String8_View binary_operator : {
             u8"!="_sv,  u8"!=="_sv, u8"%"_sv,  u8"&"_sv,          u8"&&"_sv,
             u8"*"_sv,   u8"**"_sv,  u8"+"_sv,  u8","_sv,          u8"-"_sv,
             u8"/"_sv,   u8"<"_sv,   u8"<<"_sv, u8"<="_sv,         u8"=="_sv,
             u8"==="_sv, u8">"_sv,   u8">="_sv, u8">>"_sv,         u8">>>"_sv,
             u8"??"_sv,  u8"^"_sv,   u8"in"_sv, u8"instanceof"_sv, u8"|"_sv,
             u8"||"_sv,
         }) {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8" "_sv, binary_operator, u8" other"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"other"));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(name, u8": while (go());"_sv), no_diags, javascript_options);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"go"}));
    }
  }
}

TEST_F(Test_Parse_Expression_Statement,
       async_followed_by_newline_is_a_variable_reference) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"x = async\na => b;"_sv,
                                                no_diags, javascript_options);
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

TEST_F(Test_Parse_Expression_Statement,
       async_followed_by_newline_and_arrow_function) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"x = async\n(a) => b;"_sv,  //
        u8"    ^^^^^ Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List.async\n"_diag
        u8"               ^^ .arrow"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // a
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // b
                              "visit_exit_function_scope",        //
                              "visit_variable_assignment",        // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       trailing_comma_in_comma_expression_is_disallowed) {
  test_parse_and_visit_statement(
      u8"(a, b, );"_sv,  //
      u8"     ^ Diag_Missing_Operand_For_Operator"_diag);
}

TEST_F(Test_Parse_Expression_Statement,
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
  for (String8_View statement : {
           u8"break"_sv,
           u8"case"_sv,
           u8"const"_sv,
           u8"continue"_sv,
           u8"default"_sv,
           u8"export"_sv,
           u8"try"_sv,
           u8"var"_sv,
       }) {
    Test_Parser p(concat(u8"*\n"_sv, statement), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, Not(IsEmpty()));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       incomplete_unary_expression_with_following_statement_keyword) {
  for (String8_View statement : {
           u8"for(;x;);"_sv,
           u8"if(x);"_sv,
           u8"return x;"_sv,
           u8"throw x;"_sv,
           u8"while(x);"_sv,
       }) {
    Test_Parser p(concat(u8"!\n"_sv, statement), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"!\nswitch(x){}"_sv,  //
        u8"^ Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"!\nenum E {}"_sv,                           //
        u8"^ Diag_Missing_Operand_For_Operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement, let_is_an_identifier_if_escaped) {
  {
    // The following would instead be parsed as a declaration of a variable
    // named 'x':
    //
    //   let[x] = [y];
    //
    // Escaping the 'l' in 'let' forces it to behave as an identifier, not a
    // contextual keyword.
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"\\u{6c}et[x] = [y];"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"x", u8"y"}));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       let_expression_as_statement_body_is_allowed) {
  // do-while loops, for loops, if statements, while loops, and with statements
  // all disallow lexical declarations ('let x = y;') as their body. However,
  // they do allow expression statements in their body, including expression
  // statements starting with 'let'.

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"do let(); while (cond);"_sv,
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // cond
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"cond"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"for (;cond;) let = y;"_sv,
                                                no_diags, javascript_options);
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
    Spy_Visitor p =
        test_parse_and_visit_module(u8"if (cond) let++; else let ? a : b;"_sv,
                                    no_diags, javascript_options);
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

TEST_F(Test_Parse_Expression_Statement, let_as_statement_body_allows_asi) {
  // do-while loops, for loops, if statements, while loops, and with statements
  // all disallow lexical declarations ('let x = y;') as their body. However,
  // they do allow expression statements in their body, including expression
  // statements starting with 'let'.

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"do let\nwhile (cond);"_sv,
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                              "visit_variable_use",  // cond
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"cond"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"for (;cond;) let\nx = y;"_sv,
                                                no_diags, javascript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(u8"if (cond) let\nx = y;"_sv,
                                                no_diags, javascript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(u8"if (cond) let\nelse {}"_sv,
                                                no_diags, javascript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(
        u8"if (cond) {} else let\nx = y;"_sv, no_diags, javascript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(u8"while (cond) let\nx = y;"_sv,
                                                no_diags, javascript_options);
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
    Spy_Visitor p = test_parse_and_visit_module(u8"with (obj) let\nx = y;"_sv,
                                                no_diags, javascript_options);
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

TEST_F(Test_Parse_Expression_Statement,
       disallow_await_parameter_in_async_arrow_function) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(async (await) => null)"_sv,  //
        u8"        ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(async await => null)"_sv,  //
        u8"       ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",
                              "visit_variable_declaration",
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async(await)"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
  }

  test_parse_and_visit_expression(
      u8"(async (await, await, await) => {})"_sv,  //
      u8"                      ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,  //
      u8"               ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,  //
      u8"        ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(async (await p) => {})"_sv,  //
        u8"        ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag);
    // TODO(strager): We're ignoring 'p'. Should we treat it as a parameter?
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"await"_sv)}));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       object_property_default_is_not_allowed) {
  {
    Test_Parser p(u8"({key = value})"_sv, capture_diags);
    p.parse_and_visit_expression();
    // Important: 'key' should be treated as a literal (like in {key: value}),
    // thus shouldn't be visited.
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // value
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"value"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"      ^ Diag_Object_Literal_Default_In_Expression"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression_Statement,
       object_property_with_equals_is_allowed) {
  // {x: y = z} means something special when destructuring. However, in
  // expressions, it's the same as {x: (y = z)}.
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"({x: y = z})"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // z
                              "visit_variable_assignment",  // y
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"({x: y[index] = z})"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // z
                              "visit_variable_use",  // y
                              "visit_variable_use",  // index
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"z", u8"y", u8"index"}));
  }
}

TEST_F(Test_Parse_Expression_Statement, invalid_parentheses) {
  test_parse_and_visit_expression(
      u8"()"_sv,  //
      u8"^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren\n"_diag
      u8"^ .left_paren\n"_diag
      u8" ^ .right_paren"_diag);

  test_parse_and_visit_expression(
      u8"x = ()"_sv,  //
      u8"    ^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren\n"_diag
      u8"    ^ .left_paren\n"_diag
      u8"     ^ .right_paren"_diag);

  test_parse_and_visit_expression(
      u8"() = x"_sv,  //
      u8"^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren\n"_diag
      u8"^ .left_paren\n"_diag
      u8" ^ .right_paren"_diag);
}

TEST_F(Test_Parse_Expression_Statement,
       arrow_function_statement_requires_semicolon_or_asi) {
  test_parse_and_visit_module(
      u8"() => {} foo"_sv,  //
      u8"        ` Diag_Missing_Semicolon_After_Statement"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"() => {} //ASI\nfoo"_sv,
                                                no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_use",               // foo
                              "visit_end_of_module",              //
                          }));
  }

  test_parse_and_visit_statement(
      u8"async () => {} foo"_sv,  //
      u8"              ` Diag_Missing_Semicolon_After_Statement"_diag);

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"async () => {} //ASI\nfoo"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_use",               // foo
                              "visit_end_of_module",              //
                          }));
  }
}

TEST_F(Test_Parse_Expression_Statement,
       parenthesized_expression_requires_semicolon_or_asi) {
  test_parse_and_visit_module(
      u8"(2+2) foo"_sv,  //
      u8"     ` Diag_Missing_Semicolon_After_Statement"_diag);

  test_parse_and_visit_module(
      u8"if (true) { } else (2+2) foo"_sv,  //
      u8"                        ` Diag_Missing_Semicolon_After_Statement"_diag);
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
