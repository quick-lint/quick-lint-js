// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <string>
#include <string_view>

using ::testing::ElementsAreArray;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Expression_TypeScript : public Test_Parse_Expression {};

TEST_F(Test_Parse_Expression_TypeScript, type_annotation) {
  // These would normally appear in arrow function parameter lists.

  {
    Test_Parser p(u8"x: Type"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::Type_Annotated);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x: Type"_sv));

    Spy_Visitor v;
    expression_cast<Expression::Type_Annotated*>(ast)->visit_type_annotation(v);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_enter_type_scope",   //
                              "visit_variable_type_use",  //
                              "visit_exit_type_scope",    //
                          }));
    EXPECT_THAT(v.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Test_Parser p(u8"{x}: Type"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Type_Annotated);
    EXPECT_EQ(summarize(ast->child_0()), "object(literal: var x)");
  }

  {
    Test_Parser p(u8"[x]: Type"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Type_Annotated);
    EXPECT_EQ(summarize(ast->child_0()), "array(var x)");
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       conditional_colon_is_not_a_type_annotation) {
  {
    Test_Parser p(u8"cond ? x: Type"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, var x, var Type)");
  }

  {
    Test_Parser p(u8"cond ? t : param => body"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, var t, arrowfunc(var param))");
  }

  {
    Test_Parser p(u8"cond1 ? cond2 ? t2 : param => body : f1"_sv,
                  typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "cond(var cond1, cond(var cond2, var t2, arrowfunc(var param)), "
              "var f1)");
  }

  {
    Test_Parser p(u8"cond ? (t) : (<F />)"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "cond(var cond, paren(var t), paren(jsxelement(F)))");
  }

  {
    Test_Parser p(u8"cond ? ++t : f"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, rwunary(var t), var f)");
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       colon_in_conditional_can_be_arrow_return_type_annotation) {
  {
    Test_Parser p(u8"cond ? (param): ReturnType => body : f"_sv,
                  typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, arrowfunc(var param), var f)");
  }

  {
    Test_Parser p(u8"cond ? async (param): ReturnType => body : f"_sv,
                  typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "cond(var cond, asyncarrowfunc(var param), var f)");
  }
}

TEST_F(
    Test_Parse_Expression_TypeScript,
    colon_in_conditional_can_be_arrow_return_type_annotation_despite_following_syntax_error) {
  {
    // TypeScript resolves this ambiguity as a syntax error, so we should too.
    // TypeScript's rule seems to be that '(t2)' is not a parameter list if
    // 'body' is followed by a ':'.
    test_parse_and_visit_expression(
        u8"cond1 ? cond2 ? (t2) : param => body : f1"_sv,  //
        u8"                                         ` Diag_Missing_Colon_In_Conditional_Expression.expected_colon\n"_diag
        u8"      ^ .question"_diag,
        typescript_options);
  }
}

TEST_F(
    Test_Parse_Expression_TypeScript,
    colon_in_conditional_true_branch_cannot_be_arrow_return_type_annotation_if_arrow_body_not_followed_by_colon) {
  {
    Test_Parser p(u8"cond ? (t) : param => body"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "cond(var cond, paren(var t), arrowfunc(var param))");
  }

  {
    // This example triggers backtracking in the parser. Ensure that the
    // backtracking walks back any speculative visits.
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"cond ? (t) : param => body"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // cond,
                              "visit_variable_use",               // t
                          }));
  }
}

TEST_F(
    Test_Parse_Expression_TypeScript,
    colon_in_conditional_true_branch_cannot_be_type_annotation_if_not_arrow_function) {
  {
    Test_Parser p(u8"cond ? (t) : f"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, paren(var t), var f)");
  }
}

TEST_F(Test_Parse_Expression_TypeScript, non_null_assertion) {
  {
    Test_Parser p(u8"x!"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "nonnull(var x)");
  }

  {
    Test_Parser p(u8"f()!.someprop"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(nonnull(call(var f)), someprop)");
  }

  {
    Test_Parser p(u8"x! = y"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(nonnull(var x), var y)");
  }

  {
    Test_Parser p(u8"async!"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "nonnull(var async)");
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"f(x!);"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // f
                              "visit_variable_use",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"x! = null;"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_assignment",  // x
                          }));
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       non_null_assertion_does_not_allow_newline) {
  {
    // HACK(strager): We rely on the fact that parse_expression stops parsing at
    // the end of the line. "!+y" part is unparsed.
    Test_Parser p(u8"x\n!+y"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "var x");
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       non_null_assertion_not_allowed_in_javascript) {
  Test_Parser p(u8"x!"_sv, javascript_options, capture_diags);
  Expression* ast = p.parse_expression();
  EXPECT_EQ(summarize(ast), "nonnull(var x)");
  assert_diagnostics(
      p.code, p.errors,
      {
          u8" ^ Diag_TypeScript_Non_Null_Assertion_Not_Allowed_In_JavaScript"_diag,
      });
}

TEST_F(Test_Parse_Expression_TypeScript,
       as_type_assertion_not_allowed_in_javascript) {
  {
    Test_Parser p(u8"x as y"_sv, javascript_options, capture_diags);
    EXPECT_EQ(summarize(p.parse_expression()), "as(var x)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^ Diag_TypeScript_As_Type_Assertion_Not_Allowed_In_JavaScript"_diag,
        });
  }

  {
    Test_Parser p(u8"{} as const"_sv, javascript_options, capture_diags);
    EXPECT_EQ(summarize(p.parse_expression()), "as(object())");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"   ^^ Diag_TypeScript_As_Type_Assertion_Not_Allowed_In_JavaScript"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression_TypeScript, as_type_assertion) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"f(x as T);"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // as
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    //
                              "visit_variable_use",       // f
                              "visit_variable_use",       // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(lhs as T) = rhs;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",     // as
                              "visit_variable_type_use",    // T
                              "visit_exit_type_scope",      //
                              "visit_variable_use",         // rhs
                              "visit_variable_assignment",  // lhs
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"rhs"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"lhs"}));
  }

  {
    Test_Parser p(u8"x as y"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::As_Type_Assertion);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x as y"_sv));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // as
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }

  {
    Test_Parser p(u8"x as T ? y : z"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "cond(as(var x), var y, var z)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // as
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"x as (y)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "as(var x)");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }
}

TEST_F(Test_Parse_Expression_TypeScript, as_cannot_have_newline_before) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"f\nas(T);"_sv, no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // f
                              "visit_variable_use",   // as
                              "visit_variable_use",   // T
                              "visit_end_of_module",  //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"as", u8"T"}));
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       as_type_assertion_is_not_allowed_in_function_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(x as T) => {}"_sv,  //
        u8"   ^^ Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"([x, y, z] as T) => {}"_sv,  //
        u8"Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv),
                                  arrow_param_decl(u8"y"_sv),
                                  arrow_param_decl(u8"z"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f(x as T) {}"_sv,  //
        u8"             ^^ Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation"_diag,  //
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({func_param_decl(u8"x"_sv), function_decl(u8"f"_sv)}));
  }

  {
    Test_Parser p(u8"{} as const"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::As_Type_Assertion);
    EXPECT_EQ(summarize(ast->child_0()), "object()");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"{} as const"_sv));
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       as_const_is_allowed_for_certain_expressions) {
  for (const Char8* expression : {
           u8"MyEnum.MEMBER",
           u8"'string literal'",
           u8"\"string literal\"",
           u8"`untagged template`",
           u8"`untagged template with ${expression}`",
           u8"42",
           u8"42.0",
           u8"42n",
           u8"true",
           u8"false",
           u8"[]",
           u8"{}",
           u8"(((true)))",
           // Any expression is allowed inside array and object literals:
           u8"[null, x, f()]",
           u8"{k: v, [f()]: null}",
       }) {
    Padded_String code(expression + u8" as const"s);
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options);
    p.parse_and_visit_expression();
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       as_const_is_disallowed_for_most_expressions) {
  for (String8_View expression : {
           u8"/regexp literal/"_sv,
           u8"myVariable"_sv,
           u8"'string' as string"_sv,
           u8"'string' as const"_sv,
           u8"f()"_sv,
           u8"null"_sv,
           u8"f`tagged template`"_sv,
       }) {
    Padded_String code(concat(expression, u8" as const"s));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_TypeScript_As_Const_With_Non_Literal_Typeable,  //
                        expression, 0, expression,                           //
                        as_const, expression.size() + 1, u8"as const"_sv),
                }));
  }

  test_parse_and_visit_expression(
      u8"(f()) as const"_sv,  //
      u8" ^^^ Diag_TypeScript_As_Const_With_Non_Literal_Typeable.expression"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_Expression_TypeScript,
       satisfies_operator_not_allowed_in_javascript) {
  {
    Test_Parser p(u8"x satisfies y"_sv, javascript_options, capture_diags);
    EXPECT_EQ(summarize(p.parse_expression()), "satisfies(var x)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^^^^^^^^ Diag_TypeScript_Satisfies_Not_Allowed_In_JavaScript"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression_TypeScript, satisfies) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"f(x satisfies T);"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // satisfies
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    //
                              "visit_variable_use",       // f
                              "visit_variable_use",       // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(lhs satisfies T) = rhs;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",     // satisfies
                              "visit_variable_type_use",    // T
                              "visit_exit_type_scope",      //
                              "visit_variable_use",         // rhs
                              "visit_variable_assignment",  // lhs
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"rhs"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"lhs"}));
  }

  {
    Test_Parser p(u8"x satisfies y"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::Satisfies);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x satisfies y"_sv));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // satisfies
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }

  {
    Test_Parser p(u8"x satisfies T ? y : z"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "cond(satisfies(var x), var y, var z)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // satisfies
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"x satisfies (y)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "satisfies(var x)");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }
}

TEST_F(Test_Parse_Expression_TypeScript, satisfies_cannot_have_newline_before) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"f\nsatisfies(T);"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // f
                              "visit_variable_use",   // satisfies
                              "visit_variable_use",   // T
                              "visit_end_of_module",  //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"f", u8"satisfies", u8"T"}));
  }
}

TEST_F(Test_Parse_Expression_TypeScript,
       satisfies_is_not_allowed_in_function_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"(x satisfies T) => {}"_sv,  //
        u8"   ^^^^^^^^^ Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
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
