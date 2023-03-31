// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/cli/cli-location.h>
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
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class test_parse_expression_typescript : public test_parse_expression {};

TEST_F(test_parse_expression_typescript, type_annotation) {
  // These would normally appear in arrow function parameter lists.

  {
    test_parser p(u8"x: Type"_sv, typescript_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x: Type"_sv));

    spy_visitor v;
    static_cast<expression::type_annotated*>(ast)->visit_type_annotation(v);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(v.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    test_parser p(u8"{x}: Type"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "object(literal: var x)");
  }

  {
    test_parser p(u8"[x]: Type"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "array(var x)");
  }
}

TEST_F(test_parse_expression_typescript,
       conditional_colon_is_not_a_type_annotation) {
  {
    test_parser p(u8"cond ? x: Type"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var cond, var x, var Type)");
  }
}

TEST_F(test_parse_expression_typescript, non_null_assertion) {
  {
    test_parser p(u8"x!"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "nonnull(var x)");
  }

  {
    test_parser p(u8"f()!.someprop"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(nonnull(call(var f)), someprop)");
  }

  {
    test_parser p(u8"x! = y"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(nonnull(var x), var y)");
  }

  {
    test_parser p(u8"async!"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "nonnull(var async)");
  }

  {
    test_parser p(u8"f(x!);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // f
                              "visit_variable_use",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"x"}));
  }

  {
    test_parser p(u8"x! = null;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_assignment",  // x
                          }));
  }
}

TEST_F(test_parse_expression_typescript,
       non_null_assertion_does_not_allow_newline) {
  {
    // HACK(strager): We rely on the fact that parse_expression stops parsing at
    // the end of the line. "!+y" part is unparsed.
    test_parser p(u8"x\n!+y"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "var x");
  }
}

TEST_F(test_parse_expression_typescript,
       non_null_assertion_not_allowed_in_javascript) {
  test_parser p(u8"x!"_sv, javascript_options, capture_diags);
  expression* ast = p.parse_expression();
  EXPECT_EQ(summarize(ast), "nonnull(var x)");
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(
              p.code,
              diag_typescript_non_null_assertion_not_allowed_in_javascript,  //
              bang, strlen(u8"x"), u8"!"_sv),
      }));
}

TEST_F(test_parse_expression_typescript,
       as_type_assertion_not_allowed_in_javascript) {
  {
    test_parser p(u8"x as y"_sv, javascript_options, capture_diags);
    EXPECT_EQ(summarize(p.parse_expression()), "as(var x)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_as_type_assertion_not_allowed_in_javascript,  //
                as_keyword, strlen(u8"x "), u8"as"_sv),
        }));
  }

  {
    test_parser p(u8"{} as const"_sv, javascript_options, capture_diags);
    EXPECT_EQ(summarize(p.parse_expression()), "as(object())");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_as_type_assertion_not_allowed_in_javascript,  //
                as_keyword, strlen(u8"{} "), u8"as"_sv),
        }));
  }
}

TEST_F(test_parse_expression_typescript, as_type_assertion) {
  {
    test_parser p(u8"f(x as T);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_use",       // f
                              "visit_variable_use",       // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f", u8"x"}));
  }

  {
    test_parser p(u8"(lhs as T) = rhs;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",    // T
                              "visit_variable_use",         // rhs
                              "visit_variable_assignment",  // lhs
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"rhs"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"lhs"}));
  }

  {
    test_parser p(u8"x as y"_sv, typescript_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::as_type_assertion);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x as y"_sv));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
  }

  {
    test_parser p(u8"x as T ? y : z"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "cond(as(var x), var y, var z)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(test_parse_expression_typescript,
       as_type_assertion_is_not_allowed_in_function_parameter_list) {
  {
    test_parser p(u8"(x as T) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_as_keyword_used_for_parameter_type_annotation,  //
                as_keyword, strlen(u8"(x "), u8"as"_sv),
        }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"([x, y, z] as T) => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                diag_typescript_as_keyword_used_for_parameter_type_annotation),
        }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv),
                                  arrow_param_decl(u8"y"_sv),
                                  arrow_param_decl(u8"z"_sv)}));
  }

  {
    test_parser p(u8"function f(x as T) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_as_keyword_used_for_parameter_type_annotation,  //
                as_keyword, strlen(u8"function f(x "), u8"as"_sv),
        }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), func_param_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"{} as const"_sv, typescript_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::as_type_assertion);
    EXPECT_EQ(summarize(ast->child_0()), "object()");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"{} as const"_sv));
  }
}

TEST_F(test_parse_expression_typescript,
       as_const_is_allowed_for_certain_expressions) {
  for (const char8* expression : {
           u8"MyEnum.MEMBER",
           u8"'string literal'",
           u8"\"string literal\"",
           u8"`untagged template`",
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
    padded_string code(expression + u8" as const"s);
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_expression();
  }
}

TEST_F(test_parse_expression_typescript,
       as_const_is_disallowed_for_most_expressions) {
  for (const char8* expression : {
           u8"/regexp literal/",
           u8"myVariable",
           u8"'string' as string",
           u8"'string' as const",
           u8"f()",
           u8"null",
           u8"f`tagged template`",
       }) {
    padded_string code(expression + u8" as const"s);
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_typescript_as_const_with_non_literal_typeable,  //
                        expression, 0, expression,                           //
                        as_const, strlen(expression) + 1, u8"as const"_sv),
                }));
  }

  {
    test_parser p(u8"(f()) as const"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_as_const_with_non_literal_typeable,  //
                        expression, strlen(u8"("), u8"f()"_sv),
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
