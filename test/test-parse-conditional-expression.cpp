// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class test_parse_conditional_expression : public test_parse_expression {};

TEST_F(test_parse_conditional_expression, conditional_expression) {
  {
    test_parser p(u8"x?y:z"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::conditional);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_EQ(summarize(ast->child_2()), "var z");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"x+x?y+y:z+z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::conditional);
    EXPECT_EQ(summarize(ast->child_0()), "binary(var x, var x)");
    EXPECT_EQ(summarize(ast->child_1()), "binary(var y, var y)");
    EXPECT_EQ(summarize(ast->child_2()), "binary(var z, var z)");
  }

  {
    test_parser p(u8"a ? b : c ? d : e"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, cond(var c, var d, var e))");
  }

  {
    // Regression test: This code once failed to parse with TypeScript.
    test_parser p(u8"a ? !b : !c"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, unary(var b), unary(var c))");
  }
}

TEST_F(test_parse_conditional_expression,
       conditional_expression_with_missing_condition) {
  {
    test_parser p(u8"? b : c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(missing, var b, var c)");
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"?")));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"? b : c"));
  }
}

TEST_F(test_parse_conditional_expression,
       conditional_expression_with_missing_true_component) {
  {
    test_parser p(u8"a ? : c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, missing, var c)");
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"a "), u8"?")));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? : c"));
  }
}

TEST_F(test_parse_conditional_expression,
       conditional_expression_with_missing_false_component) {
  {
    test_parser p(u8"a ? b : "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"a ? b "), u8":")));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b : "));
  }

  {
    test_parser p(u8"(a ? b :)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(cond(var a, var b, missing))");
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"(a ? b "), u8":")));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"("), u8"a ? b :)"));
  }
}

TEST_F(test_parse_conditional_expression,
       conditional_expression_with_missing_colon_and_false_component) {
  {
    test_parser p(u8"a ? b "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_colon_in_conditional_expression,  //
                    expected_colon, strlen(u8"a ? b"), u8"",               //
                    question, strlen(u8"a "), u8"?")));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b"));
  }

  {
    test_parser p(u8"a ? b c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_colon_in_conditional_expression,  //
                    expected_colon, strlen(u8"a ? b"), u8"",               //
                    question, strlen(u8"a "), u8"?")));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b"));
  }

  {
    test_parser p(u8"(a ? b)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(cond(var a, var b, missing))");
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_colon_in_conditional_expression,  //
                    expected_colon, strlen(u8"(a ? b"), u8"",              //
                    question, strlen(u8"(a "), u8"?")));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"("), u8"a ? b"));
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
