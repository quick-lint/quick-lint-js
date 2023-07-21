// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
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
#include <vector>

using ::testing::ElementsAreArray;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Conditional_Expression
    : public Test_Parse_Expression,
      public ::testing::WithParamInterface<Parser_Options> {};

TEST_P(Test_Parse_Conditional_Expression, conditional_expression) {
  {
    Test_Parser p(u8"x?y:z"_sv, GetParam());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Conditional);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_EQ(summarize(ast->child_2()), "var z");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }

  {
    Test_Parser p(u8"x+x?y+y:z+z"_sv, GetParam());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Conditional);
    EXPECT_EQ(summarize(ast->child_0()), "binary(var x, var x)");
    EXPECT_EQ(summarize(ast->child_1()), "binary(var y, var y)");
    EXPECT_EQ(summarize(ast->child_2()), "binary(var z, var z)");
  }

  {
    Test_Parser p(u8"a ? b : c ? d : e"_sv, GetParam());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, cond(var c, var d, var e))");
  }

  {
    // Regression test: This code once failed to parse with TypeScript.
    Test_Parser p(u8"a ? !b : !c"_sv, GetParam());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, unary(var b), unary(var c))");
  }

  {
    // Regression test: This code once failed to parse with TypeScript.
    Test_Parser p(u8"a ? () => foo : c"_sv, GetParam());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, arrowfunc(), var c)");
  }
}

TEST_P(Test_Parse_Conditional_Expression,
       conditional_expression_with_missing_condition) {
  {
    Test_Parser p(u8"? b : c"_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(missing, var b, var c)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Operand_For_Operator,  //
                              where, 0, u8"?"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"? b : c"_sv));
  }
}

TEST_P(Test_Parse_Conditional_Expression,
       conditional_expression_with_missing_true_component) {
  {
    Test_Parser p(u8"a ? : c"_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, missing, var c)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Operand_For_Operator,  //
                              where, u8"a "_sv.size(), u8"?"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? : c"_sv));
  }
}

TEST_P(Test_Parse_Conditional_Expression,
       conditional_expression_with_missing_false_component) {
  {
    Test_Parser p(u8"a ? b : "_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Operand_For_Operator,  //
                              where, u8"a ? b "_sv.size(), u8":"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b : "_sv));
  }

  {
    Test_Parser p(u8"(a ? b :)"_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(cond(var a, var b, missing))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Operand_For_Operator,  //
                              where, u8"(a ? b "_sv.size(), u8":"_sv),
        }));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"("_sv.size(), u8"a ? b :)"_sv));
  }
}

TEST_P(Test_Parse_Conditional_Expression,
       conditional_expression_with_missing_colon_and_false_component) {
  {
    Test_Parser p(u8"a ? b "_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, Diag_Missing_Colon_In_Conditional_Expression,  //
                expected_colon, u8"a ? b"_sv.size(), u8""_sv, question,
                u8"a "_sv.size(), u8"?"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b"_sv));
  }

  {
    Test_Parser p(u8"a ? b c"_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, Diag_Missing_Colon_In_Conditional_Expression,  //
                expected_colon, u8"a ? b"_sv.size(), u8""_sv, question,
                u8"a "_sv.size(), u8"?"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"a ? b"_sv));
  }

  {
    Test_Parser p(u8"(a ? b)"_sv, GetParam(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(cond(var a, var b, missing))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, Diag_Missing_Colon_In_Conditional_Expression,  //
                expected_colon, u8"(a ? b"_sv.size(), u8""_sv, question,
                u8"(a "_sv.size(), u8"?"_sv),
        }));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"("_sv.size(), u8"a ? b"_sv));
  }
}

INSTANTIATE_TEST_SUITE_P(javascript, Test_Parse_Conditional_Expression,
                         ::testing::Values(javascript_options));

// In TypeScript, ':' is used for both type annotations in arrow functions and
// for the conditional operator. Ensure we parse all of the examples as
// conditional operators.
INSTANTIATE_TEST_SUITE_P(typescript, Test_Parse_Conditional_Expression,
                         ::testing::Values(typescript_options));
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
