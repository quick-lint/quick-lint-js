// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class test_parse_expression_typescript : public test_parse_expression {
 public:
  using test_parse_expression::make_parser;
  using test_parse_expression::parse_expression;

  test_parser& make_parser(string8_view input) {
    return this->make_parser(input, typescript_options);
  }

  expression* parse_expression(string8_view input) {
    return this->parse_expression(input, typescript_options);
  }
};

TEST_F(test_parse_expression_typescript, type_annotation) {
  // These would normally appear in arrow function parameter lists.

  {
    test_parser& p = this->make_parser(u8"x: Type"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->type_annotation()), "var Type");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"x: Type"));
  }

  {
    expression* ast = this->parse_expression(u8"{x}: Type"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "object(literal: var x)");
    EXPECT_EQ(summarize(ast->type_annotation()), "var Type");
  }

  {
    expression* ast = this->parse_expression(u8"[x]: Type"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::type_annotated);
    EXPECT_EQ(summarize(ast->child_0()), "array(var x)");
    EXPECT_EQ(summarize(ast->type_annotation()), "var Type");
  }
}

TEST_F(test_parse_expression_typescript,
       conditional_colon_is_not_a_type_annotation) {
  {
    expression* ast = this->parse_expression(u8"cond ? x: Type"_sv);
    EXPECT_EQ(summarize(ast), "cond(var cond, var x, var Type)");
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
