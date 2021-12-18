// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
TEST_F(test_parse_expression, intrinsic_element) {
  {
    expression* ast = this->parse_expression(u8"<div />"_sv, jsx_options);
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    expression* ast = this->parse_expression(u8"<\\u{64}iv />"_sv, jsx_options);
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    expression* ast =
        this->parse_expression(u8"<My-Web-Component />"_sv, jsx_options);
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(),
              u8"My-Web-Component");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }
}

TEST_F(test_parse_expression, user_element) {
  {
    expression* ast =
        this->parse_expression(u8"<MyComponent />"_sv, jsx_options);
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent");
    EXPECT_FALSE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    expression* ast =
        this->parse_expression(u8"<\\u{4d}yComponent />"_sv, jsx_options);
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent");
    EXPECT_FALSE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }
}

TEST_F(test_parse_expression, self_closing_tag) {
  {
    test_parser p(u8"<div />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<div />"));
  }

  {
    test_parser p(u8"<my-web-component/ >"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<my-web-component/ >"));
  }
}

TEST_F(test_parse_expression, tag_with_no_children) {
  {
    test_parser p(u8"<div></div>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<div></div>"));
  }

  {
    test_parser p(u8"<my-web-component>< / my-web-component>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(),
              strlen(u8"<my-web-component>< / my-web-component>"));
  }
}

TEST_F(test_parse_expression, fragment_with_no_children) {
  {
    test_parser p(u8"<></>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_fragment);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<></>"));
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
