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

TEST_F(test_parse_expression, tag_with_text_children) {
  {
    test_parser p(u8"<div>hello world</div>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<div>hello world</div>"));
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

TEST_F(test_parse_expression, fragment_with_text_children) {
  {
    test_parser p(u8"<>hello world</>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxfragment()");
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"<>hello world</>"));
  }
}

TEST_F(test_parse_expression, tag_with_element_children) {
  {
    test_parser p(u8"<div>hello <span>world</span>!</div>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxelement(span))");
    EXPECT_EQ(p.range(ast).begin_offset(), strlen(u8""));
    EXPECT_EQ(p.range(ast).end_offset(),
              strlen(u8"<div>hello <span>world</span>!</div>"));
    EXPECT_EQ(p.range(ast->child_0()).begin_offset(), strlen(u8"<div>hello "));
    EXPECT_EQ(p.range(ast->child_0()).end_offset(),
              strlen(u8"<div>hello <span>world</span>"));
  }
}

TEST_F(test_parse_expression, fragment_with_element_children) {
  {
    test_parser p(u8"<>hello <span>world</span>!</>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxfragment(jsxelement(span))");
    EXPECT_EQ(p.range(ast).begin_offset(), strlen(u8""));
    EXPECT_EQ(p.range(ast).end_offset(),
              strlen(u8"<>hello <span>world</span>!</>"));
    EXPECT_EQ(p.range(ast->child_0()).begin_offset(), strlen(u8"<>hello "));
    EXPECT_EQ(p.range(ast->child_0()).end_offset(),
              strlen(u8"<>hello <span>world</span>"));
  }

  {
    expression* ast = this->parse_expression(
        u8"<><span>hello</span><span>world</span></>"_sv, jsx_options);
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(span), jsxelement(span))");
  }

  {
    expression* ast = this->parse_expression(
        u8"<><ul><li><a><span>hello</span></a></li></ul></>"_sv, jsx_options);
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(ul, jsxelement(li, jsxelement(a, "
              "jsxelement(span)))))");
  }
}

TEST_F(test_parse_expression, tag_with_expression_children) {
  {
    test_parser p(u8"<div>hello {name}!</div>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var name)");
    EXPECT_EQ(p.range(ast->child_0()).begin_offset(), strlen(u8"<div>hello {"));
    EXPECT_EQ(p.range(ast->child_0()).end_offset(),
              strlen(u8"<div>hello {name"));
  }

  {
    expression* ast =
        this->parse_expression(u8"<ul>{...listItems}</ul>"_sv, jsx_options);
    ASSERT_EQ(summarize(ast), "jsxelement(ul, spread(var listItems))");
  }

  {
    expression* ast =
        this->parse_expression(u8"<div>{a}{b}{c}</div>"_sv, jsx_options);
    ASSERT_EQ(summarize(ast), "jsxelement(div, var a, var b, var c)");
  }
}

TEST_F(test_parse_expression, tag_with_attributes) {
  {
    test_parser p(u8"<div className='header' />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(),
              strlen(u8"<div className='header' />"));
  }

  {
    test_parser p(u8"<div className={expr} />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var expr)");
    EXPECT_EQ(p.range(ast->child_0()).begin_offset(),
              strlen(u8"<div className={"));
    EXPECT_EQ(p.range(ast->child_0()).end_offset(),
              strlen(u8"<div className={expr"));
  }

  {
    expression* ast = this->parse_expression(
        u8"<input type=\"checkbox\" checked />"_sv, jsx_options);
    ASSERT_EQ(summarize(ast), "jsxelement(input)");
  }

  {
    expression* ast =
        this->parse_expression(u8"<input {...attributes} />"_sv, jsx_options);
    ASSERT_EQ(summarize(ast), "jsxelement(input, spread(var attributes))");
  }
}

TEST_F(test_parse_expression, tag_with_namespace) {
  {
    test_parser p(u8"<svg:g />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
  }

  {
    test_parser p(u8"<svg:g></svg:g>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
  }

  {
    test_parser p(u8"<svg /* */ : /* */ g>< / svg : g >"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
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
