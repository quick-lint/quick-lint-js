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
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class test_parse_expression_jsx : public test_parse_expression {};

TEST_F(test_parse_expression_jsx, intrinsic_element) {
  {
    test_parser p(u8"<div />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    test_parser p(u8"<\\u{64}iv />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    test_parser p(u8"<My-Web-Component />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(),
              u8"My-Web-Component");
    EXPECT_TRUE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }
}

TEST_F(test_parse_expression_jsx, user_element) {
  {
    test_parser p(u8"<MyComponent />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent");
    EXPECT_FALSE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }

  {
    test_parser p(u8"<\\u{4d}yComponent />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent");
    EXPECT_FALSE(static_cast<expression::jsx_element*>(ast)->is_intrinsic());
  }
}

TEST_F(test_parse_expression_jsx, self_closing_tag) {
  {
    test_parser p(u8"<div />"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<div />"_sv));
  }

  {
    test_parser p(u8"<my-web-component/ >"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<my-web-component/ >"_sv));
  }
}

TEST_F(test_parse_expression_jsx, tag_with_no_children) {
  {
    test_parser p(u8"<div></div>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<div></div>"_sv));
  }

  {
    test_parser p(u8"<my-web-component>< / my-web-component>"_sv, jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_element);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(0, u8"<my-web-component>< / my-web-component>"_sv));
  }
}

TEST_F(test_parse_expression_jsx, tag_with_text_children) {
  {
    test_parser p(u8"<div>hello world</div>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(0, u8"<div>hello world</div>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_jsx, fragment_with_no_children) {
  {
    test_parser p(u8"<></>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::jsx_fragment);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<></>"_sv));
  }
}

TEST_F(test_parse_expression_jsx, fragment_with_text_children) {
  {
    test_parser p(u8"<>hello world</>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxfragment()");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<>hello world</>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_jsx, tag_with_element_children) {
  {
    test_parser p(u8"<div>hello <span>world</span>!</div>"_sv, jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxelement(span))");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(strlen(u8""),
                                  u8"<div>hello <span>world</span>!</div>"_sv));
    EXPECT_THAT(
        ast->child_0()->span(),
        p.matches_offsets(strlen(u8"<div>hello "), u8"<span>world</span>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_jsx, tag_with_fragment_children) {
  {
    test_parser p(u8"<div>hello <>world</>!</div>"_sv, jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxfragment())");
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(strlen(u8""), u8"<div>hello <>world</>!</div>"_sv));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"<div>hello "), u8"<>world</>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_jsx, fragment_with_element_children) {
  {
    test_parser p(u8"<>hello <span>world</span>!</>"_sv, jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxfragment(jsxelement(span))");
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(strlen(u8""), u8"<>hello <span>world</span>!</>"_sv));
    EXPECT_THAT(
        ast->child_0()->span(),
        p.matches_offsets(strlen(u8"<>hello "), u8"<span>world</span>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"<><span>hello</span><span>world</span></>"_sv,
                  jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(span), jsxelement(span))");
  }

  {
    test_parser p(u8"<><ul><li><a><span>hello</span></a></li></ul></>"_sv,
                  jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(ul, jsxelement(li, jsxelement(a, "
              "jsxelement(span)))))");
  }
}

TEST_F(test_parse_expression_jsx, fragment_with_fragment_children) {
  {
    test_parser p(u8"<>hello <>world</>!</>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxfragment(jsxfragment())");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(strlen(u8""), u8"<>hello <>world</>!</>"_sv));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"<>hello "), u8"<>world</>"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression_jsx, tag_with_expression_children) {
  {
    test_parser p(u8"<div>hello {name}!</div>"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var name)");
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"<div>hello {"), u8"name"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"<ul>{...listItems}</ul>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(ul, spread(var listItems))");
  }

  {
    test_parser p(u8"<div>{a}{b}{c}</div>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var a, var b, var c)");
  }
}

TEST_F(test_parse_expression_jsx, tag_with_attributes) {
  {
    test_parser p(u8"<div className='header' />"_sv, jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(0, u8"<div className='header' />"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"<div className={expr} />"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var expr)");
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(strlen(u8"<div className={"), u8"expr"_sv));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"<input type=\"checkbox\" checked />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(input)");
  }

  {
    test_parser p(u8"<input {...attributes} />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(input, spread(var attributes))");
  }
}

TEST_F(test_parse_expression_jsx, tag_with_namespace_attributes) {
  {
    test_parser p(u8"<div custom:attr='val' />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
  }

  {
    test_parser p(u8"<div custom:attr />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
  }

  {
    test_parser p(u8"<div custom:attr={value} />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var value)");
  }

  {
    test_parser p(u8"<div my-custom-ns-:my-attr-={value} />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var value)");
  }
}

TEST_F(test_parse_expression_jsx, tag_with_namespace) {
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

  {
    test_parser p(u8"<s-v-g-:g-></s-v-g-:g->"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(s-v-g-, g-)");
  }
}

TEST_F(test_parse_expression_jsx, tag_with_member_expression) {
  {
    test_parser p(u8"<mod.Component />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((mod, Component))");
  }

  {
    test_parser p(u8"<a.b.c.d.e>{child}</a.b.c.d.e>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((a, b, c, d, e), var child)");
  }

  {
    // TODO(strager): Report an error for the following code. Both Babel and
    // TypeScript fail to transpile.
    test_parser p(u8"<a-b.c-d></a-b.c-d>"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((a-b, c-d))");
  }
}

TEST_F(test_parse_expression_jsx, jsx_with_binary_operator) {
  {
    test_parser p(u8"x && <div />"_sv, jsx_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "binary(var x, jsxelement(div))");
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
