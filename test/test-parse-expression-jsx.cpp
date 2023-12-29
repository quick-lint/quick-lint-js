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
#include <quick-lint-js/util/cast.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Expression_JSX : public Test_Parse_Expression {};

TEST_F(Test_Parse_Expression_JSX, intrinsic_element) {
  {
    Test_Parser p(u8"<div />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div"_sv);
    EXPECT_TRUE(expression_cast<Expression::JSX_Element*>(ast)->is_intrinsic());
  }

  {
    Test_Parser p(u8"<\\u{64}iv />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"div"_sv);
    EXPECT_TRUE(expression_cast<Expression::JSX_Element*>(ast)->is_intrinsic());
  }

  {
    Test_Parser p(u8"<My-Web-Component />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(),
              u8"My-Web-Component"_sv);
    EXPECT_TRUE(expression_cast<Expression::JSX_Element*>(ast)->is_intrinsic());
  }
}

TEST_F(Test_Parse_Expression_JSX,
       intrinsic_element_can_be_named_const_in_typescript_jsx) {
  {
    Test_Parser p(u8"<const />"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(const)");
  }

  {
    Test_Parser p(u8"<const {...props}></const>"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(const, spread(var props))");
  }

  {
    Test_Parser p(u8"<const attr1 attr2={banana.chocolate}></const>"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(const, dot(var banana, chocolate))");
  }

  // NOTE(strager): The first prop cannot be assigned with '='. If it is,
  // TypeScript interprets the tag as a generic parameter list instead. See
  // NOTE[TypeScript-const-generic-arrow-with-default-string-type] and
  // NOTE[TypeScript-const-generic-arrow-with-default-object-type].
}

TEST_F(Test_Parse_Expression_JSX, user_element) {
  {
    Test_Parser p(u8"<MyComponent />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent"_sv);
    EXPECT_FALSE(
        expression_cast<Expression::JSX_Element*>(ast)->is_intrinsic());
  }

  {
    Test_Parser p(u8"<\\u{4d}yComponent />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"MyComponent"_sv);
    EXPECT_FALSE(
        expression_cast<Expression::JSX_Element*>(ast)->is_intrinsic());
  }
}

TEST_F(Test_Parse_Expression_JSX, self_closing_tag) {
  {
    Test_Parser p(u8"<div />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<div />"_sv));
  }

  {
    Test_Parser p(u8"<my-web-component/ >"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<my-web-component/ >"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_no_children) {
  {
    Test_Parser p(u8"<div></div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<div></div>"_sv));
  }

  {
    Test_Parser p(u8"<my-web-component>< / my-web-component>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::JSX_Element);
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(0, u8"<my-web-component>< / my-web-component>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_text_children) {
  {
    Test_Parser p(u8"<div>hello world</div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(0, u8"<div>hello world</div>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, fragment_with_no_children) {
  {
    Test_Parser p(u8"<></>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::JSX_Fragment);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<></>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, fragment_with_text_children) {
  {
    Test_Parser p(u8"<>hello world</>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxfragment()");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<>hello world</>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_element_children) {
  {
    Test_Parser p(u8"<div>hello <span>world</span>!</div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxelement(span))");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(u8""_sv.size(),
                                  u8"<div>hello <span>world</span>!</div>"_sv));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"<div>hello "_sv.size(),
                                  u8"<span>world</span>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_fragment_children) {
  {
    Test_Parser p(u8"<div>hello <>world</>!</div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxfragment())");
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(u8""_sv.size(), u8"<div>hello <>world</>!</div>"_sv));
    EXPECT_THAT(
        ast->child_0()->span(),
        p.matches_offsets(u8"<div>hello "_sv.size(), u8"<>world</>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, fragment_with_element_children) {
  {
    Test_Parser p(u8"<>hello <span>world</span>!</>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxfragment(jsxelement(span))");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(u8""_sv.size(),
                                  u8"<>hello <span>world</span>!</>"_sv));
    EXPECT_THAT(
        ast->child_0()->span(),
        p.matches_offsets(u8"<>hello "_sv.size(), u8"<span>world</span>"_sv));
  }

  {
    Test_Parser p(u8"<><span>hello</span><span>world</span></>"_sv,
                  jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(span), jsxelement(span))");
  }

  {
    Test_Parser p(u8"<><ul><li><a><span>hello</span></a></li></ul></>"_sv,
                  jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast),
              "jsxfragment(jsxelement(ul, jsxelement(li, jsxelement(a, "
              "jsxelement(span)))))");
  }
}

TEST_F(Test_Parse_Expression_JSX, fragment_with_fragment_children) {
  {
    Test_Parser p(u8"<>hello <>world</>!</>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxfragment(jsxfragment())");
    EXPECT_THAT(ast->span(), p.matches_offsets(u8""_sv.size(),
                                               u8"<>hello <>world</>!</>"_sv));
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"<>hello "_sv.size(), u8"<>world</>"_sv));
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_expression_children) {
  {
    Test_Parser p(u8"<div>hello {name}!</div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var name)");
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"<div>hello {"_sv.size(), u8"name"_sv));
  }

  {
    Test_Parser p(u8"<ul>{...listItems}</ul>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(ul, spread(var listItems))");
  }

  {
    Test_Parser p(u8"<div>{a}{b}{c}</div>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var a, var b, var c)");
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_attributes) {
  {
    Test_Parser p(u8"<div className='header' />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
    EXPECT_THAT(ast->span(),
                p.matches_offsets(0, u8"<div className='header' />"_sv));
  }

  {
    Test_Parser p(u8"<div className={expr} />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var expr)");
    EXPECT_THAT(ast->child_0()->span(),
                p.matches_offsets(u8"<div className={"_sv.size(), u8"expr"_sv));
  }

  {
    Test_Parser p(u8"<input type=\"checkbox\" checked />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(input)");
  }

  {
    Test_Parser p(u8"<div attr=<span /> />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxelement(span))");
  }

  {
    Test_Parser p(u8"<div attr=<span>{child}</span> />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxelement(span, var child))");
  }

  {
    Test_Parser p(u8"<div attr=<>{child}</> />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, jsxfragment(var child))");
  }

  {
    Test_Parser p(u8"<input {...attributes} />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(input, spread(var attributes))");
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_namespace_attributes) {
  {
    Test_Parser p(u8"<div custom:attr='val' />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
  }

  {
    Test_Parser p(u8"<div custom:attr />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div)");
  }

  {
    Test_Parser p(u8"<div custom:attr={value} />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var value)");
  }

  {
    Test_Parser p(u8"<div my-custom-ns-:my-attr-={value} />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(div, var value)");
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_namespace) {
  {
    Test_Parser p(u8"<svg:g />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
  }

  {
    Test_Parser p(u8"<svg:g></svg:g>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
  }

  {
    Test_Parser p(u8"<svg /* */ : /* */ g>< / svg : g >"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(svg, g)");
  }

  {
    Test_Parser p(u8"<s-v-g-:g-></s-v-g-:g->"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxnselement(s-v-g-, g-)");
  }
}

TEST_F(Test_Parse_Expression_JSX, tag_with_member_expression) {
  {
    Test_Parser p(u8"<mod.Component />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((mod, Component))");
  }

  {
    Test_Parser p(u8"<a.b.c.d.e>{child}</a.b.c.d.e>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((a, b, c, d, e), var child)");
  }

  {
    // TODO(strager): Report an error for the following code. Both Babel and
    // TypeScript fail to transpile.
    Test_Parser p(u8"<a-b.c-d></a-b.c-d>"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxmemberelement((a-b, c-d))");
  }
}

TEST_F(Test_Parse_Expression_JSX, jsx_with_binary_operator) {
  {
    Test_Parser p(u8"x && <div />"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "binary(var x, jsxelement(div))");
  }
}

TEST_F(Test_Parse_Expression_JSX,
       element_which_almost_looks_like_typescript_generic_parameter_list) {
  {
    Test_Parser p(u8"<T extends />"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T)");
  }

  {
    Test_Parser p(u8"<T extends></T>"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T)");
  }

  {
    Test_Parser p(u8"<T extends=\"val\"></T>"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T)");
  }

  {
    Test_Parser p(u8"<T extends={val}></T>"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T, var val)");
  }

  {
    Test_Parser p(u8"<const T extends />"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(const)");
  }
}

TEST_F(Test_Parse_Expression_JSX, greater_greater_token_is_split) {
  {
    Test_Parser p(u8"<A attr=<B />> </A>"_sv, jsx_options);
    //                           ^^ Token should be split into two '>'s.
    Expression* ast = p.parse_expression();
    ASSERT_EQ(summarize(ast), "jsxelement(A, jsxelement(B))");
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
