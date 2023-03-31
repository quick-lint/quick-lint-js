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
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;
using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class test_parse_typescript_angle_type_assertion
    : public test_parse_expression {};

TEST_F(test_parse_typescript_angle_type_assertion, angle_type_assertion) {
  {
    test_parser p(u8"<Type>expr"_sv, typescript_options);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::angle_type_assertion);
    EXPECT_EQ(summarize(ast->child_0()), "var expr");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<Type>expr"_sv));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    test_parser p(u8"<Type>(expr)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeassert(paren(var expr))");
  }

  {
    test_parser p(u8"<Type>expr;\n// </Type>;"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeassert(var expr)")
        << "'<Type>' shouldn't be confused as an opening JSX tag";
  }

  {
    test_parser p(u8"f(<T>x);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_use",       // f
                              "visit_variable_use",       // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f", u8"x"}));
  }

  {
    test_parser p(u8"(<T>lhs) = rhs;"_sv, typescript_options);
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
    test_parser p(u8"<Type1 | Type2>(expr);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Type1", u8"Type2", u8"expr"}));
  }

  for (string8_view code : {
           u8"<Type>(expr);"_sv,
           u8"<(Type)>(expr);"_sv,
           u8"< | Type>(expr);"_sv,
           u8"< & Type>(expr);"_sv,
           u8"<[Type]>(expr);"_sv,
           u8"<{k: Type}>(expr);"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type", u8"expr"}));
  }

  for (const string8& type :
       typescript_builtin_type_keywords | typescript_special_type_keywords) {
    test_parser p(concat(u8"<"_sv, type, u8">expr;"_sv), typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // expr
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"expr"}));
  }
}

TEST_F(test_parse_typescript_angle_type_assertion,
       angle_type_assertion_with_parenthesized_arrow) {
  // NOTE(strager): The following syntax is a generic arrow function, not an
  // angle type assertion: <Type>(param) => body

  {
    test_parser p(u8"<Type>(param => body)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "typeassert(paren(arrowfunc(var param)))");
  }

  {
    test_parser p(u8"<Type>((param) => body)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "typeassert(paren(arrowfunc(var param)))");
  }
}

TEST_F(test_parse_typescript_angle_type_assertion,
       angle_type_assertion_in_complex_expression) {
  {
    test_parser p(u8"<Type>x ? y : z"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(typeassert(var x), var y, var z)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(test_parse_typescript_angle_type_assertion,
       angle_bracketed_type_assertion_is_jsx_tag_in_typescript_jsx_mode) {
  {
    test_parser p(u8"<Component>text;\n// </Component>;"_sv,
                  typescript_jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(Component)");
  }

  for (const string8& tag : keywords) {
    test_parser p(concat(u8"<"_sv, tag, u8">text;\n// </"_sv, tag, u8">;"_sv),
                  typescript_jsx_options);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(" + to_string(tag) + ")");
  }
}

TEST_F(test_parse_typescript_angle_type_assertion,
       angle_type_assertion_with_complex_type_is_error_in_typescript_jsx_mode) {
  {
    test_parser p(u8"<Type1 | Type2>(expr);"_sv, typescript_jsx_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_angle_type_assertion_not_allowed_in_tsx,  //
                bracketed_type, 0, u8"<Type1 | Type2>"_sv, expected_as,
                strlen(u8"<Type1 | Type2>(expr)"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"<(Type)>expr;"_sv, typescript_jsx_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_angle_type_assertion_not_allowed_in_tsx,  //
                bracketed_type, 0, u8"<(Type)>"_sv, expected_as,
                strlen(u8"<(Type)>expr"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_typescript_angle_type_assertion,
       angle_type_assertion_is_not_allowed_in_function_parameter_list) {
  {
    test_parser p(u8"(<T>x) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code,
                                      diag_invalid_parameter,  //
                                      parameter, strlen(u8"("), u8"<T>x"_sv),
                }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"function f(<T>x) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_invalid_parameter,  //
                              parameter, strlen(u8"function f("), u8"<T>x"_sv),
        }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), func_param_decl(u8"x"_sv)}));
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
