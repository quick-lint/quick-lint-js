// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Generic_Arrow : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Generic_Arrow, generic_arrow_function) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type>() => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // Type
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"Type"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type>(param) => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"Type"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type>(param): ReturnType => {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // Type
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"Type"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
  }
}

// In non-JSX mode, this is an assignment with a generic arrow function:
//
//   f = <T>(p) => {}
//
// In JSX mode, this is two assignments (joined with ','), one with a JSX
// element and another with a non-generic arrow function:
//
//   f = <T>(text)</T>,
//   f = () => {}
String8_View ambiguous_jsx_generic_arrow =
    u8R"(
    f = <T>(
        p // </T>, f = (
    ) => {}
)"_sv;

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_function_in_ts_mode_can_look_like_jsx_element) {
  {
    Test_Parser p(ambiguous_jsx_generic_arrow, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var f, arrowfunc(var p))");
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       jsx_element_in_tsx_mode_can_look_like_generic_arrow_function) {
  {
    Test_Parser p(ambiguous_jsx_generic_arrow, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(
        summarize(ast),
        "binary(assign(var f, jsxelement(T)), assign(var f, arrowfunc()))");
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       unambiguous_generic_arrow_is_not_allowed_in_tsx) {
  {
    SCOPED_TRACE(
        "'>' is invalid in JSX, so this code cannot be interpreted as legal "
        "JSX");
    Test_Parser p(u8"<T>() => {body} // </T>"_sv, typescript_jsx_options,
                  capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            // <T>() => {body} // </T>
            u8"      ^^ Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode.arrow\n"_diag
            u8"  `      .expected_comma\n"_diag
            u8"^        .generic_parameters_less"_diag,
        });
  }

  // TODO(strager): <T>({}) => {}         // '{' appears before '=>'.
  // TODO(strager): <T>(x = y > z) => {}  // '>' appears before '=>'.
  // TODO(strager): <T>(x = y < z) => {}  // '<' appears before '=>'.
  // TODO(strager): <T>(f)() => </T>      // Not an arrow function.
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       unambiguous_generic_arrow_is_treated_as_jsx_in_non_typescript) {
  {
    Test_Parser p(u8"<T>() => {body} // </T>"_sv, jsx_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T, var body)");
    EXPECT_THAT(p.visits, IsEmpty());
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^ Diag_Unexpected_Greater_In_JSX_Text"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       arrow_without_parentheses_in_tsx_is_interpreted_as_jsx_element) {
  {
    Test_Parser p(u8"<T>param => {body} // </T>"_sv, typescript_jsx_options,
                  capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T, var body)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"          ^ Diag_Unexpected_Greater_In_JSX_Text"_diag,
        });
  }
}

// TypeScript rejects this code. This seems like a TypeScript compiler bug to
// me...
TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_async_arrow_with_lone_parameter_is_not_allowed_in_tsx) {
  {
    Test_Parser p(u8"async <T>() => {}"_sv, typescript_jsx_options,
                  capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            // async <T>() => {}
            u8"            ^^ Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode.arrow\n"_diag
            u8"        `      .expected_comma\n"_diag
            u8"      ^        .generic_parameters_less"_diag,
        });
  }

  {
    Test_Parser p(u8"async <T>(): ReturnType => {}"_sv, typescript_jsx_options,
                  capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            // async <T>(): ReturnType => {}
            u8"                        ^^ Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode.arrow\n"_diag
            u8"        `                  .expected_comma\n"_diag
            u8"      ^                    .generic_parameters_less"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_comma_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"<T,>(param) => {}"_sv, no_diags, o);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // T
                              "visit_variable_declaration",       // param
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
  }

  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<T,>(): ReturnType => {}"_sv, no_diags, o);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // param
                              "visit_enter_type_scope",           // :
                              "visit_variable_type_use",          // ReturnType
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_extends_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<T extends U>(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // T
                                "visit_enter_type_scope",           // extends
                                "visit_variable_type_use",          // U
                                "visit_exit_type_scope",            //
                                "visit_variable_declaration",       // param
                                "visit_enter_function_scope_body",  // {
                                "visit_exit_function_scope",        // }
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<T extends {notProps}>(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.variable_uses, IsEmpty());
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"async <T extends U>() => { await myPromise; }"_sv, no_diags, o);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // T
                                "visit_enter_type_scope",           // extends
                                "visit_variable_type_use",          // U
                                "visit_exit_type_scope",            //
                                "visit_enter_function_scope_body",  // {
                                "visit_variable_use",               // myPromise
                                "visit_exit_function_scope",        // }
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U", u8"myPromise"}));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_default_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<T = U>(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_enter_type_scope",           // =
                                "visit_variable_type_use",          // U
                                "visit_exit_type_scope",            //
                                "visit_variable_declaration",       // T
                                "visit_variable_declaration",       // param
                                "visit_enter_function_scope_body",  // {
                                "visit_exit_function_scope",        // }
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"async <T = U>() => { await myPromise; }"_sv, no_diags, o);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_enter_type_scope",           // =
                                "visit_variable_type_use",          // U
                                "visit_exit_type_scope",            //
                                "visit_variable_declaration",       // T
                                "visit_enter_function_scope_body",  // {
                                "visit_variable_use",               // myPromise
                                "visit_exit_function_scope",        // }
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U", u8"myPromise"}));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_const_and_comma_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<const T,>(param) => {}"_sv, no_diags, o);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_const_and_extends_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<const T extends U>(param) => {}"_sv, no_diags, o);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  arrow_param_decl(u8"param"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_arrow_with_const_and_default_is_allowed_in_tsx) {
  for (const Parser_Options& o : {typescript_options, typescript_jsx_options}) {
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<const T = U>(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }

    // NOTE[TypeScript-const-generic-arrow-with-default-string-type]:
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<const T=\"value\">(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }

    // NOTE[TypeScript-const-generic-arrow-with-default-object-type]:
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          u8"<const T={}>(param) => {}"_sv, no_diags, o);
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(u8"T"_sv),
                                    arrow_param_decl(u8"param"_sv)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Generic_Arrow,
       generic_async_function_with_await_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"await <T>() => { await(myPromise); }"_sv,
        u8"^^^^^ Diag_Await_Followed_By_Arrow_Function.await_operator"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}));
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
