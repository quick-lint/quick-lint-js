// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_generic_arrow : public test_parse_expression {};

TEST_F(test_parse_typescript_generic_arrow, generic_arrow_function) {
  {
    test_parser p(u8"<Type>() => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // Type
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"Type")));
  }

  {
    test_parser p(u8"<Type>(param) => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(generic_param_decl(u8"Type"), param_decl(u8"param")));
  }

  {
    test_parser p(u8"<Type>(param): ReturnType => {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // Type
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(generic_param_decl(u8"Type"), param_decl(u8"param")));
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
string8_view ambiguous_jsx_generic_arrow =
    u8R"(
    f = <T>(
        p // </T>, f = (
    ) => {}
)"_sv;

TEST_F(test_parse_typescript_generic_arrow,
       generic_arrow_function_in_ts_mode_can_look_like_jsx_element) {
  {
    test_parser p(ambiguous_jsx_generic_arrow, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var f, arrowfunc(var p))");
  }
}

TEST_F(test_parse_typescript_generic_arrow,
       jsx_element_in_tsx_mode_can_look_like_generic_arrow_function) {
  {
    test_parser p(ambiguous_jsx_generic_arrow, typescript_jsx_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(
        summarize(ast),
        "binary(assign(var f, jsxelement(T)), assign(var f, arrowfunc()))");
  }
}

TEST_F(test_parse_typescript_generic_arrow,
       unambiguous_generic_arrow_is_not_allowed_in_tsx) {
  {
    SCOPED_TRACE(
        "'>' is invalid in JSX, so this code cannot be interpreted as legal "
        "JSX");
    test_parser p(u8"<T>() => {body} // </T>", typescript_jsx_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // T
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",  // body
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_3_OFFSETS(
                    p.code,
                    diag_typescript_generic_arrow_needs_comma_in_jsx_mode,  //
                    generic_parameters_less, 0, u8"<",                      //
                    expected_comma, strlen(u8"<T"), u8"",                   //
                    arrow, strlen(u8"<T>() "), u8"=>")));
  }

  // TODO(strager): <T>({}) => {}         // '{' appears before '=>'.
  // TODO(strager): <T>(x = y > z) => {}  // '>' appears before '=>'.
  // TODO(strager): <T>(x = y < z) => {}  // '<' appears before '=>'.
  // TODO(strager): <T>(f)() => </T>      // Not an arrow function.
}

TEST_F(test_parse_typescript_generic_arrow,
       unambiguous_generic_arrow_is_treated_as_jsx_in_non_typescript) {
  {
    test_parser p(u8"<T>() => {body} // </T>", jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(T, var body)");
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code,
                              diag_unexpected_greater_in_jsx_text,  //
                              greater, strlen(u8"<T>() ="), u8">")));
  }
}

TEST_F(test_parse_typescript_generic_arrow,
       generic_arrow_with_comma_is_allowed_in_tsx) {
  for (const parser_options& o : {typescript_options, typescript_jsx_options}) {
    test_parser p(u8"<T,>(param) => {}"_sv, o);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T"), param_decl(u8"param")));
  }

  for (const parser_options& o : {typescript_options, typescript_jsx_options}) {
    test_parser p(u8"<T,>(): ReturnType => {}"_sv, o);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T")));
  }
}

TEST_F(test_parse_typescript_generic_arrow,
       generic_arrow_with_extends_is_allowed_in_tsx) {
  for (const parser_options& o : {typescript_options, typescript_jsx_options}) {
    test_parser p(u8"<T extends U>(param) => {}"_sv, o);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_type_use",     // U
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"U"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T"), param_decl(u8"param")));
  }
}

TEST_F(test_parse_typescript_generic_arrow,
       generic_async_arrow_function_is_allowed_in_tsx) {
  for (const parser_options& o : {typescript_options, typescript_jsx_options}) {
    {
      test_parser p(u8"async <T>() => { await myPromise; }"_sv, o);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                        "visit_variable_declaration",       // T
                                        "visit_enter_function_scope_body",  // {
                                        "visit_variable_use",  // myPromise
                                        "visit_exit_function_scope"));  // }
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"myPromise"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(generic_param_decl(u8"T")));
    }

    {
      test_parser p(u8"async <T extends U>() => { await myPromise; }"_sv, o);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                        "visit_variable_declaration",       // T
                                        "visit_variable_type_use",          // U
                                        "visit_enter_function_scope_body",  // {
                                        "visit_variable_use",  // myPromise
                                        "visit_exit_function_scope"));  // }
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"U", u8"myPromise"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(generic_param_decl(u8"T")));
    }

    {
      test_parser p(
          u8"async <T>(param: ParamType): ReturnType => { await myPromise; }"_sv,
          o);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_enter_function_scope",       //
                              "visit_variable_declaration",       // T
                              "visit_variable_type_use",          // ParamType
                              "visit_variable_declaration",       // param
                              "visit_variable_type_use",          // ReturnType
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope"));      // }
      EXPECT_THAT(p.variable_uses,
                  ElementsAre(u8"ParamType", u8"ReturnType", u8"myPromise"));
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAre(generic_param_decl(u8"T"), param_decl(u8"param")));
    }
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
