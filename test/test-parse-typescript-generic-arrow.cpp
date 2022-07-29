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
