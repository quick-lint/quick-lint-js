// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Function : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_is_not_allowed_in_javascript) {
  test_parse_and_visit_statement(
      u8"declare function f();"_sv,                                      //
      u8"^^^^^^^ Diag_Declare_Function_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Function, basic_declare_function) {
  {
    Test_Parser p(u8"declare function f();"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_exit_declare_scope",    //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_cannot_have_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare function f() { }"_sv,  //
        u8"                     ^ Diag_Declare_Function_Cannot_Have_Body.body_start\n"_diag
        u8"^^^^^^^ .declare_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_declare_scope",         //
                          }));
  }

  {
    // should not receive a Diag_Missing_Semicolon_After_Statement
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare function f() { } foo"_sv,              //
        u8"Diag_Declare_Function_Cannot_Have_Body"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_declare_scope",         //
                              "visit_variable_use",               // foo
                              "visit_end_of_module",              //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_must_have_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare function ();"_sv,                                          //
        u8"                 ` Diag_Missing_Name_In_Function_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_enter_function_scope",  // (function)
                              "visit_exit_function_scope",   // (function)
                              "visit_exit_declare_scope",    //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_cannot_be_async_or_generator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare async function f();"_sv,                            //
        u8"        ^^^^^ Diag_Declare_Function_Cannot_Be_Async"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_exit_declare_scope",    //
                          }));
  }

  test_parse_and_visit_statement(
      u8"declare function* f();"_sv,                                         //
      u8"                ^ Diag_Declare_Function_Cannot_Be_Generator"_diag,  //
      typescript_options);

  test_parse_and_visit_statement(
      u8"declare async function* f();"_sv,                 //
      u8"Diag_Declare_Function_Cannot_Be_Async"_diag,      //
      u8"Diag_Declare_Function_Cannot_Be_Generator"_diag,  //
      typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare async function f() { await(myPromise); }"_sv,  //
        u8"Diag_Declare_Function_Cannot_Be_Async"_diag,           //
        u8"Diag_Declare_Function_Cannot_Have_Body"_diag,          //

        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}))
        << "'await' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in an "
           "async function";
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare function* f() { yield(myValue); }"_sv,    //
        u8"Diag_Declare_Function_Cannot_Be_Generator"_diag,  //
        u8"Diag_Declare_Function_Cannot_Have_Body"_diag,     //

        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "'yield' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in a "
           "generator function";
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_name_can_be_contextual_keyword) {
  Dirty_Set<String8> function_names =
      contextual_keywords | Dirty_Set<String8>{u8"await", u8"yield"};
  for (const String8& function_name : function_names) {
    Padded_String code(
        concat(u8"declare function "_sv, function_name, u8"(): void;"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // (function_name)
                              "visit_enter_function_scope",  // (function_name)
                              "visit_exit_function_scope",   // (function_name)
                              "visit_exit_declare_scope",    //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(function_name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_before_function_keyword_triggers_asi) {
  {
    Test_Parser p(u8"declare\nfunction f() {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // declare
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",              //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_requires_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare function f() foo"_sv,  //
        u8"                    ` Diag_Missing_Semicolon_After_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_exit_declare_scope",    //
                              "visit_variable_use",          // foo
                              "visit_end_of_module",         //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function, declare_function_performs_asi) {
  {
    Test_Parser p(u8"declare function f()\nfoo"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_exit_declare_scope",    //
                              "visit_variable_use",          // foo
                              "visit_end_of_module",         //
                          }));
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
