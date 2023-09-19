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

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_This_Parameters : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_normal_functions) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(this) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(this: MyType) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // MyType
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(this, otherparam) {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // otherparam
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_class_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(this) {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"abstract class C { abstract f(this); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_property_declaration",    // f
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static f(this) {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_interface_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I { f(this); }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_property_declaration",   // f
                              "visit_exit_interface_scope",   // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters,
       allowed_in_object_literal_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"{ method(this) {} }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_function_types) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(this) => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, disallowed_in_arrow_functions) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"this => {}"_sv,                                                 //
        u8"^^^^ Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"(this) => {}"_sv,                                                //
        u8" ^^^^ Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  test_parse_and_visit_expression(
      u8"async this => {}"_sv,  //
      u8"      ^^^^ Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"async (this) => {}"_sv,  //
      u8"       ^^^^ Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_when_destructuring) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"function([this]) {}"_sv,  //
        u8"          ^^^^ Diag_This_Parameter_Not_Allowed_When_Destructuring"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"function({key: this}) {}"_sv,  //
        u8"               ^^^^ Diag_This_Parameter_Not_Allowed_When_Destructuring"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_when_spreading) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"function(...this) {}"_sv,  //
        u8"            ^^^^ Diag_Spread_Parameter_Cannot_Be_This.this_keyword\n"_diag
        u8"         ^^^ .spread_operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, only_allowed_as_first_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"function( other, this ) {}"_sv,  //
        u8"                 ^^^^ Diag_This_Parameter_Must_Be_First.this_keyword\n"_diag
        u8"          ` .first_parameter_begin"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // other
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  test_parse_and_visit_typescript_type_expression(
      u8"(other, this) => ReturnType"_sv,  //
      u8"        ^^^^ Diag_This_Parameter_Must_Be_First.this_keyword\n"_diag
      u8" ` .first_parameter_begin"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"function(this) {}"_sv,  //
        u8"         ^^^^ Diag_This_Parameter_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters,
       multiple_issues_reports_only_one_diagnostic) {
  test_parse_and_visit_expression(
      u8"function(other, [this]) {}"_sv,                            //
      u8"Diag_This_Parameter_Not_Allowed_When_Destructuring"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"([this]) => {}"_sv,                                        //
      u8"Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"(other, this) => {}"_sv,                                   //
      u8"Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"(...this) => {}"_sv,                                       //
      u8"Diag_This_Parameter_Not_Allowed_In_Arrow_Functions"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"function(other, ...this) {}"_sv,             //
      u8"Diag_Spread_Parameter_Cannot_Be_This"_diag,  //
      typescript_options);

  test_parse_and_visit_expression(
      u8"(this) => {}"_sv,                                     //
      u8"Diag_This_Parameter_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);

  test_parse_and_visit_expression(
      u8"function(other, this) {}"_sv,                         //
      u8"Diag_This_Parameter_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
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
