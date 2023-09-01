// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
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
class Test_Parse_TypeScript_Export_Declare : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Export_Declare,
       export_declare_is_not_allowed_in_javascript) {
  test_parse_and_visit_module(
      u8"export declare class C { }"_sv,                                     //
      u8"       ^^^^^^^ Diag_Declare_Class_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Export_Declare, export_declare_class) {
  {
    Test_Parser p(u8"export declare class C { }"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Export_Declare, export_declare_abstract_class) {
  {
    Test_Parser p(u8"export declare abstract class C { }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Export_Declare,
       export_declare_import_alias_is_not_allowed) {
  test_parse_and_visit_module(
      u8"export declare import A = B;"_sv,                              //
      u8"       ^^^^^^^ Diag_Import_Cannot_Have_Declare_Keyword"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Export_Declare,
       export_declare_import_is_not_allowed) {
  test_parse_and_visit_module(
      u8"export declare import a from 'mod';"_sv,                       //
      u8"       ^^^^^^^ Diag_Import_Cannot_Have_Declare_Keyword"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Export_Declare,
       newline_is_allowed_between_export_and_declare) {
  {
    Test_Parser p(u8"export\ndeclare class C { }"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Export_Declare,
       newline_is_not_allowed_between_declare_and_following_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export declare\nclass C { }"_sv,  //
        u8"       ^^^^^^^ Diag_Newline_Not_Allowed_After_Export_Declare.declare_keyword\n"_diag
        u8"^^^^^^ .export_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",
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
