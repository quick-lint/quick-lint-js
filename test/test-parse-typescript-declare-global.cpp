// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Global : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Global,
       declare_global_is_not_allowed_in_javascript) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"declare global {}"_sv,  //
      u8"        ^^^^^^ Diag_TypeScript_Global_Block_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_global_scope",  //
                            "visit_exit_declare_global_scope",   //
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Global, empty_declare_global) {
  Spy_Visitor p = test_parse_and_visit_statement(u8"declare global {}"_sv,  //
                                                 no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_global_scope",  //
                            "visit_exit_declare_global_scope",   //
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Global, can_contain_variables) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"declare global { var x; const y; let z; }"_sv,  //
      no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_global_scope",  //
                            "visit_variable_declaration",        // x
                            "visit_variable_declaration",        // y
                            "visit_variable_declaration",        // z
                            "visit_exit_declare_global_scope",   //
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Global, can_contain_namespaces) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"declare global { namespace ns { } }"_sv,  //
      no_diags, typescript_options);
  EXPECT_THAT(p.visits,
              ElementsAreArray({
                  "visit_enter_declare_global_scope",  // declare global {
                  "visit_enter_namespace_scope",       // namespace ns {
                  "visit_exit_namespace_scope",        // namespace ns }
                  "visit_variable_declaration",        // ns
                  "visit_exit_declare_global_scope",   // declare global {
              }));
}

TEST_F(Test_Parse_TypeScript_Declare_Global, allowed_inside_declare_module) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"declare module 'mymodule' { global {} }"_sv,  //
      no_diags, typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",    // namespace ns {
                            "visit_enter_namespace_scope",  // namespace ns {
                            "visit_enter_declare_global_scope",  // global {
                            "visit_exit_declare_global_scope",   // }
                            "visit_exit_namespace_scope",  // namespace ns }
                            "visit_exit_declare_scope",    // namespace ns }
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Global, not_allowed_inside_namespace) {
  test_parse_and_visit_statement(
      u8"declare namespace ns { global {} }"_sv,  //
      u8"                       ^^^^^^ Diag_TypeScript_Global_Block_Not_Allowed_In_Namespace.global_keyword\n"_diag
      u8"        ^^^^^^^^^ .namespace_keyword"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"declare module ns { global {} }"_sv,  //
      u8"                    ^^^^^^ Diag_TypeScript_Global_Block_Not_Allowed_In_Namespace.global_keyword\n"_diag
      u8"        ^^^^^^ .namespace_keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Global,
       non_declare_global_is_not_allowed) {
  Spy_Visitor v = test_parse_and_visit_statement(
      u8"global { const x; }"_sv,  //
      u8"^^^^^^ Diag_TypeScript_Global_Block_Must_Be_Declare.global_keyword\n"_diag
      u8"` .expected_declare_keyword"_diag,
      typescript_options);
  EXPECT_THAT(v.variable_declarations,
              ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Global,
       newline_after_non_declare_global_triggers_asi) {
  Spy_Visitor v = test_parse_and_visit_module(
      u8"global\n{ const x; }"_sv,  //
      u8"                ^ Diag_Missing_Initializer_In_Const_Declaration.variable_name"_diag,
      typescript_options);
  EXPECT_THAT(v.variable_uses, ElementsAreArray({u8"global"_sv}));
  EXPECT_THAT(v.variable_declarations,
              ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Declare_Global,
       newline_after_global_does_not_trigger_asi) {
  {
    Spy_Visitor v = test_parse_and_visit_module(
        u8"declare global\n{ const x; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_uses, ::testing::IsEmpty());
  }

  {
    Spy_Visitor v = test_parse_and_visit_module(
        u8"declare module 'mod' { global\n{ const x; } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(v.variable_uses, ::testing::IsEmpty());
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
