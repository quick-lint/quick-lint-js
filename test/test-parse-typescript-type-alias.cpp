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
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Type_Alias : public Test_Parse_Expression {};

using ::testing::ElementsAreArray;

TEST_F(Test_Parse_TypeScript_Type_Alias, type_alias) {
  {
    Test_Parser p(u8"type T = U;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // T
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"T"_sv)}));
  }

  {
    Test_Parser p(u8"type MyAlias<T> = U;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // MyAlias
                              "visit_enter_type_alias_scope",  // MyAlias
                              "visit_variable_declaration",    // T
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // MyAlias
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"MyAlias"_sv),
                                  generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type_Alias, type_alias_requires_semicolon_or_asi) {
  {
    Test_Parser p(u8"type T = U"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // T
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // T
                          }));
  }

  {
    Test_Parser p(u8"type T = U\ntype V = W;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // T
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // T
                              "visit_variable_declaration",    // V
                              "visit_enter_type_alias_scope",  // V
                              "visit_variable_type_use",       // W
                              "visit_exit_type_alias_scope",   // V
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"type T = U type V = W;"_sv,                                //
        u8"          ` Diag_Missing_Semicolon_After_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // T
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // T
                              "visit_variable_declaration",    // V
                              "visit_enter_type_alias_scope",  // V
                              "visit_variable_type_use",       // W
                              "visit_exit_type_alias_scope",   // V
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type_Alias,
       type_alias_can_be_named_certain_contextual_keywords) {
  for (String8 name :
       Dirty_Set<String8>{u8"await"} |
           (contextual_keywords - typescript_builtin_type_keywords -
            typescript_special_type_keywords -
            Dirty_Set<String8>{
                u8"let",
                u8"static",
                u8"yield",
            })) {
    Test_Parser p(concat(u8"type "_sv, name, u8" = T;"_sv), typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // (name)
                              "visit_enter_type_alias_scope",  // (name)
                              "visit_variable_type_use",       // T
                              "visit_exit_type_alias_scope",   // (name)
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type_Alias,
       type_alias_cannot_have_newline_after_type_keyword) {
  {
    Test_Parser p(u8"type\nT = U;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // type
                              "visit_variable_use",         // U
                              "visit_variable_assignment",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"type", u8"U"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type_Alias, type_alias_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"type T = U;"_sv,                                                  //
        u8"^^^^ Diag_TypeScript_Type_Alias_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // (name)
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // (name)
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
