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
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Enum : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Enum, enum_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"enum E {}\nlet x = y;"_sv,  //
        u8"^^^^ Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"const enum E {}"_sv,  //
        u8"      ^^^^ Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare enum E {}"_sv,  //
        u8"        ^^^^ Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare const enum E {}"_sv,  //
        u8"              ^^^^ Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Enum, empty_enum) {
  {
    Test_Parser p(u8"enum E {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }

  {
    Test_Parser p(u8"const enum E {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }

  {
    Test_Parser p(u8"declare enum E {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }

  {
    Test_Parser p(u8"declare const enum E {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Enum,
       enum_can_be_named_certain_contextual_keywords) {
  for (String8 name : contextual_keywords - typescript_builtin_type_keywords -
                          typescript_special_type_keywords -
                          Dirty_Set<String8>{
                              u8"let",
                              u8"static",
                              u8"yield",
                          }) {
    Test_Parser p(concat(u8"enum "_sv, name, u8" {}"_sv), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // (name)
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
    EXPECT_THAT(p.variable_declarations, ElementsAreArray({enum_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Enum,
       enum_cannot_be_named_await_in_async_function) {
  Test_Parser p(u8"enum await {}"_sv, typescript_options, capture_diags);
  auto guard = p.enter_function(Function_Attributes::async);
  p.parse_and_visit_statement();
  assert_diagnostics(
      p.code, p.errors,
      {
          u8"     ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,
      });
}

TEST_F(Test_Parse_TypeScript_Enum, enum_with_auto_members) {
  {
    Test_Parser p(u8"enum E { A }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
  }

  {
    Test_Parser p(u8"enum E { A, }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
  }

  {
    Test_Parser p(u8"enum E { A, B }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Enum, enum_with_initialized_members) {
  {
    Test_Parser p(u8"enum E { A = 10, B = 20 }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
  }

  {
    Test_Parser p(u8"enum E { First = data[0], Second = data[1] }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_variable_use",          // data
                              "visit_variable_use",          // data
                              "visit_exit_enum_scope",       // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Enum, enum_members_can_be_named_keywords) {
  for (String8 keyword : keywords) {
    Test_Parser p(concat(u8"enum E { "_sv, keyword, u8" }"_sv),
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Enum, enum_members_can_be_named_string_literals) {
  Test_Parser p(u8"enum E { 'member1', \"member2\" = init, }"_sv,
                typescript_options);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",  // E
                            "visit_enter_enum_scope",      // {
                            "visit_variable_use",          // init
                            "visit_exit_enum_scope",       // }
                        }));
}

TEST_F(Test_Parse_TypeScript_Enum,
       enum_members_can_be_named_string_expressions) {
  {
    Test_Parser p(u8"enum E { ['member'] = init, }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_variable_use",          // init
                              "visit_exit_enum_scope",       // }
                          }));
  }

  {
    Test_Parser p(u8"enum E { [`member`] = init, }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_variable_use",          // init
                              "visit_exit_enum_scope",       // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Enum, enum_members_can_be_named_number_literals) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"enum E { 42 = init, }"_sv,  //
        u8"         ^^ Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_variable_use",          // init
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
  }

  test_parse_and_visit_module(
      u8"enum E { 42n = init, }"_sv,  //
      u8"         ^^^ Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number"_diag,  //
      typescript_options);

  // TODO(#758)
  if ((false)) {
    test_parse_and_visit_module(
        u8"enum E { [42] = init, }"_sv,  //
        u8"          ^^ Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number"_diag,  //
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Enum,
       enum_members_cannot_be_named_complex_expressions) {
  test_parse_and_visit_module(
      u8"enum E { [ 'mem' + 'ber' ] = init, }"_sv,  //
      u8"           ^^^^^^^^^^^^^ Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple"_diag,  //

      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { [('member')] = init, }"_sv,  //
      u8"          ^^^^^^^^^^ Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { [`template${withVariable}`] = init, }"_sv,  //
      u8"          ^^^^^^^^^^^^^^^^^^^^^^^^^ Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple"_diag,  //

      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Enum, extra_commas_are_not_allowed) {
  test_parse_and_visit_module(
      u8"enum E { , }"_sv,                                                    //
      u8"         ^ Diag_Extra_Comma_Not_Allowed_Between_Enum_Members"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { A,, B,, }"_sv,  //
      u8"               ^ Diag_Extra_Comma_Not_Allowed_Between_Enum_Members"_diag,  //
      u8"           ^ Diag_Extra_Comma_Not_Allowed_Between_Enum_Members"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Enum, declare_must_not_have_newline_before_enum) {
  {
    Test_Parser p(u8"declare\nenum E {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"}));
  }

  {
    Test_Parser p(u8"declare\nconst enum E {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"}));
  }
}

TEST_F(Test_Parse_TypeScript_Enum,
       const_and_declare_enums_require_constant_values) {
  for (String8 decl :
       {u8"const enum", u8"declare enum", u8"declare const enum"}) {
    {
      Test_Parser p(concat(decl, u8" E { A = f() }"_sv), typescript_options,
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_TypeScript_Enum_Value_Must_Be_Constant,  //
                  expression, (decl + u8" E { A = ").size(), u8"f()"_sv),
          }));
    }

    test_parse_and_visit_module(
        concat(decl, u8" E { A = f(), B, C, D }"_sv),
        u8"Diag_TypeScript_Enum_Value_Must_Be_Constant"_diag,
        typescript_options);

    {
      Test_Parser p(concat(decl, u8" E { A = (2 + f()) }"_sv),
                    typescript_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_TypeScript_Enum_Value_Must_Be_Constant,  //
                  expression, (decl + u8" E { A = ").size(), u8"(2 + f())"_sv),
          }));
    }

    test_parse_and_visit_module(
        concat(decl, u8" E { A = this }"_sv),
        u8"Diag_TypeScript_Enum_Value_Must_Be_Constant"_diag,
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Enum, enums_allow_constant_values) {
  for (String8 decl :
       {u8"enum", u8"const enum", u8"declare enum", u8"declare const enum"}) {
    for (String8 code : {
             decl + u8" E { A = 1, B = A }",
             decl + u8" E { A = 1, B = E.A }",
             decl + u8" E { A = OtherEnum.B }",
             decl + u8" E { A = (((1))) }",
             // Test all allowed binary operators:
             decl + u8" E { A = 2+2-2*2/2%2<<2>>2>>>2&2|2^2 }",
             // Test all allowed unary operators:
             decl + u8" E { A = +-~2 }",
         }) {
      SCOPED_TRACE(out_string8(code));
      Test_Parser p(code, typescript_options);
      p.parse_and_visit_module();
    }
  }
}

TEST_F(Test_Parse_TypeScript_Enum, normal_enum_allows_non_constant_values) {
  for (String8_View code : {
           u8"enum E { A = f() }"_sv,
           u8"enum E { A = someVariable }"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(Test_Parse_TypeScript_Enum,
       normal_enum_auto_is_allowed_after_constant_value) {
  for (String8_View code : {
           u8"enum E { A = 42, B }"_sv,
           u8"enum E { A = 2+2, B }"_sv,
           u8"enum E { A = OtherEnum.C, B }"_sv,
           u8"enum E { A, B = A, C, }"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(Test_Parse_TypeScript_Enum, normal_enum_auto_requires_constant_value) {
  test_parse_and_visit_module(
      u8"enum E { A = f(), B, }"_sv,  //
      u8"                  ^ Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed.auto_member_name\n"_diag
      u8"             ^^^ .computed_expression"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { A, B = f(), C, D, E, }"_sv,  //
      u8"Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { ['A'] = f(), ['B'], }"_sv,  //
      u8"                      ^^^^^ Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed.auto_member_name\n"_diag
      u8"                 ^^^ .computed_expression"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"enum E { 42 = f(), 69, }"_sv,  //
      u8"                   ^^ Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed.auto_member_name\n"_diag
      u8"              ^^^ .computed_expression"_diag,             //
      u8"Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number"_diag,  //
      u8"Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number"_diag,  //
      typescript_options);
}

// TODO(#758): Error on: enum E { A = "A", B }

TEST_F(Test_Parse_TypeScript_Enum, enum_is_allowed_inside_if) {
  test_parse_and_visit_module(u8"if (true) enum E { }"_sv, no_diags,
                              typescript_options);
  test_parse_and_visit_module(u8"if (true) const enum E { }"_sv, no_diags,
                              typescript_options);
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
