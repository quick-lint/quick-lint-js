// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_enum, enum_is_not_allowed_in_javascript) {
  {
    padded_string code(u8"enum E {}\nlet x = y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_is_not_allowed_in_javascript,  //
            enum_keyword, 0, u8"enum")));
  }

  {
    padded_string code(u8"const enum E {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_is_not_allowed_in_javascript,  //
            enum_keyword, strlen(u8"const "), u8"enum")));
  }

  {
    padded_string code(u8"declare enum E {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_is_not_allowed_in_javascript,  //
            enum_keyword, strlen(u8"declare "), u8"enum")));
  }

  {
    padded_string code(u8"declare const enum E {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_is_not_allowed_in_javascript,  //
            enum_keyword, strlen(u8"declare const "), u8"enum")));
  }
}

TEST(test_parse_typescript_enum, empty_enum) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"enum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"E", variable_kind::_enum, variable_init_kind::normal}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"const enum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"E", variable_kind::_enum, variable_init_kind::normal}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"declare enum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"E", variable_kind::_enum, variable_init_kind::normal}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"declare const enum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"E", variable_kind::_enum, variable_init_kind::normal}));
  }
}

TEST(test_parse_typescript_enum,
     enum_can_be_named_certain_contextual_keywords) {
  for (string8 name : contextual_keywords - typescript_builtin_type_keywords -
                          typescript_special_type_keywords -
                          dirty_set<string8>{
                              u8"let",
                              u8"static",
                              u8"yield",
                          }) {
    string8 code = u8"enum " + name + u8" {}";
    spy_visitor v = parse_and_visit_typescript_statement(code);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // (name)
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    name, variable_kind::_enum, variable_init_kind::normal}));
  }
}

TEST(test_parse_typescript_enum, enum_cannot_be_named_await_in_async_function) {
  padded_string code(u8"enum await {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  auto guard = p.enter_function(function_attributes::async);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  &code, diag_cannot_declare_await_in_async_function,  //
                  name, strlen(u8"enum "), u8"await")));
}

TEST(test_parse_typescript_enum, enum_with_auto_members) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"enum E { A }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"enum E { A, }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"enum E { A, B }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
  }
}

TEST(test_parse_typescript_enum, enum_with_initialized_members) {
  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"enum E { A = 10, B = 20 }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"enum E { First = data[0], Second = data[1] }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_variable_use",          // data
                                      "visit_variable_use",          // data
                                      "visit_exit_enum_scope"));     // }
  }
}

TEST(test_parse_typescript_enum, enum_members_can_be_named_keywords) {
  for (string8 keyword : keywords) {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"enum E { " + keyword + u8" }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope"));     // }
  }
}

TEST(test_parse_typescript_enum, enum_members_can_be_named_string_literals) {
  spy_visitor v = parse_and_visit_typescript_statement(
      u8"enum E { 'member1', \"member2\" = init, }"_sv);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                    "visit_enter_enum_scope",      // {
                                    "visit_variable_use",          // init
                                    "visit_exit_enum_scope"));     // }
}

TEST(test_parse_typescript_enum, enum_members_can_be_named_string_expressions) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"enum E { ['member'] = init, }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_variable_use",          // init
                                      "visit_exit_enum_scope"));     // }
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"enum E { [`member`] = init, }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_variable_use",          // init
                                      "visit_exit_enum_scope"));     // }
  }
}

TEST(test_parse_typescript_enum, enum_members_can_be_named_number_literals) {
  {
    padded_string code(u8"enum E { 42 = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_variable_use",          // init
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_member_name_cannot_be_number,  //
            number, strlen(u8"enum E { "), u8"42")));
  }

  {
    padded_string code(u8"enum E { 42n = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_member_name_cannot_be_number,  //
            number, strlen(u8"enum E { "), u8"42n")));
  }

  // TODO(#758)
  if ((false)) {
    padded_string code(u8"enum E { [42] = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_member_name_cannot_be_number,  //
            number, strlen(u8"enum E { ["), u8"42")));
  }
}

TEST(test_parse_typescript_enum,
     enum_members_cannot_be_named_complex_expressions) {
  {
    padded_string code(u8"enum E { [ 'mem' + 'ber' ] = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_computed_name_must_be_simple,  //
            expression, strlen(u8"enum E { [ "), u8"'mem' + 'ber'")));
  }

  {
    padded_string code(u8"enum E { [('member')] = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_computed_name_must_be_simple,  //
            expression, strlen(u8"enum E { ["), u8"('member')")));
  }

  {
    padded_string code(u8"enum E { [`template${withVariable}`] = init, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_typescript_enum_computed_name_must_be_simple,  //
            expression, strlen(u8"enum E { ["),
            u8"`template${withVariable}`")));
  }
}

TEST(test_parse_typescript_enum, extra_commas_are_not_allowed) {
  {
    padded_string code(u8"enum E { , }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_extra_comma_not_allowed_between_enum_members,  //
            comma, strlen(u8"enum E { "), u8",")));
  }

  {
    padded_string code(u8"enum E { A,, B,, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                &code, diag_extra_comma_not_allowed_between_enum_members,  //
                comma, strlen(u8"enum E { A,"), u8","),
            DIAG_TYPE_OFFSETS(
                &code, diag_extra_comma_not_allowed_between_enum_members,  //
                comma, strlen(u8"enum E { A,, B,"), u8",")));
  }
}

TEST(test_parse_typescript_enum, declare_must_not_have_newline_before_enum) {
  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"declare\nenum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // declare
                                      "visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"declare"}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"declare\nconst enum E {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // declare
                                      "visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"declare"}));
  }
}

TEST(test_parse_typescript_enum,
     const_and_declare_enums_require_constant_values) {
  for (string8 decl :
       {u8"const enum", u8"declare enum", u8"declare const enum"}) {
    {
      padded_string code(decl + u8" E { A = f() }");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_typescript_enum_value_must_be_constant,  //
                      expression, (decl + u8" E { A = ").size(), u8"f()")));
    }

    {
      padded_string code(decl + u8" E { A = f(), B, C, D }");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE(diag_typescript_enum_value_must_be_constant)))
          << "shouldn't complain about auto member following computed member";
    }

    {
      padded_string code(decl + u8" E { A = (2 + f()) }");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              &code, diag_typescript_enum_value_must_be_constant,  //
              expression, (decl + u8" E { A = ").size(), u8"(2 + f())")));
    }
  }
}

TEST(test_parse_typescript_enum, enums_allow_constant_values) {
  for (string8 decl :
       {u8"enum", u8"const enum", u8"declare enum", u8"declare const enum"}) {
    parse_and_visit_typescript_module(decl + u8" E { A = 1, B = A }");
    parse_and_visit_typescript_module(decl + u8" E { A = 1, B = E.A }");
    parse_and_visit_typescript_module(decl + u8" E { A = OtherEnum.B }");
    parse_and_visit_typescript_module(decl + u8" E { A = (((1))) }");
    // Test all allowed binary operators:
    parse_and_visit_typescript_module(
        decl + u8" E { A = 2+2-2*2/2%2<<2>>2>>>2&2|2^2 }");
    // Test all allowed unary operators:
    parse_and_visit_typescript_module(decl + u8" E { A = +-~2 }");
  }
}

TEST(test_parse_typescript_enum, normal_enum_allows_non_constant_values) {
  parse_and_visit_typescript_module(u8"enum E { A = f() }"_sv);
  parse_and_visit_typescript_module(u8"enum E { A = someVariable }"_sv);
}

TEST(test_parse_typescript_enum,
     normal_enum_auto_is_allowed_after_constant_value) {
  parse_and_visit_typescript_module(u8"enum E { A = 42, B }"_sv);
  parse_and_visit_typescript_module(u8"enum E { A = 2+2, B }"_sv);
  parse_and_visit_typescript_module(u8"enum E { A = OtherEnum.C, B }"_sv);
  parse_and_visit_typescript_module(u8"enum E { A, B = A, C, }"_sv);
}

TEST(test_parse_typescript_enum, normal_enum_auto_requires_constant_value) {
  {
    padded_string code(u8"enum E { A = f(), B, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            &code,
            diag_typescript_enum_auto_member_needs_initializer_after_computed,  //
            auto_member_name, strlen(u8"enum E { A = f(), "), u8"B",  //
            computed_expression, strlen(u8"enum E { A = "), u8"f()")));
  }

  {
    padded_string code(u8"enum E { A, B = f(), C, D, E, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(
            diag_typescript_enum_auto_member_needs_initializer_after_computed)));
  }

  {
    padded_string code(u8"enum E { ['A'] = f(), ['B'], }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            &code,
            diag_typescript_enum_auto_member_needs_initializer_after_computed,  //
            auto_member_name, strlen(u8"enum E { ['A'] = f(), "), u8"['B']",  //
            computed_expression, strlen(u8"enum E { ['A'] = "), u8"f()")));
  }

  {
    padded_string code(u8"enum E { 42 = f(), 69, }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE(diag_typescript_enum_member_name_cannot_be_number),
            DIAG_TYPE(diag_typescript_enum_member_name_cannot_be_number),
            DIAG_TYPE_2_OFFSETS(
                &code,
                diag_typescript_enum_auto_member_needs_initializer_after_computed,  //
                auto_member_name, strlen(u8"enum E { 42 = f(), "), u8"69",  //
                computed_expression, strlen(u8"enum E { 42 = "), u8"f()")));
  }
}

// TODO(#758): Error on: enum E { A = "A", B }
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
