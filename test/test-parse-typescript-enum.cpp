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
