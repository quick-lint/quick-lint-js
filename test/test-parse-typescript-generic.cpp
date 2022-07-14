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
TEST(test_parse_typescript_generic, single_basic_generic_parameter) {
  {
    padded_string code(u8"<T>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // T
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(visited_variable_declaration{
                    u8"T", variable_kind::_generic_parameter,
                    variable_init_kind::normal}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_typescript_generic, multiple_basic_generic_parameter) {
  {
    padded_string code(u8"<T1, T2, T3>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T1
                                      "visit_variable_declaration",    // T2
                                      "visit_variable_declaration"));  // T3
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    visited_variable_declaration{
                        u8"T1", variable_kind::_generic_parameter,
                        variable_init_kind::normal},
                    visited_variable_declaration{
                        u8"T2", variable_kind::_generic_parameter,
                        variable_init_kind::normal},
                    visited_variable_declaration{
                        u8"T3", variable_kind::_generic_parameter,
                        variable_init_kind::normal}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"<T1, T2, T3,>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T1
                                      "visit_variable_declaration",    // T2
                                      "visit_variable_declaration"));  // T3
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_typescript_generic, parameters_require_commas_between) {
  {
    padded_string code(u8"<T1 T2>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T1
                                      "visit_variable_declaration"));  // T2
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    visited_variable_declaration{
                        u8"T1", variable_kind::_generic_parameter,
                        variable_init_kind::normal},
                    visited_variable_declaration{
                        u8"T2", variable_kind::_generic_parameter,
                        variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_comma_between_generic_parameters,
                    expected_comma, strlen(u8"<T1"), u8"")));
  }
}

TEST(test_parse_typescript_generic,
     parameter_list_does_not_allow_leading_comma) {
  {
    padded_string code(u8"<, T>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // T
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_comma_not_allowed_before_first_generic_parameter,
            unexpected_comma, strlen(u8"<"), u8",")));
  }

  {
    padded_string code(u8"<,,, T>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // T
    EXPECT_THAT(
        v.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                &code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<"), u8","),
            DIAG_TYPE_OFFSETS(
                &code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<,"), u8","),
            DIAG_TYPE_OFFSETS(
                &code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<,,"), u8",")));
  }
}

TEST(test_parse_typescript_generic,
     parameter_list_must_contain_at_least_one_parameter) {
  {
    padded_string code(u8"<>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_typescript_generic_parameter_list_is_empty,
                    expected_parameter, strlen(u8"<"), u8"")));
  }

  {
    padded_string code(u8"<,>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_typescript_generic_parameter_list_is_empty,
                    expected_parameter, strlen(u8"<"), u8"")));
  }

  {
    padded_string code(u8"<,,>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
                        &code, diag_typescript_generic_parameter_list_is_empty,
                        expected_parameter, strlen(u8"<"), u8""),
                    DIAG_TYPE_OFFSETS(
                        &code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<,"), u8",")));
  }
}

TEST(test_parse_typescript_generic,
     parameter_list_does_not_allow_multiple_trailing_commas) {
  {
    padded_string code(u8"<T,,>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // T
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_multiple_commas_in_generic_parameter_list,
                    unexpected_comma, strlen(u8"<T,"), u8",")));
  }

  {
    padded_string code(u8"<T , , ,>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // T
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
                        &code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T , "), u8","),
                    DIAG_TYPE_OFFSETS(
                        &code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T , , "), u8",")));
  }
}

TEST(test_parse_typescript_generic,
     parameter_list_does_not_allow_consecutive_interior_commas) {
  {
    padded_string code(u8"<T,,U>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T
                                      "visit_variable_declaration"));  // U
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_multiple_commas_in_generic_parameter_list,
                    unexpected_comma, strlen(u8"<T,"), u8",")));
  }
}

TEST(test_parse_typescript_generic, parameter_list_extends) {
  {
    padded_string code(u8"<T extends U>"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_typescript_generic_parameters(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_variable_type_use"));   // U
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(visited_variable_declaration{
                    u8"T", variable_kind::_generic_parameter,
                    variable_init_kind::normal}));
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"U"));
    EXPECT_THAT(v.errors, IsEmpty());
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
