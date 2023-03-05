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
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_declare_var : public test_parse_expression {};

TEST_F(test_parse_typescript_declare_var,
       declare_var_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"declare var x;"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_declare_var_not_allowed_in_javascript,     //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"var"_sv),
                }));
  }

  {
    test_parser p(u8"declare const x;"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_declare_var_not_allowed_in_javascript,     //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"const"_sv),
                }));
  }

  {
    test_parser p(u8"declare let x;"_sv, javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        diag_declare_var_not_allowed_in_javascript,     //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"let"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_var, declare_var) {
  {
    test_parser p(u8"declare var x;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"declare const x;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"declare let x;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }
}

TEST_F(test_parse_typescript_declare_var,
       declare_var_allow_destructuring_without_initializer) {
  {
    test_parser p(u8"declare var [x, y, z];"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_declaration",  // y
                              "visit_variable_declaration",  // z
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({var_noinit_decl(u8"x"_sv), var_noinit_decl(u8"y"_sv),
                          var_noinit_decl(u8"z"_sv)}));
  }
}

TEST_F(test_parse_typescript_declare_var, declare_var_cannot_have_initializer) {
  {
    test_parser p(u8"declare var x = 42;"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code,
                        diag_declare_var_cannot_have_initializer,       //
                        equal, strlen(u8"declare var x "), u8"=",       //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"var"_sv),
                }));
  }

  {
    test_parser p(u8"declare const x = 42;"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code,
                        diag_declare_var_cannot_have_initializer,       //
                        equal, strlen(u8"declare const x "), u8"=",     //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"const"_sv),
                }));
  }

  {
    test_parser p(u8"declare let x = 42;"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_3_OFFSETS(
                        p.code,
                        diag_declare_var_cannot_have_initializer,       //
                        equal, strlen(u8"declare let x "), u8"=",       //
                        declare_keyword, strlen(u8""), u8"declare"_sv,  //
                        declaring_token, strlen(u8"declare "), u8"let"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_var, newline_before_var_triggers_asi) {
  {
    test_parser p(u8"declare\nvar x = initializer;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_use",          // initializer
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"declare"_sv, u8"initializer"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_init_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"declare\nconst x = initializer;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_use",          // initializer
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"declare"_sv, u8"initializer"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_init_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"declare\nlet x = initializer;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_use",          // initializer
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"declare"_sv, u8"initializer"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
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
