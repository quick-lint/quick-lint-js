// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_this_parameters : public test_parse_expression {};

TEST_F(test_parse_typescript_this_parameters, allowed_in_normal_functions) {
  {
    test_parser p(u8"function f(this) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"function f(this: MyType) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_variable_type_use",     // MyType
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    test_parser p(u8"function f(this, otherparam) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",       // f
                            "visit_enter_function_scope",       // f
                            "visit_variable_declaration",       // otherparam
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope"));      // }
  }
}

TEST_F(test_parse_typescript_this_parameters, allowed_in_class_methods) {
  {
    test_parser p(u8"class C { f(this) {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",          // C
                                      "visit_enter_class_scope_body",     // {
                                      "visit_property_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope",        // }
                                      "visit_exit_class_scope",           // }
                                      "visit_variable_declaration"));     // C
  }

  {
    test_parser p(u8"abstract class C { abstract f(this); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  // {
                                      "visit_property_declaration",    // f
                                      "visit_enter_function_scope",    // f
                                      "visit_exit_function_scope",     // f
                                      "visit_exit_class_scope",        // }
                                      "visit_variable_declaration"));  // C
  }

  {
    test_parser p(u8"class C { static f(this) {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",          // C
                                      "visit_enter_class_scope_body",     // {
                                      "visit_property_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope",        // }
                                      "visit_exit_class_scope",           // }
                                      "visit_variable_declaration"));     // C
  }
}

TEST_F(test_parse_typescript_this_parameters, allowed_in_interface_methods) {
  {
    test_parser p(u8"interface I { f(this); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",    // I
                                      "visit_enter_interface_scope",   // {
                                      "visit_property_declaration",    // f
                                      "visit_enter_function_scope",    // f
                                      "visit_exit_function_scope",     // f
                                      "visit_exit_interface_scope"));  // }
  }
}

TEST_F(test_parse_typescript_this_parameters,
       allowed_in_object_literal_methods) {
  {
    test_parser p(u8"{ method(this) {} }"_sv, typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  // method
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST_F(test_parse_typescript_this_parameters, disallowed_in_arrow_functions) {
  {
    test_parser p(u8"this => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  // method
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_in_arrow_functions,  //
                    this_keyword, strlen(u8""), u8"this")));
  }

  {
    test_parser p(u8"(this) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  // method
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_in_arrow_functions,  //
                    this_keyword, strlen(u8"("), u8"this")));
  }

  {
    test_parser p(u8"async this => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_in_arrow_functions,  //
                    this_keyword, strlen(u8"async "), u8"this")));
  }

  {
    test_parser p(u8"async (this) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_in_arrow_functions,  //
                    this_keyword, strlen(u8"async ("), u8"this")));
  }
}

TEST_F(test_parse_typescript_this_parameters, not_allowed_when_destructuring) {
  {
    test_parser p(u8"function([this]) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_when_destructuring,  //
                    this_keyword, strlen(u8"function(["), u8"this")));
  }

  {
    test_parser p(u8"function({key: this}) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code,
                    diag_this_parameter_not_allowed_when_destructuring,  //
                    this_keyword, strlen(u8"function({key: "), u8"this")));
  }
}

TEST_F(test_parse_typescript_this_parameters, only_allowed_as_first_parameter) {
  {
    test_parser p(u8"function( other, this ) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // other
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code,
                    diag_this_parameter_must_be_first,                      //
                    this_keyword, strlen(u8"function( other, "), u8"this",  //
                    first_parameter_begin, strlen(u8"function( "), u8"")));
  }
}

TEST_F(test_parse_typescript_this_parameters,
       multiple_issues_reports_only_one_diagnostic) {
  {
    test_parser p(u8"function(other, [this]) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(
                    diag_this_parameter_not_allowed_when_destructuring)))
        << "should not also report diag_this_parameter_must_be_first";
  }

  {
    test_parser p(u8"([this]) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(
                    diag_this_parameter_not_allowed_in_arrow_functions)))
        << "should not also report "
           "diag_this_parameter_not_allowed_when_destructuring";
  }

  {
    test_parser p(u8"(other, this) => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(
                    diag_this_parameter_not_allowed_in_arrow_functions)))
        << "should not also report diag_this_parameter_must_be_first";
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
