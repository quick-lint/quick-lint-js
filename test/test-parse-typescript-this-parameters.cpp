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

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_This_Parameters : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_normal_functions) {
  {
    Test_Parser p(u8"function f(this) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"function f(this: MyType) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_type_use",          // MyType
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }

  {
    Test_Parser p(u8"function f(this, otherparam) {}"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_variable_declaration",       // otherparam
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_class_methods) {
  {
    Test_Parser p(u8"class C { f(this) {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Test_Parser p(u8"abstract class C { abstract f(this); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // f
                              "visit_enter_function_scope",    // f
                              "visit_exit_function_scope",     // f
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Test_Parser p(u8"class C { static f(this) {} }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_interface_methods) {
  {
    Test_Parser p(u8"interface I { f(this); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // f
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_exit_interface_scope",   // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters,
       allowed_in_object_literal_methods) {
  {
    Test_Parser p(u8"{ method(this) {} }"_sv, typescript_options);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, allowed_in_function_types) {
  {
    Test_Parser p(u8"(this) => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
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
    Test_Parser p(u8"this => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_In_Arrow_Functions,  //
                        this_keyword, strlen(u8""), u8"this"_sv),
                }));
  }

  {
    Test_Parser p(u8"(this) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_In_Arrow_Functions,  //
                        this_keyword, strlen(u8"("), u8"this"_sv),
                }));
  }

  {
    Test_Parser p(u8"async this => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_In_Arrow_Functions,  //
                        this_keyword, strlen(u8"async "), u8"this"_sv),
                }));
  }

  {
    Test_Parser p(u8"async (this) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_In_Arrow_Functions,  //
                        this_keyword, strlen(u8"async ("), u8"this"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_when_destructuring) {
  {
    Test_Parser p(u8"function([this]) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_When_Destructuring,  //
                        this_keyword, strlen(u8"function(["), u8"this"_sv),
                }));
  }

  {
    Test_Parser p(u8"function({key: this}) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_This_Parameter_Not_Allowed_When_Destructuring,  //
                        this_keyword, strlen(u8"function({key: "), u8"this"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_when_spreading) {
  {
    Test_Parser p(u8"function(...this) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Spread_Parameter_Cannot_Be_This,  //
                        this_keyword, strlen(u8"function(..."), u8"this"_sv,
                        spread_operator, strlen(u8"function("), u8"..."_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, only_allowed_as_first_parameter) {
  {
    Test_Parser p(u8"function( other, this ) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // other
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                Diag_This_Parameter_Must_Be_First,  //
                                this_keyword, strlen(u8"function( other, "),
                                u8"this"_sv, first_parameter_begin,
                                strlen(u8"function( "), u8""_sv),
        }));
  }

  {
    Test_Parser p(u8"(other, this) => ReturnType"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                Diag_This_Parameter_Must_Be_First,  //
                                this_keyword, strlen(u8"(other, "), u8"this"_sv,
                                first_parameter_begin, strlen(u8"("), u8""_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters, not_allowed_in_javascript) {
  {
    Test_Parser p(u8"function(this) {}"_sv, javascript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              Diag_This_Parameter_Not_Allowed_In_JavaScript,  //
                              this_keyword, strlen(u8"function("), u8"this"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_This_Parameters,
       multiple_issues_reports_only_one_diagnostic) {
  {
    Test_Parser p(u8"function(other, [this]) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(Diag_This_Parameter_Not_Allowed_When_Destructuring),
        }))
        << "should not also report Diag_This_Parameter_Must_Be_First";
  }

  {
    Test_Parser p(u8"([this]) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(Diag_This_Parameter_Not_Allowed_In_Arrow_Functions),
        }))
        << "should not also report "
           "Diag_This_Parameter_Not_Allowed_When_Destructuring";
  }

  {
    Test_Parser p(u8"(other, this) => {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(Diag_This_Parameter_Not_Allowed_In_Arrow_Functions),
        }))
        << "should not also report Diag_This_Parameter_Must_Be_First";
  }

  {
    Test_Parser p(u8"(...this) => {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(Diag_This_Parameter_Not_Allowed_In_Arrow_Functions),
        }))
        << "should not also report Diag_Spread_Parameter_Cannot_Be_This";
  }

  {
    Test_Parser p(u8"function(other, ...this) {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Spread_Parameter_Cannot_Be_This),
                          }))
        << "should not also report Diag_This_Parameter_Must_Be_First";
  }

  {
    Test_Parser p(u8"(this) => {}"_sv, javascript_options, capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_This_Parameter_Not_Allowed_In_JavaScript),
                }))
        << "should not also report "
           "Diag_This_Parameter_Not_Allowed_In_Arrow_Functions";
  }

  {
    Test_Parser p(u8"function(other, this) {}"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_expression();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_This_Parameter_Not_Allowed_In_JavaScript),
                }))
        << "should not also report Diag_This_Parameter_Must_Be_First";
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
