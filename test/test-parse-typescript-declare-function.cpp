// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
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
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Function : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_is_not_allowed_in_javascript) {
  {
    Test_Parser p(u8"declare function f();"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_Declare_Function_Not_Allowed_In_JavaScript,  //
                        declare_keyword, u8""_sv.size(), u8"declare"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function, basic_declare_function) {
  {
    Test_Parser p(u8"declare function f();"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_cannot_have_body) {
  {
    Test_Parser p(u8"declare function f() { }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Declare_Function_Cannot_Have_Body,  //
                        body_start, u8"declare function f() "_sv.size(),
                        u8"{"_sv,  //
                        declare_keyword, u8""_sv.size(), u8"declare"_sv),
                }));
  }

  {
    Test_Parser p(u8"declare function f() { } foo"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_use",               // foo
                              "visit_end_of_module",              //
                          }));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Declare_Function_Cannot_Have_Body),
                          }))
        << "should not receive a Diag_Missing_Semicolon_After_Statement";
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_must_have_name) {
  {
    Test_Parser p(u8"declare function ();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // (function)
                              "visit_exit_function_scope",   // (function)
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              Diag_Missing_Name_In_Function_Statement,  //
                              where, u8"declare function "_sv.size(), u8""_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_cannot_be_async_or_generator) {
  {
    Test_Parser p(u8"declare async function f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              Diag_Declare_Function_Cannot_Be_Async,  //
                              async_keyword, u8"declare "_sv.size(), u8"async"),
        }));
  }

  {
    Test_Parser p(u8"declare function* f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              Diag_Declare_Function_Cannot_Be_Generator,  //
                              star, u8"declare function"_sv.size(), u8"*"),
        }));
  }

  {
    Test_Parser p(u8"declare async function* f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Function_Cannot_Be_Async),
                    DIAG_TYPE(Diag_Declare_Function_Cannot_Be_Generator),
                }));
  }

  {
    Test_Parser p(u8"declare async function f() { await(myPromise); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}))
        << "'await' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in an "
           "async function";
    EXPECT_THAT(p.errors, UnorderedElementsAreArray({
                              DIAG_TYPE(Diag_Declare_Function_Cannot_Be_Async),
                              DIAG_TYPE(Diag_Declare_Function_Cannot_Have_Body),
                          }));
  }

  {
    Test_Parser p(u8"declare function* f() { yield(myValue); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "'yield' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in a "
           "generator function";
    EXPECT_THAT(p.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Function_Cannot_Be_Generator),
                    DIAG_TYPE(Diag_Declare_Function_Cannot_Have_Body),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_name_can_be_contextual_keyword) {
  Dirty_Set<String8> function_names =
      contextual_keywords | Dirty_Set<String8>{u8"await", u8"yield"};
  for (const String8& function_name : function_names) {
    Padded_String code(
        concat(u8"declare function "_sv, function_name, u8"(): void;"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // (function_name)
                              "visit_enter_function_scope",  // (function_name)
                              "visit_exit_function_scope",   // (function_name)
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(function_name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_before_function_keyword_triggers_asi) {
  {
    Test_Parser p(u8"declare\nfunction f() {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // declare
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_end_of_module",              //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function,
       declare_function_requires_semicolon) {
  {
    Test_Parser p(u8"declare function f() foo"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_use",          // foo
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        Diag_Missing_Semicolon_After_Statement,  //
                        where, u8"declare function f()"_sv.size(), u8""_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Function, declare_function_performs_asi) {
  {
    Test_Parser p(u8"declare function f()\nfoo"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // f
                              "visit_enter_function_scope",  // f
                              "visit_exit_function_scope",   // f
                              "visit_variable_use",          // foo
                              "visit_end_of_module",         //
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
