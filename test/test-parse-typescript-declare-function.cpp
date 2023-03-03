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
#include <quick-lint-js/fe/diagnostic-types.h>
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
class test_parse_typescript_declare_function : public test_parse_expression {};

TEST_F(test_parse_typescript_declare_function,
       declare_function_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"declare function f();"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_declare_function_not_allowed_in_javascript,  //
                        declare_keyword, strlen(u8""), u8"declare"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_function, basic_declare_function) {
  {
    test_parser p(u8"declare function f();"_sv, typescript_options);
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

TEST_F(test_parse_typescript_declare_function,
       declare_function_cannot_have_body) {
  {
    test_parser p(u8"declare function f() { }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                diag_declare_function_cannot_have_body,  //
                                body_start, strlen(u8"declare function f() "),
                                u8"{"_sv,  //
                                declare_keyword, strlen(u8""), u8"declare"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_function,
       declare_function_must_have_name) {
  {
    test_parser p(u8"declare function ();"_sv, typescript_options,
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
                              diag_missing_name_in_function_statement,  //
                              where, strlen(u8"declare function "), u8""_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_function,
       declare_function_cannot_be_async_or_generator) {
  {
    test_parser p(u8"declare async function f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_declare_function_cannot_be_async,  //
                              async_keyword, strlen(u8"declare "), u8"async"),
        }));
  }

  {
    test_parser p(u8"declare function* f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_declare_function_cannot_be_generator,  //
                              star, strlen(u8"declare function"), u8"*"),
        }));
  }

  {
    test_parser p(u8"declare async function* f();"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE(diag_declare_function_cannot_be_async),
                    DIAG_TYPE(diag_declare_function_cannot_be_generator),
                }));
  }

  {
    test_parser p(u8"declare async function f() { await(myPromise); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}))
        << "'await' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in an "
           "async function";
    EXPECT_THAT(p.errors, UnorderedElementsAreArray({
                              DIAG_TYPE(diag_declare_function_cannot_be_async),
                              DIAG_TYPE(diag_declare_function_cannot_have_body),
                          }));
  }

  {
    test_parser p(u8"declare function* f() { yield(myValue); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "'yield' should be interpreted as an operator, not a function, "
           "because the function's body should be parsed as if it was in a "
           "generator function";
    EXPECT_THAT(p.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE(diag_declare_function_cannot_be_generator),
                    DIAG_TYPE(diag_declare_function_cannot_have_body),
                }));
  }
}

TEST_F(test_parse_typescript_declare_function,
       declare_function_name_can_be_contextual_keyword) {
  dirty_set<string8> function_names =
      contextual_keywords | dirty_set<string8>{u8"await", u8"yield"};
  for (const string8& function_name : function_names) {
    padded_string code(
        concat(u8"declare function "_sv, function_name, u8"(): void;"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
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

TEST_F(test_parse_typescript_declare_function,
       declare_before_function_keyword_triggers_asi) {
  {
    test_parser p(u8"declare\nfunction f() {}"_sv, typescript_options);
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
