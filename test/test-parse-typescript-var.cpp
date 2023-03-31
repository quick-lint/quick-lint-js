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

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_var : public test_parse_expression {};

TEST_F(test_parse_typescript_var, let_can_have_type_annotation) {
  {
    test_parser p(u8"let x: C;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // C
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
  }

  {
    test_parser p(u8"let x: C = init;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // C
                              "visit_variable_use",          // init
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C", u8"init"}));
  }

  {
    test_parser p(u8"let [x, y, z]: Array = init;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // init
                              "visit_variable_type_use",     // Array
                              "visit_variable_declaration",  // x
                              "visit_variable_declaration",  // y
                              "visit_variable_declaration",  // z
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init", u8"Array"}));
  }

  {
    test_parser p(u8"let {p1, p2: x, p3 = y}: T;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // T
                              "visit_variable_declaration",  // p1
                              "visit_variable_declaration",  // p2
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // p3
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"y"}));
  }
}

TEST_F(test_parse_typescript_var, for_loop_init_can_have_type_annotation) {
  {
    test_parser p(u8"for (let i: N = 0; ;);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_type_use",     // N
                              "visit_variable_declaration",  // i
                              "visit_exit_for_scope",
                          }));
  }
}

TEST_F(test_parse_typescript_var,
       for_of_loop_variable_can_have_type_annotation) {
  {
    test_parser p(u8"for (let x: C of xs);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // xs
                              "visit_variable_type_use",     // C
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",
                          }));
  }

  {
    test_parser p(u8"for (const x: C of xs);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // xs
                              "visit_variable_type_use",     // C
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",
                          }));
  }
}

TEST_F(test_parse_typescript_var,
       for_in_loop_variable_can_have_type_annotation) {
  {
    test_parser p(u8"for (let x: C in xs);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // xs
                              "visit_variable_type_use",     // C
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",
                          }));
  }

  {
    test_parser p(u8"for (const x: C in xs);"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // xs
                              "visit_variable_type_use",     // C
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",
                          }));
  }
}

TEST_F(test_parse_typescript_var,
       catch_variable_can_have_any_or_unknown_or_star_type_annotation) {
  for (string8 type : {u8"*", u8"any", u8"unknown"}) {
    padded_string code(concat(u8"try { } catch (e: "_sv, type, u8") {} "_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // try {
                              "visit_exit_block_scope",      // } try
                              "visit_enter_block_scope",     // catch {
                              "visit_variable_declaration",  // e
                              "visit_exit_block_scope",      // } catch
                          }));
  }
}

TEST_F(test_parse_typescript_var,
       catch_variable_cannot_have_arbitrary_type_annotation) {
  {
    test_parser p(u8"try { } catch (e: SomeType) {} "_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // try {
                              "visit_exit_block_scope",      // } try
                              "visit_enter_block_scope",     // catch {
                              "visit_variable_declaration",  // e
                              "visit_exit_block_scope",      // } catch
                          }))
        << "SomeType should be ignored (no visit_variable_type_use)";
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_typescript_catch_type_annotation_must_be_any,  //
                        type_expression, strlen(u8"try { } catch (e: "),
                        u8"SomeType"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_var,
       catch_variable_type_annotations_are_not_allowed_in_javascript) {
  for (string8 type : {u8"*", u8"any", u8"unknown", u8"SomeType"}) {
    test_parser p(concat(u8"try { } catch (e: "_sv, type, u8") {} "_sv),
                  javascript_options, capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // try {
                              "visit_exit_block_scope",      // } try
                              "visit_enter_block_scope",     // catch {
                              "visit_variable_declaration",  // e
                              "visit_exit_block_scope",      // } catch
                          }))
        << "type should be ignored (no visit_variable_type_use)";
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_type_annotations_not_allowed_in_javascript,  //
                type_colon, strlen(u8"try { } catch (e"), u8":"_sv),
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
