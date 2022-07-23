// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
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

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_var, let_can_have_type_annotation) {
  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"let x: C;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // C
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.variable_declarations, ElementsAre(let_noinit_decl(u8"x")));
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C"));
  }

  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"let x: C = init;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // C
                                      "visit_variable_use",            // init
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"C", u8"init"));
  }

  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"let [x, y, z]: Array = init;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // init
                                      "visit_variable_type_use",       // Array
                                      "visit_variable_declaration",    // x
                                      "visit_variable_declaration",    // y
                                      "visit_variable_declaration"));  // z
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"init", u8"Array"));
  }

  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"let {p1, p2: x, p3 = y}: T;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // T
                                      "visit_variable_declaration",    // p1
                                      "visit_variable_declaration",    // p2
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // p3
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"T", u8"y"));
  }
}

TEST(test_parse_typescript_var, function_parameter_can_have_type_annotation) {
  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"function f(p1: A, p2: B = init) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  // f
                                      "visit_variable_type_use",     // A
                                      "visit_variable_declaration",  // p1
                                      "visit_variable_use",          // init
                                      "visit_variable_type_use",     // B
                                      "visit_variable_declaration",  // p2
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"A", u8"init", u8"B"));
  }

  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"function f([a, b]: C) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       // f
                                      "visit_variable_type_use",          // C
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST(test_parse_typescript_var, method_parameter_can_have_type_annotation) {
  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"class C { method(param: Type) {} }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",       // C
                                      "visit_enter_class_scope_body",  // {
                                      "visit_property_declaration",    // f
                                      "visit_enter_function_scope",    // f
                                      "visit_variable_type_use",       // Type
                                      "visit_variable_declaration",    // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope",        // }
                                      "visit_exit_class_scope",           // }
                                      "visit_variable_declaration"));     // C
  }

  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"({ method(param: Type) {} });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  // f
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST(test_parse_typescript_var, arrow_parameter_can_have_type_annotation) {
  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"((param: Type) => {});"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }

  {
    parse_visit_collector v = parse_and_visit_typescript_statement(
        u8"((p1: T1, {p2}: T2 = init, [p3]: T3) => {});"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // T1
                                      "visit_variable_declaration",  // p1
                                      "visit_variable_use",          // init
                                      "visit_variable_type_use",     // T2
                                      "visit_variable_declaration",  // p2
                                      "visit_variable_type_use",     // T3
                                      "visit_variable_declaration",  // p3
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
  }
}

TEST(test_parse_typescript_var,
     arrow_parameter_without_parens_cannot_have_type_annotation) {
  {
    padded_string code(u8"(param: Type => {});"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            &code,
            diag_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"("), u8"param: Type",        //
            type_colon, strlen(u8"(param"), u8":")));
  }

  {
    padded_string code(u8"(async param: Type => {});"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",     // Type
                                      "visit_variable_declaration",  // param
                                      "visit_enter_function_scope_body",  // {
                                      "visit_exit_function_scope"));      // }
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            &code,
            diag_arrow_parameter_with_type_annotation_requires_parentheses,  //
            parameter_and_annotation, strlen(u8"(async "), u8"param: Type",  //
            type_colon, strlen(u8"(async param"), u8":")));
  }
}

TEST(test_parse_typescript_var, for_loop_init_can_have_type_annotation) {
  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"for (let i: N = 0; ;);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_type_use",     // N
                                      "visit_variable_declaration",  // i
                                      "visit_exit_for_scope"));
  }
}

TEST(test_parse_typescript_var, for_of_loop_variable_can_have_type_annotation) {
  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"for (let x: C of xs);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // xs
                                      "visit_variable_type_use",     // C
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope"));
  }

  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"for (const x: C of xs);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // xs
                                      "visit_variable_type_use",     // C
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope"));
  }
}

TEST(test_parse_typescript_var, for_in_loop_variable_can_have_type_annotation) {
  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"for (let x: C in xs);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // xs
                                      "visit_variable_type_use",     // C
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope"));
  }

  {
    parse_visit_collector v =
        parse_and_visit_typescript_statement(u8"for (const x: C in xs);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // xs
                                      "visit_variable_type_use",     // C
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope"));
  }
}

TEST(test_parse_typescript_var,
     catch_variable_can_have_any_or_unknown_or_star_type_annotation) {
  for (string8 type : {u8"*", u8"any", u8"unknown"}) {
    padded_string code(u8"try { } catch (e: " + type + u8") {} ");
    SCOPED_TRACE(code);
    parse_visit_collector v =
        parse_and_visit_typescript_statement(code.string_view());
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // try {
                                      "visit_exit_block_scope",      // } try
                                      "visit_enter_block_scope",     // catch {
                                      "visit_variable_declaration",  // e
                                      "visit_exit_block_scope"));    // } catch
  }
}

TEST(test_parse_typescript_var,
     catch_variable_cannot_have_arbitrary_type_annotation) {
  {
    padded_string code(u8"try { } catch (e: SomeType) {} "_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // try {
                                      "visit_exit_block_scope",      // } try
                                      "visit_enter_block_scope",     // catch {
                                      "visit_variable_declaration",  // e
                                      "visit_exit_block_scope"))     // } catch
        << "SomeType should be ignored (no visit_variable_type_use)";
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_typescript_catch_type_annotation_must_be_any,  //
            type_expression, strlen(u8"try { } catch (e: "), u8"SomeType")));
  }
}

TEST(test_parse_typescript_var,
     catch_variable_type_annotations_are_not_allowed_in_javascript) {
  for (string8 type : {u8"*", u8"any", u8"unknown", u8"SomeType"}) {
    padded_string code(u8"try { } catch (e: " + type + u8") {} ");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // try {
                                      "visit_exit_block_scope",      // } try
                                      "visit_enter_block_scope",     // catch {
                                      "visit_variable_declaration",  // e
                                      "visit_exit_block_scope"))     // } catch
        << "type should be ignored (no visit_variable_type_use)";
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_typescript_type_annotations_not_allowed_in_javascript,  //
            type_colon, strlen(u8"try { } catch (e"), u8":")));
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
