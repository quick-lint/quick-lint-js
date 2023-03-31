// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_variable_analyzer_parse,
     let_variable_use_before_declaration_with_parsing) {
  padded_string input(u8"let x = y, y = x;"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  EXPECT_TRUE(p.parse_and_visit_statement(l));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_OFFSETS(&input, diag_variable_used_before_declaration,  //
                              use, 8, u8"y"_sv, declaration, 11, u8"y"_sv),
      }));
}

TEST(test_variable_analyzer_parse, generic_parameter_use_before_declaration) {
  padded_string input(u8"function f<T extends T>() {}"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, typescript_var_options);
  parser p(&input, &v, typescript_options);
  EXPECT_TRUE(p.parse_and_visit_statement(l));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_OFFSETS(
                      &input, diag_variable_used_before_declaration,     //
                      use, strlen(u8"function f<T extends "), u8"T"_sv,  //
                      declaration, strlen(u8"function f<"), u8"T"_sv),
              }));
}

TEST(
    test_variable_analyzer_parse,
    variables_with_different_escape_sequences_are_equivalent_after_normalization) {
  padded_string input(u8"let \\u{69} = 0; i += 1; \\u0069;"_sv);
  diag_collector v;

  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_parse,
     errors_for_variables_with_escape_sequences_cover_entire_variable_name) {
  padded_string input(u8R"(const immut\u{61}ble = 0; immut\u{61}ble = 1;)"_sv);
  diag_collector v;

  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_FIELDS(
                      diag_assignment_to_const_variable,                 //
                      assignment, offsets_matcher(&input, 26, 26 + 14),  //
                      declaration, offsets_matcher(&input, 6, 6 + 14)),
              }));
}

TEST(test_variable_analyzer_parse,
     escape_sequences_are_allowed_for_arguments_variable) {
  padded_string input(u8R"(function f() { return \u{61}rgument\u{73}; })"_sv);
  diag_collector v;

  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_parse,
     function_statement_inside_if_does_not_conflict_with_let_variable) {
  padded_string input(u8"let f;\nif (true)\n  function f() {}"_sv);

  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_parse, typeof_with_conditional_operator) {
  {
    padded_string input(u8"typeof x ? 10 : 20;"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_parse, prefix_plusplus_on_const_variable) {
  {
    padded_string input(u8"const x = 42; ++x;"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_assignment_to_const_variable, assignment,
                               offsets_matcher(&input, 16, 16 + 1), declaration,
                               offsets_matcher(&input, 6, 6 + 1)),
        }));
  }

  {
    padded_string input(u8"const x = {y : 10};\n ++x.y;"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_parse, prefix_plusplus_plus_operand) {
  {
    padded_string input(u8"const x = [42]; ++x[0];"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string input(u8"const x = 42;\n const y =10;\n ++x + y;"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_assignment_to_const_variable, assignment,
                               offsets_matcher(&input, 31, 31 + 1), declaration,
                               offsets_matcher(&input, 6, 6 + 1)),
        }));
  }
}

TEST(test_variable_analyzer_parse, use_await_label_in_non_async_function) {
  padded_string input(u8"function f() {await: for(;;){break await;}}"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_parse, use_yield_label_in_non_generator_function) {
  padded_string input(u8"function f() {yield: for(;;){break yield;}}"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_parse, escape_sequence_in_keyword_identifier) {
  // The parser should not report a stray 'finally' keyword.
  // The linter should not report that 'finally' is undeclared.
  padded_string input(u8"let which = \\u{66}inally;"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
              }));
}

TEST(test_variable_analyzer_parse, delete_local_variable) {
  padded_string input(
      u8"function f(param) { let v; delete v; delete param; }"_sv);
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE(diag_redundant_delete_statement_on_variable),
                  DIAG_TYPE(diag_redundant_delete_statement_on_variable),
              }));
}

TEST(test_variable_analyzer_parse, extends_self) {
  {
    padded_string input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  class C extends C {}"_sv
        u8"}"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_variable_used_before_declaration),
                          }));
  }

  {
    padded_string input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  class C extends (null, [C][0], Object) {}"_sv
        u8"}"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_variable_used_before_declaration),
                          }));
  }

  {
    padded_string input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  (class C extends C {})"_sv
        u8"}"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_variable_used_before_declaration),
                          }));
  }
}

TEST(test_variable_analyzer_parse,
     typescript_static_block_can_reference_class) {
  {
    padded_string input(u8"class C { static { C; } }"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, typescript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string input(u8"(class C { static { C; } });"_sv);
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    parser p(&input, &v, typescript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();
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
