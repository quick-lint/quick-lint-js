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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Parse,
     let_variable_use_before_declaration_with_parsing) {
  Padded_String input(u8"let x = y, y = x;"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  EXPECT_TRUE(p.parse_and_visit_statement(l));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_OFFSETS(&input, Diag_Variable_Used_Before_Declaration,  //
                              use, 8, u8"y"_sv, declaration, 11, u8"y"_sv),
      }));
}

TEST(Test_Variable_Analyzer_Parse, generic_parameter_use_before_declaration) {
  Padded_String input(u8"function f<T extends T>() {}"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, typescript_var_options);
  Parser p(&input, &v, typescript_options);
  EXPECT_TRUE(p.parse_and_visit_statement(l));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_OFFSETS(
                      &input, Diag_Cyclic_TypeScript_Type_Definition,      //
                      use, u8"function f<T extends "_sv.size(), u8"T"_sv,  //
                      declaration, u8"function f<"_sv.size(), u8"T"_sv),
              }));
}

TEST(
    Test_Variable_Analyzer_Parse,
    variables_with_different_escape_sequences_are_equivalent_after_normalization) {
  Padded_String input(u8"let \\u{69} = 0; i += 1; \\u0069;"_sv);
  Diag_Collector v;

  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Parse,
     errors_for_variables_with_escape_sequences_cover_entire_variable_name) {
  test_parse_and_analyze(
      u8"const immut\\u{61}ble = 0; immut\\u{61}ble = 1;"_sv,  //
      u8"                           ^^^^^^^^^^^^^^^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^^^^^^^^^^^^^^^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse,
     escape_sequences_are_allowed_for_arguments_variable) {
  Padded_String input(u8R"(function f() { return \u{61}rgument\u{73}; })"_sv);
  Diag_Collector v;

  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Parse,
     function_statement_inside_if_does_not_conflict_with_let_variable) {
  Padded_String input(u8"let f;\nif (true)\n  function f() {}"_sv);

  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Parse, typeof_with_conditional_operator) {
  {
    Padded_String input(u8"typeof x ? 10 : 20;"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Parse, prefix_plusplus_on_const_variable) {
  test_parse_and_analyze(
      u8"const x = 42; ++x;"_sv,  //
      u8"                ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag,
      javascript_analyze_options, default_globals);

  {
    Padded_String input(u8"const x = {y : 10};\n ++x.y;"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Parse, prefix_plusplus_plus_operand) {
  {
    Padded_String input(u8"const x = [42]; ++x[0];"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  test_parse_and_analyze(
      u8"const x = 42;\n const y =10;\n ++x + y;"_sv,  //
      u8"                                 ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, use_await_label_in_non_async_function) {
  Padded_String input(u8"function f() {await: for(;;){break await;}}"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Parse, use_yield_label_in_non_generator_function) {
  Padded_String input(u8"function f() {yield: for(;;){break yield;}}"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer_Parse, escape_sequence_in_keyword_identifier) {
  // The parser should not report a stray 'finally' keyword.
  // The linter should not report that 'finally' is undeclared.
  Padded_String input(u8"let which = \\u{66}inally;"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE(Diag_Keywords_Cannot_Contain_Escape_Sequences),
              }));
}

TEST(Test_Variable_Analyzer_Parse, delete_local_variable) {
  Padded_String input(
      u8"function f(param) { let v; delete v; delete param; }"_sv);
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  Parser p(&input, &v, javascript_options);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE(Diag_Redundant_Delete_Statement_On_Variable),
                  DIAG_TYPE(Diag_Redundant_Delete_Statement_On_Variable),
              }));
}

TEST(Test_Variable_Analyzer_Parse, extends_self) {
  {
    Padded_String input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  class C extends C {}"_sv
        u8"}"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Variable_Used_Before_Declaration),
                          }));
  }

  {
    Padded_String input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  class C extends (null, [C][0], Object) {}"_sv
        u8"}"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Variable_Used_Before_Declaration),
                          }));
  }

  {
    Padded_String input(
        u8"function C() {}\n"_sv
        u8"{\n"_sv
        u8"  (class C extends C {})"_sv
        u8"}"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, javascript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Variable_Used_Before_Declaration),
                          }));
  }
}

TEST(Test_Variable_Analyzer_Parse,
     typescript_static_block_can_reference_class) {
  {
    Padded_String input(u8"class C { static { C; } }"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, typescript_options);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    Padded_String input(u8"(class C { static { C; } });"_sv);
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    Parser p(&input, &v, typescript_options);
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
