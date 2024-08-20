// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Parse,
     let_variable_use_before_declaration_with_parsing) {
  test_parse_and_analyze(
      u8"let x = y, y = x;"_sv,  //
      u8"           ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag  //
      u8"        ^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, generic_parameter_use_before_declaration) {
  test_parse_and_analyze(
      u8"function f<T extends T>() {}"_sv,
      u8"                     ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8"           ^ .declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer_Parse,
    variables_with_different_escape_sequences_are_equivalent_after_normalization) {
  test_parse_and_analyze(u8"let \\u{69} = 0; i += 1; \\u0069;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
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
  test_parse_and_analyze(u8R"(function f() { return \u{61}rgument\u{73}; })"_sv,
                         no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse,
     function_statement_inside_if_does_not_conflict_with_let_variable) {
  test_parse_and_analyze(u8"let f;\nif (true)\n  function f() {}"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, typeof_with_conditional_operator) {
  test_parse_and_analyze(u8"typeof x ? 10 : 20;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, prefix_plusplus_on_const_variable) {
  test_parse_and_analyze(
      u8"const x = 42; ++x;"_sv,  //
      u8"                ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(u8"const x = {y : 10};\n ++x.y;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, prefix_plusplus_plus_operand) {
  test_parse_and_analyze(u8"const x = [42]; ++x[0];"_sv, no_diags,
                         javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"const x = 42;\n const y =10;\n ++x + y;"_sv,  //
      u8"                                 ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, use_await_label_in_non_async_function) {
  test_parse_and_analyze(u8"function f() {await: for(;;){break await;}}"_sv,
                         no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, use_yield_label_in_non_generator_function) {
  test_parse_and_analyze(u8"function f() {yield: for(;;){break yield;}}"_sv,
                         no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, escape_sequence_in_keyword_identifier) {
  // The parser should not report a stray 'finally' keyword.
  // The linter should not report that 'finally' is undeclared.
  test_parse_and_analyze(u8"let which = \\u{66}inally;"_sv,
                         u8"Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, delete_local_variable) {
  test_parse_and_analyze(
      u8"function f(param) { let v; delete v; delete param; }"_sv,
      u8"Diag_Redundant_Delete_Statement_On_Variable"_diag,
      u8"Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse, extends_self) {
  test_parse_and_analyze(
      u8"function C() {}\n"_sv
      u8"{\n"_sv
      u8"  class C extends C {}"_sv
      u8"}"_sv,
      u8"Diag_Variable_Used_Before_Declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function C() {}\n"_sv
      u8"{\n"_sv
      u8"  class C extends (null, [C][0], Object) {}"_sv
      u8"}"_sv,
      u8"Diag_Variable_Used_Before_Declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function C() {}\n"_sv
      u8"{\n"_sv
      u8"  (class C extends C {})"_sv
      u8"}"_sv,
      u8"Diag_Variable_Used_Before_Declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Parse,
     typescript_static_block_can_reference_class) {
  test_parse_and_analyze(u8"class C { static { C; } }"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"(class C { static { C; } });"_sv, no_diags,
                         javascript_analyze_options, default_globals);
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
