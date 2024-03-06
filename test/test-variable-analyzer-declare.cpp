// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Declare,
     variables_marked_declare_can_be_used_before_declaration) {
  test_parse_and_analyze(u8"C; declare class C {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"a; declare const a;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Declare,
     uses_in_declare_context_can_use_variables_before_declaration) {
  test_parse_and_analyze(
      u8"declare class Derived extends Base {}  class Base {}"_sv, no_diags,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare namespace ns {\n"_sv
      u8"  class Derived extends Base {}\n"_sv
      u8"}\n"_sv
      u8"class Base {}"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer_Declare,
    typescript_declare_global_variables_are_usable_before_or_after_declaration) {
  test_parse_and_analyze(u8"x;  declare global { const x; }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"declare global { const x; }  x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Declare,
     typescript_declare_global_variables_are_shadowable) {
  // This should not report Diag_Redeclaration_Of_Variable or
  // Diag_Assignment_To_Const_Variable.
  test_parse_and_analyze(u8"let x;  x = 42;  declare global { const x; }"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x;  declare global { const x; }  x = 42;"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"declare global { const x; }  let x; x = 42;"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Declare,
     parameters_in_declare_global_are_not_usable_outside) {
  test_parse_and_analyze(u8"x;  declare global { function f(x); }"_sv,  //
                         u8"^ Diag_Use_Of_Undeclared_Variable"_diag,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare global { function f(x); }  x;"_sv,  //
      u8"                                   ^ Diag_Use_Of_Undeclared_Variable"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare global { class C<T> {} }  T;"_sv,  //
      u8"                                  ^ Diag_Use_Of_Undeclared_Variable"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Declare,
     undeclared_type_or_variable_in_global_declare_block_reports_error) {
  test_parse_and_analyze(
      u8"declare global { let x: UndefinedType; }"_sv,  //
      u8"                        ^^^^^^^^^^^^^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare global { let x: typeof undefinedVariable; }"_sv,  //
      u8"                               ^^^^^^^^^^^^^^^^^ Diag_Use_Of_Undeclared_Variable"_diag,
      typescript_analyze_options, default_globals);
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
