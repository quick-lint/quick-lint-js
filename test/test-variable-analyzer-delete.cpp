// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_local_variable_is_a_warning) {
  test_parse_and_analyze(
      u8"(() => { let v; delete v; });"_sv,
      u8"                ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => { let v; { delete v; } });"_sv,
      u8"                  ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => { let v; (() => { delete v; }); });"_sv,
      u8"                         ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => { (() => { delete v; }); let v; });"_sv,
      u8"                  ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => { let v; (() => { (() => { delete v; }); }); });"_sv,
      u8"                                  ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_local_variable_declared_later_is_a_warning) {
  test_parse_and_analyze(
      u8"(() => { delete v; let v; });"_sv,
      u8"         ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_declared_module_variable_is_a_warning) {
  test_parse_and_analyze(
      u8"let v; delete v;"_sv,
      u8"       ^^^^^^^^ Diag_Redundant_Delete_Statement_On_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_declared_global_variable_is_ok) {
  Global_Declared_Variable_Set globals;
  globals.add_global_variable(Global_Declared_Variable{
      .name = u8"myGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
      .is_type_only = false,
  });

  test_parse_and_analyze(u8"delete myGlobalVariable;"_sv, no_diags,
                         javascript_analyze_options, globals);
  test_parse_and_analyze(u8"(() => { delete myGlobalVariable; });"_sv, no_diags,
                         javascript_analyze_options, globals);
}

TEST(Test_Variable_Analyzer_Delete_JavaScript,
     deleting_undeclared_global_variable_is_ok) {
  Global_Declared_Variable_Set globals;
  ASSERT_FALSE(
      globals.find_runtime_or_type(u8"myGlobalVariable"_sv).has_value());

  test_parse_and_analyze(u8"delete myGlobalVariable;"_sv, no_diags,
                         javascript_analyze_options, globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  delete myGlobalVariable;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, globals);
}

TEST(Test_Variable_Analyzer_Delete_TypeScript,
     deleting_local_variable_is_an_error) {
  test_parse_and_analyze(
      u8"let myVar; delete myVar;"_sv,
      u8"           ^^^^^^^^^^^^ Diag_TypeScript_Delete_Cannot_Delete_Variables"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"delete myVar; let myVar;"_sv,
      u8"^^^^^^^^^^^^ Diag_TypeScript_Delete_Cannot_Delete_Variables"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Delete_TypeScript,
     deleting_global_variable_is_an_error) {
  {
    Global_Declared_Variable_Set globals;
    ASSERT_FALSE(
        globals.find_runtime_or_type(u8"myGlobalVariable"_sv).has_value());

    test_parse_and_analyze(
        u8"delete myGlobalVariable;"_sv,
        u8"^^^^^^^^^^^^^^^^^^^^^^^ Diag_TypeScript_Delete_Cannot_Delete_Variables"_diag,
        typescript_analyze_options, globals);
  }

  {
    Global_Declared_Variable_Set globals;
    globals.add_global_variable(Global_Declared_Variable{
        .name = u8"myGlobalVariable"_sv,
        .is_writable = true,
        .is_shadowable = true,
        .is_type_only = false,
    });

    test_parse_and_analyze(
        u8"delete myGlobalVariable;"_sv,
        u8"^^^^^^^^^^^^^^^^^^^^^^^ Diag_TypeScript_Delete_Cannot_Delete_Variables"_diag,
        typescript_analyze_options, globals);
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
