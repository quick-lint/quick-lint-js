// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Namespace, empty_namespace) {
  test_parse_and_analyze(u8"namespace NS { } "_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_visible_outside_namespace) {
  test_parse_and_analyze(
      u8"namespace NS { } "_sv
      u8"NS;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

// TODO(strager): Is this correct? TypeScript's compiler (as of v4.8.2)
// complains about accessing members of the namespace, but doesn't complain
// about referencing the namespace itself.
TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_usable_before_namespace) {
  test_parse_and_analyze(
      u8"NS;"_sv
      u8"namespace NS { } "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     namespace_name_is_visible_inside_namespace) {
  test_parse_and_analyze(
      u8"namespace NS {"_sv
      u8"  NS;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     variables_declared_inside_namespace_are_not_accessible_outside) {
  test_parse_and_analyze(
      u8"namespace NS { export class C {}  }  C;"_sv,
      u8"                                     ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace NS { export var v; }  v;"_sv,
      u8"                                ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     uses_in_namespace_might_refer_to_symbols_in_other_files) {
  test_parse_and_analyze(
      u8"namespace NS {"_sv
      u8"  myVar;"_sv                    // visit_variable_use
      u8"  myVarWithAssignment = 0;"_sv  // visit_variable_assignment
      u8"  delete myVarWithDelete;"_sv   // visit_variable_delete_use
      u8"  typeof myVarWithTypeof;"_sv   // visit_variable_typeof_use
      u8"  null as MyType;"_sv           // visit_variable_type_use
      u8"} "_sv,
      no_diags,
      Test_Parse_And_Analyze_Options{
          .parse_options = typescript_options,
          .analyze_options =
              Variable_Analyzer_Options{
                  // FIXME(strager): Should
                  // Diag_TypeScript_Delete_Cannot_Delete_Variables
                  // be reported here in typescript mode? We suppress it for
                  // now:
                  .allow_deleting_typescript_variable = true,
                  .eval_can_declare_variables = false,
              },
      },
      default_globals);
}

TEST(Test_Variable_Analyzer_Namespace,
     eval_in_namespace_cannot_declare_variables_outside_namespace) {
  test_parse_and_analyze(
      u8"namespace NS { eval('let myVar'); }  myVar;"_sv,
      u8"                                     ^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
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
