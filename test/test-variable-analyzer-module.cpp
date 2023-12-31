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
TEST(Test_Variable_Analyzer_Module, import_use_before_declaration_is_okay) {
  test_parse_and_analyze(u8"x; import x from '';"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module, assign_to_immutable_imported_variable) {
  test_parse_and_analyze(
      u8"import {x} from 'module'; { x = 42; }"_sv,
      u8"                            ^ Diag_Assignment_To_Imported_Variable.assignment\n"_diag
      u8"        ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_import}"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"x = 42; import {x} from 'module';"_sv,
      u8"                ^ Diag_Assignment_To_Imported_Variable.declaration\n"_diag
      u8"^ .assignment"_diag
      u8"{.var_kind=Variable_Kind::_import}"_diag,
      javascript_analyze_options, default_globals);

  // TODO(#1141): Report Diag_Assignment_To_Imported_Variable in TypeScript.
}

TEST(Test_Variable_Analyzer_Module, export_use_after_declaration_is_okay) {
  test_parse_and_analyze(u8"class x {}  export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"const x = null; export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"function x() {}  export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import x from ''; export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface x {}  export {x};"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x; export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"var x; export {x};"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module, export_use_before_declaration_is_okay) {
  test_parse_and_analyze(u8"export {x}; class x {} "_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; const x = null;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; function x() {} "_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; import x from '';"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; interface x {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; let x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {x}; var x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module, variable_export_with_no_declaration) {
  test_parse_and_analyze(
      u8"export {x};"_sv,
      u8"        ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module,
     variable_export_default_cannot_use_before_declaration) {
  test_parse_and_analyze(
      u8"export default C; class C {}"_sv,
      u8"                        ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"               ^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer_Module,
    variable_export_default_inside_typescript_declare_module_can_use_before_declaration) {
  test_parse_and_analyze(
      u8"declare module 'm' { export default C; class C {} }"_sv, no_diags,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module,
     variable_export_can_export_typescript_types) {
  test_parse_and_analyze(u8"interface I {}; export {I};"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export {T}; type T = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module,
     export_default_can_export_typescript_types_after_declaration) {
  test_parse_and_analyze(u8"interface I {}; export default I;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"type T = null; export default T;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Module,
     export_default_can_export_typescript_types_before_declaration) {
  test_parse_and_analyze(u8"export default I; interface I {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"export default T; type T = null;"_sv, no_diags,
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
