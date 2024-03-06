// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
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
TEST(Test_Variable_Analyzer_Enum,
     member_initializers_can_reference_other_members) {
  test_parse_and_analyze(
      u8"enum E {"_sv
      u8"  A = 42,"_sv
      u8"  B = A,"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum, enum_can_merge_with_another_enum) {
  test_parse_and_analyze(
      u8"enum E {} "_sv
      u8"enum E {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum, enum_can_shadow_catch_variables) {
  test_parse_and_analyze(
      u8"try {"_sv
      u8"} catch (e) {"_sv
      u8"  enum e {} "_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum,
     enum_conflicts_with_most_variables_in_same_scope) {
  test_parse_and_analyze(
      u8"((E) => { enum E {} });"_sv,
      u8"               ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"  ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"class E {}  enum E {}"_sv,
      u8"                 ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const E = null; enum E {}"_sv,
      u8"                     ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function E() {}  enum E {}"_sv,
      u8"                      ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"         ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function(E) { enum E {} });"_sv,
      u8"                    ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"          ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function<E>() { enum E {} });"_sv,
      u8"                      ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"          ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import {E} from 'mod'; enum E {}"_sv,
      u8"                            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  // TODO(strager): Is declaring after _index_signature_parameter possible?
  test_parse_and_analyze(
      u8"interface E {}  enum E {}"_sv,
      u8"                     ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"          ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let E; enum E {}"_sv,
      u8"            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"var E; enum E {}"_sv,
      u8"            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);

  // TODO(strager): Is declaring before _arrow_parameter in the same scope
  // possible?
  test_parse_and_analyze(
      u8"enum E {}  class E {}"_sv,
      u8"                 ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"enum E {}  const E = null;"_sv,
      u8"                 ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"enum E {}  function E() {}"_sv,
      u8"                    ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  // TODO(strager): Is declaring before _function_parameter in the same scope
  // possible?
  // TODO(strager): Is declaring before _generic_parameter in the same scope
  // possible?
  test_parse_and_analyze(
      u8"enum E {}  import {E} from 'mod';"_sv,
      u8"                   ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  // TODO(strager): Is declaring before _index_signature_parameter in the same
  // scope possible?
  test_parse_and_analyze(
      u8"enum E {}  interface E {}"_sv,
      u8"                     ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"enum E {}  let E;"_sv,
      u8"               ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"enum E {}  var E;"_sv,
      u8"               ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum, function_shadows_enum_in_outer_scope) {
  test_parse_and_analyze(
      u8"enum E {} "_sv
      u8"{"_sv
      u8"  function E() {} "_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum, var_conflicts_with_enum_in_outer_scope) {
  test_parse_and_analyze(
      u8"enum e {}  { var e; }"_sv,
      u8"                 ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"{ var e; }  enum e {}"_sv,
      u8"                 ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Enum, enum_shadows_most_variables_in_outer_scope) {
  test_parse_and_analyze(u8"((E) => { { enum E {} } });"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class E {}  { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"const E = null; { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"function E() {}  { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"(function(E) { { enum E {} } });"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"(function<E>() { { enum E {} } });"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import {E} from 'mod'; { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  // TODO(strager): Is shadowing a _index_signature_parameter possible?
  test_parse_and_analyze(u8"interface E {}  { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let E; { enum E {} }"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"var E; { enum E {} }"_sv, no_diags,
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
