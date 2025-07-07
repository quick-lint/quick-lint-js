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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Typeof,
     using_undeclared_variable_in_typeof_is_not_an_error) {
  test_parse_and_analyze(u8"typeof v;"_sv, no_diags, typescript_analyze_options,
                         default_globals);
}

TEST(Test_Variable_Analyzer_Typeof, typeof_declares_variable_automagically) {
  test_parse_and_analyze(
      u8"typeof v;"_sv
      u8"v;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_declares_variable_automagically_in_parent_function) {
  test_parse_and_analyze(
      u8"v;"_sv
      u8"(() => {"_sv
      u8"  typeof v;"_sv
      u8"});"_sv
      u8"v;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_refers_to_already_declared_variable) {
  test_parse_and_analyze(
      u8"let v;"_sv
      u8"typeof v;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Typeof,
     typeof_variable_declared_later_is_an_error) {
  test_parse_and_analyze(
      u8"typeof v; let v;"_sv,
      u8"       ^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"              ^ .declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer_Typeof,
    typeof_already_declared_variable_does_not_declare_variable_in_parent_function) {
  test_parse_and_analyze(
      u8"v; (() => { let v; typeof v; }); v;"_sv,
      u8"                                 ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      u8"^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Typeof, typeof_comparison_invalid_string_literals) {
  test_parse_and_analyze(
      u8"let v = 5; typeof v == 'nmber'"_sv,
      u8"                       ^^^^^^^ Diag_Typeof_Invalid_String_Comparison"_diag,
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
