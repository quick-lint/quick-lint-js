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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Interface,
     interface_body_can_reference_types_outside) {
  test_parse_and_analyze(
      u8"import {C} from 'other-module';"_sv
      u8"interface I {"_sv
      u8"  method(): C;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"interface I { method(): C; }"_sv,
      u8"                        ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Interface,
     generic_interface_parameters_are_usable_inside) {
  test_parse_and_analyze(
      u8"interface I<T> {"_sv
      u8"  method(): T;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_can_use_outside_types) {
  test_parse_and_analyze(
      u8"import {C} from 'other-module';"_sv
      u8"interface I {"_sv
      u8"  [index: C]: C;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"interface I { [index: C]: C; }"_sv,
      u8"                          ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      u8"                      ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_variable_is_usable_inside) {
  test_parse_and_analyze(
      u8"interface I {"_sv
      u8"  [index: number]: typeof index;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Interface,
     interface_index_signature_variable_is_not_usable_outside) {
  test_parse_and_analyze(
      u8"interface I { [index: number]: number; index: typeof index; }  index;"_sv,
      u8"                                                               ^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      u8"                                                     ^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     interface_can_use_runtime_variable_before_declaration) {
  test_parse_and_analyze(
      u8"interface I { field: typeof y; }  let y: string;"_sv, no_diags,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface I { [y]: string; }  let y: string;"_sv,
                         no_diags, typescript_analyze_options, default_globals);
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
