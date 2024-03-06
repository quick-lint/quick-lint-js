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
TEST(Test_Variable_Analyzer_Arguments,
     arguments_magic_variable_is_usable_within_functions) {
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  arguments;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments,
     arguments_magic_variable_is_unusable_in_global_scope) {
  test_parse_and_analyze(
      u8"arguments;"_sv,
      u8"^^^^^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments,
     parameter_named_arguments_does_not_conflict) {
  test_parse_and_analyze(
      u8"(function(arguments) {"_sv
      u8"  arguments;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments,
     parameter_default_values_can_reference_arguments) {
  test_parse_and_analyze(
      u8"(function(p = arguments) {"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  // 'arguments' refers to magic-arguments, not a local variable. If 'arguments'
  // referred to a local variable, this test would fail with a
  // use-before-declaration error.
  test_parse_and_analyze(
      u8"(function(p = arguments) {"_sv
      u8"  let arguments;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments,
     var_does_not_conflict_with_magic_arguments) {
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  var arguments;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments, let_shadows_magic_arguments) {
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  const arguments = null;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  let arguments;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() { arguments; const arguments = null; });"_sv,
      u8"              ^^^^^^^^^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                               ^^^^^^^^^ .declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function() { arguments; let arguments; });"_sv,
      u8"              ^^^^^^^^^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                             ^^^^^^^^^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments, function_shadows_magic_arguments) {
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  function arguments() {} "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Arguments, catch_variable_shadows_magic_arguments) {
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  try {"_sv
      u8"  } catch (arguments) {"_sv
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

// TODO(#204): 'arguments' should not be declared in arrow functions.
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
