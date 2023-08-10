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
TEST(Test_Variable_Analyzer_Eval_JavaScript,
     disable_variable_lookup_in_presence_of_eval) {
  test_parse_and_analyze(
      u8"eval('var x = 42');"_sv
      u8"x;"_sv
      u8"x = 10;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"{"_sv
      u8"  eval('var x = 42');"_sv
      u8"} "_sv
      u8"x;"_sv
      u8"x = 10;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"eval('var x = 42');"_sv
      u8"(function() {"_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"});"_sv
      u8"eval('var x = 42');"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"  eval('var x = 42;');"_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"  {"_sv
      u8"    eval('var x = 42;');"_sv
      u8"  } "_sv
      u8"  x;"_sv
      u8"  x = 10;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Eval_JavaScript,
     disable_variable_lookup_in_presence_of_eval_for_limited_scope) {
  test_parse_and_analyze(
      u8"(function() { eval('var x = 42;'); }); (function() { x; });"_sv,
      u8"                                                     ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() { eval('var x = 42;'); }); x;"_sv,
      u8"                                       ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() { (function() { eval('var x = 42;'); }); x; x = 10; });"_sv,
      u8"                                                        ^ Diag_Assignment_To_Undeclared_Variable"_diag,
      u8"                                                     ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function f(a = eval('var x = 42;')) { x; }); x;"_sv,
      u8"                                              ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let eval = null; eval('var x = 42;'); x; x = 10;"_sv,
      u8"                                         ^ Diag_Assignment_To_Undeclared_Variable"_diag,
      u8"                                      ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() { eval = null; eval('var x = 42;'); x; x = 10; { var eval; } });"_sv,
      u8"                                                   ^ Diag_Assignment_To_Undeclared_Variable"_diag,
      u8"                                                ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Eval_JavaScript,
     false_negatives_on_redeclaration_of_eval) {
  test_parse_and_analyze(
      u8"let eval = () => {};"_sv
      u8"(function() {"_sv
      u8"  eval('var x = 42;');"_sv
      u8"  x;"_sv       // TODO: ERROR (use of undeclared variable)
      u8"  x = 10;"_sv  // TODO: ERROR (assignment to undeclared variable)
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  const x = 42;"_sv
      u8"  {"_sv
      u8"    eval('var x = 0');"_sv
      u8"    x = 3;"_sv  // TODO: ERROR (assignment to const variable)
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Eval_TypeScript,
     eval_does_not_disable_variable_lookup) {
  test_parse_and_analyze(
      u8"eval('var x = 42'); x; x = 10;"_sv,
      u8"                       ^ Diag_Assignment_To_Undeclared_Variable"_diag,
      u8"                    ^ Diag_Use_Of_Undeclared_Variable"_diag,
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
