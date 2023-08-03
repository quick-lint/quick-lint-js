// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
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
TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_initialized_var_without_use_in_block_scope_is_warning) {
  struct Test_Case {
    String8_View outer;
    String8_View inner;
  };
  for (Test_Case tc : {
           Test_Case{u8"const            "_sv, u8"const              "_sv},
           Test_Case{u8"const            "_sv, u8"let                "_sv},
           Test_Case{u8"let              "_sv, u8"const              "_sv},
           Test_Case{u8"let              "_sv, u8"let                "_sv},
           Test_Case{u8"var              "_sv, u8"const              "_sv},
           Test_Case{u8"var              "_sv, u8"let                "_sv},
       }) {
    // clang-format off
    test_parse_and_analyze(
        concat(tc.outer, u8" x = 5; { "_sv, tc.inner, u8" x = 6; }"_sv),
        u8"                                               ^ Diag_Unused_Variable_Shadows.shadowing_declaration\n"_diag
        u8"                  ^ .shadowed_declaration"_diag,
        javascript_analyze_options, default_globals);

    // TODO(strager): See NOTE[unused-var-shadows-nested-block].
    if ((false)) {
      test_parse_and_analyze(
          concat(tc.outer, u8" x = 5; { { "_sv, tc.inner, u8" x = 6; } }"_sv),
          u8"                                                 ^ Diag_Unused_Variable_Shadows.shadowing_declaration\n"_diag
          u8"                  ^ .shadowed_declaration"_diag,
          javascript_analyze_options, default_globals);
    }
    // clang-format on
  }
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_function_scope_var_without_use_in_block_scope_is_not_a_warning) {
  test_parse_and_analyze(
      u8"var x = 5;"_sv
      u8"{"_sv
      u8"  var x = 6;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_unassigned_var_in_block_scope_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_var_without_use_in_function_scope_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"(function() {"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_parameter_is_not_a_warning) {
  test_parse_and_analyze(
      u8"(function(x) {"_sv
      u8"  {"_sv
      u8"    let x = 6;"_sv  // no warning
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_class_or_function_or_import_is_not_a_warning) {
  test_parse_and_analyze(
      u8"class C {}\n"_sv
      u8"{"_sv
      u8"  let C = 6;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function C() {}\n"_sv
      u8"{"_sv
      u8"  let C = 6;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import {C} from 'module';\n"_sv
      u8"{"_sv
      u8"  let C = 6;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     shadowing_catch_variable_is_not_a_warning) {
  test_parse_and_analyze(
      u8"try {"_sv
      u8"} catch (e) {"_sv
      u8"  {"_sv
      u8"    let e = 6;"_sv  // no warning
      u8"  } "_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     using_shadowing_variable_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  x;"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  {"_sv
      u8"    x;"_sv
      u8"  } "_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  (function() {"_sv
      u8"    x;"_sv
      u8"  });"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     using_shadowing_variable_before_its_declaration_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5; { x; let x = 6; }"_sv,
      u8"             ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let x = 5; { { x; }  let x = 6; }"_sv,
      u8"               ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  (function() {"_sv
      u8"    x;"_sv  // no error
      u8"  });"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     using_shadowing_variable_with_eval_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  eval('x');"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  {"_sv
      u8"    eval('x');"_sv
      u8"  } "_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  {"_sv
      u8"    {"_sv
      u8"      eval('x');"_sv
      u8"    } "_sv
      u8"  } "_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  (function() {"_sv
      u8"    eval('x');"_sv
      u8"  });"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  (function() {"_sv
      u8"    (function() {"_sv
      u8"      eval('x');"_sv
      u8"    });"_sv
      u8"  });"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Unused_Shadow,
     assigning_to_shadowing_variable_is_not_a_warning) {
  test_parse_and_analyze(
      u8"let x = 5;"_sv
      u8"{"_sv
      u8"  let x = 6;"_sv  // no warning
      u8"  x = 7;"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
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
