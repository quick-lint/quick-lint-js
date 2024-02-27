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
TEST(Test_Variable_Analyzer_Assign, assign_to_mutable_variable) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  let x;"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  var x;"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  class x {}"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  function x() {}"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  try {"_sv
      u8"  } catch (x) {"_sv  // x is mutable
      u8"    x = 42;"_sv
      u8"  }"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  ((x) => {"_sv  // x is mutable
      u8"    x = 42;"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (function(x) {"_sv  // x is mutable
      u8"    x = 42;"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_mutable_variable_shadowing_immutable_variable) {
  test_parse_and_analyze(
      u8"import x from '';"_sv  // x is immutable
      u8"(() => {"_sv
      u8"  let x;"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, assign_to_immutable_const_variable) {
  test_parse_and_analyze(
      u8"(() => { const x = null; x = 42; });"_sv,
      u8"                         ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"               ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"const x = null; { x = 42; }"_sv,
      u8"                  ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_immutable_variable_before_declaration) {
  test_parse_and_analyze(
      u8"x = 42; const x = null;"_sv,
      u8"^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.assignment\n"_diag
      u8"              ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_shadowing_immutable_variable_before_declaration) {
  test_parse_and_analyze(
      u8"let x; { x = 42; const x = null; }"_sv,
      u8"                       ^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.declaration\n"_diag
      u8"         ^ .assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_immutable_variable_declared_in_parent_scope) {
  test_parse_and_analyze(
      u8"const x = null; (() => { x = 42; });"_sv,
      u8"                         ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_immutable_variable_declared_later_in_parent_scope) {
  test_parse_and_analyze(
      u8"(() => { x = 42; }); const x = null;"_sv,
      u8"                           ^ Diag_Assignment_To_Const_Variable.declaration\n"_diag
      u8"         ^ .assignment"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assignment_to_shadowed_const_variable_before_declaration_in_parent_scope) {
  test_parse_and_analyze(
      u8"let x; { { x = 42; } const x = null; }"_sv,
      u8"                           ^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.declaration\n"_diag
      u8"           ^ .assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assignment_to_const_variable_declared_in_grandparent_scope) {
  test_parse_and_analyze(
      u8"const x = null; (() => { (() => { x = 42; }); });"_sv,
      u8"                                  ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, assign_to_undeclared_variable) {
  test_parse_and_analyze(
      u8"x = null;"_sv,
      u8"^ Diag_Assignment_To_Undeclared_Variable.assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_inside_function_to_undeclared_variable) {
  test_parse_and_analyze(
      u8"(function() { x = null; });"_sv,
      u8"              ^ Diag_Assignment_To_Undeclared_Variable.assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, assign_to_variable_before_declaration) {
  test_parse_and_analyze(
      u8"x = null; let x;"_sv,
      u8"^ Diag_Assignment_Before_Variable_Declaration.assignment\n"_diag
      u8"              ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     assign_to_variable_before_hoistable_declaration) {
  test_parse_and_analyze(
      u8"x = null;"_sv
      u8"var x;"_sv,  // x is hoisted.
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, cannot_assign_to_typescript_enum) {
  test_parse_and_analyze(
      u8"enum E {}  E = null;"_sv,  //
      u8"           ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"     ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_enum}"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"E = null; enum E {}"_sv,  //
      u8"               ^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.declaration\n"_diag
      u8"^ .assignment"_diag
      u8"{.var_kind=Variable_Kind::_enum}"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     cannot_assign_to_typescript_namespace_alias) {
  test_parse_and_analyze(
      u8"namespace A {}  import B = A;  B = null;"_sv,  //
      u8"                               ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"                       ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_import_alias}"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace A {}  B = null; import B = A;"_sv,  //
      u8"                                 ^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.declaration\n"_diag
      u8"                ^ .assignment"_diag
      u8"{.var_kind=Variable_Kind::_import_alias}"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, cannot_assign_to_typescript_namespace) {
  test_parse_and_analyze(
      u8"namespace ns {}  ns = null;"_sv,  //
      u8"                 ^^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"          ^^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_namespace}"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"ns = null; namespace ns {}"_sv,  //
      u8"                     ^^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.declaration\n"_diag
      u8"^^ .assignment"_diag
      u8"{.var_kind=Variable_Kind::_namespace}"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, can_assign_to_class) {
  test_parse_and_analyze(u8"class C {}  C = null;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, cannot_assign_to_class_before_declaration) {
  test_parse_and_analyze(
      u8"C = null; class C {}"_sv,  //
      u8"                ^ Diag_Assignment_Before_Variable_Declaration.declaration\n"_diag
      u8"^ .assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign, cannot_assign_to_class_in_typescript) {
  test_parse_and_analyze(
      u8"class C {}  C = null;"_sv,  //
      u8"            ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"      ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_class}"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  (C) = null;"_sv,
                         u8"Diag_Assignment_To_Const_Variable"_diag,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  C! = null;"_sv,
                         u8"Diag_Assignment_To_Const_Variable"_diag,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Assign,
     can_assign_to_class_with_typescript_type_assertion) {
  test_parse_and_analyze(u8"class C {}  (<any>C) = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  (C as any) = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  (C satisfies any) = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  [C as any] = [null];"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  (C! satisfies any)! = null;"_sv,
                         no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"function f() { (<any>C) = null; }  class C {}"_sv,
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
