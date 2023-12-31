// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

// This file contains tests for multiple declarations with the same name.
namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Multiple_Declarations,
     enum_and_namespace_do_not_conflict) {
  test_parse_and_analyze(
      u8"namespace A {} "_sv
      u8"enum A {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"enum A {} "_sv
      u8"namespace A {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     variable_and_namespace_do_not_conflict) {
  test_parse_and_analyze(u8"namespace n {}  const n = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"const n = null; namespace n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"namespace n {}  let n;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let n; namespace n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"namespace n {}  var n;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"var n; namespace n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     type_or_interface_does_not_conflict_with_namespace) {
  test_parse_and_analyze(u8"namespace n {}   type n = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"namespace n {;}  type n = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"type n = null;   namespace n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"type n = null;   namespace n {;}"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"namespace n {}   interface n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"namespace n {;}  interface n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface n {}   namespace n {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface n {}   namespace n {;}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     namespace_can_be_declared_multiple_times) {
  test_parse_and_analyze(
      u8"namespace ns {} "_sv
      u8"namespace ns {} "_sv
      u8"namespace ns {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     type_alias_and_local_variable_do_not_conflict) {
  test_parse_and_analyze(u8"type x = null; const x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"const x = null; type x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"type x = null; let x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x; type x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"type x = null; var x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"var x; type x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_does_not_conflict_with_interface_or_type_alias) {
  test_parse_and_analyze(u8"type x = null; function x() {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"function x() {}  type x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"interface x {}  function x() {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"function x() {}  interface x {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     namespace_can_appear_after_function_or_class_with_same_name) {
  test_parse_and_analyze(
      u8"function x() {} "_sv
      u8"namespace x {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"class x {} "_sv
      u8"namespace x {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_or_class_cannot_appear_after_non_empty_namespace_with_same_name) {
  test_parse_and_analyze(
      u8"namespace x { ; }  function x() {}"_sv,
      u8"                            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"          ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"namespace x { ; }  class x {}"_sv,
      u8"                         ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"          ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_or_class_can_appear_after_empty_namespace_with_same_name) {
  test_parse_and_analyze(
      u8"namespace x {} "_sv
      u8"function x() {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"namespace x {} "_sv
      u8"class x {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     function_parameter_can_have_same_name_as_generic_parameter) {
  test_parse_and_analyze(u8"(function <T>(T) {});"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"(<T>(T) => {});"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     declare_class_does_not_conflict_with_function) {
  test_parse_and_analyze(u8"declare class C {}  function C() {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"function C() {}  declare class C {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"declare class C {}  declare function C();"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"declare function C(); declare class C {}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     declare_function_conflicts_with_non_declare_class) {
  test_parse_and_analyze(
      u8"class C {}  declare function C();"_sv,  //
      u8"                             ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare function C(); class C {}"_sv,  //
      u8"                            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                 ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     declare_class_does_not_conflict_with_namespace) {
  test_parse_and_analyze(u8"declare class C {}  namespace C {var x;}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"namespace C {var x;}  declare class C {}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     declare_namespace_does_not_conflict_with_class) {
  test_parse_and_analyze(u8"declare namespace C {var x;}  class C {}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  declare namespace C {var x;}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     declare_namespace_does_not_conflict_with_declare_class) {
  test_parse_and_analyze(
      u8"declare namespace C {var x;}  declare class C {}"_sv, no_diags,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"declare class C {}  declare namespace C {var x;}"_sv, no_diags,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_conflicts_with_any_variable_declaration) {
  test_parse_and_analyze(
      u8"import x from ''; class x {}"_sv,
      u8"                        ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import x from ''; const x = null;"_sv,
      u8"                        ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import x from ''; function x() {}"_sv,
      u8"                           ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import x from ''; import x from '';"_sv,
      u8"                         ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import x from ''; let x;"_sv,
      u8"                      ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"import x from ''; var x;"_sv,
      u8"                      ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"       ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"class x {}  import x from '';"_sv,
      u8"                   ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const x = null; import x from '';"_sv,
      u8"                       ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"      ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function x() {}  import x from '';"_sv,
      u8"                        ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"         ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let x; import x from '';"_sv,
      u8"              ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"var x; import x from '';"_sv,
      u8"              ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     typescript_import_does_not_conflict_with_runtime_only_variables) {
  for (String8_View other_thing : {
           u8"const x = null;"_sv,
           u8"function x() {}"_sv,
           u8"let x;"_sv,
           u8"var x;"_sv,
       }) {
    test_parse_and_analyze(concat(u8"import x from ''; "_sv, other_thing),
                           no_diags, typescript_analyze_options,
                           default_globals);
    test_parse_and_analyze(concat(other_thing, u8" import x from '';"_sv),
                           no_diags, typescript_analyze_options,
                           default_globals);
  }
}

TEST(
    Test_Variable_Analyzer_Multiple_Declarations,
    using_implicit_type_import_and_runtime_variable_with_same_name_is_not_an_error) {
  test_parse_and_analyze(u8"import x from ''; let x; x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x; import x from ''; x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import x from ''; x; var x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import x from ''; let x; x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import x from ''; let x; x = null;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import x from ''; x = null; var x;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     typescript_import_does_not_conflict_with_type_only_variables) {
  for (String8_View other_thing : {
           u8"interface x {}"_sv,
           u8"type x = null;"_sv,
           u8"import {type x} from 'othermod';"_sv,
       }) {
    test_parse_and_analyze(concat(u8"import x from ''; "_sv, other_thing),
                           no_diags, typescript_analyze_options,
                           default_globals);
    test_parse_and_analyze(concat(other_thing, u8" import x from '';"_sv),
                           no_diags, typescript_analyze_options,
                           default_globals);
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     typescript_import_conflicts_with_mixed_runtime_and_type_variables) {
  for (String8_View other_thing : {
           u8"class x {}"_sv,
           u8"enum x {}"_sv,
       }) {
    test_parse_and_analyze(concat(u8"import x from ''; "_sv, other_thing),
                           u8"Diag_Redeclaration_Of_Variable"_diag,
                           typescript_analyze_options, default_globals);
    test_parse_and_analyze(concat(other_thing, u8" import x from '';"_sv),
                           u8"Diag_Redeclaration_Of_Variable"_diag,
                           typescript_analyze_options, default_globals);
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     typescript_import_always_conflicts_with_another_import) {
  for (String8_View other_thing : {
           u8"import {x} from 'othermod';"_sv,
           u8"import x from 'othermod';"_sv,
           u8"import * as x from 'othermod';"_sv,
           u8"import x = require('othermod');"_sv,
       }) {
    test_parse_and_analyze(concat(u8"import x from ''; "_sv, other_thing),
                           u8"Diag_Redeclaration_Of_Variable"_diag,
                           typescript_analyze_options, default_globals);
    test_parse_and_analyze(concat(other_thing, u8" import x from '';"_sv),
                           u8"Diag_Redeclaration_Of_Variable"_diag,
                           typescript_analyze_options, default_globals);
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_alias_does_not_conflict_with_most_other_things) {
  for (String8_View other_thing : {
           u8"class A {}"_sv,
           u8"const A = 42;"_sv,
           u8"function A() {}"_sv,
           u8"interface A {}"_sv,
           u8"let A;"_sv,
           u8"type A = null;"_sv,
           u8"var A;"_sv,
       }) {
    test_parse_and_analyze(
        concat(u8"namespace ns {}  import A = ns; "_sv, other_thing), no_diags,
        typescript_analyze_options, default_globals);
    test_parse_and_analyze(
        concat(u8"namespace ns {}  "_sv, other_thing, u8" import A = ns;"_sv),
        no_diags, typescript_analyze_options, default_globals);
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_alias_conflicts_with_import) {
  test_parse_and_analyze(
      u8"namespace ns {}  import A = ns; import A from 'mod';"_sv,  //
      u8"                                       ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace ns {}  import A from 'mod'; import A = ns;"_sv,  //
      u8"                                             ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"namespace ns {}  import A = ns; import A = require('mod');"_sv,  //
      u8"                                       ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace ns {}  import A = require('mod'); import A = ns;"_sv,  //
      u8"                                                   ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_alias_conflicts_with_import_alias) {
  test_parse_and_analyze(
      u8"namespace ns1 {}  namespace ns2 {}  import A = ns1; import A = ns2;"_sv,  //
      u8"                                                           ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                                           ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_alias_conflicts_with_namespace) {
  test_parse_and_analyze(
      u8"namespace ns {}  import A = ns; namespace A {}"_sv,  //
      u8"                                          ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace ns {}  namespace A {}  import A = ns;"_sv,  //
      u8"                                        ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                           ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     import_alias_conflicts_with_enum) {
  test_parse_and_analyze(
      u8"namespace ns {}  import A = ns; enum A {}"_sv,  //
      u8"                                     ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                        ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"namespace ns {}  enum A {}  import A = ns;"_sv,  //
      u8"                                   ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                      ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     imported_type_might_not_conflict_with_runtime_only_declarations) {
  // These things conflict when importing a class, but do not conflict when
  // importing an interface or type alias:
  for (String8_View other_thing : {
           u8"const T = 42;"_sv,
           u8"function T() {}"_sv,
           u8"let T;"_sv,
           u8"var T;"_sv,
           // This is a non-empty namespace. For an empty namespace, see
           // imported_type_never_conflicts_with_empty_namespace.
           u8"namespace T {;}"_sv,
       }) {
    test_parse_and_analyze(
        concat(u8"import {type T} from 'mod'; "_sv, other_thing), no_diags,
        typescript_analyze_options, default_globals);
    test_parse_and_analyze(
        concat(other_thing, u8" import {type T} from 'mod';"_sv), no_diags,
        typescript_analyze_options, default_globals);
  }
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     imported_type_never_conflicts_with_empty_namespace) {
  test_parse_and_analyze(u8"import {type T} from 'mod'; namespace T {}"_sv,
                         no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"namespace T {}  import {type T} from 'mod';"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     typescript_infer_can_be_repeated_in_one_extends_clause) {
  test_parse_and_analyze(
      u8"class A<T> {}\n"_sv
      u8"class B<T> {}\n"_sv
      u8"type T<U> = U extends A<infer I> | B<infer I> ? I : null"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     generic_parameter_can_have_same_name_as_class_or_interface) {
  test_parse_and_analyze(u8"class C<C> {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface I<I> {}"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Multiple_Declarations,
     redeclaration_check_considers_all_previous_declarations) {
  // 'type X' should be ignored when checking 'let X':
  test_parse_and_analyze(
      u8"type X = null; let X; let X;"_sv,  //
      u8"                          ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                   ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let X; type X = null; let X;"_sv,  //
      u8"                          ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  // 'let X' should be ignored when checking 'type X':
  test_parse_and_analyze(
      u8"let X; type X = null; type X = null;"_sv,  //
      u8"                           ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"            ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"type X = null; let X; type X = null;"_sv,  //
      u8"                           ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"     ^ .original_declaration"_diag,
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
