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

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer,
     let_or_const_or_class_variable_use_before_declaration) {
  test_parse_and_analyze(
      u8"x; class x {}"_sv,
      u8"         ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"^ .use"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"x; let x;"_sv,
      u8"       ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"^ .use"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"x; const x = null;"_sv,
      u8"         ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, import_use_before_declaration_is_okay) {
  test_parse_and_analyze(u8"x; import x from '';"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, export_use_after_declaration_is_okay) {
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

TEST(Test_Variable_Analyzer, export_use_before_declaration_is_okay) {
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

TEST(Test_Variable_Analyzer,
     let_variable_use_before_declaration_within_function) {
  test_parse_and_analyze(
      u8"(() => { x; let x; });"_sv,
      u8"                ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"         ^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     let_variable_use_before_declaration_within_for_loop_scope) {
  test_parse_and_analyze(
      u8"for (let _ of []) { x; let x; }"_sv,
      u8"                           ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"                    ^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     let_variable_use_before_declaration_of_shadowing_variable) {
  test_parse_and_analyze(
      u8"(() => { x; let x; }); let x; "_sv,
      u8"                ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"         ^ .use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, var_or_function_variable_use_before_declaration) {
  // x is hoisted.
  test_parse_and_analyze(u8"x; var x;"_sv, no_diags, javascript_analyze_options,
                         default_globals);
  test_parse_and_analyze(u8"x; function x() {}"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     var_or_function_variable_use_before_declaration_all_in_for_scope) {
  // x is hoisted.
  test_parse_and_analyze(u8"for (let _ of []) { x; function x() {} }"_sv,
                         no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"for (let _ of []) { x; var x; }"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     var_or_function_variable_use_after_declaration_in_block_scope) {
  // x has function scope.
  test_parse_and_analyze(u8"{ function x() {} } x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"{ var x; } x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    var_or_function_variable_cannot_be_used_after_declaration_in_inner_function_scope) {
  test_parse_and_analyze(
      u8"(() => { var x; }); x;"_sv,
      u8"                    ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => { function x() {} }); x;"_sv,
      u8"                             ^ Diag_Use_Of_Undeclared_Variable"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     var_variable_use_before_declaration_in_block_scope) {
  test_parse_and_analyze(
      u8"x;"_sv
      u8"{"_sv
      u8"  var x;"_sv  // x is hoisted
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     function_variable_use_before_declaration_in_block_scope) {
  test_parse_and_analyze(
      u8"f(); { function f() {}  }"_sv,
      u8"^ Diag_Function_Call_Before_Declaration_In_Block_Scope.use\n"_diag
      u8"                ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     var_variable_use_before_declaration_in_block_scope_all_in_function) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  x;"_sv
      u8"  {"_sv
      u8"    var x;"_sv  // x is hoisted
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     function_variable_use_before_declaration_in_block_scope_all_in_function) {
  test_parse_and_analyze(
      u8"(() => { f(); { function f() {}  }  });"_sv,
      u8"         ^ Diag_Function_Call_Before_Declaration_In_Block_Scope.use\n"_diag
      u8"                         ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    var_or_function_variable_use_before_declaration_in_different_block_scopes) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  {"_sv
      u8"    x;"_sv
      u8"  } "_sv
      u8"  var x;"_sv  // x is hoisted
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  {"_sv
      u8"    x;"_sv
      u8"  } "_sv
      u8"  function x() {}"_sv  // x is hoisted
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, variable_use_after_declaration) {
  test_parse_and_analyze(u8"const x = null; x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x; x;"_sv, no_diags, javascript_analyze_options,
                         default_globals);
  test_parse_and_analyze(u8"var x; x;"_sv, no_diags, javascript_analyze_options,
                         default_globals);
}

TEST(Test_Variable_Analyzer, variable_use_with_no_declaration) {
  test_parse_and_analyze(u8"x;"_sv,
                         u8"^ Diag_Use_Of_Undeclared_Variable.name"_diag,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, variable_export_with_no_declaration) {
  test_parse_and_analyze(
      u8"export {x};"_sv,
      u8"        ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, variable_use_in_function_with_no_declaration) {
  test_parse_and_analyze(
      u8"(() => { x; });"_sv,
      u8"         ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     variable_use_with_declaration_in_different_function) {
  test_parse_and_analyze(
      u8"(() => { let x; }); (() => { x; });"_sv,
      u8"                             ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     use_of_shadowed_let_variable_before_declaration_in_parent_scope) {
  test_parse_and_analyze(
      u8"let x; { { x; }  let x; }"_sv,
      u8"           ^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                     ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, use_of_variable_declared_in_grandparent_scope) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  let x;"_sv
      u8"  (() => {"_sv
      u8"    (() => {"_sv
      u8"      x;"_sv
      u8"    });"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_usable_within_function) {
  test_parse_and_analyze(
      u8"(function f() {"_sv
      u8"  f;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_usable_within_inner_function) {
  test_parse_and_analyze(
      u8"(function f() {"_sv
      u8"  (function() {"_sv
      u8"    f;"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    name_of_named_function_expression_is_usable_within_default_parameter_values) {
  test_parse_and_analyze(
      u8"(function f(x = f) {"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_not_usable_outside_function) {
  test_parse_and_analyze(
      u8"f; (function f() { }); f;"_sv,
      u8"                       ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      u8"^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, use_global_variable_within_functions) {
  test_parse_and_analyze(
      u8"let x;"_sv
      u8"(() => {"_sv
      u8"  x;"_sv
      u8"});"_sv
      u8"(() => {"_sv
      u8"  x;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     function_uses_variable_declared_in_outer_function) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"     x;"_sv
      u8"  });"_sv
      u8"  let x;"_sv
      u8"  (() => {"_sv
      u8"     x;"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     function_uses_global_variable_declared_later_in_module) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  x;"_sv
      u8"});"_sv
      u8"let x;"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, assign_to_mutable_variable) {
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

TEST(Test_Variable_Analyzer,
     assign_to_mutable_variable_shadowing_immutable_variable) {
  test_parse_and_analyze(
      u8"import x from '';"_sv  // x is immutable
      u8"(() => {"_sv
      u8"  let x;"_sv  // x is mutable
      u8"  x = 42;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, assign_to_immutable_const_variable) {
  const Char8 declaration[] = u8"x";
  const Char8 assignment[] = u8"x";

  test_parse_and_analyze(
      u8"(() => { const x = null; x = 42; });"_sv,
      u8"                         ^ Diag_Assignment_To_Const_Variable.assignment\n"_diag
      u8"               ^ .declaration"_diag
      u8"{.var_kind=Variable_Kind::_const}"_diag,
      javascript_analyze_options, default_globals);

  {
    // const x = null;  // x is immutable
    // {
    //   x = 42;        // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(
        identifier_of(declaration), Variable_Kind::_const,
        Variable_Declaration_Flags::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Const_Variable,       //
                               assignment, Span_Matcher(assignment),    //
                               declaration, Span_Matcher(declaration),  //
                               var_kind, Variable_Kind::_const),
        }));
  }
}

TEST(Test_Variable_Analyzer, assign_to_immutable_imported_variable) {
  const Char8 declaration[] = u8"x";
  const Char8 assignment[] = u8"x";

  {
    // import {x} from "module";   // x is immutable
    // {
    //   x = 42;  // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_block_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Imported_Variable,    //
                               assignment, Span_Matcher(assignment),    //
                               declaration, Span_Matcher(declaration),  //
                               var_kind, Variable_Kind::_import),
        }));
  }

  {
    // x = 42;  // ERROR
    // import {x} from "module";   // x is immutable
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Imported_Variable,    //
                               assignment, Span_Matcher(assignment),    //
                               declaration, Span_Matcher(declaration),  //
                               var_kind, Variable_Kind::_import),
        }));
  }
}

TEST(Test_Variable_Analyzer, assign_to_immutable_variable_before_declaration) {
  test_parse_and_analyze(
      u8"x = 42; const x = null;"_sv,
      u8"^ Diag_Assignment_To_Const_Variable_Before_Its_Declaration.assignment\n"_diag
      u8"              ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     assign_to_shadowing_immutable_variable_before_declaration) {
  const Char8 outer_declaration[] = u8"x";
  const Char8 assignment[] = u8"x";
  const Char8 inner_declaration[] = u8"x";

  // let x;             // x is shadowed.
  // {
  //   x = 42;          // ERROR
  //   const x = null;  // x is immutable
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(
      identifier_of(inner_declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              Diag_Assignment_To_Const_Variable_Before_Its_Declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(inner_declaration)),
      }));
}

TEST(Test_Variable_Analyzer,
     assign_to_immutable_variable_declared_in_parent_scope) {
  const Char8 assignment[] = u8"x";
  const Char8 declaration[] = u8"x";

  // const x = null;  // x is immutable
  // (() => {
  //   x = 42;        // ERROR
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(
      identifier_of(declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Const_Variable,       //
                                     assignment, Span_Matcher(assignment),    //
                                     declaration, Span_Matcher(declaration),  //
                                     var_kind, Variable_Kind::_const),
              }));
}

TEST(Test_Variable_Analyzer,
     assign_to_immutable_variable_declared_later_in_parent_scope) {
  const Char8 assignment[] = u8"x";
  const Char8 declaration[] = u8"x";

  // (() => {
  //   x = 42;        // ERROR
  // });
  // const x = null;  // x is immutable
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(
      identifier_of(declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Const_Variable,       //
                                     assignment, Span_Matcher(assignment),    //
                                     declaration, Span_Matcher(declaration),  //
                                     var_kind, Variable_Kind::_const),
              }));
}

TEST(Test_Variable_Analyzer,
     assignment_to_shadowed_const_variable_before_declaration_in_parent_scope) {
  const Char8 assignment[] = u8"x";
  const Char8 outer_declaration[] = u8"x";
  const Char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   {
  //     x = 42;        // ERROR
  //   }
  //   const x = null;  // x is immutable
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(
      identifier_of(inner_declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              Diag_Assignment_To_Const_Variable_Before_Its_Declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(inner_declaration)),
      }));
}

TEST(Test_Variable_Analyzer,
     assignment_to_const_variable_declared_in_grandparent_scope) {
  const Char8 declaration[] = u8"x";
  const Char8 assignment[] = u8"x";

  // const x = null;
  // (() => {
  //   (() => {
  //     x = 42;  // ERROR
  //   });
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(
      identifier_of(declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Const_Variable,       //
                                     assignment, Span_Matcher(assignment),    //
                                     declaration, Span_Matcher(declaration),  //
                                     var_kind, Variable_Kind::_const),
              }));
}

TEST(Test_Variable_Analyzer, assign_to_undeclared_variable) {
  test_parse_and_analyze(
      u8"x = null;"_sv,
      u8"^ Diag_Assignment_To_Undeclared_Variable.assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, assign_inside_function_to_undeclared_variable) {
  test_parse_and_analyze(
      u8"(function() { x = null; });"_sv,
      u8"              ^ Diag_Assignment_To_Undeclared_Variable.assignment"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, assign_to_variable_before_declaration) {
  test_parse_and_analyze(
      u8"x = null; let x;"_sv,
      u8"^ Diag_Assignment_Before_Variable_Declaration.assignment\n"_diag
      u8"              ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, assign_to_variable_before_hoistable_declaration) {
  test_parse_and_analyze(
      u8"x = null;"_sv
      u8"var x;"_sv,  // x is hoisted.
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, use_variable_declared_in_parent_function) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"    f;"_sv
      u8"  });"_sv
      u8"  let f;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"    f;"_sv
      u8"  });"_sv
      u8"  function f() {}"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, use_variable_declared_in_grandparent_function) {
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"    (() => {"_sv
      u8"      f;"_sv
      u8"    });"_sv
      u8"  });"_sv
      u8"  let f;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"    (() => {"_sv
      u8"      f;"_sv
      u8"    });"_sv
      u8"  });"_sv
      u8"  function f() {}"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, use_for_loop_let_variable_before_or_after_loop) {
  test_parse_and_analyze(
      u8"element; for (let element of []); element;"_sv,
      u8"                                  ^^^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      u8"^^^^^^^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     use_variable_in_for_scope_declared_outside_for_scope) {
  test_parse_and_analyze(
      u8"let v;"_sv
      u8"for (let _ of [])"_sv
      u8"  v;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"for (let _ of [])"_sv
      u8"  v;"_sv
      u8"var v;"_sv,  // v is hoisted
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"for (let _ of []) v; let v;"_sv,
      u8"                  ^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                         ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     use_undeclared_variable_in_function_scope_in_for_scope) {
  test_parse_and_analyze(
      u8"for (let _ of []) (() => { v; });"_sv,
      u8"                           ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     use_variable_in_function_scope_in_for_scope_before_declaration) {
  test_parse_and_analyze(
      u8"for (let _ of [])"_sv
      u8"  (() => {"_sv
      u8"    v;"_sv
      u8"  });"_sv
      u8"let v;"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     use_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  test_parse_and_analyze(
      u8"let v; for (let _ of []) { v; let v; }"_sv,
      u8"                           ^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                                  ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    assign_to_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  test_parse_and_analyze(
      u8"let v; for (let _ of []) { v = null; let v; }"_sv,
      u8"                           ^ Diag_Assignment_Before_Variable_Declaration.assignment\n"_diag
      u8"                                         ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, shadowing_variable_in_parent_block_scope_is_okay) {
  test_parse_and_analyze(
      u8"let x;"_sv
      u8"{"_sv
      u8"  let x;"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, declaring_variable_twice_is_an_error) {
  test_parse_and_analyze(
      u8"let x; let x; let x;"_sv,
      u8"                  ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      u8"           ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"    ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, declaring_variable_twice_with_var_is_okay) {
  test_parse_and_analyze(
      u8"var x;"_sv
      u8"var x;"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, declaring_parameter_twice_is_okay) {
  test_parse_and_analyze(u8"((x, x) => {});"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, declaring_function_twice_is_okay) {
  test_parse_and_analyze(
      u8"function f() {} "_sv
      u8"function f() {} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     mixing_var_and_function_in_same_function_scope_is_okay) {
  test_parse_and_analyze(
      u8"var x;"_sv
      u8"function x() {} "_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"function x() {} "_sv
      u8"var x;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"function x() {} "_sv
      u8"{"_sv
      u8"  var x;"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, mixing_parameter_and_var_or_function_is_okay) {
  test_parse_and_analyze(
      u8"((x) => {"_sv
      u8"  var x;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((x) => {"_sv
      u8"  function x() {} "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    mixing_let_or_const_or_class_with_other_variable_kind_in_same_scope_is_an_error) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";

  for (Variable_Kind declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_function,
        Variable_Kind::_let, Variable_Kind::_var}) {
    for (Variable_Kind second_declaration_kind :
         {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_let}) {
      // var x;
      // let x; // ERROR
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration), declaration_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_end_of_module();

      EXPECT_THAT(
          v.errors,
          ElementsAreArray({
              DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,              //
                                redeclaration, span_of(second_declaration),  //
                                original_declaration, span_of(declaration)),
          }));
    }
  }

  for (Variable_Kind declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_let}) {
    for (Variable_Kind second_declaration_kind :
         {Variable_Kind::_class, Variable_Kind::_const,
          Variable_Kind::_function, Variable_Kind::_let, Variable_Kind::_var}) {
      // let x;
      // var x; // ERROR
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration), declaration_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind,
                                   Variable_Declaration_Flags::none);
      l.visit_end_of_module();

      EXPECT_THAT(
          v.errors,
          ElementsAreArray({
              DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,              //
                                redeclaration, span_of(second_declaration),  //
                                original_declaration, span_of(declaration)),
          }));
    }
  }
}

TEST(Test_Variable_Analyzer,
     strict_variables_conflict_with_var_in_block_scope) {
  const Char8 var_declaration[] = u8"x";
  const Char8 other_declaration[] = u8"x";

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_import,
        Variable_Kind::_let}) {
    // {
    //   var x;
    // }
    // let x;    // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,             //
                              redeclaration, span_of(other_declaration),  //
                              original_declaration, span_of(var_declaration)),
        }));
  }

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_import,
        Variable_Kind::_let}) {
    // let x;
    // {
    //   var x;  // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,           //
                              redeclaration, span_of(var_declaration),  //
                              original_declaration, span_of(other_declaration)),
        }));
  }
}

TEST(Test_Variable_Analyzer,
     strict_variables_do_not_conflict_with_functions_in_block_scope) {
  test_parse_and_analyze(u8"{ function x() {} }  class x {}"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"{ function x() {} }  const x = null;"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"{ function x() {} }  import {x} from 'module';"_sv,
                         no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"{ function x() {} }  let x;"_sv, no_diags,
                         javascript_analyze_options, default_globals);

  test_parse_and_analyze(u8"class x {}  { function x() {} }"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"const x = null;  { function x() {} }"_sv, no_diags,
                         javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"import {x} from 'module';  { function x() {} }"_sv,
                         no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let x;  { function x() {} }"_sv, no_diags,
                         javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, import_conflicts_with_any_variable_declaration) {
  const Char8 import_declaration[] = u8"x";
  const Char8 other_declaration[] = u8"x";

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_function,
        Variable_Kind::_import, Variable_Kind::_let, Variable_Kind::_var}) {
    // import x from "";
    // let x;             // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        Diag_Redeclaration_Of_Variable,             //
                        redeclaration, span_of(other_declaration),  //
                        original_declaration, span_of(import_declaration)),
                }));
  }

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_function,
        Variable_Kind::_import, Variable_Kind::_let, Variable_Kind::_var}) {
    // let x;
    // import x from ""; // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,              //
                              redeclaration, span_of(import_declaration),  //
                              original_declaration, span_of(other_declaration)),
        }));
  }
}

TEST(Test_Variable_Analyzer,
     catch_variable_conflicts_with_catch_variable_declared_in_same_scope) {
  test_parse_and_analyze(
      u8"try { } catch ([e, e]) { }"_sv,
      u8"                   ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"                ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     let_style_variable_in_same_scope_as_parameter_redeclares) {
  test_parse_and_analyze(
      u8"((x) => { let x; });"_sv,
      u8"              ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"  ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"((x) => { const x = null; });"_sv,
      u8"                ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"  ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"((x) => { class x {} });"_sv,
      u8"                ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"  ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, let_variable_in_inner_scope_as_parameter_shadows) {
  test_parse_and_analyze(
      u8"((x) => {"_sv
      u8"  {"_sv
      u8"    const x = null;"_sv
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"((x) => {"_sv
      u8"  {"_sv
      u8"    let x;"_sv
      u8"  } "_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     catch_variable_does_not_conflict_with_var_variable) {
  test_parse_and_analyze(
      u8"try {"_sv
      u8"} catch (e) {"_sv
      u8"  var e;"_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, catch_variable_conflicts_with_non_var_variables) {
  test_parse_and_analyze(
      u8"try { } catch (e) { class e {} }"_sv,
      u8"                          ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"               ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try { } catch (e) { const e = null; }"_sv,
      u8"                          ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"               ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try { } catch (e) { function e() {} }"_sv,
      u8"                             ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"               ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try { } catch (e) { let e; }"_sv,
      u8"                        ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"               ^ .original_declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     parameter_default_value_cannot_refer_to_local_variables) {
  test_parse_and_analyze(u8"((p = l) => { var l; });"_sv,
                         u8"      ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
                         javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((p = (() => l)) => { var l; });"_sv,
      u8"             ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, parameter_default_value_uses_undeclared_variable) {
  test_parse_and_analyze(u8"((p = x) => { });"_sv,
                         u8"      ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
                         javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((p = (() => x)) => { });"_sv,
      u8"             ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, parameter_shadows_named_function_name) {
  test_parse_and_analyze(
      u8"(function f(f) {"_sv
      u8"  f;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, let_shadows_named_function_name) {
  test_parse_and_analyze(
      u8"(function f() {"_sv
      u8"  let f;"_sv
      u8"  f;"_sv
      u8"});"_sv,
      no_diags, javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(function f() { f; let f; });"_sv,
      u8"                ^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"                       ^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, let_shadows_global_variable) {
  test_parse_and_analyze(u8"let Array;"_sv, no_diags,
                         javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"Array; let Array;"_sv,
      u8"^^^^^ Diag_Variable_Used_Before_Declaration.use\n"_diag
      u8"           ^^^^^ .declaration"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer,
     class_declared_inside_class_scope_is_not_accessible_outside_class_scope) {
  test_parse_and_analyze(
      u8"(class C {}); C;"_sv,
      u8"              ^ Diag_Use_Of_Undeclared_Variable.name"_diag,
      javascript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(class C {});"_sv
      u8"class C {} "_sv
      u8"(class C {});"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, class_extends_cannot_use_declared_class_name) {
  test_parse_and_analyze(
      u8"class C extends C {}"_sv,
      u8"                ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(
    Test_Variable_Analyzer,
    regression_assigning_to_variable_in_function_scope_does_not_interact_with_different_variable_in_parent_scope) {
  // assigning to 'b' should not be an error; 'a' should not be confused with
  // 'b'
  test_parse_and_analyze(
      u8"(function() {"_sv
      u8"  b = null;"_sv
      u8"});"_sv
      u8"const a = null;"_sv
      u8"let b;"_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer, with_does_not_propagate_variable_uses) {
  const Char8 declaration[] = u8"a";
  const Char8 assignment[] = u8"a";

  // use of undeclared variable should not be an error inside with scope
  test_parse_and_analyze(
      u8"with({})"_sv
      u8"  a;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  // assigning to 'a' should not be an error inside with scope
  test_parse_and_analyze(
      u8"const a = 1;"_sv
      u8"with ({})"_sv
      u8"  a = 2;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  // assigning to 'a' should not be an error inside with scope
  test_parse_and_analyze(
      u8"with ({})"_sv
      u8"  a = 2;"_sv
      u8"let a;"_sv,
      no_diags, javascript_analyze_options, default_globals);

  {
    // with ({}) {
    //   const a = 1;
    //   a = 2;
    // }

    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(
        identifier_of(declaration), Variable_Kind::_const,
        Variable_Declaration_Flags::initialized_with_equals);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(Diag_Assignment_To_Const_Variable,       //
                               assignment, Span_Matcher(assignment),    //
                               declaration, Span_Matcher(declaration),  //
                               var_kind, Variable_Kind::_const),
        }));
  }

  // use of undeclared variable should not be an error inside a function inside
  // awith scope
  test_parse_and_analyze(
      u8"with ({}) {"_sv
      u8"  function f() {"_sv
      u8"    a;"_sv
      u8"  } "_sv
      u8"} "_sv,
      no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class, generic_class_parameters_are_usable_inside) {
  test_parse_and_analyze(
      u8"abstract class C<T> {"_sv
      u8"  abstract method(): T;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class,
     generic_class_parameters_are_not_usable_outside) {
  test_parse_and_analyze(
      u8"class C<T> { }  (null as T);"_sv,
      u8"                         ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type_Alias, type_alias_can_use_outside_types) {
  test_parse_and_analyze(
      u8"import {C} from 'other-module';"_sv
      u8"type Alias = C;"_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"type Alias = C;"_sv,
      u8"             ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type_Alias,
     generic_type_alias_parameters_are_usable_inside) {
  test_parse_and_analyze(u8"type Alias<T> = T;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type_Alias,
     generic_type_alias_parameters_are_not_usable_outside) {
  test_parse_and_analyze(
      u8"type Alias<T> = null; (null as T);"_sv,
      u8"                               ^ Diag_Use_Of_Undeclared_Type.name"_diag,
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
