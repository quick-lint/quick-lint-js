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
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // x;
  // {
  //   var x;  // x is hoisted
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     function_variable_use_before_declaration_in_block_scope) {
  const Char8 declaration[] = u8"f";
  const Char8 use[] = u8"f";

  // f();
  // {
  //   function f() {}
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(
                      Diag_Function_Call_Before_Declaration_In_Block_Scope,  //
                      use, span_of(use),                                     //
                      declaration, span_of(declaration)),
              }));
}

TEST(Test_Variable_Analyzer,
     var_variable_use_before_declaration_in_block_scope_all_in_function) {
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // (() => {
  //   x;
  //   {
  //     var x;  // x is hoisted
  //   }
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     function_variable_use_before_declaration_in_block_scope_all_in_function) {
  const Char8 declaration[] = u8"f";
  const Char8 use[] = u8"f";

  // (() => {
  //   f();
  //   {
  //     function f() {}
  //   }
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(
                      Diag_Function_Call_Before_Declaration_In_Block_Scope,  //
                      use, span_of(use),                                     //
                      declaration, span_of(declaration)),
              }));
}

TEST(
    Test_Variable_Analyzer,
    var_or_function_variable_use_before_declaration_in_different_block_scopes) {
  for (Variable_Kind kind : {Variable_Kind::_function, Variable_Kind::_var}) {
    const Char8 declaration[] = u8"x";
    const Char8 use[] = u8"x";

    // (() => {
    //   {
    //     x;
    //   }
    //   var x;  // x is hoisted
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, variable_use_after_declaration) {
  for (Variable_Kind kind :
       {Variable_Kind::_const, Variable_Kind::_let, Variable_Kind::_var}) {
    const Char8 declaration[] = u8"x";
    const Char8 use[] = u8"x";

    // let x;
    // x;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, variable_use_with_no_declaration) {
  const Char8 use[] = u8"x";

  // x;  // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name, span_of(use)),
      }));
}

TEST(Test_Variable_Analyzer, variable_export_with_no_declaration) {
  const Char8 use[] = u8"x";

  // export {x};  // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_export_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name, span_of(use)),
      }));
}

TEST(Test_Variable_Analyzer, variable_use_in_function_with_no_declaration) {
  const Char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name, span_of(use)),
      }));
}

TEST(Test_Variable_Analyzer,
     variable_use_with_declaration_in_different_function) {
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // (() => {
  //   let x;
  // });
  // (() => {
  //   x;      // ERROR
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name, span_of(use)),
      }));
}

TEST(Test_Variable_Analyzer,
     use_of_shadowed_let_variable_before_declaration_in_parent_scope) {
  const Char8 outer_declaration[] = u8"x";
  const Char8 use[] = u8"x";
  const Char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   {
  //     x;    // ERROR
  //   }
  //   let x;
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(inner_declaration)),
              }));
}

TEST(Test_Variable_Analyzer, use_of_variable_declared_in_grandparent_scope) {
  const Char8 use[] = u8"x";
  const Char8 declaration[] = u8"x";

  // (() => {
  //   let x;
  //   (() => {
  //     (() => {
  //       x;
  //     });
  //   });
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_usable_within_function) {
  const Char8 declaration[] = u8"f";
  const Char8 use[] = u8"f";

  // (function f() {
  //   f;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_usable_within_inner_function) {
  const Char8 declaration[] = u8"f";
  const Char8 use[] = u8"f";

  // (function f() {
  //   (function() {
  //     f;
  //   });
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();

  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();

  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(
    Test_Variable_Analyzer,
    name_of_named_function_expression_is_usable_within_default_parameter_values) {
  const Char8 declaration[] = u8"f";
  const Char8 parameter_declaration[] = u8"x";
  const Char8 use[] = u8"f";

  // (function f(x = f) {
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               Variable_Kind::_function_parameter,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     name_of_named_function_expression_is_not_usable_outside_function) {
  const Char8 declaration[] = u8"f";
  const Char8 use_before[] = u8"f";
  const Char8 use_after[] = u8"f";

  // f;               // ERROR
  // (function f() {
  // });
  // f;               // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_before)),
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_after)),
                        }));
}

TEST(Test_Variable_Analyzer, use_global_variable_within_functions) {
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // let x;
  // (() => {
  //   x;
  // });
  // (() => {
  //   x;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     function_uses_variable_declared_in_outer_function) {
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // (() => {
  //   (() => {
  //      x;
  //   });
  //   let x;
  //   (() => {
  //      x;
  //   });
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  {
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();
  }
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     function_uses_global_variable_declared_later_in_module) {
  const Char8 declaration[] = u8"x";
  const Char8 use[] = u8"x";

  // (() => {
  //   x;
  // });
  // let x;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, assign_to_mutable_variable) {
  for (Variable_Kind kind :
       {Variable_Kind::_let, Variable_Kind::_var, Variable_Kind::_class,
        Variable_Kind::_function, Variable_Kind::_catch,
        Variable_Kind::_arrow_parameter, Variable_Kind::_function_parameter}) {
    const Char8 declaration[] = u8"x";
    const Char8 assignment[] = u8"x";

    // (() => {
    //   let x;  // x is mutable
    //   x = 42;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer,
     assign_to_mutable_variable_shadowing_immutable_variable) {
  const Char8 immutable_declaration[] = u8"x";
  const Char8 mutable_declaration[] = u8"x";
  const Char8 assignment[] = u8"x";

  // import x from ""; // x is immutable
  // (() => {
  //   let x;          // x is mutable
  //   x = 42;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(immutable_declaration),
                               Variable_Kind::_import,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(mutable_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, assign_to_immutable_const_variable) {
  const Char8 declaration[] = u8"x";
  const Char8 assignment[] = u8"x";

  {
    // (() => {
    //   const x = null;  // x is immutable
    //   x = 42;          // ERROR
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(
        identifier_of(declaration), Variable_Kind::_const,
        Variable_Declaration_Flags::initialized_with_equals);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
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
  const Char8 assignment[] = u8"x";
  const Char8 declaration[] = u8"x";

  // x = 42;          // ERROR
  // const x = null;  // x is immutable
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(
      identifier_of(declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              Diag_Assignment_To_Const_Variable_Before_Its_Declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(declaration)),
      }));
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
  const Char8 assignment[] = u8"x";

  // x = null;  // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_SPAN(Diag_Assignment_To_Undeclared_Variable,
                                 assignment, span_of(assignment)),
              }));
}

TEST(Test_Variable_Analyzer, assign_inside_function_to_undeclared_variable) {
  const Char8 assignment[] = u8"x";

  // (function() {
  //   x = null;  // ERROR
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_SPAN(Diag_Assignment_To_Undeclared_Variable,
                                 assignment, span_of(assignment)),
              }));
}

TEST(Test_Variable_Analyzer, assign_to_variable_before_declaration) {
  const Char8 assignment[] = u8"x";
  const Char8 declaration[] = u8"x";

  // x = null;
  // let x;     // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(Diag_Assignment_Before_Variable_Declaration,  //
                            assignment, span_of(assignment),              //
                            declaration, span_of(declaration)),
      }));
}

TEST(Test_Variable_Analyzer, assign_to_variable_before_hoistable_declaration) {
  const Char8 assignment[] = u8"x";
  const Char8 declaration[] = u8"x";

  // x = null;
  // var x;     // x is hoisted.
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, use_variable_declared_in_parent_function) {
  for (Variable_Kind var_kind :
       {Variable_Kind::_function, Variable_Kind::_let}) {
    SCOPED_TRACE(::testing::PrintToString(var_kind));

    const Char8 declaration[] = u8"f";
    const Char8 use[] = u8"f";

    // (() => {
    //   (() => {
    //     f;
    //   });
    //   let f;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), var_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, use_variable_declared_in_grandparent_function) {
  for (Variable_Kind var_kind :
       {Variable_Kind::_function, Variable_Kind::_let}) {
    SCOPED_TRACE(::testing::PrintToString(var_kind));

    const Char8 declaration[] = u8"f";
    const Char8 use[] = u8"f";

    // (() => {
    //   (() => {
    //     (() => {
    //       f;
    //     });
    //   });
    //   let f;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), var_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, use_for_loop_let_variable_before_or_after_loop) {
  const Char8 declaration[] = u8"element";
  const Char8 use_before[] = u8"element";
  const Char8 use_after[] = u8"element";

  // element;                  // ERROR
  // for (let element of []);
  // element;                  // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_for_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_before)),
                            DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                           name, span_of(use_after)),
                        }));
}

TEST(Test_Variable_Analyzer,
     use_variable_in_for_scope_declared_outside_for_scope) {
  {
    const Char8 declaration[] = u8"v";
    const Char8 use[] = u8"v";

    // let v;
    // for (let _ of [])
    //   v;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const Char8 declaration[] = u8"v";
    const Char8 use[] = u8"v";

    // for (let _ of [])
    //   v;
    // var v;             // v is hoisted
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const Char8 declaration[] = u8"v";
    const Char8 use[] = u8"v";

    // for (let _ of [])
    //   v;               // ERROR
    // let v;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                      use, span_of(use),                      //
                                      declaration, span_of(declaration)),
                }));
  }
}

TEST(Test_Variable_Analyzer,
     use_undeclared_variable_in_function_scope_in_for_scope) {
  const Char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;             // ERROR
  //   });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name, span_of(use)),
      }));
}

TEST(Test_Variable_Analyzer,
     use_variable_in_function_scope_in_for_scope_before_declaration) {
  const Char8 declaration[] = u8"v";
  const Char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;
  //   });
  // let v;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     use_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  const Char8 outer_declaration[] = u8"v";
  const Char8 inner_declaration[] = u8"v";
  const Char8 use[] = u8"v";

  // let v;
  // for (let _ of []) {
  //   v;                 // ERROR
  //   let v;
  // }
  // TODO(strager): Code above doesn't match visits below.
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(inner_declaration)),
              }));
}

TEST(
    Test_Variable_Analyzer,
    assign_to_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  const Char8 outer_declaration[] = u8"v";
  const Char8 inner_declaration[] = u8"v";
  const Char8 assignment[] = u8"v";

  // let v;
  // for (let _ of []) {
  //   v = null;          // ERROR
  //   let v;
  // }
  // TODO(strager): Code above doesn't match visits below.
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_for_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(Diag_Assignment_Before_Variable_Declaration,  //
                            assignment, span_of(assignment),              //
                            declaration, span_of(inner_declaration)),
      }));
}

TEST(Test_Variable_Analyzer, shadowing_variable_in_parent_block_scope_is_okay) {
  const Char8 outer_declaration[] = u8"x";
  const Char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   let x;
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, declaring_variable_twice_is_an_error) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";
  const Char8 third_declaration[] = u8"x";

  // let x;
  // let x;  // ERROR
  // let x;  // ERROR
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(third_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,  //
                            redeclaration,
                            span_of(second_declaration),  //
                            original_declaration, span_of(declaration)),
          DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,             //
                            redeclaration, span_of(third_declaration),  //
                            original_declaration, span_of(declaration)),
      }));
}

TEST(Test_Variable_Analyzer, declaring_variable_twice_with_var_is_okay) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";

  // var x;
  // var x;
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, declaring_parameter_twice_is_okay) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";

  // ((x, x) => {});
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               Variable_Kind::_arrow_parameter,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               Variable_Kind::_arrow_parameter,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, declaring_function_twice_is_okay) {
  const Char8 declaration[] = u8"f";
  const Char8 second_declaration[] = u8"f";

  // function f() {}
  // function f() {}
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(second_declaration),
                               Variable_Kind::_function,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer,
     mixing_var_and_function_in_same_function_scope_is_okay) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";

  {
    // var x;
    // function x() {}
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // var x;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // {
    //   var x;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, mixing_parameter_and_var_or_function_is_okay) {
  const Char8 declaration[] = u8"x";
  const Char8 second_declaration[] = u8"x";

  {
    // ((x) => {
    //   var x;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // ((x) => {
    //   function x() {}
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
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
  const Char8 function_declaration[] = u8"x";
  const Char8 other_declaration[] = u8"x";

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_import,
        Variable_Kind::_let}) {
    // {
    //   function x() {}
    // }
    // let x;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (Variable_Kind other_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_import,
        Variable_Kind::_let}) {
    // let x;
    // {
    //   function x() {}
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
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
  const Char8 catch_declaration_1[] = u8"e";
  const Char8 catch_declaration_2[] = u8"e";

  // try {
  // } catch ([e, e]) {  // ERROR
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration_1),
                               Variable_Kind::_catch,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(catch_declaration_2),
                               Variable_Kind::_catch,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,               //
                            redeclaration, span_of(catch_declaration_2),  //
                            original_declaration, span_of(catch_declaration_1)),
      }));
}

TEST(Test_Variable_Analyzer,
     let_style_variable_in_same_scope_as_parameter_redeclares) {
  const Char8 parameter_declaration[] = u8"x";
  const Char8 local_declaration[] = u8"x";

  for (Variable_Kind local_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_let}) {
    // ((x) => {
    //   let x; // ERROR
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        Diag_Redeclaration_Of_Variable,             //
                        redeclaration, span_of(local_declaration),  //
                        original_declaration, span_of(parameter_declaration)),
                }));
  }
}

TEST(Test_Variable_Analyzer, let_variable_in_inner_scope_as_parameter_shadows) {
  const Char8 parameter_declaration[] = u8"x";
  const Char8 local_declaration[] = u8"x";

  for (Variable_Kind local_declaration_kind :
       {Variable_Kind::_const, Variable_Kind::_let}) {
    // ((x) => {
    //   {
    //     let x;
    //   }
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer,
     catch_variable_does_not_conflict_with_var_variable) {
  const Char8 catch_declaration[] = u8"e";
  const Char8 var_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   var e;
  // }
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration),
                               Variable_Kind::_catch,
                               Variable_Declaration_Flags::none);
  l.visit_variable_declaration(identifier_of(var_declaration),
                               Variable_Kind::_var,
                               Variable_Declaration_Flags::none);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, catch_variable_conflicts_with_non_var_variables) {
  const Char8 catch_declaration[] = u8"e";
  const Char8 local_declaration[] = u8"e";

  for (Variable_Kind local_declaration_kind :
       {Variable_Kind::_class, Variable_Kind::_const, Variable_Kind::_function,
        Variable_Kind::_let}) {
    // try {
    // } catch (e) {
    //   let e;       // ERROR
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(catch_declaration),
                                 Variable_Kind::_catch,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(Diag_Redeclaration_Of_Variable,             //
                              redeclaration, span_of(local_declaration),  //
                              original_declaration, span_of(catch_declaration)),
        }));
  }
}

TEST(Test_Variable_Analyzer,
     parameter_default_value_cannot_refer_to_local_variables) {
  const Char8 parameter_declaration[] = u8"p";
  const Char8 parameter_default_value[] = u8"l";
  const Char8 local_declaration[] = u8"l";

  {
    // ((p = l) => {  // ERROR
    //   var l;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(parameter_default_value)),
                }));
  }

  {
    // ((p = (() => l)) => {  // ERROR
    //   var l;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();

    // (() => l)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 Variable_Kind::_var,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(parameter_default_value)),
                }));
  }
}

TEST(Test_Variable_Analyzer, parameter_default_value_uses_undeclared_variable) {
  const Char8 parameter_declaration[] = u8"p";
  const Char8 parameter_default_value[] = u8"x";

  {
    // ((p = x) => {  // ERROR
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(parameter_default_value)),
                }));
  }

  {
    // ((p = (() => x)) => {  // ERROR
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();

    // (() => x)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_arrow_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                                   span_of(parameter_default_value)),
                }));
  }
}

TEST(Test_Variable_Analyzer, parameter_shadows_named_function_name) {
  const Char8 function_declaration[] = u8"f";
  const Char8 parameter_declaration[] = u8"f";
  const Char8 parameter_use[] = u8"f";

  // (function f(f) {
  //   f;
  // });
  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(function_declaration));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               Variable_Kind::_function_parameter,
                               Variable_Declaration_Flags::none);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(Test_Variable_Analyzer, let_shadows_named_function_name) {
  const Char8 function_declaration[] = u8"f";
  const Char8 var_declaration[] = u8"f";
  const Char8 var_use[] = u8"f";

  {
    // (function f() {
    //   let f;
    //   f;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_use(identifier_of(var_use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function f() {
    //   f;             // ERROR
    //   let f;
    // });
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                      use, span_of(var_use),                  //
                                      declaration, span_of(var_declaration)),
                }));
  }
}

TEST(Test_Variable_Analyzer, let_shadows_global_variable) {
  const Char8 var_declaration[] = u8"Array";
  const Char8 var_use[] = u8"Array";

  {
    // let Array;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // Array;
    // let Array;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(Diag_Variable_Used_Before_Declaration,  //
                                      use, span_of(var_use),                  //
                                      declaration, span_of(var_declaration)),
                }));
  }
}

TEST(Test_Variable_Analyzer,
     class_declared_inside_class_scope_is_not_accessible_outside_class_scope) {
  {
    // (class C {});
    // C;             // ERROR
    const Char8 class_declaration[] = u8"C";
    const Char8 class_use[] = u8"C";
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_use(identifier_of(class_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable,
                                             name, span_of(class_use)),
                          }));
  }

  {
    // (class C {});
    // class C {}
    // (class C {});
    const Char8 class_declaration_1[] = u8"C";
    const Char8 class_declaration_2[] = u8"C";
    const Char8 class_declaration_3[] = u8"C";
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_1));
    l.visit_exit_class_scope();

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_2));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration_2),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_3));
    l.visit_exit_class_scope();

    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer, class_extends_cannot_use_declared_class_name) {
  {
    // class C extends C {} // ERROR
    const Char8 class_declaration[] = u8"C";
    const Char8 class_use[] = u8"C";
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_use(identifier_of(class_use));
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Variable_Used_Before_Declaration, use,
                                   span_of(class_use)),
                }));
  }
}

TEST(
    Test_Variable_Analyzer,
    regression_assigning_to_variable_in_function_scope_does_not_interact_with_different_variable_in_parent_scope) {
  // (function() {
  //   b = null;
  // });
  // const a = null;
  // let b;
  const Char8 a_declaration[] = u8"a";
  const Char8 b_declaration[] = u8"b";
  const Char8 b_assignment[] = u8"b";

  Diag_Collector v;
  Variable_Analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(b_assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(
      identifier_of(a_declaration), Variable_Kind::_const,
      Variable_Declaration_Flags::initialized_with_equals);
  l.visit_variable_declaration(identifier_of(b_declaration),
                               Variable_Kind::_let,
                               Variable_Declaration_Flags::none);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty())
      << "assigning to 'b' should not be an error; 'a' should not be confused "
         "with 'b'";
}

TEST(Test_Variable_Analyzer, with_does_not_propagate_variable_uses) {
  const Char8 declaration[] = u8"a";
  const Char8 assignment[] = u8"a";
  const Char8 use[] = u8"a";

  {
    // with({})
    //   a;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "use of undeclared variable should not "
                                        "be an error inside with scope";
  }

  {
    // const a = 1;
    // with ({})
    //   a = 2;

    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(
        identifier_of(declaration), Variable_Kind::_const,
        Variable_Declaration_Flags::initialized_with_equals);
    l.visit_enter_with_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "assigning to 'a' should not "
                                        "be an error inside with scope";
  }

  {
    // with ({})
    //   a = 2;
    // let a;

    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_with_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 Variable_Kind::_let,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "assigning to 'a' should not "
                                        "be an error inside with scope";
  }

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

  {
    // with ({}) {
    //   function f() {
    //     a;
    //   }
    // }

    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_enter_block_scope();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_exit_block_scope();
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "use of undeclared variable should not "
                                        "be an error inside a function inside a"
                                        "with scope";
  }
}

TEST(Test_Variable_Analyzer_Class, generic_class_parameters_are_usable_inside) {
  const Char8 class_declaration[] = u8"C";
  const Char8 parameter_declaration[] = u8"T";
  const Char8 parameter_use[] = u8"T";
  const Char8 method_name[] = u8"method";

  {
    // class C<T> {
    //   method(): T;
    // }
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_function_scope();
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Class,
     generic_class_parameters_are_not_usable_outside) {
  const Char8 class_declaration[] = u8"C";
  const Char8 parameter_declaration[] = u8"T";
  const Char8 parameter_use[] = u8"T";

  {
    // class C<T> { }
    // (null: T); // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                             span_of(parameter_use)),
                          }));
  }
}

TEST(Test_Variable_Analyzer_Type_Alias, type_alias_can_use_outside_types) {
  const Char8 imported_declaration[] = u8"C";
  const Char8 type_alias_declaration[] = u8"Alias";
  const Char8 type_use[] = u8"C";

  {
    // import {C} from "other-module";
    // type Alias = C;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(imported_declaration),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 Variable_Kind::_type_alias,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_type_alias_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // type Alias = C;  // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 Variable_Kind::_type_alias,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_type_alias_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                             span_of(type_use)),
                          }));
  }
}

TEST(Test_Variable_Analyzer_Type_Alias,
     generic_type_alias_parameters_are_usable_inside) {
  const Char8 type_alias_declaration[] = u8"Alias";
  const Char8 parameter_declaration[] = u8"T";
  const Char8 parameter_use[] = u8"T";

  {
    // type Alias<T> = T;
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 Variable_Kind::_type_alias,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_type_alias_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(Test_Variable_Analyzer_Type_Alias,
     generic_type_alias_parameters_are_not_usable_outside) {
  const Char8 type_alias_declaration[] = u8"Alias";
  const Char8 parameter_declaration[] = u8"T";
  const Char8 parameter_use_outside_type_alias[] = u8"T";

  {
    // type Alias<T> = null;
    // (null as T);           // ERROR
    Diag_Collector v;
    Variable_Analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 Variable_Kind::_type_alias,
                                 Variable_Declaration_Flags::none);
    l.visit_enter_type_alias_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 Variable_Kind::_generic_parameter,
                                 Variable_Declaration_Flags::none);
    l.visit_exit_type_alias_scope();
    l.visit_variable_type_use(identifier_of(parameter_use_outside_type_alias));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Type, name,
                                   span_of(parameter_use_outside_type_alias)),
                }));
  }
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
