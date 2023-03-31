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

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_variable_analyzer,
     let_or_const_or_class_variable_use_before_declaration) {
  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;      // ERROR
    // let x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                      use, span_of(use),                      //
                                      declaration, span_of(declaration)),
                }));
  }
}

TEST(test_variable_analyzer, import_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // x;
  // import x from "";
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_import,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, export_use_after_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  for (variable_kind kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_import,
           variable_kind::_interface,
           variable_kind::_let,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(kind);

    // let x;
    // export {x};
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_variable_export_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, export_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  for (variable_kind kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_import,
           variable_kind::_interface,
           variable_kind::_let,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(kind);

    // export {x};
    // let x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_export_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer,
     let_variable_use_before_declaration_within_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  //   let x;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(declaration)),
              }));
}

TEST(test_variable_analyzer,
     let_variable_use_before_declaration_within_for_loop_scope) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // for (let _ of []) {
  //   x;
  //   let x;             // ERROR
  // }
  // TODO(strager): Code above doesn't match visits below.
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(declaration)),
              }));
}

TEST(test_variable_analyzer,
     let_variable_use_before_declaration_of_shadowing_variable) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  //   let x;
  // });
  // let x;
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(declaration)),
              }));
}

TEST(test_variable_analyzer, var_or_function_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;
    // var x;  // x is hoisted
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer,
     var_or_function_variable_use_before_declaration_all_in_for_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // for (let _ of []) {
    //   x;
    //   var x;             // x is hoisted
    // }
    // TODO(strager): Code above doesn't match visits below.
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_for_scope();
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer,
     var_or_function_variable_use_after_declaration_in_block_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // {
    //   var x;  // x has function scope
    // }
    // x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(
    test_variable_analyzer,
    var_or_function_variable_cannot_be_used_after_declaration_in_inner_function_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // (() => {
    //   var x;
    // });
    // x;        // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
        }));
  }
}

TEST(test_variable_analyzer,
     var_variable_use_before_declaration_in_block_scope) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // x;
  // {
  //   var x;  // x is hoisted
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var,
                               variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     function_variable_use_before_declaration_in_block_scope) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // f();
  // {
  //   function f() {}
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(
                      diag_function_call_before_declaration_in_block_scope,  //
                      use, span_of(use),                                     //
                      declaration, span_of(declaration)),
              }));
}

TEST(test_variable_analyzer,
     var_variable_use_before_declaration_in_block_scope_all_in_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;
  //   {
  //     var x;  // x is hoisted
  //   }
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var,
                               variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     function_variable_use_before_declaration_in_block_scope_all_in_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (() => {
  //   f();
  //   {
  //     function f() {}
  //   }
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(
                      diag_function_call_before_declaration_in_block_scope,  //
                      use, span_of(use),                                     //
                      declaration, span_of(declaration)),
              }));
}

TEST(
    test_variable_analyzer,
    var_or_function_variable_use_before_declaration_in_different_block_scopes) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // (() => {
    //   {
    //     x;
    //   }
    //   var x;  // x is hoisted
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, variable_use_after_declaration) {
  for (variable_kind kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // let x;
    // x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, variable_use_with_no_declaration) {
  const char8 use[] = u8"x";

  // x;  // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
      }));
}

TEST(test_variable_analyzer, variable_export_with_no_declaration) {
  const char8 use[] = u8"x";

  // export {x};  // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_export_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
      }));
}

TEST(test_variable_analyzer, variable_use_in_function_with_no_declaration) {
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
      }));
}

TEST(test_variable_analyzer,
     variable_use_with_declaration_in_different_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   let x;
  // });
  // (() => {
  //   x;      // ERROR
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
      }));
}

TEST(test_variable_analyzer,
     use_of_shadowed_let_variable_before_declaration_in_parent_scope) {
  const char8 outer_declaration[] = u8"x";
  const char8 use[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   {
  //     x;    // ERROR
  //   }
  //   let x;
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(inner_declaration)),
              }));
}

TEST(test_variable_analyzer, use_of_variable_declared_in_grandparent_scope) {
  const char8 use[] = u8"x";
  const char8 declaration[] = u8"x";

  // (() => {
  //   let x;
  //   (() => {
  //     (() => {
  //       x;
  //     });
  //   });
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
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

TEST(test_variable_analyzer,
     name_of_named_function_expression_is_usable_within_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (function f() {
  //   f;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     name_of_named_function_expression_is_usable_within_inner_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (function f() {
  //   (function() {
  //     f;
  //   });
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
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
    test_variable_analyzer,
    name_of_named_function_expression_is_usable_within_default_parameter_values) {
  const char8 declaration[] = u8"f";
  const char8 parameter_declaration[] = u8"x";
  const char8 use[] = u8"f";

  // (function f(x = f) {
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_function_parameter,
                               variable_init_kind::normal);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     name_of_named_function_expression_is_not_usable_outside_function) {
  const char8 declaration[] = u8"f";
  const char8 use_before[] = u8"f";
  const char8 use_after[] = u8"f";

  // f;               // ERROR
  // (function f() {
  // });
  // f;               // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                           name, span_of(use_before)),
                            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                           name, span_of(use_after)),
                        }));
}

TEST(test_variable_analyzer, use_global_variable_within_functions) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // let x;
  // (() => {
  //   x;
  // });
  // (() => {
  //   x;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
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

TEST(test_variable_analyzer,
     function_uses_variable_declared_in_outer_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   (() => {
  //      x;
  //   });
  //   let x;
  //   (() => {
  //      x;
  //   });
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  {
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);

    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();
  }
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     function_uses_global_variable_declared_later_in_module) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;
  // });
  // let x;
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, assign_to_mutable_variable) {
  for (variable_kind kind :
       {variable_kind::_let, variable_kind::_var, variable_kind::_class,
        variable_kind::_function, variable_kind::_catch,
        variable_kind::_arrow_parameter, variable_kind::_function_parameter}) {
    const char8 declaration[] = u8"x";
    const char8 assignment[] = u8"x";

    // (() => {
    //   let x;  // x is mutable
    //   x = 42;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind,
                                 variable_init_kind::normal);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer,
     assign_to_mutable_variable_shadowing_immutable_variable) {
  const char8 immutable_declaration[] = u8"x";
  const char8 mutable_declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  // import x from ""; // x is immutable
  // (() => {
  //   let x;          // x is mutable
  //   x = 42;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(immutable_declaration),
                               variable_kind::_import,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(mutable_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, assign_to_immutable_const_variable) {
  const char8 declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  {
    // (() => {
    //   const x = null;  // x is immutable
    //   x = 42;          // ERROR
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                               assignment, span_matcher(assignment),    //
                               declaration, span_matcher(declaration),  //
                               var_kind, variable_kind::_const),
        }));
  }

  {
    // const x = null;  // x is immutable
    // {
    //   x = 42;        // ERROR
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const,
                                 variable_init_kind::initialized_with_equals);
    l.visit_enter_block_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                               assignment, span_matcher(assignment),    //
                               declaration, span_matcher(declaration),  //
                               var_kind, variable_kind::_const),
        }));
  }
}

TEST(test_variable_analyzer, assign_to_immutable_imported_variable) {
  const char8 declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  {
    // import {x} from "module";   // x is immutable
    // {
    //   x = 42;  // ERROR
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(diag_assignment_to_imported_variable,    //
                               assignment, span_matcher(assignment),    //
                               declaration, span_matcher(declaration),  //
                               var_kind, variable_kind::_import),
        }));
  }

  {
    // x = 42;  // ERROR
    // import {x} from "module";   // x is immutable
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(diag_assignment_to_imported_variable,    //
                               assignment, span_matcher(assignment),    //
                               declaration, span_matcher(declaration),  //
                               var_kind, variable_kind::_import),
        }));
  }
}

TEST(test_variable_analyzer, assign_to_immutable_variable_before_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = 42;          // ERROR
  // const x = null;  // x is immutable
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              diag_assignment_to_const_variable_before_its_declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(declaration)),
      }));
}

TEST(test_variable_analyzer,
     assign_to_shadowing_immutable_variable_before_declaration) {
  const char8 outer_declaration[] = u8"x";
  const char8 assignment[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;             // x is shadowed.
  // {
  //   x = 42;          // ERROR
  //   const x = null;  // x is immutable
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              diag_assignment_to_const_variable_before_its_declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(inner_declaration)),
      }));
}

TEST(test_variable_analyzer,
     assign_to_immutable_variable_declared_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // const x = null;  // x is immutable
  // (() => {
  //   x = 42;        // ERROR
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                                     assignment, span_matcher(assignment),    //
                                     declaration, span_matcher(declaration),  //
                                     var_kind, variable_kind::_const),
              }));
}

TEST(test_variable_analyzer,
     assign_to_immutable_variable_declared_later_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // (() => {
  //   x = 42;        // ERROR
  // });
  // const x = null;  // x is immutable
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                                     assignment, span_matcher(assignment),    //
                                     declaration, span_matcher(declaration),  //
                                     var_kind, variable_kind::_const),
              }));
}

TEST(test_variable_analyzer,
     assignment_to_shadowed_const_variable_before_declaration_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   {
  //     x = 42;        // ERROR
  //   }
  //   const x = null;  // x is immutable
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(
              diag_assignment_to_const_variable_before_its_declaration,  //
              assignment, span_of(assignment),                           //
              declaration, span_of(inner_declaration)),
      }));
}

TEST(test_variable_analyzer,
     assignment_to_const_variable_declared_in_grandparent_scope) {
  const char8 declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  // const x = null;
  // (() => {
  //   (() => {
  //     x = 42;  // ERROR
  //   });
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
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
                  DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                                     assignment, span_matcher(assignment),    //
                                     declaration, span_matcher(declaration),  //
                                     var_kind, variable_kind::_const),
              }));
}

TEST(test_variable_analyzer, assign_to_undeclared_variable) {
  const char8 assignment[] = u8"x";

  // x = null;  // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable,
                                 assignment, span_of(assignment)),
              }));
}

TEST(test_variable_analyzer, assign_inside_function_to_undeclared_variable) {
  const char8 assignment[] = u8"x";

  // (function() {
  //   x = null;  // ERROR
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable,
                                 assignment, span_of(assignment)),
              }));
}

TEST(test_variable_analyzer, assign_to_variable_before_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // let x;     // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(diag_assignment_before_variable_declaration,  //
                            assignment, span_of(assignment),              //
                            declaration, span_of(declaration)),
      }));
}

TEST(test_variable_analyzer, assign_to_variable_before_hoistable_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // var x;     // x is hoisted.
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, use_variable_declared_in_parent_function) {
  for (variable_kind var_kind :
       {variable_kind::_function, variable_kind::_let}) {
    SCOPED_TRACE(::testing::PrintToString(var_kind));

    const char8 declaration[] = u8"f";
    const char8 use[] = u8"f";

    // (() => {
    //   (() => {
    //     f;
    //   });
    //   let f;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), var_kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, use_variable_declared_in_grandparent_function) {
  for (variable_kind var_kind :
       {variable_kind::_function, variable_kind::_let}) {
    SCOPED_TRACE(::testing::PrintToString(var_kind));

    const char8 declaration[] = u8"f";
    const char8 use[] = u8"f";

    // (() => {
    //   (() => {
    //     (() => {
    //       f;
    //     });
    //   });
    //   let f;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
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
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, use_for_loop_let_variable_before_or_after_loop) {
  const char8 declaration[] = u8"element";
  const char8 use_before[] = u8"element";
  const char8 use_after[] = u8"element";

  // element;                  // ERROR
  // for (let element of []);
  // element;                  // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_exit_for_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAreArray({
                            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                           name, span_of(use_before)),
                            DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                           name, span_of(use_after)),
                        }));
}

TEST(test_variable_analyzer,
     use_variable_in_for_scope_declared_outside_for_scope) {
  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // let v;
    // for (let _ of [])
    //   v;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // for (let _ of [])
    //   v;
    // var v;             // v is hoisted
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // for (let _ of [])
    //   v;               // ERROR
    // let v;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                      use, span_of(use),                      //
                                      declaration, span_of(declaration)),
                }));
  }
}

TEST(test_variable_analyzer,
     use_undeclared_variable_in_function_scope_in_for_scope) {
  const char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;             // ERROR
  //   });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
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
          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name, span_of(use)),
      }));
}

TEST(test_variable_analyzer,
     use_variable_in_function_scope_in_for_scope_before_declaration) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;
  //   });
  // let v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     use_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  const char8 outer_declaration[] = u8"v";
  const char8 inner_declaration[] = u8"v";
  const char8 use[] = u8"v";

  // let v;
  // for (let _ of []) {
  //   v;                 // ERROR
  //   let v;
  // }
  // TODO(strager): Code above doesn't match visits below.
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAreArray({
                  DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                    use, span_of(use),                      //
                                    declaration, span_of(inner_declaration)),
              }));
}

TEST(
    test_variable_analyzer,
    assign_to_variable_before_declaration_but_variable_is_declared_in_outer_scope) {
  const char8 outer_declaration[] = u8"v";
  const char8 inner_declaration[] = u8"v";
  const char8 assignment[] = u8"v";

  // let v;
  // for (let _ of []) {
  //   v = null;          // ERROR
  //   let v;
  // }
  // TODO(strager): Code above doesn't match visits below.
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_for_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(diag_assignment_before_variable_declaration,  //
                            assignment, span_of(assignment),              //
                            declaration, span_of(inner_declaration)),
      }));
}

TEST(test_variable_analyzer, shadowing_variable_in_parent_block_scope_is_okay) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   let x;
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, declaring_variable_twice_is_an_error) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";
  const char8 third_declaration[] = u8"x";

  // let x;
  // let x;  // ERROR
  // let x;  // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(third_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,  //
                            redeclaration,
                            span_of(second_declaration),  //
                            original_declaration, span_of(declaration)),
          DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,             //
                            redeclaration, span_of(third_declaration),  //
                            original_declaration, span_of(declaration)),
      }));
}

TEST(test_variable_analyzer, declaring_variable_twice_with_var_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  // var x;
  // var x;
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_var, variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, declaring_parameter_twice_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  // ((x, x) => {});
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_arrow_parameter,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_arrow_parameter,
                               variable_init_kind::normal);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, declaring_function_twice_is_okay) {
  const char8 declaration[] = u8"f";
  const char8 second_declaration[] = u8"f";

  // function f() {}
  // function f() {}
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_function,
                               variable_init_kind::normal);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer,
     mixing_var_and_function_in_same_function_scope_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  {
    // var x;
    // function x() {}
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // var x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // {
    //   var x;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, mixing_parameter_and_var_or_function_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  {
    // ((x) => {
    //   var x;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // ((x) => {
    //   function x() {}
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(
    test_variable_analyzer,
    mixing_let_or_const_or_class_with_other_variable_kind_in_same_scope_is_an_error) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  for (variable_kind declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_let, variable_kind::_var}) {
    for (variable_kind second_declaration_kind :
         {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
      // var x;
      // let x; // ERROR
      diag_collector v;
      variable_analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration), declaration_kind,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(
          v.errors,
          ElementsAreArray({
              DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,              //
                                redeclaration, span_of(second_declaration),  //
                                original_declaration, span_of(declaration)),
          }));
    }
  }

  for (variable_kind declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    for (variable_kind second_declaration_kind :
         {variable_kind::_class, variable_kind::_const,
          variable_kind::_function, variable_kind::_let, variable_kind::_var}) {
      // let x;
      // var x; // ERROR
      diag_collector v;
      variable_analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration), declaration_kind,
                                   variable_init_kind::normal);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind,
                                   variable_init_kind::normal);
      l.visit_end_of_module();

      EXPECT_THAT(
          v.errors,
          ElementsAreArray({
              DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,              //
                                redeclaration, span_of(second_declaration),  //
                                original_declaration, span_of(declaration)),
          }));
    }
  }
}

TEST(test_variable_analyzer,
     strict_variables_conflict_with_var_in_block_scope) {
  const char8 var_declaration[] = u8"x";
  const char8 other_declaration[] = u8"x";

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // {
    //   var x;
    // }
    // let x;    // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,             //
                              redeclaration, span_of(other_declaration),  //
                              original_declaration, span_of(var_declaration)),
        }));
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // let x;
    // {
    //   var x;  // ERROR
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,           //
                              redeclaration, span_of(var_declaration),  //
                              original_declaration, span_of(other_declaration)),
        }));
  }
}

TEST(test_variable_analyzer,
     strict_variables_do_not_conflict_with_functions_in_block_scope) {
  const char8 function_declaration[] = u8"x";
  const char8 other_declaration[] = u8"x";

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // {
    //   function x() {}
    // }
    // let x;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // let x;
    // {
    //   function x() {}
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, import_conflicts_with_any_variable_declaration) {
  const char8 import_declaration[] = u8"x";
  const char8 other_declaration[] = u8"x";

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_import, variable_kind::_let, variable_kind::_var}) {
    // import x from "";
    // let x;             // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        diag_redeclaration_of_variable,             //
                        redeclaration, span_of(other_declaration),  //
                        original_declaration, span_of(import_declaration)),
                }));
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_import, variable_kind::_let, variable_kind::_var}) {
    // let x;
    // import x from ""; // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,              //
                              redeclaration, span_of(import_declaration),  //
                              original_declaration, span_of(other_declaration)),
        }));
  }
}

TEST(test_variable_analyzer,
     catch_variable_conflicts_with_catch_variable_declared_in_same_scope) {
  const char8 catch_declaration_1[] = u8"e";
  const char8 catch_declaration_2[] = u8"e";

  // try {
  // } catch ([e, e]) {  // ERROR
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration_1),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(catch_declaration_2),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAreArray({
          DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,               //
                            redeclaration, span_of(catch_declaration_2),  //
                            original_declaration, span_of(catch_declaration_1)),
      }));
}

TEST(test_variable_analyzer,
     let_style_variable_in_same_scope_as_parameter_redeclares) {
  const char8 parameter_declaration[] = u8"x";
  const char8 local_declaration[] = u8"x";

  for (variable_kind local_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    // ((x) => {
    //   let x; // ERROR
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(
                        diag_redeclaration_of_variable,             //
                        redeclaration, span_of(local_declaration),  //
                        original_declaration, span_of(parameter_declaration)),
                }));
  }
}

TEST(test_variable_analyzer, let_variable_in_inner_scope_as_parameter_shadows) {
  const char8 parameter_declaration[] = u8"x";
  const char8 local_declaration[] = u8"x";

  for (variable_kind local_declaration_kind :
       {variable_kind::_const, variable_kind::_let}) {
    // ((x) => {
    //   {
    //     let x;
    //   }
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer,
     catch_variable_does_not_conflict_with_var_variable) {
  const char8 catch_declaration[] = u8"e";
  const char8 var_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   var e;
  // }
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration),
                               variable_kind::_catch,
                               variable_init_kind::normal);
  l.visit_variable_declaration(identifier_of(var_declaration),
                               variable_kind::_var, variable_init_kind::normal);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, catch_variable_conflicts_with_non_var_variables) {
  const char8 catch_declaration[] = u8"e";
  const char8 local_declaration[] = u8"e";

  for (variable_kind local_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_let}) {
    // try {
    // } catch (e) {
    //   let e;       // ERROR
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(catch_declaration),
                                 variable_kind::_catch,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind,
                                 variable_init_kind::normal);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_2_SPANS(diag_redeclaration_of_variable,             //
                              redeclaration, span_of(local_declaration),  //
                              original_declaration, span_of(catch_declaration)),
        }));
  }
}

TEST(test_variable_analyzer,
     parameter_default_value_cannot_refer_to_local_variables) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"l";
  const char8 local_declaration[] = u8"l";

  {
    // ((p = l) => {  // ERROR
    //   var l;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(parameter_default_value)),
                }));
  }

  {
    // ((p = (() => l)) => {  // ERROR
    //   var l;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();

    // (() => l)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_var,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(parameter_default_value)),
                }));
  }
}

TEST(test_variable_analyzer, parameter_default_value_uses_undeclared_variable) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"x";

  {
    // ((p = x) => {  // ERROR
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(parameter_default_value)),
                }));
  }

  {
    // ((p = (() => x)) => {  // ERROR
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();

    // (() => x)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_arrow_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                   span_of(parameter_default_value)),
                }));
  }
}

TEST(test_variable_analyzer, parameter_shadows_named_function_name) {
  const char8 function_declaration[] = u8"f";
  const char8 parameter_declaration[] = u8"f";
  const char8 parameter_use[] = u8"f";

  // (function f(f) {
  //   f;
  // });
  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_named_function_scope(identifier_of(function_declaration));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_function_parameter,
                               variable_init_kind::normal);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer, let_shadows_named_function_name) {
  const char8 function_declaration[] = u8"f";
  const char8 var_declaration[] = u8"f";
  const char8 var_use[] = u8"f";

  {
    // (function f() {
    //   let f;
    //   f;
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
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
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                      use, span_of(var_use),                  //
                                      declaration, span_of(var_declaration)),
                }));
  }
}

TEST(test_variable_analyzer, let_shadows_global_variable) {
  const char8 var_declaration[] = u8"Array";
  const char8 var_use[] = u8"Array";

  {
    // let Array;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // Array;
    // let Array;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_SPANS(diag_variable_used_before_declaration,  //
                                      use, span_of(var_use),                  //
                                      declaration, span_of(var_declaration)),
                }));
  }
}

TEST(test_variable_analyzer,
     class_declared_inside_class_scope_is_not_accessible_outside_class_scope) {
  {
    // (class C {});
    // C;             // ERROR
    const char8 class_declaration[] = u8"C";
    const char8 class_use[] = u8"C";
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_use(identifier_of(class_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                             name, span_of(class_use)),
                          }));
  }

  {
    // (class C {});
    // class C {}
    // (class C {});
    const char8 class_declaration_1[] = u8"C";
    const char8 class_declaration_2[] = u8"C";
    const char8 class_declaration_3[] = u8"C";
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_1));
    l.visit_exit_class_scope();

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_2));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration_2),
                                 variable_kind::_class,
                                 variable_init_kind::normal);

    l.visit_enter_class_scope();
    l.visit_enter_class_scope_body(identifier_of(class_declaration_3));
    l.visit_exit_class_scope();

    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer, class_extends_cannot_use_declared_class_name) {
  {
    // class C extends C {} // ERROR
    const char8 class_declaration[] = u8"C";
    const char8 class_use[] = u8"C";
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_use(identifier_of(class_use));
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_variable_used_before_declaration, use,
                                   span_of(class_use)),
                }));
  }
}

TEST(
    test_variable_analyzer,
    regression_assigning_to_variable_in_function_scope_does_not_interact_with_different_variable_in_parent_scope) {
  // (function() {
  //   b = null;
  // });
  // const a = null;
  // let b;
  const char8 a_declaration[] = u8"a";
  const char8 b_declaration[] = u8"b";
  const char8 b_assignment[] = u8"b";

  diag_collector v;
  variable_analyzer l(&v, &default_globals, javascript_var_options);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(b_assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(a_declaration),
                               variable_kind::_const,
                               variable_init_kind::initialized_with_equals);
  l.visit_variable_declaration(identifier_of(b_declaration),
                               variable_kind::_let, variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty())
      << "assigning to 'b' should not be an error; 'a' should not be confused "
         "with 'b'";
}

TEST(test_variable_analyzer, with_does_not_propagate_variable_uses) {
  const char8 declaration[] = u8"a";
  const char8 assignment[] = u8"a";
  const char8 use[] = u8"a";

  {
    // with({})
    //   a;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
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

    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const,
                                 variable_init_kind::initialized_with_equals);
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

    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_with_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "assigning to 'a' should not "
                                        "be an error inside with scope";
  }

  {
    // with ({}) {
    //   const a = 1;
    //   a = 2;
    // }

    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_with_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const,
                                 variable_init_kind::initialized_with_equals);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(diag_assignment_to_const_variable,       //
                               assignment, span_matcher(assignment),    //
                               declaration, span_matcher(declaration),  //
                               var_kind, variable_kind::_const),
        }));
  }

  {
    // with ({}) {
    //   function f() {
    //     a;
    //   }
    // }

    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
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

TEST(test_variable_analyzer_class, generic_class_parameters_are_usable_inside) {
  const char8 class_declaration[] = u8"C";
  const char8 parameter_declaration[] = u8"T";
  const char8 parameter_use[] = u8"T";
  const char8 method_name[] = u8"method";

  {
    // class C<T> {
    //   method(): T;
    // }
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_generic_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_property_declaration(identifier_of(method_name));
    l.visit_enter_function_scope();
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_function_scope();
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class,
                                 variable_init_kind::normal);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_class,
     generic_class_parameters_are_not_usable_outside) {
  const char8 class_declaration[] = u8"C";
  const char8 parameter_declaration[] = u8"T";
  const char8 parameter_use[] = u8"T";

  {
    // class C<T> { }
    // (null: T); // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_generic_parameter,
                                 variable_init_kind::normal);
    l.visit_enter_class_scope_body(identifier_of(class_declaration));
    l.visit_exit_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class,
                                 variable_init_kind::normal);
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
                                             span_of(parameter_use)),
                          }));
  }
}

TEST(test_variable_analyzer_type_alias, type_alias_can_use_outside_types) {
  const char8 imported_declaration[] = u8"C";
  const char8 type_alias_declaration[] = u8"Alias";
  const char8 type_use[] = u8"C";

  {
    // import {C} from "other-module";
    // type Alias = C;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(imported_declaration),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 variable_kind::_type_alias,
                                 variable_init_kind::normal);
    l.visit_enter_type_alias_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // type Alias = C;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 variable_kind::_type_alias,
                                 variable_init_kind::normal);
    l.visit_enter_type_alias_scope();
    l.visit_variable_type_use(identifier_of(type_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
                                             span_of(type_use)),
                          }));
  }
}

TEST(test_variable_analyzer_type_alias,
     generic_type_alias_parameters_are_usable_inside) {
  const char8 type_alias_declaration[] = u8"Alias";
  const char8 parameter_declaration[] = u8"T";
  const char8 parameter_use[] = u8"T";

  {
    // type Alias<T> = T;
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 variable_kind::_type_alias,
                                 variable_init_kind::normal);
    l.visit_enter_type_alias_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_generic_parameter,
                                 variable_init_kind::normal);
    l.visit_variable_type_use(identifier_of(parameter_use));
    l.visit_exit_type_alias_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_type_alias,
     generic_type_alias_parameters_are_not_usable_outside) {
  const char8 type_alias_declaration[] = u8"Alias";
  const char8 parameter_declaration[] = u8"T";
  const char8 parameter_use_outside_type_alias[] = u8"T";

  {
    // type Alias<T> = null;
    // (null as T);           // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_declaration(identifier_of(type_alias_declaration),
                                 variable_kind::_type_alias,
                                 variable_init_kind::normal);
    l.visit_enter_type_alias_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_generic_parameter,
                                 variable_init_kind::normal);
    l.visit_exit_type_alias_scope();
    l.visit_variable_type_use(identifier_of(parameter_use_outside_type_alias));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_use_of_undeclared_type, name,
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
