// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>

using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
source_code_span span_of(const char8 *code) {
  return source_code_span(&code[0], &code[strlen(code)]);
}

identifier identifier_of(const char8 *name) {
  return identifier(span_of(name));
}

global_declared_variable_set default_globals = configuration().globals();

constexpr const char8 *writable_global_variables[] = {
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"globalThis",

    // ECMA-262 18.2 Function Properties of the Global Object
    u8"decodeURI",
    u8"decodeURIComponent",
    u8"encodeURI",
    u8"encodeURIComponent",
    u8"eval",
    u8"isFinite",
    u8"isNaN",
    u8"parseFloat",
    u8"parseInt",

    // ECMA-262 18.3 Constructor Properties of the Global Object
    u8"Array",
    u8"ArrayBuffer",
    u8"BigInt",
    u8"BigInt64Array",
    u8"BigUint64Array",
    u8"Boolean",
    u8"DataView",
    u8"Date",
    u8"Error",
    u8"EvalError",
    u8"Float32Array",
    u8"Float64Array",
    u8"Function",
    u8"Int16Array",
    u8"Int32Array",
    u8"Int8Array",
    u8"Map",
    u8"Number",
    u8"Object",
    u8"Promise",
    u8"Proxy",
    u8"RangeError",
    u8"ReferenceError",
    u8"RegExp",
    u8"Set",
    u8"SharedArrayBuffer",
    u8"String",
    u8"Symbol",
    u8"SyntaxError",
    u8"TypeError",
    u8"URIError",
    u8"Uint16Array",
    u8"Uint32Array",
    u8"Uint8Array",
    u8"Uint8ClampedArray",
    u8"WeakMap",
    u8"WeakSet",

    // ECMA-262 18.4 Other Properties of the Global Object
    u8"Atomics",
    u8"JSON",
    u8"Math",
    u8"Reflect",
};

constexpr const char8 *non_writable_global_variables[] = {
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"Infinity",
    u8"NaN",
    u8"undefined",
};

TEST(test_lint, global_variables_are_usable) {
  // Array = null;
  // Array;
  for (const char8 *global_variable : writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }

  // NaN;
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, immutable_global_variables_are_not_assignable) {
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // NaN = null;  // ERROR
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_assignment_to_const_global_variable,
                              assignment, span_matcher(global_variable))));
  }

  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // (() => {
    //   NaN = null;  // ERROR
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_assignment_to_const_global_variable,
                              assignment, span_matcher(global_variable))));
  }
}

namespace {
constexpr const char8 *nodejs_global_variables[] = {
    u8"Array",
    u8"ArrayBuffer",
    u8"Atomics",
    u8"BigInt",
    u8"BigInt64Array",
    u8"BigUint64Array",
    u8"Boolean",
    u8"Buffer",
    u8"DataView",
    u8"Date",
    u8"Error",
    u8"EvalError",
    u8"Float32Array",
    u8"Float64Array",
    u8"Function",
    u8"GLOBAL",
    u8"Infinity",
    u8"Int16Array",
    u8"Int32Array",
    u8"Int8Array",
    u8"Intl",
    u8"JSON",
    u8"Map",
    u8"Math",
    u8"NaN",
    u8"Number",
    u8"Object",
    u8"Promise",
    u8"Proxy",
    u8"RangeError",
    u8"ReferenceError",
    u8"Reflect",
    u8"RegExp",
    u8"Set",
    u8"SharedArrayBuffer",
    u8"String",
    u8"Symbol",
    u8"SyntaxError",
    u8"TextDecoder",
    u8"TextEncoder",
    u8"TypeError",
    u8"URIError",
    u8"URL",
    u8"URLSearchParams",
    u8"Uint16Array",
    u8"Uint32Array",
    u8"Uint8Array",
    u8"Uint8ClampedArray",
    u8"WeakMap",
    u8"WeakSet",
    u8"WebAssembly",
    u8"clearImmediate",
    u8"clearInterval",
    u8"clearTimeout",
    u8"console",
    u8"decodeURI",
    u8"decodeURIComponent",
    u8"encodeURI",
    u8"encodeURIComponent",
    u8"escape",
    u8"eval",
    u8"global",
    u8"globalThis",
    u8"isFinite",
    u8"isNaN",
    u8"parseFloat",
    u8"parseInt",
    u8"process",
    u8"queueMicrotask",
    u8"root",
    u8"setImmediate",
    u8"setInterval",
    u8"setTimeout",
    u8"undefined",
    u8"unescape",
};
}

TEST(test_lint, nodejs_global_variables_are_usable) {
  for (const char8 *global_variable : nodejs_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, non_module_nodejs_global_variables_are_shadowable) {
  error_collector v;
  linter l(&v, &default_globals);
  // Intentionally excluded: __dirname, __filename, exports, module, require
  for (const char8 *global_variable : nodejs_global_variables) {
    l.visit_variable_declaration(identifier_of(global_variable),
                                 variable_kind::_let);
  }
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, any_variable_is_declarable_and_usable_if_opted_into) {
  // This tests the "literally-anything" global group.

  configuration config;
  config.allow_literally_any_global_variable();

  const char8 builtin_1_declaration[] = u8"Object";
  const char8 builtin_2_use[] = u8"Array";
  const char8 anything_1_declaration[] = u8"thisVariableDoesNotExistInAnyList";
  const char8 anything_2_use[] = u8"iDoNotExistInAnyList";

  error_collector v;
  linter l(&v, &config.globals());
  l.visit_variable_declaration(identifier_of(builtin_1_declaration),
                               variable_kind::_let);
  l.visit_variable_use(identifier_of(builtin_2_use));
  l.visit_variable_declaration(identifier_of(anything_1_declaration),
                               variable_kind::_let);
  l.visit_variable_use(identifier_of(anything_2_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, let_or_const_or_class_variable_use_before_declaration) {
  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;      // ERROR
    // let x;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_variable_used_before_declaration,  //
                              use, span_matcher(use),                  //
                              declaration, span_matcher(declaration))));
  }
}

TEST(test_lint, import_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // x;
  // import x from "";
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_import);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, export_use_after_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  for (variable_kind kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_import,
           variable_kind::_let,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(kind);

    // let x;
    // export {x};
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_export_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, export_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  for (variable_kind kind : {
           variable_kind::_class,
           variable_kind::_const,
           variable_kind::_function,
           variable_kind::_import,
           variable_kind::_let,
           variable_kind::_var,
       }) {
    SCOPED_TRACE(kind);

    // export {x};
    // let x;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_export_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, let_variable_use_before_declaration_within_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  //   let x;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(declaration))));
}

TEST(test_lint, let_variable_use_before_declaration_within_for_loop_scope) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // for (let _ of []) {
  //   x;
  //   let x;             // ERROR
  // }
  // TODO(strager): Code above doesn't match visits below.
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(declaration))));
}

TEST(test_lint, let_variable_use_before_declaration_of_shadowing_variable) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  //   let x;
  // });
  // let x;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(declaration))));
}

TEST(test_lint, var_or_function_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;
    // var x;  // x is hoisted
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint,
     var_or_function_variable_use_before_declaration_all_in_for_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // for (let _ of []) {
    //   x;
    //   var x;             // x is hoisted
    // }
    // TODO(strager): Code above doesn't match visits below.
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_for_scope();
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, var_or_function_variable_use_after_declaration_in_block_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // {
    //   var x;  // x has function scope
    // }
    // x;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(
    test_lint,
    var_or_function_variable_cannot_be_used_after_declaration_in_inner_function_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // (() => {
    //   var x;
    // });
    // x;        // ERROR
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                             name, span_matcher(use))));
  }
}

TEST(test_lint, var_variable_use_before_declaration_in_block_scope) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // x;
  // {
  //   var x;  // x is hoisted
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, function_variable_use_before_declaration_in_block_scope) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // f();
  // {
  //   function f() {}
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_2_FIELDS(
                  error_function_call_before_declaration_in_block_scope,  //
                  use, span_matcher(use),                                 //
                  declaration, span_matcher(declaration))));
}

TEST(test_lint,
     var_variable_use_before_declaration_in_block_scope_all_in_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;
  //   {
  //     var x;  // x is hoisted
  //   }
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
     function_variable_use_before_declaration_in_block_scope_all_in_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (() => {
  //   f();
  //   {
  //     function f() {}
  //   }
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_2_FIELDS(
                  error_function_call_before_declaration_in_block_scope,  //
                  use, span_matcher(use),                                 //
                  declaration, span_matcher(declaration))));
}

TEST(
    test_lint,
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, variable_use_after_declaration) {
  for (variable_kind kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // let x;
    // x;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, variable_use_with_no_declaration) {
  const char8 use[] = u8"x";

  // x;  // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use))));
}

TEST(test_lint, variable_export_with_no_declaration) {
  const char8 use[] = u8"x";

  // export {x};  // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_export_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use))));
}

TEST(test_lint, variable_use_in_function_with_no_declaration) {
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use))));
}

TEST(test_lint, variable_use_with_declaration_in_different_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   let x;
  // });
  // (() => {
  //   x;      // ERROR
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use))));
}

TEST(test_lint,
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
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(inner_declaration))));
}

TEST(test_lint, use_of_variable_declared_in_grandparent_scope) {
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
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
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

TEST(test_lint, name_of_named_function_expression_is_usable_within_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (function f() {
  //   f;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
     name_of_named_function_expression_is_usable_within_inner_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (function f() {
  //   (function() {
  //     f;
  //   });
  // });
  error_collector v;
  linter l(&v, &default_globals);
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
    test_lint,
    name_of_named_function_expression_is_usable_within_default_parameter_values) {
  const char8 declaration[] = u8"f";
  const char8 parameter_declaration[] = u8"x";
  const char8 use[] = u8"f";

  // (function f(x = f) {
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_parameter);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
     name_of_named_function_expression_is_not_usable_outside_function) {
  const char8 declaration[] = u8"f";
  const char8 use_before[] = u8"f";
  const char8 use_after[] = u8"f";

  // f;               // ERROR
  // (function f() {
  // });
  // f;               // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_before)),
                          ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_after))));
}

TEST(test_lint, use_global_variable_within_functions) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // let x;
  // (() => {
  //   x;
  // });
  // (() => {
  //   x;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
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

TEST(test_lint, function_uses_variable_declared_in_outer_function) {
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
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  {
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);

    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();
  }
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, function_uses_global_variable_declared_later_in_module) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;
  // });
  // let x;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, assign_to_mutable_variable) {
  for (variable_kind kind :
       {variable_kind::_let, variable_kind::_var, variable_kind::_class,
        variable_kind::_function, variable_kind::_catch,
        variable_kind::_parameter}) {
    const char8 declaration[] = u8"x";
    const char8 assignment[] = u8"x";

    // (() => {
    //   let x;  // x is mutable
    //   x = 42;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, assign_to_mutable_variable_shadowing_immutable_variable) {
  const char8 immutable_declaration[] = u8"x";
  const char8 mutable_declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  // import x from ""; // x is immutable
  // (() => {
  //   let x;          // x is mutable
  //   x = 42;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(immutable_declaration),
                               variable_kind::_import);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(mutable_declaration),
                               variable_kind::_let);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, assign_to_immutable_variable) {
  const char8 declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  for (variable_kind kind : {variable_kind::_const, variable_kind::_import}) {
    // (() => {
    //   const x;  // x is immutable
    //   x = 42;   // ERROR
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                              error_assignment_to_const_variable,      //
                              assignment, span_matcher(assignment),    //
                              declaration, span_matcher(declaration),  //
                              var_kind, kind)));
  }

  for (variable_kind kind : {variable_kind::_const, variable_kind::_import}) {
    // const x;   // x is immutable
    // {
    //   x = 42;  // ERROR
    // }
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_enter_block_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                              error_assignment_to_const_variable,      //
                              assignment, span_matcher(assignment),    //
                              declaration, span_matcher(declaration),  //
                              var_kind, kind)));
  }
}

TEST(test_lint, assign_to_immutable_variable_before_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = 42;   // ERROR
  // const x;  // x is immutable
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              UnorderedElementsAre(ERROR_TYPE_2_FIELDS(
                  error_assignment_to_const_variable_before_its_declaration,  //
                  assignment, span_matcher(assignment),                       //
                  declaration, span_matcher(declaration))));
}

TEST(test_lint, assign_to_shadowing_immutable_variable_before_declaration) {
  const char8 outer_declaration[] = u8"x";
  const char8 assignment[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;      // x is shadowed.
  // {
  //   x = 42;   // ERROR
  //   const x;  // x is immutable
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_const);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              UnorderedElementsAre(ERROR_TYPE_2_FIELDS(
                  error_assignment_to_const_variable_before_its_declaration,  //
                  assignment, span_matcher(assignment),                       //
                  declaration, span_matcher(inner_declaration))));
}

TEST(test_lint, assign_to_immutable_variable_declared_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // const x;   // x is immutable
  // (() => {
  //   x = 42;  // ERROR
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                            error_assignment_to_const_variable,      //
                            assignment, span_matcher(assignment),    //
                            declaration, span_matcher(declaration),  //
                            var_kind, variable_kind::_const)));
}

TEST(test_lint, assign_to_immutable_variable_declared_later_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // (() => {
  //   x = 42;  // ERROR
  // });
  // const x;   // x is immutable
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                            error_assignment_to_const_variable,      //
                            assignment, span_matcher(assignment),    //
                            declaration, span_matcher(declaration),  //
                            var_kind, variable_kind::_const)));
}

TEST(test_lint,
     assignment_to_shadowed_const_variable_before_declaration_in_parent_scope) {
  const char8 assignment[] = u8"x";
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   {
  //     x = 42;  // ERROR
  //   }
  //   const x;   // x is immutable
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_const);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              UnorderedElementsAre(ERROR_TYPE_2_FIELDS(
                  error_assignment_to_const_variable_before_its_declaration,  //
                  assignment, span_matcher(assignment),                       //
                  declaration, span_matcher(inner_declaration))));
}

TEST(test_lint, assignment_to_const_variable_declared_in_grandparent_scope) {
  const char8 declaration[] = u8"x";
  const char8 assignment[] = u8"x";

  // const x;
  // (() => {
  //   (() => {
  //     x = 42;  // ERROR
  //   });
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_const);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                            error_assignment_to_const_variable,      //
                            assignment, span_matcher(assignment),    //
                            declaration, span_matcher(declaration),  //
                            var_kind, variable_kind::_const)));
}

TEST(test_lint, assign_to_undeclared_variable) {
  const char8 assignment[] = u8"x";

  // x = null;  // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                            error_assignment_to_undeclared_variable, assignment,
                            span_matcher(assignment))));
}

TEST(test_lint, assign_inside_function_to_undeclared_variable) {
  const char8 assignment[] = u8"x";

  // (function() {
  //   x = null;  // ERROR
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                            error_assignment_to_undeclared_variable, assignment,
                            span_matcher(assignment))));
}

TEST(test_lint, assign_to_variable_before_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // let x;     // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_assignment_before_variable_declaration,  //
                            assignment, span_matcher(assignment),          //
                            declaration, span_matcher(declaration))));
}

TEST(test_lint, assign_to_variable_before_hoistable_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // var x;     // x is hoisted.
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, use_variable_declared_in_parent_function) {
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), var_kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, use_variable_declared_in_grandparent_function) {
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), var_kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, use_for_loop_let_variable_before_or_after_loop) {
  const char8 declaration[] = u8"element";
  const char8 use_before[] = u8"element";
  const char8 use_after[] = u8"element";

  // element;                  // ERROR
  // for (let element of []);
  // element;                  // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_before)),
                          ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_after))));
}

TEST(test_lint, use_variable_in_for_scope_declared_outside_for_scope) {
  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // let v;
    // for (let _ of [])
    //   v;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_var);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // for (let _ of [])
    //   v;               // ERROR
    // let v;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_variable_used_before_declaration,  //
                              use, span_matcher(use),                  //
                              declaration, span_matcher(declaration))));
  }
}

TEST(test_lint, use_undeclared_variable_in_function_scope_in_for_scope) {
  const char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;             // ERROR
  //   });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use))));
}

TEST(test_lint,
     use_variable_in_function_scope_in_for_scope_before_declaration) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;
  //   });
  // let v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
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
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(inner_declaration))));
}

TEST(
    test_lint,
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
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_for_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_assignment_before_variable_declaration,  //
                            assignment, span_matcher(assignment),          //
                            declaration, span_matcher(inner_declaration))));
}

TEST(test_lint, shadowing_variable_in_parent_block_scope_is_okay) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   let x;
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, declaring_variable_twice_is_an_error) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";
  const char8 third_declaration[] = u8"x";

  // let x;
  // let x;  // ERROR
  // let x;  // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_let);
  l.visit_variable_declaration(identifier_of(third_declaration),
                               variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(
      v.errors,
      ElementsAre(ERROR_TYPE_2_FIELDS(
                      error_redeclaration_of_variable,                  //
                      redeclaration, span_matcher(second_declaration),  //
                      original_declaration, span_matcher(declaration)),
                  ERROR_TYPE_2_FIELDS(
                      error_redeclaration_of_variable,                 //
                      redeclaration, span_matcher(third_declaration),  //
                      original_declaration, span_matcher(declaration))));
}

TEST(test_lint, declaring_variable_twice_with_var_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  // var x;
  // var x;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_var);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_var);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, declaring_parameter_twice_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  // ((x, x) => {});
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_parameter);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_parameter);
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, declaring_function_twice_is_okay) {
  const char8 declaration[] = u8"f";
  const char8 second_declaration[] = u8"f";

  // function f() {}
  // function f() {}
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_function);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_function);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, mixing_var_and_function_in_same_function_scope_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  {
    // var x;
    // function x() {}
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_var);
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_function);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // var x;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_function);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // function x() {}
    // {
    //   var x;
    // }
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_function);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, mixing_parameter_and_var_or_function_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  {
    // ((x) => {
    //   var x;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_var);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // ((x) => {
    //   function x() {}
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(second_declaration),
                                 variable_kind::_function);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(
    test_lint,
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
      error_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration),
                                   declaration_kind);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_2_FIELDS(
                      error_redeclaration_of_variable,                  //
                      redeclaration, span_matcher(second_declaration),  //
                      original_declaration, span_matcher(declaration))));
    }
  }

  for (variable_kind declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    for (variable_kind second_declaration_kind :
         {variable_kind::_class, variable_kind::_const,
          variable_kind::_function, variable_kind::_let, variable_kind::_var}) {
      // let x;
      // var x; // ERROR
      error_collector v;
      linter l(&v, &default_globals);
      l.visit_variable_declaration(identifier_of(declaration),
                                   declaration_kind);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_2_FIELDS(
                      error_redeclaration_of_variable,                  //
                      redeclaration, span_matcher(second_declaration),  //
                      original_declaration, span_matcher(declaration))));
    }
  }
}

TEST(test_lint, strict_variables_conflict_with_var_in_block_scope) {
  const char8 var_declaration[] = u8"x";
  const char8 other_declaration[] = u8"x";

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // {
    //   var x;
    // }
    // let x;    // ERROR
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_redeclaration_of_variable,                 //
                    redeclaration, span_matcher(other_declaration),  //
                    original_declaration, span_matcher(var_declaration))));
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // let x;
    // {
    //   var x;  // ERROR
    // }
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_redeclaration_of_variable,               //
                    redeclaration, span_matcher(var_declaration),  //
                    original_declaration, span_matcher(other_declaration))));
  }
}

TEST(test_lint,
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(function_declaration),
                                 variable_kind::_function);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, import_conflicts_with_any_variable_declaration) {
  const char8 import_declaration[] = u8"x";
  const char8 other_declaration[] = u8"x";

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_import, variable_kind::_let, variable_kind::_var}) {
    // import x from "";
    // let x;             // ERROR
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_redeclaration_of_variable,                 //
                    redeclaration, span_matcher(other_declaration),  //
                    original_declaration, span_matcher(import_declaration))));
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_import, variable_kind::_let, variable_kind::_var}) {
    // let x;
    // import x from ""; // ERROR
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_redeclaration_of_variable,                  //
                    redeclaration, span_matcher(import_declaration),  //
                    original_declaration, span_matcher(other_declaration))));
  }
}

TEST(test_lint,
     catch_variable_conflicts_with_catch_variable_declared_in_same_scope) {
  const char8 catch_declaration_1[] = u8"e";
  const char8 catch_declaration_2[] = u8"e";

  // try {
  // } catch ([e, e]) {  // ERROR
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration_1),
                               variable_kind::_catch);
  l.visit_variable_declaration(identifier_of(catch_declaration_2),
                               variable_kind::_catch);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_2_FIELDS(
                  error_redeclaration_of_variable,                   //
                  redeclaration, span_matcher(catch_declaration_2),  //
                  original_declaration, span_matcher(catch_declaration_1))));
}

TEST(test_lint, let_style_variable_in_same_scope_as_parameter_redeclares) {
  const char8 parameter_declaration[] = u8"x";
  const char8 local_declaration[] = u8"x";

  for (variable_kind local_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    // ((x) => {
    //   let x; // ERROR
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_redeclaration_of_variable,                 //
            redeclaration, span_matcher(local_declaration),  //
            original_declaration, span_matcher(parameter_declaration))));
  }
}

TEST(test_lint, let_variable_in_inner_scope_as_parameter_shadows) {
  const char8 parameter_declaration[] = u8"x";
  const char8 local_declaration[] = u8"x";

  for (variable_kind local_declaration_kind :
       {variable_kind::_const, variable_kind::_let}) {
    // ((x) => {
    //   {
    //     let x;
    //   }
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, catch_variable_does_not_conflict_with_var_variable) {
  const char8 catch_declaration[] = u8"e";
  const char8 var_declaration[] = u8"e";

  // try {
  // } catch (e) {
  //   var e;
  // }
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(catch_declaration),
                               variable_kind::_catch);
  l.visit_variable_declaration(identifier_of(var_declaration),
                               variable_kind::_var);
  l.visit_exit_block_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, catch_variable_conflicts_with_non_var_variables) {
  const char8 catch_declaration[] = u8"e";
  const char8 local_declaration[] = u8"e";

  for (variable_kind local_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_let}) {
    // try {
    // } catch (e) {
    //   let e;       // ERROR
    // }
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(catch_declaration),
                                 variable_kind::_catch);
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_redeclaration_of_variable,                 //
                    redeclaration, span_matcher(local_declaration),  //
                    original_declaration, span_matcher(catch_declaration))));
  }
}

TEST(test_lint, parameter_default_value_cannot_refer_to_local_variables) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"l";
  const char8 local_declaration[] = u8"l";

  {
    // ((p = l) => {  // ERROR
    //   var l;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_var);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_use_of_undeclared_variable, name,
                              span_matcher(parameter_default_value))));
  }

  {
    // ((p = (() => l)) => {  // ERROR
    //   var l;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();

    // (() => l)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_var);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_use_of_undeclared_variable, name,
                              span_matcher(parameter_default_value))));
  }
}

TEST(test_lint, parameter_default_value_uses_undeclared_variable) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"x";

  {
    // ((p = x) => {  // ERROR
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_use_of_undeclared_variable, name,
                              span_matcher(parameter_default_value))));
  }

  {
    // ((p = (() => x)) => {  // ERROR
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();

    // (() => x)
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_use_of_undeclared_variable, name,
                              span_matcher(parameter_default_value))));
  }
}

TEST(test_lint, parameter_shadows_named_function_name) {
  const char8 function_declaration[] = u8"f";
  const char8 parameter_declaration[] = u8"f";
  const char8 parameter_use[] = u8"f";

  // (function f(f) {
  //   f;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_named_function_scope(identifier_of(function_declaration));
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_parameter);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, let_shadows_named_function_name) {
  const char8 function_declaration[] = u8"f";
  const char8 var_declaration[] = u8"f";
  const char8 var_use[] = u8"f";

  {
    // (function f() {
    //   let f;
    //   f;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let);
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
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_variable_used_before_declaration,  //
                              use, span_matcher(var_use),              //
                              declaration, span_matcher(var_declaration))));
  }
}

TEST(test_lint, let_shadows_global_variable) {
  const char8 var_declaration[] = u8"Array";
  const char8 var_use[] = u8"Array";

  {
    // let Array;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // Array;
    // let Array;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_variable_used_before_declaration,  //
                              use, span_matcher(var_use),              //
                              declaration, span_matcher(var_declaration))));
  }
}

TEST(test_lint,
     class_declared_inside_class_scope_is_not_accessible_outside_class_scope) {
  {
    // (class C {});
    // C;             // ERROR
    const char8 class_declaration[] = u8"C";
    const char8 class_use[] = u8"C";
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration),
                                 variable_kind::_class);
    l.visit_exit_class_scope();
    l.visit_variable_use(identifier_of(class_use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                             name, span_matcher(class_use))));
  }

  {
    // (class C {});
    // class C {}
    // (class C {});
    const char8 class_declaration_1[] = u8"C";
    const char8 class_declaration_2[] = u8"C";
    const char8 class_declaration_3[] = u8"C";
    error_collector v;
    linter l(&v, &default_globals);

    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration_1),
                                 variable_kind::_class);
    l.visit_exit_class_scope();

    l.visit_variable_declaration(identifier_of(class_declaration_2),
                                 variable_kind::_class);
    l.visit_enter_class_scope();
    l.visit_exit_class_scope();

    l.visit_enter_class_scope();
    l.visit_variable_declaration(identifier_of(class_declaration_3),
                                 variable_kind::_class);
    l.visit_exit_class_scope();

    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(
    test_lint,
    regression_assigning_to_variable_in_function_scope_does_not_interact_with_different_variable_in_parent_scope) {
  // (function() {
  //   b = null;
  // });
  // const a;
  // let b;
  const char8 a_declaration[] = u8"a";
  const char8 b_declaration[] = u8"b";
  const char8 b_assignment[] = u8"b";

  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_assignment(identifier_of(b_assignment));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(a_declaration),
                               variable_kind::_const);
  l.visit_variable_declaration(identifier_of(b_declaration),
                               variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty())
      << "assigning to 'b' should not be an error; 'a' should not be confused "
         "with 'b'";
}

TEST(test_lint, with_does_not_propagate_variable_uses) {
  const char8 declaration[] = u8"a";
  const char8 assignment[] = u8"a";
  const char8 use[] = u8"a";

  {
    // with({})
    //   a;
    error_collector v;
    linter l(&v, &default_globals);
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

    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const);
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

    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_with_scope();
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_with_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty()) << "assigning to 'a' should not "
                                        "be an error inside with scope";
  }

  {
    // with ({}) {
    //   const a = 1;
    //   a = 2;
    // }

    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_with_scope();
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_const);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_block_scope();
    l.visit_exit_with_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_3_FIELDS(
                              error_assignment_to_const_variable,      //
                              assignment, span_matcher(assignment),    //
                              declaration, span_matcher(declaration),  //
                              var_kind, variable_kind::_const)));
  }

  {
    // with ({}) {
    //   function f() {
    //     a;
    //   }
    // }

    error_collector v;
    linter l(&v, &default_globals);
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

TEST(test_lint_magic_arguments,
     arguments_magic_variable_is_usable_within_functions) {
  const char8 arguments_use[] = u8"arguments";

  // (function() {
  //   arguments;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_magic_arguments,
     arguments_magic_variable_is_unusable_in_global_scope) {
  const char8 arguments_use[] = u8"arguments";

  // arguments;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(arguments_use))));
}

TEST(test_lint_magic_arguments, parameter_named_arguments_does_not_conflict) {
  const char8 parameter_declaration[] = u8"arguments";
  const char8 parameter_use[] = u8"arguments";

  // (function(arguments) {
  //   arguments;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(parameter_declaration),
                               variable_kind::_parameter);
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(parameter_use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_magic_arguments,
     parameter_default_values_can_reference_arguments) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"arguments";

  {
    // (function(p = arguments) {
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  // 'arguments' refers to magic-arguments, not a local variable. If 'arguments'
  // referred to a local variable, this test would fail with a
  // use-before-declaration error.
  {
    const char8 local_declaration[] = u8"arguments";

    // (function(p = arguments) {
    //   let arguments;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_let);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_magic_arguments, var_does_not_conflict_with_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   var arguments;
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_var);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_magic_arguments, let_shadows_magic_arguments) {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char8 arguments_declaration[] = u8"arguments";

    // (function() {
    //   let arguments;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char8 arguments_declaration[] = u8"arguments";
    const char8 arguments_use[] = u8"arguments";

    // (function() {
    //   arguments;      // ERROR
    //   let arguments;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(arguments_use));
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_variable_used_before_declaration,  //
                    use, span_matcher(arguments_use),        //
                    declaration, span_matcher(arguments_declaration))));
  }
}

TEST(test_lint_magic_arguments, function_shadows_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   function arguments() {}
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_function);
  l.visit_enter_function_scope();
  l.visit_exit_function_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_magic_arguments, catch_variable_shadows_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   try {
  //   } catch (arguments) {
  //   }
  // });
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_enter_block_scope();
  l.visit_exit_block_scope();
  l.visit_enter_block_scope();
  l.visit_variable_declaration(identifier_of(arguments_declaration),
                               variable_kind::_catch);
  l.visit_exit_block_scope();
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

// TODO(#204): 'arguments' should not be declared in arrow functions.

TEST(test_lint_typeof, using_undeclared_variable_in_typeof_is_not_an_error) {
  const char8 use[] = u8"v";

  // typeof v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_typeof, typeof_declares_variable_automagically) {
  const char8 typeof_use[] = u8"v";
  const char8 other_use[] = u8"v";

  // typeof v;
  // v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_variable_use(identifier_of(other_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_typeof,
     typeof_declares_variable_automagically_in_parent_function) {
  const char8 use_before[] = u8"v";
  const char8 typeof_use[] = u8"v";
  const char8 use_after[] = u8"v";

  // v;
  // (() => {
  //   typeof v;
  // });
  // v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_typeof, typeof_refers_to_already_declared_variable) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // let v;
  // typeof v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint_typeof, typeof_variable_declared_later_is_an_error) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // typeof v;  // ERROR
  // let v;
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, span_matcher(use),                  //
                            declaration, span_matcher(declaration))));
}

TEST(
    test_lint_typeof,
    typeof_already_declared_variable_does_not_declare_variable_in_parent_function) {
  const char8 use_before[] = u8"v";
  const char8 declaration[] = u8"v";
  const char8 typeof_use[] = u8"v";
  const char8 use_after[] = u8"v";

  // v;           // ERROR
  // (() => {
  //   let v;
  //   typeof v;
  // });
  // v;           // ERROR
  error_collector v;
  linter l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_before)),
                          ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                           name, span_matcher(use_after))));
}

TEST(test_lint_eval, disable_variable_lookup_in_presence_of_eval) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    // eval("var x = 42");
    // x;
    // x = 10;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // {
    //   eval("var x = 42");
    // }
    // x;
    // x = 10;
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // eval("var x = 42");
    // (function() {
    //   x;
    //   x = 10;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    // });
    // eval("var x = 42");
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    //   eval("var x = 42;");
    //   x;
    //   x = 10;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // (function() {
    //   x;
    //   x = 10;
    //   {
    //     eval("var x = 42;");
    //   }
    //   x;
    //   x = 10;
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_block_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint_eval,
     disable_variable_lookup_in_presence_of_eval_for_limited_scope) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    // (function() {
    //   eval("var x = 42;");
    // });
    // (function() {
    //   x;  // ERROR (use of undeclared variable)
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                             name, span_matcher(use))));
  }

  {
    // (function() {
    //   eval("var x = 42;");
    // });
    // x;  // ERROR (use of undeclared variable)
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                             name, span_matcher(use))));
  }

  {
    // (function() {
    //   (function() {
    //     eval("var x = 42;");
    //   });
    //   x;      // ERROR (use of undeclared variable)
    //   x = 10; // ERROR (assignment to undeclared variable)
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable, name,
                                     span_matcher(use)),
                    ERROR_TYPE_FIELD(error_assignment_to_undeclared_variable,
                                     assignment, span_matcher(use))));
  }

  {
    const char8 parameter_declaration[] = u8"a";
    const char8 function_declaration[] = u8"f";

    // function f(a = eval('var x = 42;')) {
    //   x;
    // }
    // x; // ERROR (use of undeclared variable)
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable,
                                             name, span_matcher(use))));
  }

  {
    const char8 eval_declaration[] = u8"eval";

    // let eval = () => {};
    // eval("var x = 42;");
    // x;  // ERROR (use of undeclared variable)
    // x = 10;  // ERROR (assignment to undeclared variable)
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_let);
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable, name,
                                     span_matcher(use)),
                    ERROR_TYPE_FIELD(error_assignment_to_undeclared_variable,
                                     assignment, span_matcher(use))));
  }

  {
    const char8 eval_declaration[] = u8"eval";

    // function f() {
    //   eval = () => {};
    //   eval("var x = 42;");
    //   x;  // ERROR (use of undeclared variable)
    //   x = 10;  // ERROR (assignment to undeclared variable)
    //   {
    //     var eval;
    //   }
    // }
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_assignment(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(error_use_of_undeclared_variable, name,
                                     span_matcher(use)),
                    ERROR_TYPE_FIELD(error_assignment_to_undeclared_variable,
                                     assignment, span_matcher(use))));
  }
}

TEST(test_lint_eval, false_negatives_on_redeclaration_of_eval) {
  const char8 use_eval[] = u8"eval";
  const char8 use[] = u8"x";

  {
    const char8 eval_declaration[] = u8"eval";

    // let eval = () => {};
    // (function() {
    //   eval("var x = 42;");
    //   x;  // TODO: ERROR (use of undeclared variable)
    //   x = 10;  // TODO: ERROR (assignment to undeclared variable)
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_variable_declaration(identifier_of(eval_declaration),
                                 variable_kind::_let);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_assignment(identifier_of(use));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    const char8 const_declaration[] = u8"x";
    const char8 const_assignment[] = u8"x";

    // (function() {
    //   const x = 42;
    //   {
    //     eval("var x = 0");
    //     x = 3;  // TODO: ERROR (assignment to const variable)
    //   }
    // });
    error_collector v;
    linter l(&v, &default_globals);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(const_declaration),
                                 variable_kind::_const);
    l.visit_enter_block_scope();
    l.visit_variable_use(identifier_of(use_eval));
    l.visit_variable_assignment(identifier_of(const_assignment));
    l.visit_exit_block_scope();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
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
