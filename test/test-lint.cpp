// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>

using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
source_code_span span_of(const char8 *code) {
  return source_code_span(&code[0], &code[strlen(code)]);
}

identifier identifier_of(const char8 *name) {
  return identifier(span_of(name));
}

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
  error_collector v;
  linter l(&v);
  // Array = null;
  // Array;
  for (const char8 *global_variable : writable_global_variables) {
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_variable_use(identifier_of(global_variable));
  }
  // NaN;
  for (const char8 *global_variable : non_writable_global_variables) {
    l.visit_variable_use(identifier_of(global_variable));
  }
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, immutable_global_variables_are_not_assignable) {
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // NaN = null;  // ERROR
    error_collector v;
    linter l(&v);
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_assignment_to_const_global_variable);
    EXPECT_EQ(v.errors[0].where.begin(), global_variable);
  }
}

TEST(test_lint, let_or_const_or_class_variable_use_before_declaration) {
  for (variable_kind kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_let}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;      // ERROR
    // let x;
    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_variable_used_before_declaration);
    EXPECT_EQ(v.errors[0].where.begin(), use);
    EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
  }
}

TEST(test_lint, import_use_before_declaration_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // x;
  // import x from "";
  error_collector v;
  linter l(&v);
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration),
                               variable_kind::_import);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, let_variable_use_before_declaration_within_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  //   let x;
  // });
  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
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
  linter l(&v);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
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
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
}

TEST(test_lint, var_or_function_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;
    // var x;  // x is hoisted
    error_collector v;
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_function_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_use_of_undeclared_variable);
    EXPECT_EQ(v.errors[0].where.begin(), use);
  }
}

TEST(test_lint,
     var_or_function_variable_use_before_declaration_in_block_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    // x;
    // {
    //   var x;  // x is hoisted
    // }
    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
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
    linter l(&v);
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
    linter l(&v);
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
  linter l(&v);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use);
}

TEST(test_lint, variable_use_in_function_with_no_declaration) {
  const char8 use[] = u8"x";

  // (() => {
  //   x;      // ERROR
  // });
  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use);
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
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use);
}

TEST(test_lint, name_of_named_function_expression_is_usable_within_function) {
  const char8 declaration[] = u8"f";
  const char8 use[] = u8"f";

  // (function f() {
  //   f;
  // });
  error_collector v;
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_named_function_scope(identifier_of(declaration));
  l.visit_enter_function_scope_body();
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 2);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use_before);
  EXPECT_EQ(v.errors[1].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[1].where.begin(), use_after);
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
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
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
    linter l(&v);
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
  linter l(&v);
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
  for (variable_kind kind : {variable_kind::_const, variable_kind::_import}) {
    const char8 declaration[] = u8"x";
    const char8 assignment[] = u8"x";

    // (() => {
    //   const x;  // x is immutable
    //   x = 42;   // ERROR
    // });
    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_assignment_to_const_variable);
    EXPECT_EQ(v.errors[0].where.begin(), assignment);
    EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
    EXPECT_EQ(v.errors[0].var_kind, kind);
  }
}

TEST(test_lint, assign_to_undeclared_variable) {
  const char8 assignment[] = u8"x";

  // x = null;  // ERROR
  error_collector v;
  linter l(&v);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_assignment_to_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), assignment);
}

TEST(test_lint, assign_to_variable_before_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // let x;     // ERROR
  error_collector v;
  linter l(&v);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_assignment_before_variable_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), assignment);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
}

TEST(test_lint, assign_to_variable_before_hoistable_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

  // x = null;
  // var x;     // x is hoisted.
  error_collector v;
  linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
  linter l(&v);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_for_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 2);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use_before);
  EXPECT_EQ(v.errors[1].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[1].where.begin(), use_after);
}

TEST(test_lint, use_variable_in_for_scope_declared_outside_for_scope) {
  {
    const char8 declaration[] = u8"v";
    const char8 use[] = u8"v";

    // let v;
    // for (let _ of [])
    //   v;
    error_collector v;
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
    l.visit_enter_for_scope();
    l.visit_variable_use(identifier_of(use));
    l.visit_exit_for_scope();
    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_variable_used_before_declaration);
    EXPECT_EQ(v.errors[0].where.begin(), use);
    EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
  }
}

TEST(test_lint, use_undeclared_variable_in_function_scope_in_for_scope) {
  const char8 use[] = u8"v";

  // for (let _ of [])
  //   (() => {
  //     v;             // ERROR
  //   });
  error_collector v;
  linter l(&v);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use);
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
  linter l(&v);
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
  linter l(&v);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_for_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
  EXPECT_EQ(v.errors[0].other_where.begin(), inner_declaration);
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
  linter l(&v);
  l.visit_variable_declaration(identifier_of(outer_declaration),
                               variable_kind::_let);
  l.visit_enter_for_scope();
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(inner_declaration),
                               variable_kind::_let);
  l.visit_exit_for_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_assignment_before_variable_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), assignment);
  EXPECT_EQ(v.errors[0].other_where.begin(), inner_declaration);
}

TEST(test_lint, shadowing_variable_in_parent_block_scope_is_okay) {
  const char8 outer_declaration[] = u8"x";
  const char8 inner_declaration[] = u8"x";

  // let x;
  // {
  //   let x;
  // }
  error_collector v;
  linter l(&v);
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
  linter l(&v);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_variable_declaration(identifier_of(second_declaration),
                               variable_kind::_let);
  l.visit_variable_declaration(identifier_of(third_declaration),
                               variable_kind::_let);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 2);
  EXPECT_EQ(v.errors[0].kind, error_collector::error_redeclaration_of_variable);
  EXPECT_EQ(v.errors[0].where.begin(), second_declaration);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
  EXPECT_EQ(v.errors[1].kind, error_collector::error_redeclaration_of_variable);
  EXPECT_EQ(v.errors[1].where.begin(), third_declaration);
  EXPECT_EQ(v.errors[1].other_where.begin(), declaration);
}

TEST(test_lint, declaring_variable_twice_with_var_is_okay) {
  const char8 declaration[] = u8"x";
  const char8 second_declaration[] = u8"x";

  // var x;
  // var x;
  error_collector v;
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
      linter l(&v);
      l.visit_variable_declaration(identifier_of(declaration),
                                   declaration_kind);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind);
      l.visit_end_of_module();

      ASSERT_EQ(v.errors.size(), 1);
      EXPECT_EQ(v.errors[0].kind,
                error_collector::error_redeclaration_of_variable);
      EXPECT_EQ(v.errors[0].where.begin(), second_declaration);
      EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
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
      linter l(&v);
      l.visit_variable_declaration(identifier_of(declaration),
                                   declaration_kind);
      l.visit_variable_declaration(identifier_of(second_declaration),
                                   second_declaration_kind);
      l.visit_end_of_module();

      ASSERT_EQ(v.errors.size(), 1);
      EXPECT_EQ(v.errors[0].kind,
                error_collector::error_redeclaration_of_variable);
      EXPECT_EQ(v.errors[0].where.begin(), second_declaration);
      EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
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
    linter l(&v);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), other_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), var_declaration);
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_import,
        variable_kind::_let}) {
    // let x;
    // {
    //   var x;  // ERROR
    // }
    error_collector v;
    linter l(&v);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_var);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), var_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), other_declaration);
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
    linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), other_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), import_declaration);
  }

  for (variable_kind other_declaration_kind :
       {variable_kind::_class, variable_kind::_const, variable_kind::_function,
        variable_kind::_import, variable_kind::_let, variable_kind::_var}) {
    // let x;
    // import x from ""; // ERROR
    error_collector v;
    linter l(&v);
    l.visit_variable_declaration(identifier_of(other_declaration),
                                 other_declaration_kind);
    l.visit_variable_declaration(identifier_of(import_declaration),
                                 variable_kind::_import);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), import_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), other_declaration);
  }
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
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), local_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), parameter_declaration);
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
    linter l(&v);
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
  linter l(&v);
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
    linter l(&v);
    l.visit_enter_block_scope();
    l.visit_variable_declaration(identifier_of(catch_declaration),
                                 variable_kind::_catch);
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 local_declaration_kind);
    l.visit_exit_block_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_redeclaration_of_variable);
    EXPECT_EQ(v.errors[0].where.begin(), local_declaration);
    EXPECT_EQ(v.errors[0].other_where.begin(), catch_declaration);
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
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_variable_declaration(identifier_of(local_declaration),
                                 variable_kind::_var);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_use_of_undeclared_variable);
    EXPECT_EQ(v.errors[0].where.begin(), parameter_default_value);
  }

  {
    // ((p = (() => l)) => {  // ERROR
    //   var l;
    // });
    error_collector v;
    linter l(&v);
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

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_use_of_undeclared_variable);
    EXPECT_EQ(v.errors[0].where.begin(), parameter_default_value);
  }
}

TEST(test_lint, parameter_default_value_uses_undeclared_variable) {
  const char8 parameter_declaration[] = u8"p";
  const char8 parameter_default_value[] = u8"x";

  {
    // ((p = x) => {  // ERROR
    // });
    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_variable_use(identifier_of(parameter_default_value));
    l.visit_variable_declaration(identifier_of(parameter_declaration),
                                 variable_kind::_parameter);
    l.visit_enter_function_scope_body();
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_use_of_undeclared_variable);
    EXPECT_EQ(v.errors[0].where.begin(), parameter_default_value);
  }

  {
    // ((p = (() => x)) => {  // ERROR
    // });
    error_collector v;
    linter l(&v);
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

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_use_of_undeclared_variable);
    EXPECT_EQ(v.errors[0].where.begin(), parameter_default_value);
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
  linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
    l.visit_enter_named_function_scope(identifier_of(function_declaration));
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(var_use));
    l.visit_variable_declaration(identifier_of(var_declaration),
                                 variable_kind::_let);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_variable_used_before_declaration);
    EXPECT_EQ(v.errors[0].where.begin(), var_use);
    EXPECT_EQ(v.errors[0].other_where.begin(), var_declaration);
  }
}

TEST(test_lint_magic_arguments,
     arguments_magic_variable_is_usable_within_functions) {
  const char8 arguments_use[] = u8"arguments";

  // (function() {
  //   arguments;
  // });
  error_collector v;
  linter l(&v);
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
  linter l(&v);
  l.visit_variable_use(identifier_of(arguments_use));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), arguments_use);
}

TEST(test_lint_magic_arguments, parameter_named_arguments_does_not_conflict) {
  const char8 parameter_declaration[] = u8"arguments";
  const char8 parameter_use[] = u8"arguments";

  // (function(arguments) {
  //   arguments;
  // });
  error_collector v;
  linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
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
  linter l(&v);
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
    linter l(&v);
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
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_use(identifier_of(arguments_use));
    l.visit_variable_declaration(identifier_of(arguments_declaration), kind);
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_variable_used_before_declaration);
    EXPECT_EQ(v.errors[0].where.begin(), arguments_use);
    EXPECT_EQ(v.errors[0].other_where.begin(), arguments_declaration);
  }
}

TEST(test_lint_magic_arguments, function_shadows_magic_arguments) {
  const char8 arguments_declaration[] = u8"arguments";

  // (function() {
  //   function arguments() {}
  // });
  error_collector v;
  linter l(&v);
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
  linter l(&v);
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

// TODO(strager): 'arguments' should not be declared in arrow functions.

TEST(test_lint_typeof, using_undeclared_variable_in_typeof_is_not_an_error) {
  const char8 use[] = u8"v";

  // typeof v;
  error_collector v;
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
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
  linter l(&v);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
  EXPECT_EQ(v.errors[0].other_where.begin(), declaration);
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
  linter l(&v);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 2);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use_before);
  EXPECT_EQ(v.errors[1].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[1].where.begin(), use_after);
}
}
}
