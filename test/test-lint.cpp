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
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>

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
  for (const char8 *global_variable : writable_global_variables) {
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_variable_use(identifier_of(global_variable));
  }
  for (const char8 *global_variable : non_writable_global_variables) {
    l.visit_variable_use(identifier_of(global_variable));
  }
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, immutable_global_variables_are_not_assignable) {
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

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

TEST(test_lint, let_or_const_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

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

TEST(test_lint, let_variable_use_before_declaration_with_parsing) {
  padded_string input(u8"let x = y, y = x;");
  error_collector v;
  linter l(&v);
  parser p(&input, &v);
  p.parse_and_visit_statement(l);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 8);
  EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 9);
  EXPECT_EQ(locator(&input).range(v.errors[0].other_where).begin_offset(), 11);
}

TEST(test_lint, let_variable_use_before_declaration_within_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
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

    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    ASSERT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, var_or_function_variable_use_before_declaration_in_for_scope) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

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

TEST(
    test_lint,
    var_or_function_variable_use_before_declaration_in_different_block_scopes) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char8 declaration[] = u8"x";
    const char8 use[] = u8"x";

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_use_of_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), use);
}

TEST(test_lint, use_global_variable_within_functions) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  error_collector v;
  linter l(&v);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, function_uses_variable_declared_in_outer_function) {
  const char8 declaration[] = u8"x";
  const char8 use[] = u8"x";

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  {
    l.visit_enter_function_scope();
    { l.visit_variable_use(identifier_of(use)); }
    l.visit_exit_function_scope();

    l.visit_variable_declaration(identifier_of(declaration),
                                 variable_kind::_let);

    l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
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

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_variable_declaration(identifier_of(immutable_declaration),
                               variable_kind::_import);
  l.visit_enter_function_scope();
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

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_variable_assignment(identifier_of(assignment));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_assignment_to_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), assignment);
}

TEST(test_lint, assign_to_variable_before_hoistable_declaration) {
  const char8 assignment[] = u8"x";
  const char8 declaration[] = u8"x";

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

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope();
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

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope();
    l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
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

  error_collector v;
  linter l(&v);
  l.visit_enter_for_scope();
  l.visit_enter_function_scope();
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
            error_collector::error_assignment_to_undeclared_variable);
  EXPECT_EQ(v.errors[0].where.begin(), assignment);
}
}  // namespace
}  // namespace quick_lint_js
