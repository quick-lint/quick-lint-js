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

#include <gtest/gtest.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
namespace {
template <std::size_t N>
source_code_span span_of(const char (&code)[N]) {
  return source_code_span(&code[0], &code[N]);
}

template <std::size_t N>
identifier identifier_of(const char (&name)[N]) {
  return identifier(span_of(name));
}

TEST(test_lint, let_or_const_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind,
              error_collector::error_variable_used_before_declaration);
    EXPECT_EQ(v.errors[0].where.begin(), use);
  }
}

TEST(test_lint, let_variable_use_before_declaration_with_parsing) {
  const char *input = "let x = y, y = x;";
  error_collector v;
  linter l(&v);
  parser p(input, &v);
  p.parse_and_visit_statement(l);
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(locator(input).range(v.errors[0].where).begin_offset(), 8);
  EXPECT_EQ(locator(input).range(v.errors[0].where).end_offset(), 9);
}

TEST(test_lint, let_variable_use_before_declaration_within_function) {
  const char declaration[] = "x";
  const char use[] = "x";

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
}

TEST(test_lint, let_variable_use_before_declaration_of_shadowing_variable) {
  const char declaration[] = "x";
  const char use[] = "x";

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
}

TEST(test_lint, var_or_function_variable_use_before_declaration) {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    ASSERT_TRUE(v.errors.empty());
  }
}

TEST(test_lint, variable_use_after_declaration) {
  for (variable_kind kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();
    EXPECT_TRUE(v.errors.empty());
  }
}

TEST(test_lint, variable_use_with_no_declaration) {
  const char use[] = "x";

  error_collector v;
  linter l(&v);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  ASSERT_EQ(v.errors.size(), 1);
  EXPECT_EQ(v.errors[0].kind,
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
}

TEST(test_lint, variable_use_with_declaration_in_different_function) {
  const char declaration[] = "x";
  const char use[] = "x";

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
            error_collector::error_variable_used_before_declaration);
  EXPECT_EQ(v.errors[0].where.begin(), use);
}

TEST(test_lint, use_global_variable_within_functions) {
  const char declaration[] = "x";
  const char use[] = "x";

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

  EXPECT_TRUE(v.errors.empty());
}

TEST(test_lint, function_uses_global_variable_declared_later_in_module) {
  const char declaration[] = "x";
  const char use[] = "x";

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  EXPECT_TRUE(v.errors.empty());
}

TEST(test_lint, assign_to_mutable_variable) {
  for (variable_kind kind :
       {variable_kind::_let, variable_kind::_var, variable_kind::_class,
        variable_kind::_function, variable_kind::_catch,
        variable_kind::_parameter}) {
    const char declaration[] = "x";
    const char assignment[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_enter_function_scope();
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_assignment(identifier_of(assignment));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_TRUE(v.errors.empty());
  }
}

TEST(test_lint, assign_to_immutable_variable) {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_import}) {
    const char declaration[] = "x";
    const char assignment[] = "x";

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
}  // namespace
}  // namespace quick_lint_js
