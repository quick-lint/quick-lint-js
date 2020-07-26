// quicklint-js finds bugs in JavaScript programs.
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

#include <doctest/doctest.h>
#include <quicklint-js/error-collector.h>
#include <quicklint-js/language.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/lint.h>
#include <quicklint-js/parse.h>

namespace quicklint_js {
namespace {
template <std::size_t N>
source_code_span span_of(const char (&code)[N]) {
  return source_code_span(&code[0], &code[N]);
}

template <std::size_t N>
identifier identifier_of(const char (&name)[N]) {
  return identifier(span_of(name));
}

TEST_CASE("let or const variable use before declaration") {
  for (variable_kind kind : {variable_kind::_const, variable_kind::_let}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    REQUIRE(v.errors.size() == 1);
    CHECK(v.errors[0].kind ==
          error_collector::error_variable_used_before_declaration);
    CHECK(v.errors[0].where.begin() == use);
  }
}

TEST_CASE("let variable use before declaration (with parsing)") {
  const char *input = "let x = y, y = x;";
  error_collector v;
  linter l(&v);
  parser p(input, &v);
  p.parse_statement(l);
  l.visit_end_of_module();

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(locator(input).range(v.errors[0].where).begin_offset() == 8);
  CHECK(locator(input).range(v.errors[0].where).end_offset() == 9);
}

TEST_CASE("let variable use before declaration within function") {
  const char declaration[] = "x";
  const char use[] = "x";

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_exit_function_scope();
  l.visit_end_of_module();

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(v.errors[0].where.begin() == use);
}

TEST_CASE("let variable use before declaration of shadowing variable") {
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

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(v.errors[0].where.begin() == use);
}

TEST_CASE("var or function variable use before declaration") {
  for (variable_kind kind : {variable_kind::_function, variable_kind::_var}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_use(identifier_of(use));
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_end_of_module();

    REQUIRE(v.errors.empty());
  }
}

TEST_CASE("variable use after declaration") {
  for (variable_kind kind :
       {variable_kind::_const, variable_kind::_let, variable_kind::_var}) {
    const char declaration[] = "x";
    const char use[] = "x";

    error_collector v;
    linter l(&v);
    l.visit_variable_declaration(identifier_of(declaration), kind);
    l.visit_variable_use(identifier_of(use));
    l.visit_end_of_module();
    CHECK(v.errors.empty());
  }
}

TEST_CASE("variable use with no declaration") {
  const char use[] = "x";

  error_collector v;
  linter l(&v);
  l.visit_variable_use(identifier_of(use));
  l.visit_end_of_module();

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(v.errors[0].where.begin() == use);
}

TEST_CASE("variable use with declaration in different function") {
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

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(v.errors[0].where.begin() == use);
}

TEST_CASE("use global variable within functions") {
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

  CHECK(v.errors.empty());
}

TEST_CASE("function uses global variable declared later in module") {
  const char declaration[] = "x";
  const char use[] = "x";

  error_collector v;
  linter l(&v);
  l.visit_enter_function_scope();
  l.visit_variable_use(identifier_of(use));
  l.visit_exit_function_scope();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let);
  l.visit_end_of_module();

  CHECK(v.errors.empty());
}

TEST_CASE("assign to mutable variable") {
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

    CHECK(v.errors.empty());
  }
}

TEST_CASE("assign to immutable variable") {
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

    REQUIRE(v.errors.size() == 1);
    CHECK(v.errors[0].kind ==
          error_collector::error_assignment_to_const_variable);
    CHECK(v.errors[0].where.begin() == assignment);
    CHECK(v.errors[0].other_where.begin() == declaration);
    CHECK(v.errors[0].var_kind == kind);
  }
}
}  // namespace
}  // namespace quicklint_js
