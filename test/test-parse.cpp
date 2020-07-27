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

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/parse.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
struct visitor : public error_collector {
  std::vector<const char *> visits;

  void visit_end_of_module() {
    this->visits.emplace_back("visit_end_of_module");
  }

  void visit_enter_block_scope() {
    this->visits.emplace_back("visit_enter_block_scope");
  }

  void visit_enter_class_scope() {
    this->visits.emplace_back("visit_enter_class_scope");
  }

  void visit_enter_function_scope() {
    this->visits.emplace_back("visit_enter_function_scope");
  }

  void visit_exit_block_scope() {
    this->visits.emplace_back("visit_exit_block_scope");
  }

  void visit_exit_class_scope() {
    this->visits.emplace_back("visit_exit_class_scope");
  }

  void visit_exit_function_scope() {
    this->visits.emplace_back("visit_exit_function_scope");
  }

  void visit_property_declaration(identifier name) {
    this->property_declarations.emplace_back(
        visited_property_declaration{std::string(name.string_view())});
    this->visits.emplace_back("visit_property_declaration");
  }

  struct visited_property_declaration {
    std::string name;
  };
  std::vector<visited_property_declaration> property_declarations;

  void visit_variable_assignment(identifier name) {
    this->variable_assignments.emplace_back(
        visited_variable_assignment{std::string(name.string_view())});
    this->visits.emplace_back("visit_variable_assignment");
  }

  struct visited_variable_assignment {
    std::string name;
  };
  std::vector<visited_variable_assignment> variable_assignments;

  void visit_variable_declaration(identifier name, variable_kind kind) {
    this->variable_declarations.emplace_back(
        visited_variable_declaration{std::string(name.string_view()), kind});
    this->visits.emplace_back("visit_variable_declaration");
  }

  struct visited_variable_declaration {
    std::string name;
    variable_kind kind;
  };
  std::vector<visited_variable_declaration> variable_declarations;

  void visit_variable_use(identifier name) {
    this->variable_uses.emplace_back(
        visited_variable_use{std::string(name.string_view())});
    this->visits.emplace_back("visit_variable_use");
  }

  struct visited_variable_use {
    std::string name;
  };
  std::vector<visited_variable_use> variable_uses;
};

TEST(test_parse, parse_simple_let) {
  {
    visitor v;
    parser p("let x", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let a, b", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "a");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[1].name, "b");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_let);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let a, b, c, d, e, f, g", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 7);
    EXPECT_EQ(v.variable_declarations[0].name, "a");
    EXPECT_EQ(v.variable_declarations[1].name, "b");
    EXPECT_EQ(v.variable_declarations[2].name, "c");
    EXPECT_EQ(v.variable_declarations[3].name, "d");
    EXPECT_EQ(v.variable_declarations[4].name, "e");
    EXPECT_EQ(v.variable_declarations[5].name, "f");
    EXPECT_EQ(v.variable_declarations[6].name, "g");
    for (const auto &declaration : v.variable_declarations) {
      EXPECT_EQ(declaration.kind, variable_kind::_let);
    }
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let first; let second", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "first");
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "first");
    EXPECT_EQ(v.variable_declarations[1].name, "second");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_simple_var) {
  visitor v;
  parser p("var x", &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_simple_const) {
  visitor v;
  parser p("const x", &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_let_with_initializers) {
  {
    visitor v;
    parser p("let x = 2", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let x = 2, y = 3", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let x = other, y = x", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "other");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_let_with_object_destructuring) {
  {
    visitor v;
    parser p("let {x} = 2", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let {x, y, z} = 2", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    EXPECT_EQ(v.variable_declarations[2].name, "z");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let {{x}, {y}, {z}} = 2", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    EXPECT_EQ(v.variable_declarations[2].name, "z");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("let {} = x;", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_function_parameters_with_object_destructuring) {
  visitor v;
  parser p("function f({x, y, z}) {}", &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 4);
  EXPECT_EQ(v.variable_declarations[0].name, "f");
  EXPECT_EQ(v.variable_declarations[1].name, "x");
  EXPECT_EQ(v.variable_declarations[2].name, "y");
  EXPECT_EQ(v.variable_declarations[3].name, "z");
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse,
     variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  visitor v;
  parser p("let x = x", &v);
  p.parse_and_visit_statement(v);

  ASSERT_EQ(v.visits.size(), 2);
  EXPECT_EQ(v.visits[0], "visit_variable_use");
  EXPECT_EQ(v.visits[1], "visit_variable_declaration");

  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  ASSERT_EQ(v.variable_uses.size(), 1);
  EXPECT_EQ(v.variable_uses[0].name, "x");
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_invalid_let) {
  {
    visitor v;
    parser p("let", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_let_with_no_bindings);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 0);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    visitor v;
    parser p("let a,", &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_stray_comma_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 5);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }

  {
    visitor v;
    parser p("let x, 42", &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 7);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 9);
  }

  {
    visitor v;
    parser p("let if", &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }

  {
    visitor v;
    parser p("let 42", &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }
}

TEST(test_parse, parse_and_visit_import) {
  {
    visitor v;
    parser p("import fs from 'fs'", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("import * as fs from 'fs'", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("import fs from 'fs'; import net from 'net';", &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_EQ(v.variable_declarations[1].name, "net");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_import);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("import { readFile, writeFile } from 'fs';", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "readFile");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_EQ(v.variable_declarations[1].name, "writeFile");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_import);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_math_expression) {
  for (const char *input :
       {"2", "2+2", "2^2", "2 + + 2", "2 * (3 + 4)", "1+1+1+1+1"}) {
    SCOPED_TRACE("input = " + std::string(input));
    visitor v;
    parser p(input, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("some_var", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("some_var + some_other_var", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
    EXPECT_EQ(v.variable_uses[1].name, "some_other_var");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("+ v", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "v");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_invalid_math_expression) {
  {
    visitor v;
    parser p("2 +", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    visitor v;
    parser p("^ 2", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 0);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 1);
  }

  {
    visitor v;
    parser p("2 * * 2", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    visitor v;
    parser p("2 & & & 2", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 2);

    auto *error = &v.errors[0];
    EXPECT_EQ(error->kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 3);

    error = &v.errors[1];
    EXPECT_EQ(error->kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 5);
  }

  {
    visitor v;
    parser p("(2 *)", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 3);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 4);
  }
  {
    visitor v;
    parser p("2 * (3 + 4", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 5);
  }

  {
    visitor v;
    parser p("2 * (3 + (4", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 2);

    auto *error = &v.errors[0];
    EXPECT_EQ(error->kind, visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 9);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 10);

    error = &v.errors[1];
    EXPECT_EQ(error->kind, visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 5);
  }
}

TEST(test_parse, DISABLED_parse_invalid_math_expression_2) {
  {
    visitor v;
    parser p("ten ten", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_unexpected_identifier);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 7);
  }
}

TEST(test_parse, parse_assignment) {
  {
    visitor v;
    parser p("x = y", &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, "x");

    ASSERT_EQ(v.visits.size(), 2);
    EXPECT_EQ(v.visits[0], "visit_variable_use");
    EXPECT_EQ(v.visits[1], "visit_variable_assignment");
  }

  {
    visitor v;
    parser p("(x) = y", &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, "x");

    ASSERT_EQ(v.visits.size(), 2);
    EXPECT_EQ(v.visits[0], "visit_variable_use");
    EXPECT_EQ(v.visits[1], "visit_variable_assignment");
  }

  {
    visitor v;
    parser p("x.p = y", &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "x");
    EXPECT_EQ(v.variable_uses[1].name, "y");

    EXPECT_EQ(v.variable_assignments.size(), 0);
  }

  {
    visitor v;
    parser p("x = y = z", &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "z");

    EXPECT_EQ(v.variable_assignments.size(), 2);
    EXPECT_EQ(v.variable_assignments[0].name, "y");
    EXPECT_EQ(v.variable_assignments[1].name, "x");
  }
}

TEST(test_parse, parse_function_calls) {
  {
    visitor v;
    parser p("f(x)", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("f(x, y)", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_EQ(v.variable_uses[2].name, "y");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("o.f(x, y)", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "o");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_EQ(v.variable_uses[2].name, "y");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("console.log('hello', 42)", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "console");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_templates_in_expressions) {
  {
    visitor v;
    parser p("`hello`", &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("`hello${world}`", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "world");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("`${one}${two}${three}`", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "one");
    EXPECT_EQ(v.variable_uses[1].name, "two");
    EXPECT_EQ(v.variable_uses[2].name, "three");
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("`${2+2, four}`", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "four");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, DISABLED_parse_invalid_function_calls) {
  {
    visitor v;
    parser p("(x)f", &v);
    p.parse_and_visit_statement(v);

    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, visitor::error_unexpected_identifier);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 3);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 4);

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "x");
    EXPECT_EQ(v.variable_uses[1].name, "f");
  }
}

TEST(test_parse, parse_function_call_as_statement) {
  {
    visitor v;
    parser p("f(x); g(y);", &v);

    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "x");

    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_uses.size(), 4);
    EXPECT_EQ(v.variable_uses[2].name, "g");
    EXPECT_EQ(v.variable_uses[3].name, "y");

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_property_lookup) {
  {
    visitor v;
    parser p("some_var.some_property", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_new_expression) {
  {
    visitor v;
    parser p("new Foo()", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "Foo");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_await_expression) {
  {
    visitor v;
    parser p("await myPromise", &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "myPromise");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_function_statement) {
  {
    visitor v;
    parser p("function foo() {}", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("export function foo() {}", &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    visitor v;
    parser p("function sin(theta) {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "sin");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, "theta");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);

    ASSERT_EQ(v.visits.size(), 4);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");
    EXPECT_EQ(v.visits[1], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[2], "visit_variable_declaration");
    EXPECT_EQ(v.visits[3], "visit_exit_function_scope");
  }

  {
    visitor v;
    parser p("function pow(base, exponent) {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "pow");
    EXPECT_EQ(v.variable_declarations[1].name, "base");
    EXPECT_EQ(v.variable_declarations[2].name, "exponent");

    ASSERT_EQ(v.visits.size(), 5);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");
    EXPECT_EQ(v.visits[1], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[2], "visit_variable_declaration");
    EXPECT_EQ(v.visits[3], "visit_variable_declaration");
    EXPECT_EQ(v.visits[4], "visit_exit_function_scope");
  }

  {
    visitor v;
    parser p("function f(x, y = x) {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
    EXPECT_EQ(v.variable_declarations[1].name, "x");
    EXPECT_EQ(v.variable_declarations[2].name, "y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "x");

    ASSERT_EQ(v.visits.size(), 6);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");  // f
    EXPECT_EQ(v.visits[1], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[2], "visit_variable_declaration");  // x
    EXPECT_EQ(v.visits[3], "visit_variable_use");          // x
    EXPECT_EQ(v.visits[4], "visit_variable_declaration");  // y
    EXPECT_EQ(v.visits[5], "visit_exit_function_scope");
  }

  {
    visitor v;
    parser p("function f() { return x; }", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "f");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "x");

    ASSERT_EQ(v.visits.size(), 4);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");  // f
    EXPECT_EQ(v.visits[1], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[2], "visit_variable_use");  // x
    EXPECT_EQ(v.visits[3], "visit_exit_function_scope");
  }
}

TEST(test_parse, parse_async_function) {
  {
    visitor v;
    parser p("async function f() {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
  }
}

TEST(test_parse, parse_empty_module) {
  visitor v;
  parser p("", &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, IsEmpty());

  ASSERT_EQ(v.visits.size(), 1);
  EXPECT_EQ(v.visits[0], "visit_end_of_module");
}

TEST(test_parse, parse_class_statement) {
  {
    visitor v;
    parser p("class C {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    ASSERT_EQ(v.visits.size(), 3);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");
    EXPECT_EQ(v.visits[1], "visit_enter_class_scope");
    EXPECT_EQ(v.visits[2], "visit_exit_class_scope");
  }

  {
    visitor v;
    parser p("export class C {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    visitor v;
    parser p("class Derived extends Base {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "Derived");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "Base");

    ASSERT_EQ(v.visits.size(), 4);
    EXPECT_EQ(v.visits[0], "visit_variable_use");
    EXPECT_EQ(v.visits[1], "visit_variable_declaration");
    EXPECT_EQ(v.visits[2], "visit_enter_class_scope");
    EXPECT_EQ(v.visits[3], "visit_exit_class_scope");
  }

  {
    visitor v;
    parser p("class FileStream extends fs.ReadStream {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "fs");
  }

  {
    visitor v;
    parser p("class Monster { eatMuffins(muffinCount) { } }", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "Monster");
    EXPECT_EQ(v.variable_declarations[1].name, "muffinCount");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, "eatMuffins");

    ASSERT_EQ(v.visits.size(), 7);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");
    EXPECT_EQ(v.visits[1], "visit_enter_class_scope");
    EXPECT_EQ(v.visits[2], "visit_property_declaration");
    EXPECT_EQ(v.visits[3], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[4], "visit_variable_declaration");
    EXPECT_EQ(v.visits[5], "visit_exit_function_scope");
    EXPECT_EQ(v.visits[6], "visit_exit_class_scope");
  }

  {
    visitor v;
    parser p("class C { static m() { } }", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, "m");

    ASSERT_EQ(v.visits.size(), 6);
    EXPECT_EQ(v.visits[0], "visit_variable_declaration");
    EXPECT_EQ(v.visits[1], "visit_enter_class_scope");
    EXPECT_EQ(v.visits[2], "visit_property_declaration");
    EXPECT_EQ(v.visits[3], "visit_enter_function_scope");
    EXPECT_EQ(v.visits[4], "visit_exit_function_scope");
    EXPECT_EQ(v.visits[5], "visit_exit_class_scope");
  }

  {
    visitor v;
    parser p("class C { a(){} b(){} c(){} }", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, "a");
    EXPECT_EQ(v.property_declarations[1].name, "b");
    EXPECT_EQ(v.property_declarations[2].name, "c");
  }
}

TEST(test_parse, parse_and_visit_try) {
  {
    visitor v;
    parser p("try {} finally {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.visits.size(), 4);
    EXPECT_EQ(v.visits[0], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[1], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[2], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[3], "visit_exit_block_scope");
  }

  {
    visitor v;
    parser p("try {} catch (e) {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.visits.size(), 5);
    EXPECT_EQ(v.visits[0], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[1], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[2], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[3], "visit_variable_declaration");
    EXPECT_EQ(v.visits[4], "visit_exit_block_scope");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    visitor v;
    parser p("try {} catch (e) {} finally {}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.visits.size(), 7);
    EXPECT_EQ(v.visits[0], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[1], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[2], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[3], "visit_variable_declaration");
    EXPECT_EQ(v.visits[4], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[5], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[6], "visit_exit_block_scope");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    visitor v;
    parser p("try {f();} catch (e) {g();} finally {h();}", &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    ASSERT_EQ(v.visits.size(), 10);
    EXPECT_EQ(v.visits[0], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[1], "visit_variable_use");
    EXPECT_EQ(v.visits[2], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[3], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[4], "visit_variable_declaration");
    EXPECT_EQ(v.visits[5], "visit_variable_use");
    EXPECT_EQ(v.visits[6], "visit_exit_block_scope");
    EXPECT_EQ(v.visits[7], "visit_enter_block_scope");
    EXPECT_EQ(v.visits[8], "visit_variable_use");
    EXPECT_EQ(v.visits[9], "visit_exit_block_scope");

    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "g");
    EXPECT_EQ(v.variable_uses[2].name, "h");
  }
}
}  // namespace
}  // namespace quick_lint_js
