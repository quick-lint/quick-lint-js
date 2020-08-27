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
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
spy_visitor parse_and_visit_statement(const char *raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

spy_visitor parse_and_visit_expression(const char *raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_expression(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

TEST(test_parse, parse_simple_let) {
  {
    spy_visitor v = parse_and_visit_statement("let x");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement("let a, b");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "a");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[1].name, "b");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement("let a, b, c, d, e, f, g");
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
  }

  {
    spy_visitor v;
    padded_string code("let first; let second");
    parser p(&code, &v);
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

TEST(test_parse, export_let) {
  {
    spy_visitor v = parse_and_visit_statement("export let x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
  }
}

TEST(test_parse, parse_simple_var) {
  spy_visitor v;
  padded_string code("var x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_simple_const) {
  spy_visitor v;
  padded_string code("const x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_let_with_initializers) {
  {
    spy_visitor v = parse_and_visit_statement("let x = 2");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
  }

  {
    spy_visitor v = parse_and_visit_statement("let x = 2, y = 3");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
  }

  {
    spy_visitor v = parse_and_visit_statement("let x = other, y = x");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "other");
    EXPECT_EQ(v.variable_uses[1].name, "x");
  }
}

TEST(test_parse, parse_let_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement("let {x} = 2");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
  }

  {
    spy_visitor v = parse_and_visit_statement("let {x, y, z} = 2");
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    EXPECT_EQ(v.variable_declarations[2].name, "z");
  }

  {
    spy_visitor v = parse_and_visit_statement("let {key: variable} = 2");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "variable", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement("let {} = x;");
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
  }

  {
    spy_visitor v = parse_and_visit_statement("let {key = defaultValue} = x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // defaultValue
                                      "visit_variable_declaration"));  // key
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "key", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"defaultValue"}));
  }
}

TEST(test_parse, parse_let_with_array_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement("let [first, second] = xs;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // x
                                      "visit_variable_declaration",    // first
                                      "visit_variable_declaration"));  // second
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        "first", variable_kind::_let},
                    spy_visitor::visited_variable_declaration{
                        "second", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"}));
  }
}

TEST(test_parse, parse_function_parameters_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement("function f({x, y, z}) {}");
    ASSERT_EQ(v.variable_declarations.size(), 4);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
    EXPECT_EQ(v.variable_declarations[1].name, "x");
    EXPECT_EQ(v.variable_declarations[2].name, "y");
    EXPECT_EQ(v.variable_declarations[3].name, "z");
  }

  {
    spy_visitor v = parse_and_visit_expression("({x, y, z}) => {}");
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "x");
    EXPECT_EQ(v.variable_declarations[1].name, "y");
    EXPECT_EQ(v.variable_declarations[2].name, "z");
  }
}

TEST(test_parse,
     variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  spy_visitor v;
  padded_string code("let x = x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);

  EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                    "visit_variable_declaration"));

  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, "x");
  ASSERT_EQ(v.variable_uses.size(), 1);
  EXPECT_EQ(v.variable_uses[0].name, "x");
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_invalid_let) {
  {
    spy_visitor v;
    padded_string code("let");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_let_with_no_bindings);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 0);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    spy_visitor v;
    padded_string code("let a,");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_stray_comma_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 5);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }

  {
    spy_visitor v;
    padded_string code("let x, 42");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 7);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 9);
  }

  {
    spy_visitor v;
    padded_string code("let if");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }

  {
    spy_visitor v;
    padded_string code("let 42");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_invalid_binding_in_let_statement);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 6);
  }
}

TEST(test_parse, parse_and_visit_import) {
  {
    spy_visitor v = parse_and_visit_statement("import fs from 'fs'");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v = parse_and_visit_statement("import * as fs from 'fs'");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v;
    padded_string code("import fs from 'fs'; import net from 'net';");
    parser p(&code, &v);
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
    spy_visitor v =
        parse_and_visit_statement("import { readFile, writeFile } from 'fs';");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "readFile");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_EQ(v.variable_declarations[1].name, "writeFile");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_import);
  }
}

TEST(test_parse, return_statement) {
  {
    spy_visitor v = parse_and_visit_statement("return a;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"a"}));
  }

  {
    spy_visitor v;
    padded_string code("return a\nreturn b");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"a"},
                            spy_visitor::visited_variable_use{"b"}));
  }

  {
    spy_visitor v;
    padded_string code("return a; return b;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"a"},
                            spy_visitor::visited_variable_use{"b"}));
  }

  {
    spy_visitor v;
    padded_string code("if (true) return; x;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
  }
}

TEST(test_parse, throw_statement) {
  {
    spy_visitor v = parse_and_visit_statement("throw new Error('ouch');");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"Error"}));
  }
}

TEST(test_parse, parse_math_expression) {
  for (const char *input :
       {"2", "2+2", "2^2", "2 + + 2", "2 * (3 + 4)", "1+1+1+1+1"}) {
    SCOPED_TRACE("input = " + std::string(input));
    spy_visitor v = parse_and_visit_expression(input);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression("some_var");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
  }

  {
    spy_visitor v = parse_and_visit_expression("some_var + some_other_var");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
    EXPECT_EQ(v.variable_uses[1].name, "some_other_var");
  }

  {
    spy_visitor v = parse_and_visit_expression("+ v");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "v");
  }
}

TEST(test_parse, parse_invalid_math_expression) {
  {
    spy_visitor v;
    padded_string code("2 +");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    spy_visitor v;
    padded_string code("^ 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 0);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 1);
  }

  {
    spy_visitor v;
    padded_string code("2 * * 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 3);
  }

  {
    spy_visitor v;
    padded_string code("2 & & & 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 2);

    auto *error = &v.errors[0];
    EXPECT_EQ(error->kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 2);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 3);

    error = &v.errors[1];
    EXPECT_EQ(error->kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 5);
  }

  {
    spy_visitor v;
    padded_string code("(2 *)");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_missing_operand_for_operator);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 3);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 4);
  }
  {
    spy_visitor v;
    padded_string code("2 * (3 + 4");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 5);
  }

  {
    spy_visitor v;
    padded_string code("2 * (3 + (4");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    ASSERT_EQ(v.errors.size(), 2);

    auto *error = &v.errors[0];
    EXPECT_EQ(error->kind, spy_visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 9);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 10);

    error = &v.errors[1];
    EXPECT_EQ(error->kind, spy_visitor::error_unmatched_parenthesis);
    EXPECT_EQ(p.locator().range(error->where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error->where).end_offset(), 5);
  }
}

TEST(test_parse, DISABLED_parse_invalid_math_expression_2) {
  {
    spy_visitor v;
    padded_string code("ten ten");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_unexpected_identifier);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 4);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 7);
  }
}

TEST(test_parse, parse_assignment) {
  {
    spy_visitor v = parse_and_visit_expression("x = y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, "x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression("(x) = y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, "x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression("x.p = y");

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "x");
    EXPECT_EQ(v.variable_uses[1].name, "y");

    EXPECT_EQ(v.variable_assignments.size(), 0);
  }

  {
    spy_visitor v = parse_and_visit_expression("x = y = z");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "z");

    EXPECT_EQ(v.variable_assignments.size(), 2);
    EXPECT_EQ(v.variable_assignments[0].name, "y");
    EXPECT_EQ(v.variable_assignments[1].name, "x");
  }

  {
    spy_visitor v = parse_and_visit_expression("xs[i] = j");
    EXPECT_THAT(v.variable_assignments, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"i"},   //
                            spy_visitor::visited_variable_use{"j"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{x: y} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{[x]: y} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{k1: {k2: x, k3: y}} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"},  //
                            spy_visitor::visited_variable_assignment{"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"z"}));
  }
}

TEST(test_parse, parse_compound_assignment) {
  {
    spy_visitor v = parse_and_visit_expression("x += y");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("x.p += y");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"y"}));
    EXPECT_THAT(v.variable_assignments, IsEmpty());
  }
}

TEST(test_parse, parse_plusplus_minusminus) {
  {
    spy_visitor v = parse_and_visit_expression("++x");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression("y--");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"y"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }
}

TEST(test_parse, parse_array_subscript) {
  {
    spy_visitor v = parse_and_visit_expression("array[index]");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"array"},
                            spy_visitor::visited_variable_use{"index"}));
  }
}

TEST(test_parse, array_literal) {
  {
    spy_visitor v = parse_and_visit_expression("[]");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression("[...elements]");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"elements"}));
  }
}

TEST(test_parse, object_literal) {
  {
    spy_visitor v = parse_and_visit_expression("{key: value}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{[key1 + key2]: value}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // key1
                            "visit_variable_use",    // key2
                            "visit_variable_use"));  // value
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"key1"},
                            spy_visitor::visited_variable_use{"key2"},
                            spy_visitor::visited_variable_use{"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{...other1, ...other2}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // other1
                            "visit_variable_use"));  // other2
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"other1"},
                            spy_visitor::visited_variable_use{"other2"}));
  }

  {
    spy_visitor v = parse_and_visit_expression("{func(a, b) { body; }}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",  // a
                                      "visit_variable_declaration",  // b
                                      "visit_variable_use",          // body
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, expression_statement) {
  {
    spy_visitor v = parse_and_visit_statement("console.log('hello');");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"console"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("this.x = xPos;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xPos"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("null;");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement("++x;");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("delete x;");
    // TODO(strager): Should this be visit_variable_assignment instead? Or
    // something else?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("void x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(R"("use strict";)");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, asi_plusplus_minusminus) {
  {
    spy_visitor v;
    padded_string code("x\n++\ny;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"y"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_use",  //
                            "visit_variable_assignment"));
  }
}

TEST(test_parse, asi_for_statement_at_right_curly) {
  {
    spy_visitor v;
    padded_string code(
        "function f() { console.log(\"hello\") } function g() { }");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        "f", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        "g", variable_kind::_function}));
  }
}

TEST(test_parse, asi_for_statement_at_newline) {
  {
    spy_visitor v;
    padded_string code("console.log('hello')\nconsole.log('world')\n");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"console"},
                            spy_visitor::visited_variable_use{"console"}));
  }

  {
    // This code should emit an error, but also use ASI for error recovery.
    spy_visitor v;
    padded_string code("console.log('hello') console.log('world');");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"console"},
                            spy_visitor::visited_variable_use{"console"}));

    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind,
              spy_visitor::error_missing_semicolon_after_expression);
    int end_of_first_expression = strlen("console.log('hello')");
    EXPECT_EQ(p.locator().range(error.where).begin_offset(),
              end_of_first_expression);
    EXPECT_EQ(p.locator().range(error.where).end_offset(),
              end_of_first_expression);
  }

  for (std::string variable_kind : {"const", "let", "var"}) {
    padded_string code(variable_kind + " a = 1\n" + variable_kind + " b = 2\n");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",    // a
                            "visit_variable_declaration"));  // b
  }
}

TEST(test_parse, asi_for_statement_at_end_of_file) {
  {
    spy_visitor v = parse_and_visit_statement("console.log(2+2)");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_function_calls) {
  {
    spy_visitor v = parse_and_visit_expression("f(x)");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "x");
  }

  {
    spy_visitor v = parse_and_visit_expression("f(x, y)");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_EQ(v.variable_uses[2].name, "y");
  }

  {
    spy_visitor v = parse_and_visit_expression("o.f(x, y)");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "o");
    EXPECT_EQ(v.variable_uses[1].name, "x");
    EXPECT_EQ(v.variable_uses[2].name, "y");
  }

  {
    spy_visitor v = parse_and_visit_expression("console.log('hello', 42)");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "console");
  }
}

TEST(test_parse, parse_templates_in_expressions) {
  {
    spy_visitor v = parse_and_visit_expression("`hello`");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression("`hello${world}`");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "world");
  }

  {
    spy_visitor v = parse_and_visit_expression("`${one}${two}${three}`");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "one");
    EXPECT_EQ(v.variable_uses[1].name, "two");
    EXPECT_EQ(v.variable_uses[2].name, "three");
  }

  {
    spy_visitor v = parse_and_visit_expression("`${2+2, four}`");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "four");
  }
}

TEST(test_parse, DISABLED_parse_invalid_function_calls) {
  {
    spy_visitor v;
    padded_string code("(x)f");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);

    ASSERT_EQ(v.errors.size(), 1);
    auto &error = v.errors[0];
    EXPECT_EQ(error.kind, spy_visitor::error_unexpected_identifier);
    EXPECT_EQ(p.locator().range(error.where).begin_offset(), 3);
    EXPECT_EQ(p.locator().range(error.where).end_offset(), 4);

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, "x");
    EXPECT_EQ(v.variable_uses[1].name, "f");
  }
}

TEST(test_parse, parse_function_call_as_statement) {
  {
    spy_visitor v;
    padded_string code("f(x); g(y);");
    parser p(&code, &v);

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
    spy_visitor v = parse_and_visit_expression("some_var.some_property");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "some_var");
  }
}

TEST(test_parse, parse_new_expression) {
  {
    spy_visitor v = parse_and_visit_expression("new Foo()");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "Foo");
  }
}

TEST(test_parse, parse_await_expression) {
  {
    spy_visitor v = parse_and_visit_expression("await myPromise");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "myPromise");
  }
}

TEST(test_parse, super_in_class) {
  {
    spy_visitor v = parse_and_visit_statement(
        "class C extends Base { constructor() { super(); } }");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, conditional_expression) {
  {
    spy_visitor v = parse_and_visit_expression("x ? y : z");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"},  //
                            spy_visitor::visited_variable_use{"y"},  //
                            spy_visitor::visited_variable_use{"z"}));
  }
}

TEST(test_parse, parse_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement("function foo() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement("export function foo() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement("function sin(theta) {}");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "sin");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, "theta");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("function pow(base, exponent) {}");

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "pow");
    EXPECT_EQ(v.variable_declarations[1].name, "base");
    EXPECT_EQ(v.variable_declarations[2].name, "exponent");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  //
                                      "visit_variable_declaration",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("function f(x, y = x) {}");

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
    EXPECT_EQ(v.variable_declarations[1].name, "x");
    EXPECT_EQ(v.variable_declarations[2].name, "y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("function f() { return x; }");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "f");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_enter_function_scope",  //
                                      "visit_variable_use",          // x
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("function g(first, ...args) {}");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        "g", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        "first", variable_kind::_parameter},
                    spy_visitor::visited_variable_declaration{
                        "args", variable_kind::_parameter}));
  }
}

TEST(test_parse, parse_async_function) {
  {
    spy_visitor v = parse_and_visit_statement("async function f() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
  }

  {
    spy_visitor v =
        parse_and_visit_statement("async function f() { await null; }");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "f");
  }
}

TEST(test_parse, parse_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement("(function() {});");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("(function(x, y) {});");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",  //
                            "visit_variable_declaration",  //
                            "visit_variable_declaration",  //
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("(function() {let x = y;});");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",  //
                            "visit_variable_use",          //
                            "visit_variable_declaration",  //
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("(a, function(b) {c;}(d));");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",          //
                            "visit_enter_function_scope",  //
                            "visit_variable_declaration",  //
                            "visit_variable_use",          //
                            "visit_exit_function_scope",   //
                            "visit_variable_use"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "b", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"a"},
                            spy_visitor::visited_variable_use{"c"},
                            spy_visitor::visited_variable_use{"d"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("(function recur() { recur(); })();");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_named_function_scope",  //
                            "visit_variable_use",                //
                            "visit_exit_function_scope"));
    EXPECT_THAT(
        v.enter_named_function_scopes,
        ElementsAre(spy_visitor::visited_enter_named_function_scope{"recur"}));
  }
}

TEST(test_parse, arrow_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement("(() => x);");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_use",          //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("(x => y);");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"y"}));
  }
}

TEST(test_parse, arrow_function_expression_with_statements) {
  {
    spy_visitor v = parse_and_visit_statement("(() => { x; });");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_use",          //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("(x => { y; });");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"y"}));
  }
}

TEST(test_parse, parse_empty_module) {
  spy_visitor v;
  padded_string code("");
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, IsEmpty());
  EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
}

TEST(test_parse, parse_class_statement) {
  {
    spy_visitor v = parse_and_visit_statement("class C {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("export class C {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    spy_visitor v = parse_and_visit_statement("class Derived extends Base {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "Derived");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "Base");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("class FileStream extends fs.ReadStream {}");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, "fs");
  }

  {
    spy_visitor v = parse_and_visit_statement(
        "class Monster { eatMuffins(muffinCount) { } }");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, "Monster");
    EXPECT_EQ(v.variable_declarations[1].name, "muffinCount");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, "eatMuffins");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_property_declaration",  //
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  //
                                      "visit_exit_function_scope",   //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("class C { static m() { } }");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, "m");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_property_declaration",  //
                                      "visit_enter_function_scope",  //
                                      "visit_exit_function_scope",   //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("class C { async m() { } }");
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("class C { static async m() { } }");
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{"m"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("class C { get length() { } }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{"length"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("class C { a(){} b(){} c(){} }");
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, "a");
    EXPECT_EQ(v.property_declarations[1].name, "b");
    EXPECT_EQ(v.property_declarations[2].name, "c");
  }

  {
    spy_visitor v;
    padded_string code("class A {} class B {}");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        "A", variable_kind::_class},
                    spy_visitor::visited_variable_declaration{
                        "B", variable_kind::_class}));
  }
}

TEST(test_parse, parse_and_visit_try) {
  {
    spy_visitor v = parse_and_visit_statement("try {} finally {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("try {} catch (e) {}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement("try {} catch (e) {} finally {}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, "e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v =
        parse_and_visit_statement("try {f();} catch (e) {g();} finally {h();}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, "f");
    EXPECT_EQ(v.variable_uses[1].name, "g");
    EXPECT_EQ(v.variable_uses[2].name, "h");
  }
}

TEST(test_parse, if_without_else) {
  {
    spy_visitor v = parse_and_visit_statement("if (a) { b; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("if (a) b;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_with_else) {
  {
    spy_visitor v = parse_and_visit_statement("if (a) { b; } else { c; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("if (a) b; else c;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, do_while) {
  {
    spy_visitor v = parse_and_visit_statement("do { a; } while (b)");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, c_style_for_loop) {
  {
    spy_visitor v = parse_and_visit_statement("for (;;) { a; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("for (init; cond; after) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"init"},  //
                            spy_visitor::visited_variable_use{"cond"},  //
                            spy_visitor::visited_variable_use{"body"},  //
                            spy_visitor::visited_variable_use{"after"}));
  }

  for (const char *variable_kind : {"const", "let"}) {
    SCOPED_TRACE(variable_kind);
    std::string code =
        std::string("for (") + variable_kind + " i = 0; cond; after) { body; }";
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_declaration",  //
                                      "visit_variable_use",          //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use",          //
                                      "visit_exit_for_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("for (var i = 0; ; ) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("for (i = 0, j = 0; ; ) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_assignment",  // i
                                      "visit_variable_assignment",  // j
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, for_in_loop) {
  {
    spy_visitor v = parse_and_visit_statement("for (x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("for (let x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("for (var x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }
}

TEST(test_parse, for_of_loop) {
  {
    spy_visitor v = parse_and_visit_statement("for (x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("for (let x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement("for (var x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    "x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"xs"},  //
                            spy_visitor::visited_variable_use{"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("for await (let x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
  }
}

TEST(test_parse, block_statement) {
  {
    spy_visitor v = parse_and_visit_statement("{ }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("{ first; second; third; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{"first"},   //
                            spy_visitor::visited_variable_use{"second"},  //
                            spy_visitor::visited_variable_use{"third"}));
  }
}

TEST(test_parse, switch_statement) {
  {
    spy_visitor v = parse_and_visit_statement("switch (x) {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("switch (true) {case y:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // y
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement("switch (true) {default:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        "switch (true) {case x: case y: default: case z:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // x
                                      "visit_variable_use",       // y
                                      "visit_variable_use",       // z
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement("switch (true) { case true: x; let y; z; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_variable_use",          // z
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, while_statement) {
  {
    spy_visitor v = parse_and_visit_statement("while (cond) body;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement("while (cond) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, break_statement) {
  {
    spy_visitor v = parse_and_visit_statement("break;");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, continue_statement) {
  {
    spy_visitor v = parse_and_visit_statement("continue;");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}
}  // namespace
}  // namespace quick_lint_js
