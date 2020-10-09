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
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
spy_visitor parse_and_visit_statement(const char8 *raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

spy_visitor parse_and_visit_expression(const char8 *raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_expression(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

TEST(test_parse, parse_simple_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"a");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[1].name, u8"b");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b, c, d, e, f, g");
    ASSERT_EQ(v.variable_declarations.size(), 7);
    EXPECT_EQ(v.variable_declarations[0].name, u8"a");
    EXPECT_EQ(v.variable_declarations[1].name, u8"b");
    EXPECT_EQ(v.variable_declarations[2].name, u8"c");
    EXPECT_EQ(v.variable_declarations[3].name, u8"d");
    EXPECT_EQ(v.variable_declarations[4].name, u8"e");
    EXPECT_EQ(v.variable_declarations[5].name, u8"f");
    EXPECT_EQ(v.variable_declarations[6].name, u8"g");
    for (const auto &declaration : v.variable_declarations) {
      EXPECT_EQ(declaration.kind, variable_kind::_let);
    }
  }

  {
    spy_visitor v;
    padded_string code(u8"let first; let second");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"first");
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"first");
    EXPECT_EQ(v.variable_declarations[1].name, u8"second");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, export_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export let x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
  }
}

TEST(test_parse, export_default) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export default x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default function f() {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export default function() {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default async function f() {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default async function() {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default (function f() {})");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export default class C {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export default class {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
  }
}

TEST(test_parse, export_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export {one, two};");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, export_from) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export * from 'other';");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export {} from 'other';");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"export {util1, util2, util3} from 'other';");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"export {readFileSync as readFile} from 'fs';");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export {promises as default} from 'fs';");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export {default} from 'other';");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, parse_simple_var) {
  spy_visitor v;
  padded_string code(u8"var x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_simple_const) {
  spy_visitor v;
  padded_string code(u8"const x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_let_with_initializers) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2, y = 3");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = other, y = x");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"other");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = y in z;");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");
    EXPECT_EQ(v.variable_uses[1].name, u8"z");
  }
}

TEST(test_parse, parse_let_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let {x} = 2");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {x, y, z} = 2");
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[2].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {key: variable} = 2");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"variable", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {} = x;");
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"let {key = defaultValue} = x;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // defaultValue
                                      "visit_variable_declaration"));  // key
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"key", variable_kind::_let}));
    EXPECT_THAT(
        v.variable_uses,
        ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                    spy_visitor::visited_variable_use{u8"defaultValue"}));
  }
}

TEST(test_parse, parse_let_with_array_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let [first, second] = xs;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // x
                                      "visit_variable_declaration",    // first
                                      "visit_variable_declaration"));  // second
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"first", variable_kind::_let},
                    spy_visitor::visited_variable_declaration{
                        u8"second", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
  }
}

TEST(test_parse, parse_function_parameters_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function f({x, y, z}) {}");
    ASSERT_EQ(v.variable_declarations.size(), 4);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
    EXPECT_EQ(v.variable_declarations[1].name, u8"x");
    EXPECT_EQ(v.variable_declarations[2].name, u8"y");
    EXPECT_EQ(v.variable_declarations[3].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"({x, y, z}) => {}");
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[2].name, u8"z");
  }
}

TEST(test_parse,
     variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  spy_visitor v;
  padded_string code(u8"let x = x");
  parser p(&code, &v);
  p.parse_and_visit_statement(v);

  EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                    "visit_variable_declaration"));

  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  ASSERT_EQ(v.variable_uses.size(), 1);
  EXPECT_EQ(v.variable_uses[0].name, u8"x");
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_invalid_let) {
  {
    spy_visitor v;
    padded_string code(u8"let");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_let_with_no_bindings, where,
                                             offsets_matcher(&code, 0, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let a,");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_stray_comma_in_let_statement, where,
                              offsets_matcher(&code, 5, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x, 42");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 7, 9))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let if");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let 42");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let debugger");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 12))));
  }
}

TEST(test_parse, parse_and_visit_import) {
  {
    spy_visitor v = parse_and_visit_statement(u8"import fs from 'fs'");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"import * as fs from 'fs'");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v;
    padded_string code(u8"import fs from 'fs'; import net from 'net';");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_EQ(v.variable_declarations[1].name, u8"net");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_import);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"import { readFile, writeFile } from 'fs';");
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"readFile");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
    EXPECT_EQ(v.variable_declarations[1].name, u8"writeFile");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_import);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"import {readFileSync as rf} from 'fs';");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"rf");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }
}

TEST(test_parse, return_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"return a;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"return a\nreturn b");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"b"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"return a; return b;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"b"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (true) return; x;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"if (true) { return } else { other }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_exit_block_scope",   //
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // other
                            "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other"}));
  }
}

TEST(test_parse, throw_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"throw new Error('ouch');");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Error"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_expected_expression_before_semicolon, where,
                              offsets_matcher(&code, 5, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw\nnew Error();");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_expected_expression_before_newline, where,
                              offsets_matcher(&code, 5, 5))));
  }
}

TEST(test_parse, parse_math_expression) {
  for (const char8 *input :
       {u8"2", u8"2+2", u8"2^2", u8"2 + + 2", u8"2 * (3 + 4)", u8"1+1+1+1+1"}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    spy_visitor v = parse_and_visit_expression(input);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"some_var");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"some_var + some_other_var");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
    EXPECT_EQ(v.variable_uses[1].name, u8"some_other_var");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"+ v");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"v");
  }
}

TEST(test_parse, parse_invalid_math_expression) {
  {
    spy_visitor v;
    padded_string code(u8"2 +");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 2, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"^ 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, 1))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * * 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 2, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 & & & 2");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                                     offsets_matcher(&code, 2, 3)),
                    ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                                     offsets_matcher(&code, 4, 5))));
  }

  {
    spy_visitor v;
    padded_string code(u8"(2 *)");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 3, 4))));
  }
  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + 4");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 4, 5))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + (4");
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 9, 10)),
                            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 4, 5))));
  }
}

TEST(test_parse, DISABLED_parse_invalid_math_expression_2) {
  {
    spy_visitor v;
    padded_string code(u8"ten ten");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unexpected_identifier, where,
                                             offsets_matcher(&code, 4, 7))));
  }
}

TEST(test_parse, parse_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x = y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"(x) = y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p = y");

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");

    EXPECT_EQ(v.variable_assignments.size(), 0);
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x = y = z");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"z");

    EXPECT_EQ(v.variable_assignments.size(), 2);
    EXPECT_EQ(v.variable_assignments[0].name, u8"y");
    EXPECT_EQ(v.variable_assignments[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"xs[i] = j");
    EXPECT_THAT(v.variable_assignments, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"i"},   //
                            spy_visitor::visited_variable_use{u8"j"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{x: y} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[x]: y} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{k1: {k2: x, k3: y}} = z");
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"},  //
                            spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, parse_compound_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x += y");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p += y");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments, IsEmpty());
  }
}

TEST(test_parse, parse_plusplus_minusminus) {
  {
    spy_visitor v = parse_and_visit_expression(u8"++x");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"y--");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }
}

TEST(test_parse, parse_typeof_with_just_variable) {
  {
    spy_visitor v = parse_and_visit_expression(u8"typeof x");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"typeof(x)");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_typeof_with_non_variable) {
  {
    spy_visitor v = parse_and_visit_expression(u8"typeof x.prop");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_array_subscript) {
  {
    spy_visitor v = parse_and_visit_expression(u8"array[index]");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"array"},
                            spy_visitor::visited_variable_use{u8"index"}));
  }
}

TEST(test_parse, array_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"[]");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"[...elements]");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"elements"}));
  }
}

TEST(test_parse, object_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"{key: value}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[key1 + key2]: value}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // key1
                            "visit_variable_use",    // key2
                            "visit_variable_use"));  // value
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"key1"},
                            spy_visitor::visited_variable_use{u8"key2"},
                            spy_visitor::visited_variable_use{u8"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{...other1, ...other2}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // other1
                            "visit_variable_use"));  // other2
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other1"},
                            spy_visitor::visited_variable_use{u8"other2"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{func(a, b) { body; }}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // a
                                      "visit_variable_declaration",       // b
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",  // body
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, expression_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"console.log('hello');");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"this.x = xPos;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xPos"}));
  }

  for (string8 literal : {u8"null", u8"true", u8"false"}) {
    SCOPED_TRACE(out_string8(literal));
    spy_visitor v = parse_and_visit_statement((literal + u8";").c_str());
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"++x;");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"new C();");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"C"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"delete x;");
    // TODO(strager): Should this be visit_variable_assignment instead? Or
    // something else?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8R"("use strict";)");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"`hello`");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"`hello ${world}`");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // world
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"42");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"import(url).then(); secondStatement;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // url
                            "visit_variable_use"));  // secondStatement
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"typeof x");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"[x, y, z];");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // x
                            "visit_variable_use",    // y
                            "visit_variable_use"));  // z
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/regexp/;");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/=regexp/;");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  for (string8 op : {u8"void ", u8"!", u8"~", u8"+", u8"-"}) {
    string8 code = op + u8" x;";
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, asi_plusplus_minusminus) {
  {
    spy_visitor v;
    padded_string code(u8"x\n++\ny;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
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
        u8"function f() { console.log(\"hello\") } function g() { }");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"f", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        u8"g", variable_kind::_function}));
  }
}

TEST(test_parse, asi_for_statement_at_newline) {
  {
    spy_visitor v;
    padded_string code(u8"console.log('hello')\nconsole.log('world')\n");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x = 2\nfor (;;) { console.log(); }");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // This code should emit an error, but also use ASI for error recovery.
    spy_visitor v;
    padded_string code(u8"console.log('hello') console.log('world');");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
    source_position::offset_type end_of_first_expression =
        strlen(u8"console.log('hello')");
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_expression, where,
                              offsets_matcher(&code, end_of_first_expression,
                                              end_of_first_expression))));
  }

  for (string8 variable_kind : {u8"const", u8"let", u8"var"}) {
    padded_string code(variable_kind + u8" a = 1\n" + variable_kind +
                       u8" b = 2\n");
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
    spy_visitor v = parse_and_visit_statement(u8"console.log(2+2)");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_function_calls) {
  {
    spy_visitor v = parse_and_visit_expression(u8"f(x)");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"f(x, y)");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"o.f(x, y)");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"o");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"console.log('hello', 42)");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"console");
  }
}

TEST(test_parse, parse_templates_in_expressions) {
  {
    spy_visitor v = parse_and_visit_expression(u8"`hello`");
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`hello${world}`");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"world");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${one}${two}${three}`");
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"one");
    EXPECT_EQ(v.variable_uses[1].name, u8"two");
    EXPECT_EQ(v.variable_uses[2].name, u8"three");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${2+2, four}`");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"four");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"tag`${inside}`");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"tag");
    EXPECT_EQ(v.variable_uses[1].name, u8"inside");
  }
}

TEST(test_parse, DISABLED_parse_invalid_function_calls) {
  {
    spy_visitor v;
    padded_string code(u8"(x)f");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unexpected_identifier, where,
                                             offsets_matcher(&code, 3, 4))));

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"f");
  }
}

TEST(test_parse, parse_function_call_as_statement) {
  {
    spy_visitor v;
    padded_string code(u8"f(x); g(y);");
    parser p(&code, &v);

    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");

    p.parse_and_visit_statement(v);
    ASSERT_EQ(v.variable_uses.size(), 4);
    EXPECT_EQ(v.variable_uses[2].name, u8"g");
    EXPECT_EQ(v.variable_uses[3].name, u8"y");

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_property_lookup) {
  {
    spy_visitor v = parse_and_visit_expression(u8"some_var.some_property");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }
}

TEST(test_parse, parse_new_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"new Foo()");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"Foo");
  }
}

TEST(test_parse, parse_await_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"await myPromise");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"myPromise");
  }
}

TEST(test_parse, super_in_class) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C extends Base { constructor() { super(); } }");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, conditional_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x ? y : z");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, parse_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function foo() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export function foo() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function sin(theta) {}");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"sin");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, u8"theta");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // sin
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // theta
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"function pow(base, exponent) {}");

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"pow");
    EXPECT_EQ(v.variable_declarations[1].name, u8"base");
    EXPECT_EQ(v.variable_declarations[2].name, u8"exponent");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // pow
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // base
                                      "visit_variable_declaration",  // exponent
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function f(x, y = x) {}");

    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
    EXPECT_EQ(v.variable_declarations[1].name, u8"x");
    EXPECT_EQ(v.variable_declarations[2].name, u8"y");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_variable_use",               // x
                                      "visit_variable_declaration",       // y
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function f() { return x; }");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"function g(first, ...args) {}");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"g", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        u8"first", variable_kind::_parameter},
                    spy_visitor::visited_variable_declaration{
                        u8"args", variable_kind::_parameter}));
  }
}

TEST(test_parse, parse_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"async function f() {}");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"async function f() { await null; }");
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }
}

TEST(test_parse, parse_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(function() {});");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(function(x, y) {});");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // x
                            "visit_variable_declaration",       // y
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(function() {let x = y;});");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // y
                            "visit_variable_declaration",       // x
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(a, function(b) {c;}(d));");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",               // a
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       // b
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // c
                            "visit_exit_function_scope",        //
                            "visit_variable_use"));             // d
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"b", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"c"},
                            spy_visitor::visited_variable_use{u8"d"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function recur() { recur(); })();");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_named_function_scope",  // recur
                            "visit_enter_function_scope_body",   //
                            "visit_variable_use",                // recur
                            "visit_exit_function_scope"));
    EXPECT_THAT(v.enter_named_function_scopes,
                ElementsAre(spy_visitor::visited_enter_named_function_scope{
                    u8"recur"}));
  }
}

TEST(test_parse, arrow_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(() => x);");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => y);");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
  }
}

TEST(test_parse, arrow_function_expression_with_statements) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(() => { x; });");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => { y; });");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
  }
}

TEST(test_parse, parse_empty_module) {
  spy_visitor v;
  padded_string code(u8"");
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, IsEmpty());
  EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
}

TEST(test_parse, parse_class_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class C {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export class C {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class Derived extends Base {}");

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Derived");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"Base");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class FileStream extends fs.ReadStream {}");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"fs");
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class Monster { eatMuffins(muffinCount) { } }");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Monster");
    EXPECT_EQ(v.variable_declarations[1].name, u8"muffinCount");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, u8"eatMuffins");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // Monster
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // eatMuffins
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       // muffinCount
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { static m() { } }");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, u8"m");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // C
                                      "visit_enter_class_scope",          //
                                      "visit_property_declaration",       // m
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { async m() { } }");
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static async m() { } }");
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { get length() { } }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C {\n"
        u8"  static get length() { }\n"
        u8"  static set length(l) { }\n"
        u8"}");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"},
                    spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { a(){} b(){} c(){} }");
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, u8"a");
    EXPECT_EQ(v.property_declarations[1].name, u8"b");
    EXPECT_EQ(v.property_declarations[2].name, u8"c");
  }

  {
    spy_visitor v;
    padded_string code(u8"class A {} class B {}");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"A", variable_kind::_class},
                    spy_visitor::visited_variable_declaration{
                        u8"B", variable_kind::_class}));
  }
}

TEST(test_parse, class_statement_with_keyword_property) {
  for (string8 keyword : {u8"async", u8"catch", u8"class", u8"default", u8"get",
                          u8"set", u8"try"}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      string8 code = u8"class C { " + keyword + u8"(){} }";
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }
  }
}

TEST(test_parse, parse_and_visit_try) {
  {
    spy_visitor v = parse_and_visit_statement(u8"try {} finally {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try {} catch (e) {}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch (e) {} finally {}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"try {f();} catch (e) {g();} finally {h();}");

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
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"g");
    EXPECT_EQ(v.variable_uses[2].name, u8"h");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch ({message, code}) {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     // (catch)
                                      "visit_variable_declaration",  // message
                                      "visit_variable_declaration",  // code
                                      "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"message");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    EXPECT_EQ(v.variable_declarations[1].name, u8"code");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_catch);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch ([message, code]) {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     // (catch)
                                      "visit_variable_declaration",  // message
                                      "visit_variable_declaration",  // code
                                      "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"message");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    EXPECT_EQ(v.variable_declarations[1].name, u8"code");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_catch);
  }
}

TEST(test_parse, if_without_else) {
  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_with_else) {
  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; } else { c; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b; else c;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, do_while) {
  {
    spy_visitor v = parse_and_visit_statement(u8"do { a; } while (b)");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, c_style_for_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (;;) { a; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (init; cond; after) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"},  //
                            spy_visitor::visited_variable_use{u8"cond"},  //
                            spy_visitor::visited_variable_use{u8"body"},  //
                            spy_visitor::visited_variable_use{u8"after"}));
  }

  for (const char8 *variable_kind : {u8"const", u8"let"}) {
    SCOPED_TRACE(out_string8(variable_kind));
    string8 code =
        string8(u8"for (") + variable_kind + u8" i = 0; cond; after) { body; }";
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
    spy_visitor v =
        parse_and_visit_statement(u8"for (var i = 0; ; ) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (i = 0, j = 0; ; ) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_assignment",  // i
                                      "visit_variable_assignment",  // j
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, for_in_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (let x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (var x in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // xs
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = init in xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // init
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // xs
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"},  //
                            spy_visitor::visited_variable_use{u8"xs"},    //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  // TODO(strager): Report error for the following code:
  //
  //   for (let x = init in xs) {}
  //
  // ('var' is allowed, but not 'let'.)

  // TODO(strager): Report error for the following code:
  //
  //   for (var x = init of xs) {}
  //
  // (for-in is allowed, but not for-of.)
}

TEST(test_parse, for_of_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (let x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (var x of xs) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for await (let x of xs) { body; }");
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
    spy_visitor v = parse_and_visit_statement(u8"{ }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"{ first; second; third; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"first"},   //
                            spy_visitor::visited_variable_use{u8"second"},  //
                            spy_visitor::visited_variable_use{u8"third"}));
  }
}

TEST(test_parse, switch_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"switch (x) {}");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {case y:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // y
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {default:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"switch (true) {case x: case y: default: case z:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // x
                                      "visit_variable_use",       // y
                                      "visit_variable_use",       // z
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"switch (true) { case true: x; let y; z; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_variable_use",          // z
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, while_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"while (cond) body;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"while (cond) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, with_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) body;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) { body; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, break_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"break;");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, continue_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"continue;");
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, debugger_statement) {
  {
    spy_visitor v;
    padded_string code(u8"debugger; x;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, labelled_statement) {
  {
    spy_visitor v;
    padded_string code(u8"some_label: ; x;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"foob: for (;;) body");
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"one: two: three: while (false) body;");
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the labels with a visit or visits.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }
}

TEST(test_parse, report_missing_semicolon_for_declarations) {
  {
    spy_visitor v;
    padded_string code(u8"let x = 2 for (;;) { console.log(); }");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
    source_position::offset_type end_of_let_statement = strlen(u8"let x = 2");
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_expression, where,
                              offsets_matcher(&code, end_of_let_statement,
                                              end_of_let_statement))));
  }
  {
    spy_visitor v;
    padded_string code(u8"const x debugger");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_const}));
    source_position::offset_type end_of_const_statement = strlen(u8"const x");
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_expression, where,
                              offsets_matcher(&code, end_of_const_statement,
                                              end_of_const_statement))));
  }
}

TEST(test_parse, variables_can_be_named_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"var let = initial;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // initial
                                      "visit_variable_declaration"));  // let
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function let(let) {}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // let (function)
                            "visit_enter_function_scope",
                            "visit_variable_declaration",  // let (parameter)
                            "visit_enter_function_scope_body",
                            "visit_exit_function_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, u8"let");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try { } catch (let) { }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope", "visit_exit_block_scope",
                            "visit_enter_block_scope",
                            "visit_variable_declaration",  // let
                            "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {x = let} = o;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // o
                                      "visit_variable_use",            // let
                                      "visit_variable_declaration"));  // x
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[1].name, u8"let");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"console.log(let);");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // console
                                      "visit_variable_use"));  // let
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[1].name, u8"let");
  }

  for (const char8 *code : {
           u8"(async let => null)",
           u8"(async (let) => null)",
           u8"(let => null)",
           u8"((let) => null)",
       }) {
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",  // let
                                      "visit_enter_function_scope_body",
                                      "visit_exit_function_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_parameter);
  }
}

TEST(test_parse, new_style_variables_cannot_be_named_let) {
  for (string8 declaration_kind : {u8"const", u8"let"}) {
    spy_visitor v;
    padded_string code(declaration_kind + u8" let = null;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_declare_variable_named_let_with_let, name,
                    offsets_matcher(&code, declaration_kind.size() + 1,
                                    declaration_kind.size() + 1 + 3))));

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
  }

  {
    spy_visitor v;
    padded_string code(u8"let {other, let} = stuff;");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_declare_variable_named_let_with_let,
                              name, offsets_matcher(&code, 12, 12 + 3))));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import let from 'weird';");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_import_let, import_name,
                              offsets_matcher(&code, 7, 7 + 3))));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import * as let from 'weird';");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_import_let, import_name,
                              offsets_matcher(&code, 12, 12 + 3))));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import { let } from 'weird';");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_import_let, import_name,
                              offsets_matcher(&code, 9, 9 + 3))));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // TODO(strager): export implies strict mode (because modules imply strict
  // mode).
  if ((false)) {
    spy_visitor v;
    padded_string code(u8"export function let() {}");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_export_let, export_name,
                              offsets_matcher(&code, 16, 16 + 3))));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  // class implies strict mode.
  {
    spy_visitor v;
    padded_string code(u8"class let {}");
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_declare_class_named_let, name,
                              offsets_matcher(&code, 6, 6 + 3))));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }
}

TEST(test_parse, variables_can_be_named_private_protected_public_static) {
  for (string8 variable_name :
       {u8"private", u8"protected", u8"public", u8"static"}) {
    SCOPED_TRACE(out_string8(variable_name));

    for (string8 variable_kind : {u8"const", u8"let", u8"var"}) {
      string8 code = variable_kind + u8" " + variable_name + u8" = initial;";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // initial
                                        "visit_variable_declaration"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, variable_name);
    }

    {
      string8 code =
          u8"function " + variable_name + u8"(" + variable_name + u8") {}";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",  // (function name)
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // (parameter name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 2);
      EXPECT_EQ(v.variable_declarations[0].name, variable_name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
      EXPECT_EQ(v.variable_declarations[1].name, variable_name);
      EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);
    }

    {
      string8 code = u8"try { } catch (" + variable_name + u8") { }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  //
                              "visit_exit_block_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, variable_name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    }

    {
      string8 code = u8"console.log(" + variable_name + u8");";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",    // console
                              "visit_variable_use"));  // (variable_name)
      ASSERT_EQ(v.variable_uses.size(), 2);
      EXPECT_EQ(v.variable_uses[1].name, variable_name);
    }

    {
      string8 code = variable_name + u8";";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
      ASSERT_EQ(v.variable_uses.size(), 1);
      EXPECT_EQ(v.variable_uses[0].name, variable_name);
    }

    for (string8 code : {
             u8"(async " + variable_name + u8" => null)",
             u8"(async (" + variable_name + u8") => null)",
             u8"(" + variable_name + u8" => null)",
             u8"((" + variable_name + u8") => null)",
         }) {
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",
                              "visit_variable_declaration",  // (variable_name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, variable_name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_parameter);
    }
  }
}
}
}
