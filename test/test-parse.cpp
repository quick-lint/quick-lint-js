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
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
spy_visitor parse_and_visit_statement(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

spy_visitor parse_and_visit_expression(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_expression(v);
  EXPECT_THAT(v.errors, IsEmpty());
  return v;
}

constexpr std::array keywords = make_array(
    u8"as", u8"async", u8"await", u8"break", u8"case", u8"catch", u8"class",
    u8"const", u8"continue", u8"debugger", u8"default", u8"delete", u8"do",
    u8"else", u8"export", u8"extends", u8"false", u8"finally", u8"for",
    u8"from", u8"function", u8"get", u8"if", u8"import", u8"in", u8"instanceof",
    u8"let", u8"new", u8"null", u8"of", u8"return", u8"set", u8"static",
    u8"super", u8"switch", u8"this", u8"throw", u8"true", u8"try", u8"typeof",
    u8"var", u8"void", u8"while", u8"with", u8"yield");

TEST(test_parse, parse_simple_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"a");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[1].name, u8"b");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_let);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b, c, d, e, f, g"_sv);
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
    padded_string code(u8"let first; let second"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"export let x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
  }
}

TEST(test_parse, export_default) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export default x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default function f() {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default function() {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default async function f() {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default async function() {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default (function f() {})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export default class C {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export default class {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export default async (a) => b;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // a
                                      "visit_enter_function_scope_body",
                                      "visit_variable_use",  // b
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, export_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export {one, two};"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_export_use",    // one
                                      "visit_variable_export_use"));  // two
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export {one as two, three as four};"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_export_use",    // one
                                      "visit_variable_export_use"));  // three
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"one"},
                            spy_visitor::visited_variable_use{u8"three"}));
  }
}

TEST(test_parse, export_from) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export * from 'other';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export {} from 'other';"_sv);
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
    spy_visitor v = parse_and_visit_statement(
        u8"export {promises as default} from 'fs';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export {default} from 'other';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, parse_simple_var) {
  spy_visitor v;
  padded_string code(u8"var x"_sv);
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_simple_const) {
  spy_visitor v;
  padded_string code(u8"const x"_sv);
  parser p(&code, &v);
  p.parse_and_visit_statement(v);
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_let_with_initializers) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2, y = 3"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = other, y = x"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"other");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = y in z;"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");
    EXPECT_EQ(v.variable_uses[1].name, u8"z");
  }
}

TEST(test_parse, parse_let_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let {x} = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {x, y, z} = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[2].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {key: variable} = 2"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"variable", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {} = x;"_sv);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"let {key = defaultValue} = x;"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"let [first, second] = xs;"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"function f({x, y, z}) {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 4);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
    EXPECT_EQ(v.variable_declarations[1].name, u8"x");
    EXPECT_EQ(v.variable_declarations[2].name, u8"y");
    EXPECT_EQ(v.variable_declarations[3].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"({x, y, z}) => {}"_sv);
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
  padded_string code(u8"let x = x"_sv);
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
    padded_string code(u8"let"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_let_with_no_bindings, where,
                                             offsets_matcher(&code, 0, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let a,"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_stray_comma_in_let_statement, where,
                              offsets_matcher(&code, 5, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x, 42"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 7, 9))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let if"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let 42"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let debugger"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_binding_in_let_statement, where,
                              offsets_matcher(&code, 4, 12))));
  }

  {
    spy_visitor v;
    padded_string code(u8"let true, true, y\nlet x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_EQ(v.variable_declarations.size(), 2);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(error_invalid_binding_in_let_statement,
                                     where, offsets_matcher(&code, 4, 8)),
                    ERROR_TYPE_FIELD(error_invalid_binding_in_let_statement,
                                     where, offsets_matcher(&code, 10, 14))));
  }
}

TEST(test_parse, parse_and_visit_import) {
  {
    spy_visitor v = parse_and_visit_statement(u8"import 'foo';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"import fs from 'fs'"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"import * as fs from 'fs'"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"fs");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v;
    padded_string code(u8"import fs from 'fs'; import net from 'net';"_sv);
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
    spy_visitor v = parse_and_visit_statement(
        u8"import {readFileSync as rf} from 'fs';"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"rf");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"import fs, {readFileSync} from 'fs';"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"fs", variable_kind::_import},
                    spy_visitor::visited_variable_declaration{
                        u8"readFileSync", variable_kind::_import}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"import fsDefault, * as fsExports from 'fs';");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"fsDefault", variable_kind::_import},
                    spy_visitor::visited_variable_declaration{
                        u8"fsExports", variable_kind::_import}));
  }
}

TEST(test_parse, return_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"return a;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"return a\nreturn b"_sv);
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
    padded_string code(u8"return a; return b;"_sv);
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
    padded_string code(u8"if (true) return; x;"_sv);
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
        parse_and_visit_statement(u8"if (true) { return } else { other }"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"throw new Error('ouch');"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Error"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_expected_expression_before_semicolon, where,
                              offsets_matcher(&code, 5, 6))));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw\nnew Error();"_sv);
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
    spy_visitor v = parse_and_visit_expression(u8"some_var"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }

  {
    spy_visitor v =
        parse_and_visit_expression(u8"some_var + some_other_var"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
    EXPECT_EQ(v.variable_uses[1].name, u8"some_other_var");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"+ v"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"v");
  }
}

TEST(test_parse, parse_invalid_math_expression) {
  {
    spy_visitor v;
    padded_string code(u8"2 +"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 2, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"^ 2"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, 1))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * * 2"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 2, 3))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 & & & 2"_sv);
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
    padded_string code(u8"(2 *)"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 3, 4))));
  }
  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + 4"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 4, 5))));
  }

  {
    spy_visitor v;
    padded_string code(u8"2 * (3 + (4"_sv);
    parser p(&code, &v);
    p.parse_and_visit_expression(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 9, 10)),
                            ERROR_TYPE_FIELD(error_unmatched_parenthesis, where,
                                             offsets_matcher(&code, 4, 5))));
  }
}

TEST(test_parse, statement_starting_with_binary_only_operator) {
  for (string8_view op : {
           u8"!=", u8"!==", u8"%",  u8"&",          u8"&&",  u8"*", u8"**",
           u8"<",  u8"<<",  u8"<=", u8"==",         u8"===", u8">", u8">=",
           u8">>", u8">>>", u8"^",  u8"instanceof", u8"|",
       }) {
    padded_string code(string8(op) + u8" x");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 0, op.size()))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }
}

TEST(test_parse, DISABLED_parse_invalid_math_expression_2) {
  {
    spy_visitor v;
    padded_string code(u8"ten ten"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(error_unexpected_identifier, where,
                                             offsets_matcher(&code, 4, 7))));
  }
}

TEST(test_parse, parse_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"(x) = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");

    ASSERT_EQ(v.variable_assignments.size(), 1);
    EXPECT_EQ(v.variable_assignments[0].name, u8"x");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p = y"_sv);

    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");

    EXPECT_EQ(v.variable_assignments.size(), 0);
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x = y = z"_sv);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"z");

    EXPECT_EQ(v.variable_assignments.size(), 2);
    EXPECT_EQ(v.variable_assignments[0].name, u8"y");
    EXPECT_EQ(v.variable_assignments[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"xs[i] = j"_sv);
    EXPECT_THAT(v.variable_assignments, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"i"},   //
                            spy_visitor::visited_variable_use{u8"j"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{x: y} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[x]: y} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{k1: {k2: x, k3: y}} = z"_sv);
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"},  //
                            spy_visitor::visited_variable_assignment{u8"y"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, parse_compound_assignment) {
  {
    spy_visitor v = parse_and_visit_expression(u8"x += y"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",           // x
                                      "visit_variable_use",           // y
                                      "visit_variable_assignment"));  // x
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"x.p += y"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // x
                                      "visit_variable_use"));  // y
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_assignments, IsEmpty());
  }
}

TEST(test_parse, parse_plusplus_minusminus) {
  {
    spy_visitor v = parse_and_visit_expression(u8"++x"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_assignment"));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"y--"_sv);
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
    spy_visitor v = parse_and_visit_expression(u8"typeof x"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"typeof(x)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_typeof_with_non_variable) {
  {
    spy_visitor v = parse_and_visit_expression(u8"typeof x.prop"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, parse_array_subscript) {
  {
    spy_visitor v = parse_and_visit_expression(u8"array[index]"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"array"},
                            spy_visitor::visited_variable_use{u8"index"}));
  }
}

TEST(test_parse, array_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"[]"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"[...elements]"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"elements"}));
  }
}

TEST(test_parse, object_literal) {
  {
    spy_visitor v = parse_and_visit_expression(u8"{key: value}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"value"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{[key1 + key2]: value}"_sv);
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
    spy_visitor v = parse_and_visit_expression(u8"{...other1, ...other2}"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // other1
                            "visit_variable_use"));  // other2
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other1"},
                            spy_visitor::visited_variable_use{u8"other2"}));
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"{func(a, b) { body; }}"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"console.log('hello');"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"this.x = xPos;"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"++x;"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  //
                            "visit_variable_assignment"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"new C();"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"C"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"delete x;"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"`hello`"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"`hello ${world}`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // world
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"42"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"import(url).then(); secondStatement;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // url
                            "visit_variable_use"));  // secondStatement
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"typeof x"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"[x, y, z];"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",    // x
                            "visit_variable_use",    // y
                            "visit_variable_use"));  // z
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/regexp/;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"/=regexp/;"_sv);
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

  {
    spy_visitor v = parse_and_visit_statement(u8"async => rhs;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                      "visit_variable_declaration",  // async
                                      "visit_enter_function_scope_body",
                                      "visit_variable_use",  // rhs
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, asi_plusplus_minusminus) {
  {
    spy_visitor v;
    padded_string code(u8"x\n++\ny;"_sv);
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
        u8"function f() { console.log(\"hello\") } function g() { }"_sv);
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
    padded_string code(u8"console.log('hello')\nconsole.log('world')\n"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
  }

  for (string8_view second_statement : {
           u8"do {} while (cond)"_sv,
           u8"for (; cond; ) {}"_sv,
           u8"if (cond) {}"_sv,
           u8"switch (cond) {}"_sv,
           u8"while (cond) {}"_sv,
       }) {
    spy_visitor v;
    padded_string code(string8(u8"let x = 2\n"_sv) + string8(second_statement));
    SCOPED_TRACE(code);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"cond"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // This code should emit an error, but also use ASI for error recovery.
    spy_visitor v;
    padded_string code(u8"console.log('hello') console.log('world');"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
    cli_source_position::offset_type end_of_first_expression =
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
    spy_visitor v = parse_and_visit_statement(u8"console.log(2+2)"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_function_calls) {
  {
    spy_visitor v = parse_and_visit_expression(u8"f(x)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"f(x, y)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"o.f(x, y)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"o");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
    EXPECT_EQ(v.variable_uses[2].name, u8"y");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"console.log('hello', 42)"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"console");
  }
}

TEST(test_parse, parse_templates_in_expressions) {
  {
    spy_visitor v = parse_and_visit_expression(u8"`hello`"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`hello${world}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"world");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${one}${two}${three}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"one");
    EXPECT_EQ(v.variable_uses[1].name, u8"two");
    EXPECT_EQ(v.variable_uses[2].name, u8"three");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"`${2+2, four}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"four");
  }

  {
    spy_visitor v = parse_and_visit_expression(u8"tag`${inside}`"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"tag");
    EXPECT_EQ(v.variable_uses[1].name, u8"inside");
  }
}

TEST(test_parse, DISABLED_parse_invalid_function_calls) {
  {
    spy_visitor v;
    padded_string code(u8"(x)f"_sv);
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
    padded_string code(u8"f(x); g(y);"_sv);
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
    spy_visitor v = parse_and_visit_expression(u8"some_var.some_property"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"some_var");
  }
}

TEST(test_parse, parse_new_expression) {
  {
    spy_visitor v = parse_and_visit_expression(u8"new Foo()"_sv);
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"Foo");
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
    spy_visitor v = parse_and_visit_expression(u8"x ? y : z"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                            spy_visitor::visited_variable_use{u8"y"},  //
                            spy_visitor::visited_variable_use{u8"z"}));
  }
}

TEST(test_parse, parse_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function foo() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export function foo() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function sin(theta) {}"_sv);

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
        parse_and_visit_statement(u8"function pow(base, exponent) {}"_sv);

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
    spy_visitor v = parse_and_visit_statement(u8"function f(x, y = x) {}"_sv);

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
    spy_visitor v =
        parse_and_visit_statement(u8"function f() { return x; }"_sv);

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
        parse_and_visit_statement(u8"function g(first, ...args) {}"_sv);
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

TEST(test_parse, function_statement_with_no_name) {
  {
    padded_string code(u8"function() {x;}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_name_in_function_statement, where,
                    offsets_matcher(&code, 0, strlen(u8"function(")))));
  }

  {
    padded_string code(u8"async function() {x;}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_name_in_function_statement, where,
                              offsets_matcher(&code, strlen(u8"async "),
                                              strlen(u8"async function(")))));
  }

  {
    padded_string code(u8"async function(x) {y;}(z)"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use"));             // z
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_name_or_parentheses_for_function,  //
            where,
            offsets_matcher(&code, strlen(u8"async "),
                            strlen(u8"async function(")),  //
            function,
            offsets_matcher(&code, 0, strlen(u8"async function(x) {y;}")))));
  }
}

TEST(test_parse, async_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"async function f() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"async function f() { await null; }"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"f");
  }
}

TEST(test_parse, generator_function_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function* f() {}"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"f", variable_kind::_function}));
  }
}

TEST(test_parse, await_in_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() { await myPromise; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"async () => { await myPromise; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() { await myPromise; })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ async f() { await myPromise; } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C { async f() { await myPromise; } }");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() {\n"
        u8"  function g() {}\n"
        u8"  await myPromise;\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }
}

TEST(test_parse, await_asi_in_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() { await a\nawait b }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},  //
                            spy_visitor::visited_variable_use{u8"b"}));
  }
}

TEST(test_parse, use_await_in_non_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"await(x);"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() {\n"
        u8"  function g() { await(x); }\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function f() {\n"
        u8"  async function g() {}\n"
        u8"  await();\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(() => {\n"
        u8"  async () => {};\n"
        u8"  await();\n"
        u8"})");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(async => { await(); })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ async() { await(); } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async() { await(); } }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }
}

TEST(test_parse, declare_await_in_non_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function await() { }"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_function}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let await = 42;"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() {\n"
        u8"  (function(await) { })\n"
        u8"})");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_parameter}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(function() {\n"
        u8"  async function await() { }\n"
        u8"})");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_function}));
  }
}

TEST(test_parse, yield_in_generator_function) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"function *f() { yield myValue; }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function*() { yield myValue; })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ *f() { yield myValue; } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { *f() { yield myValue; } }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function* f() {\n"
        u8"  function g() {}\n"
        u8"  yield myValue;\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }
}

TEST(test_parse, use_yield_in_non_generator_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"yield(x);"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function* f() {\n"
        u8"  function g() { yield(x); }\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function f() {\n"
        u8"  function* g() {}\n"
        u8"  yield();\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"}));
  }
}

TEST(test_parse, declare_yield_in_non_generator_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function yield() { }"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"yield", variable_kind::_function}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let yield = 42;"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"yield", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() {\n"
        u8"  (function(yield) { })\n"
        u8"})");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"yield", variable_kind::_parameter}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(function() {\n"
        u8"  function* yield() { }\n"
        u8"})");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"yield", variable_kind::_function}));
  }
}

TEST(test_parse, parse_function_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(function() {});"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(function(x, y) {});"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_variable_declaration",       // x
                            "visit_variable_declaration",       // y
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(function() {let x = y;});"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // y
                            "visit_variable_declaration",       // x
                            "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(a, function(b) {c;}(d));"_sv);
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
        parse_and_visit_statement(u8"(function recur() { recur(); })();"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"(() => x);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => y);"_sv);
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

  {
    spy_visitor v = parse_and_visit_statement(u8"((x = y) => z);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_use",               // y
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // z
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_parameter}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"},
                            spy_visitor::visited_variable_use{u8"z"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async (x) => y;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async (x) => y, z;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",
                                      "visit_variable_use"));  // z
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"async x => y;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, arrow_function_expression_with_statements) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(() => { x; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // x
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(x => { y; });"_sv);
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

TEST(test_parse, function_statements_allow_trailing_commas_in_parameter_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function f(x,) { y; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, arrow_functions_allow_trailing_commas_in_parameter_list) {
  {
    spy_visitor v = parse_and_visit_statement(u8"((x,) => { y; });"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope"));
  }
}

TEST(test_parse, parse_empty_module) {
  spy_visitor v;
  padded_string code(u8""_sv);
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, IsEmpty());
  EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
}

TEST(test_parse, parse_class_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class C {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export class C {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class Derived extends Base {}"_sv);

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
    spy_visitor v;
    padded_string code(u8"class A {} class B {}"_sv);
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

TEST(test_parse, class_statement_with_odd_extends) {
  // TODO(strager): Should these report errors?
  {
    spy_visitor v = parse_and_visit_statement(u8"class C extends 0 {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }
}

TEST(test_parse, class_statement_with_methods) {
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
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static m() { } }"_sv);

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
    spy_visitor v = parse_and_visit_statement(u8"class C { async m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static async m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { *m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { get length() { } }"_sv);
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
        parse_and_visit_statement(u8"class C { a(){} b(){} c(){} }"_sv);
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, u8"a");
    EXPECT_EQ(v.property_declarations[1].name, u8"b");
    EXPECT_EQ(v.property_declarations[2].name, u8"c");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { \"stringKey\"() {} }");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { [x + y]() {} }"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }
}

TEST(test_parse, class_statement_with_keyword_property) {
  for (string8 keyword : {u8"async", u8"await", u8"catch", u8"class",
                          u8"default", u8"get", u8"set", u8"static", u8"try"}) {
    {
      string8 code = u8"class C { " + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    {
      string8 code = u8"class C { *" + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    for (string8 prefix : {u8"async", u8"get", u8"set", u8"static",
                           u8"static async", u8"static get", u8"static set"}) {
      string8 code = u8"class C { " + prefix + u8" " + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }
  }
}

TEST(test_parse, class_statement_with_number_methods) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class Wat { 42.0() { } }"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // Wat
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // 42.0
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
  }
}

TEST(test_parse, class_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(class C { })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",     //
                                      "visit_variable_declaration",  // C
                                      "visit_exit_class_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class { })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(class { a() {} [b]() {} })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",          //
                                      "visit_property_declaration",       // a
                                      "visit_enter_function_scope",       // a
                                      "visit_enter_function_scope_body",  // a
                                      "visit_exit_function_scope",        // a
                                      "visit_variable_use",               // b
                                      "visit_property_declaration",       // [b]
                                      "visit_enter_function_scope",       // [b]
                                      "visit_enter_function_scope_body",  // [b]
                                      "visit_exit_function_scope",        // [b]
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class A extends B {})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",     //
                                      "visit_variable_use",          // B
                                      "visit_variable_declaration",  // A
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class extends C {})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_variable_use",       // C
                                      "visit_exit_class_scope"));
  }
}

TEST(test_parse, class_statement_allows_stray_semicolons) {
  spy_visitor v = parse_and_visit_statement(u8"class C{ ; f(){} ; }"_sv);
  ASSERT_EQ(v.property_declarations.size(), 1);
  EXPECT_EQ(v.property_declarations[0].name, u8"f");
}

TEST(test_parse, parse_and_visit_try) {
  {
    spy_visitor v = parse_and_visit_statement(u8"try {} finally {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try {} catch (e) {}"_sv);

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
    spy_visitor v = parse_and_visit_statement(u8"try {} catch {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // try
                                      "visit_exit_block_scope",    // try
                                      "visit_enter_block_scope",   // catch
                                      "visit_exit_block_scope"));  // catch
    EXPECT_THAT(v.variable_declarations, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch (e) {} finally {}"_sv);

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
        parse_and_visit_statement(u8"try {} catch ({message, code}) {}"_sv);
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
        parse_and_visit_statement(u8"try {} catch ([message, code]) {}"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_with_else) {
  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; } else { c; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b; else c;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"if cond { body; }"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_expected_parentheses_around_if_condition, condition,
                    offsets_matcher(&code, strlen(u8"if "),
                                    // TODO(#139): End the error at the 'd'.
                                    strlen(u8"if cond ")))));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (cond { body; }"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_expected_parenthesis_around_if_condition,  //
                    where,
                    // TODO(#139): Place the error immediately after the 'd'.
                    offsets_matcher(&code, strlen(u8"if (cond "),
                                    strlen(u8"if (cond ")),  //
                    token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"if cond) { body; }"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_expected_parenthesis_around_if_condition,  //
                    where,
                    offsets_matcher(&code, strlen(u8"if "),
                                    strlen(u8"if ")),  //
                    token, u8'(')));
  }
}

TEST(test_parse, utter_garbage) {
  {
    spy_visitor v;
    padded_string code(u8"if :\nkjaslkjd;kjaslkjd"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // kjaslkjd
                                      "visit_variable_use"));  // kjaslkjd
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_expected_parentheses_around_if_condition, condition,
                offsets_matcher(&code, strlen(u8"if "),
                                // TODO(#139): End the error at the ':'.
                                strlen(u8"if :\n"))),
            ERROR_TYPE_FIELD(
                error_unexpected_token, token,
                offsets_matcher(&code, strlen(u8"if "), strlen(u8"if :")))));
  }
}

TEST(test_parse, do_while) {
  {
    spy_visitor v = parse_and_visit_statement(u8"do { a; } while (b)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"do do {a;} while(b) while(c);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use",       // b
                                      "visit_variable_use"));     // c
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"do do {a;} while(b); while(c);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use",       // b
                                      "visit_variable_use"));     // c
  }
}

TEST(test_parse, c_style_for_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (;;) { a; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (init; cond; after) { body; }"_sv);
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
        parse_and_visit_statement(u8"for (var i = 0; ; ) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (i = 0, j = 0; ; ) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_assignment",  // i
                                      "visit_variable_assignment",  // j
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, for_in_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (x in xs) { body; }"_sv);
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
    spy_visitor v =
        parse_and_visit_statement(u8"for (let x in xs) { body; }"_sv);
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
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x in xs) { body; }"_sv);
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
        parse_and_visit_statement(u8"for (var x = init in xs) { body; }"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"for (x of xs) { body; }"_sv);
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
    spy_visitor v =
        parse_and_visit_statement(u8"for (let x of xs) { body; }"_sv);
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
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x of xs) { body; }"_sv);
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
        parse_and_visit_statement(u8"for await (let x of xs) { body; }"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"{ }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"{ first; second; third; }"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"switch (x) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {case y:}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // y
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {default:}"_sv);
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
    spy_visitor v = parse_and_visit_statement(u8"while (cond) body;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"while (cond) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, with_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) body;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, break_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"break;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  // TODO(strager): Are contextual keywords allowed as labels?
  // TODO(#72): Visit the label.
  {
    spy_visitor v = parse_and_visit_statement(u8"break label;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, continue_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"continue;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  // TODO(strager): Are contextual keywords allowed as labels?
  // TODO(#72): Visit the label.
  {
    spy_visitor v = parse_and_visit_statement(u8"continue label;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, debugger_statement) {
  {
    spy_visitor v;
    padded_string code(u8"debugger; x;"_sv);
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
    padded_string code(u8"some_label: ; x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"foob: for (;;) body"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"one: two: three: while (false) body;"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the labels with a visit or visits.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }
}

TEST(test_parse, statement_label_can_be_a_contextual_keyword) {
  for (string8_view keyword : {u8"await"_sv, u8"yield"_sv}) {
    SCOPED_TRACE(out_string8(keyword));
    spy_visitor v = parse_and_visit_statement(string8(keyword) + u8": x;");
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }
}

TEST(test_parse, report_missing_semicolon_for_declarations) {
  {
    spy_visitor v;
    padded_string code(u8"let x = 2 for (;;) { console.log(); }"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
    cli_source_position::offset_type end_of_let_statement =
        strlen(u8"let x = 2");
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_expression, where,
                              offsets_matcher(&code, end_of_let_statement,
                                              end_of_let_statement))));
  }
  {
    spy_visitor v;
    padded_string code(u8"const x debugger"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_const}));
    cli_source_position::offset_type end_of_const_statement =
        strlen(u8"const x");
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_semicolon_after_expression, where,
                              offsets_matcher(&code, end_of_const_statement,
                                              end_of_const_statement))));
  }
}

TEST(test_parse, variables_can_be_named_contextual_keywords) {
  for (string8 name : {u8"as", u8"async", u8"from", u8"get", u8"let", u8"of",
                       u8"set", u8"yield"}) {
    SCOPED_TRACE(out_string8(name));

    {
      spy_visitor v =
          parse_and_visit_statement(u8"var " + name + u8" = initial;");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
    }

    {
      spy_visitor v = parse_and_visit_statement(u8"function " + name + u8"(" +
                                                name + u8") {}");
      EXPECT_THAT(
          v.visits,
          ElementsAre("visit_variable_declaration",  // (name) (function)
                      "visit_enter_function_scope",
                      "visit_variable_declaration",  // (name) (parameter)
                      "visit_enter_function_scope_body",
                      "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 2);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
      EXPECT_EQ(v.variable_declarations[1].name, name);
      EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"(function " + name + u8"() {})");
      EXPECT_THAT(
          v.visits,
          ElementsAre("visit_enter_named_function_scope",  // (name) (function)
                      "visit_enter_function_scope_body",
                      "visit_exit_function_scope"));
      EXPECT_THAT(
          v.enter_named_function_scopes,
          ElementsAre(spy_visitor::visited_enter_named_function_scope{name}));
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"try { } catch (" + name + u8") { }");
      EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                        "visit_exit_block_scope",
                                        "visit_enter_block_scope",
                                        "visit_variable_declaration",  // (name)
                                        "visit_exit_block_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"let {x = " + name + u8"} = o;");
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // o
                                        "visit_variable_use",  // (name)
                                        "visit_variable_declaration"));  // x
      ASSERT_EQ(v.variable_uses.size(), 2);
      EXPECT_EQ(v.variable_uses[1].name, name);
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"console.log(" + name + u8");");
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // console
                                        "visit_variable_use"));  // name
      ASSERT_EQ(v.variable_uses.size(), 2);
      EXPECT_EQ(v.variable_uses[1].name, name);
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8".method();");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    for (string8 code : {
             u8"(async " + name + u8" => null)",
             u8"(async (" + name + u8") => null)",
             u8"(" + name + u8" => null)",
             u8"((" + name + u8") => null)",
         }) {
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code);
      EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",
                                        "visit_variable_declaration",  // (name)
                                        "visit_enter_function_scope_body",
                                        "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_parameter);
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"for (" + name + u8" in xs) ;");
      EXPECT_THAT(v.visits,
                  ::testing::AnyOf(
                      // TODO(strager): A for scope shouldn't be introduced by
                      // this syntax. (No variable is being declared.)
                      ElementsAre("visit_enter_for_scope",      //
                                  "visit_variable_use",         // xs
                                  "visit_variable_assignment",  // (name)
                                  "visit_exit_for_scope"),
                      ElementsAre("visit_variable_use",            // xs
                                  "visit_variable_assignment")));  // (name)
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"for (" + name + u8".prop in xs) ;");
      EXPECT_THAT(v.variable_uses,
                  // TODO(strager): Why are 'let' and 'async' giving different
                  // orders for these variable uses?
                  ::testing::AnyOf(
                      ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                                  spy_visitor::visited_variable_use{name}),   //
                      ElementsAre(spy_visitor::visited_variable_use{name},    //
                                  spy_visitor::visited_variable_use{u8"xs"})));
    }
  }
}

TEST(test_parse, imported_variables_can_be_named_contextual_keywords) {
  for (string8 name : {u8"async"}) {
    SCOPED_TRACE(out_string8(name));

    {
      spy_visitor v = parse_and_visit_statement(u8"import { " + name +
                                                u8" } from 'other';");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"import " + name + u8" from 'other';");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
    }

    {
      spy_visitor v = parse_and_visit_statement(u8"import * as " + name +
                                                u8" from 'other';");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
    }
  }
}

TEST(test_parse, statement_beginning_with_async_or_let) {
  for (string8 name : {u8"async", u8"let"}) {
    SCOPED_TRACE(out_string8(name));

    {
      spy_visitor v = parse_and_visit_statement(name + u8" = other;");
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"other"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8"();");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8".method();");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8";");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    for (const char8 *unary_operator : {u8"++", u8"--"}) {
      string8 code = name + unary_operator;
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",  //
                              "visit_variable_assignment"));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8" ? a : b;");
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"a"},
                              spy_visitor::visited_variable_use{u8"b"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8" => {body;};");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",       //
                              "visit_variable_declaration",       // (name)
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // body
                              "visit_exit_function_scope"));
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8"`template`;");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use"));  // (name)
    }

    {
      spy_visitor v =
          parse_and_visit_statement(name + u8"`template${variable}`;");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",    // (name)
                              "visit_variable_use"));  // variable
    }

    for (const char8 *binary_operator : {
             u8"&=",
             u8"^=",
             u8">>=",
             u8">>>=",
             u8"<<=",
             u8"-=",
             u8"%=",
             u8"|=",
             u8"+=",
             u8"/=",
             u8"*=",
             u8"**=",
         }) {
      string8 code = name + u8" " + binary_operator + u8" other";
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"other"}));
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
    }

    for (const char8 *binary_operator : {
             u8"!=", u8"!==",        u8"%", u8"&",  u8"&&", u8"*",   u8"**",
             u8"+",  u8",",          u8"-", u8"/",  u8"<",  u8"<<",  u8"<=",
             u8"==", u8"===",        u8">", u8">=", u8">>", u8">>>", u8"^",
             u8"in", u8"instanceof", u8"|", u8"||",
         }) {
      string8 code = name + u8" " + binary_operator + u8" other";
      SCOPED_TRACE(out_string8(code));

      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"other"}));
    }

    {
      string8 code = name + u8": while (go());";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code);
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"go"}));
    }
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
    padded_string code(u8"let {other, let} = stuff;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_cannot_declare_variable_named_let_with_let,
                              name, offsets_matcher(&code, 12, 12 + 3))));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import let from 'weird';"_sv);
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
    padded_string code(u8"import * as let from 'weird';"_sv);
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
    padded_string code(u8"import { let } from 'weird';"_sv);
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
    padded_string code(u8"export function let() {}"_sv);
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
    padded_string code(u8"class let {}"_sv);
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

TEST(test_parse, exported_names_can_be_named_keywords) {
  for (string8 export_name : keywords) {
    string8 code = u8"export {someFunction as " + export_name + u8"};";
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_export_use"));  // someFunction
    EXPECT_THAT(
        v.variable_uses,
        ElementsAre(spy_visitor::visited_variable_use{u8"someFunction"}));
  }
}

TEST(test_parse, imported_names_can_be_named_keywords) {
  for (string8 import_name : keywords) {
    string8 code =
        u8"import {" + import_name + u8" as someFunction} from 'somewhere';";
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration"));  // someFunction
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"someFunction", variable_kind::_import}));
  }
}

TEST(test_parse, parameters_can_be_named_contextual_keywords) {
  for (string8 parameter_name :
       {u8"as", u8"async", u8"await", u8"from", u8"get", u8"let", u8"set",
        u8"static", u8"yield"}) {
    {
      string8 code = u8"function f(" + parameter_name + u8") {}";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",  // f
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // (parameter_name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(
                      spy_visitor::visited_variable_declaration{
                          u8"f", variable_kind::_function},
                      spy_visitor::visited_variable_declaration{
                          parameter_name, variable_kind::_parameter}));
    }

    {
      string8 code = u8"(" + parameter_name + u8") => null";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",
                              "visit_variable_declaration",  // (parameter_name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      parameter_name, variable_kind::_parameter}));
    }

    {
      string8 code = u8"async (" + parameter_name + u8") => null";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",
                              "visit_variable_declaration",  // (parameter_name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      parameter_name, variable_kind::_parameter}));
    }

    {
      string8 code = u8"async " + parameter_name + u8" => null";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_function_scope",
                              "visit_variable_declaration",  // (parameter_name)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      parameter_name, variable_kind::_parameter}));
    }
  }
}

TEST(test_parse, trailing_comma_in_comma_expression_is_disallowed) {
  {
    spy_visitor v;
    padded_string code(u8"(a, b, );"_sv);
    parser p(&code, &v);
    p.parse_and_visit_statement(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_operand_for_operator, where,
                              offsets_matcher(&code, 5, 5 + 1))));
  }
}
}
}
