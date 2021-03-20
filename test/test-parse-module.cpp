// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

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
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse, export_variable) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export let x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export var x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export const x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_const}));
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

TEST(test_parse, export_default_of_variable_is_illegal) {
  for (string8 declaration_kind : {u8"const", u8"let", u8"var"}) {
    padded_string code(u8"export default " + declaration_kind + u8" x = y;");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_export_default_variable, declaring_token,
                    offsets_matcher(&code, strlen(u8"export default "),
                                    declaration_kind))));
  }
}

TEST(test_parse, export_sometimes_requires_semicolon) {
  {
    padded_string code(u8"export {x} console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_export_use",  // x
                                      "visit_variable_use",         // console
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_semicolon_after_statement, where,
                    offsets_matcher(&code, strlen(u8"export {x}"), u8""))));
  }

  {
    padded_string code(u8"export * from 'other' console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_semicolon_after_statement, where,
            offsets_matcher(&code, strlen(u8"export * from 'other'"), u8""))));
  }

  {
    padded_string code(u8"export default x+y console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // y
                                      "visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_semicolon_after_statement, where,
            offsets_matcher(&code, strlen(u8"export default x+y"), u8""))));
  }

  {
    padded_string code(u8"export default async () => {} console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_semicolon_after_statement, where,
            offsets_matcher(&code, strlen(u8"export default async () => {}"),
                            u8""))));
  }
}

TEST(test_parse, export_sometimes_does_not_require_semicolon) {
  {
    padded_string code(
        u8"export default async function f() {} console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"export default function() {} console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, IsEmpty());
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

TEST(test_parse, exported_variables_cannot_be_named_reserved_keywords) {
  for (string8 keyword : reserved_keywords) {
    padded_string code(u8"export {" + keyword + u8"};");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_export_use"));  // (keyword)
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{keyword}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_export_variable_named_keyword, export_name,
                    offsets_matcher(&code, strlen(u8"export {"), keyword))));
  }

  for (string8 keyword : reserved_keywords) {
    padded_string code(u8"export {" + keyword + u8" as thing};");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_export_use"));  // (keyword)
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{keyword}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_export_variable_named_keyword, export_name,
                    offsets_matcher(&code, strlen(u8"export {"), keyword))));
  }
}

TEST(test_parse, export_from) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export * from 'other';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export * as mother from 'other';"_sv);
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

  for (string8 keyword : keywords) {
    padded_string code(u8"export {" + keyword + u8"} from 'other';");
    SCOPED_TRACE(code);
    spy_visitor v = parse_and_visit_statement(code.string_view());
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, invalid_export_expression) {
  {
    spy_visitor v;
    padded_string code(u8"export stuff;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_exporting_requires_curlies, names,
                    offsets_matcher(&code, strlen(u8"export "), u8"stuff"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // stuff
  }

  {
    spy_visitor v;
    padded_string code(u8"export a, b, c;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            // TODO(strager): Report error_exporting_requires_curlies instead.
            error_exporting_requires_default, expression,
            offsets_matcher(&code, strlen(u8"export "), u8"a, b, c"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // a
                                      "visit_variable_use",    // b
                                      "visit_variable_use"));  // c
  }

  {
    spy_visitor v;
    padded_string code(u8"export a, b, c+d;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            // TODO(strager): Should we report
            // error_exporting_requires_curlies instead?
            error_exporting_requires_default, expression,
            offsets_matcher(&code, strlen(u8"export "), u8"a, b, c+d"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // a
                                      "visit_variable_use",    // b
                                      "visit_variable_use",    // c
                                      "visit_variable_use"));  // d
  }

  {
    spy_visitor v;
    padded_string code(u8"export 2 + x;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_exporting_requires_default, expression,
                    offsets_matcher(&code, strlen(u8"export "), u8"2 + x"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }
}

TEST(test_parse, invalid_export) {
  {
    spy_visitor v;
    padded_string code(u8"export ;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_token_after_export, export_token,
                              offsets_matcher(&code, 0, u8"export"))));
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"export "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_token_after_export, export_token,
                              offsets_matcher(&code, 0, u8"export"))));
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"export = x"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_token_after_export, unexpected_token,
                    offsets_matcher(&code, strlen(u8"export "), u8"="))));
    EXPECT_TRUE(p.parse_and_visit_statement(v));               // Parse '= x'.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
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
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
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

TEST(test_parse, import_star_without_as_keyword) {
  {
    padded_string code(u8"import * myExport from 'other';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_as_before_imported_namespace_alias, star_token,
            offsets_matcher(&code, strlen(u8"import "), u8"*"),  //
            alias,
            offsets_matcher(&code, strlen(u8"import * "), u8"myExport"))));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration"));  // myExport
  }
}

TEST(test_parse, import_without_from_keyword) {
  {
    padded_string code(u8"import { x } 'other';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_from_before_module_specifier, module_specifier,
            offsets_matcher(&code, strlen(u8"import { x } "), u8"'other'"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // x
  }

  {
    padded_string code(u8"import { x } ;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_expected_from_and_module_specifier, where,
                    offsets_matcher(&code, strlen(u8"import { x }"), u8""))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // x
  }
}

TEST(test_parse, export_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export function foo() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export async function foo() {}"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"foo");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }
}

TEST(test_parse, export_function_requires_a_name) {
  {
    padded_string code(u8"export function() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_name_of_exported_function, function_keyword,
            offsets_matcher(&code, strlen(u8"export "), u8"function"))));
  }

  {
    padded_string code(u8"export async function() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_name_of_exported_function, function_keyword,
            offsets_matcher(&code, strlen(u8"export async "), u8"function"))));
  }
}

TEST(test_parse, export_class) {
  {
    spy_visitor v = parse_and_visit_statement(u8"export class C {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }
}

TEST(test_parse, export_class_requires_a_name) {
  {
    padded_string code(u8"export class {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_name_of_exported_class, class_keyword,
                    offsets_matcher(&code, strlen(u8"export "), u8"class"))));
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

TEST(test_parse, imported_variables_can_be_named_contextual_keywords) {
  for (string8 name : contextual_keywords) {
    if (name == u8"let") {
      continue;
    }

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

TEST(test_parse, imported_variables_cannot_be_named_reserved_keywords) {
  for (string8 name : reserved_keywords) {
    {
      padded_string code(u8"import { " + name + u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_cannot_import_variable_named_keyword, import_name,
                      offsets_matcher(&code, strlen(u8"import { "), name))));
    }

    {
      padded_string code(u8"import { someFunction as " + name +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_cannot_import_variable_named_keyword, import_name,
              offsets_matcher(&code, strlen(u8"import { someFunction as "),
                              name))));
    }

    {
      padded_string code(u8"import " + name + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_cannot_import_variable_named_keyword, import_name,
                      offsets_matcher(&code, strlen(u8"import "), name))));
    }

    {
      padded_string code(u8"import * as " + name + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_cannot_import_variable_named_keyword, import_name,
                      offsets_matcher(&code, strlen(u8"import * as "), name))));
    }
  }

  struct test_case {
    string8 name;
    string8 expected_identifier;
  };

  // TODO(strager): test_case{u8"\\u{61}wait", u8"await"}
  // TODO(strager): test_case{u8"\\u{79}ield", u8"yield"}
  for (test_case tc : {
           test_case{u8"\\u{62}reak", u8"break"},
           test_case{u8"\\u{63}ase", u8"case"},
           test_case{u8"\\u{63}atch", u8"catch"},
           test_case{u8"\\u{63}lass", u8"class"},
           test_case{u8"\\u{63}onst", u8"const"},
           test_case{u8"\\u{63}ontinue", u8"continue"},
           test_case{u8"\\u{64}ebugger", u8"debugger"},
           test_case{u8"\\u{64}efault", u8"default"},
           test_case{u8"\\u{64}elete", u8"delete"},
           test_case{u8"\\u{64}o", u8"do"},
           test_case{u8"\\u{65}lse", u8"else"},
           test_case{u8"\\u{65}num", u8"enum"},
           test_case{u8"\\u{65}xport", u8"export"},
           test_case{u8"\\u{65}xtends", u8"extends"},
           test_case{u8"\\u{66}alse", u8"false"},
           test_case{u8"\\u{66}inally", u8"finally"},
           test_case{u8"\\u{66}or", u8"for"},
           test_case{u8"\\u{66}unction", u8"function"},
           test_case{u8"\\u{69}f", u8"if"},
           test_case{u8"\\u{69}mport", u8"import"},
           test_case{u8"\\u{69}n", u8"in"},
           test_case{u8"\\u{69}nstanceof", u8"instanceof"},
           test_case{u8"\\u{6e}ew", u8"new"},
           test_case{u8"\\u{6e}ull", u8"null"},
           test_case{u8"\\u{72}eturn", u8"return"},
           test_case{u8"\\u{73}uper", u8"super"},
           test_case{u8"\\u{73}witch", u8"switch"},
           test_case{u8"\\u{74}his", u8"this"},
           test_case{u8"\\u{74}hrow", u8"throw"},
           test_case{u8"\\u{74}rue", u8"true"},
           test_case{u8"\\u{74}ry", u8"try"},
           test_case{u8"\\u{74}ypeof", u8"typeof"},
           test_case{u8"\\u{76}ar", u8"var"},
           test_case{u8"\\u{76}oid", u8"void"},
           test_case{u8"\\u{77}hile", u8"while"},
           test_case{u8"\\u{77}ith", u8"with"},
       }) {
    {
      padded_string code(u8"import { " + tc.name + u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      tc.expected_identifier, variable_kind::_import}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_keywords_cannot_contain_escape_sequences, escape_sequence,
              offsets_matcher(&code, strlen(u8"import { "), u8"\\u{??}"))));
    }

    {
      padded_string code(u8"import { someFunction as " + tc.name +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      tc.expected_identifier, variable_kind::_import}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_keywords_cannot_contain_escape_sequences, escape_sequence,
              offsets_matcher(&code, strlen(u8"import { someFunction as "),
                              u8"\\u{??}"))));
    }

    {
      padded_string code(u8"import " + tc.name + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      tc.expected_identifier, variable_kind::_import}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_keywords_cannot_contain_escape_sequences, escape_sequence,
              offsets_matcher(&code, strlen(u8"import "), u8"\\u{??}"))));
    }

    {
      padded_string code(u8"import * as " + tc.name + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      tc.expected_identifier, variable_kind::_import}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_keywords_cannot_contain_escape_sequences, escape_sequence,
              offsets_matcher(&code, strlen(u8"import * as "), u8"\\u{??}"))));
    }
  }
}

TEST(test_parse, exported_names_can_be_named_keywords) {
  for (string8 export_name : keywords) {
    {
      string8 code = u8"export {someFunction as " + export_name + u8"};";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_export_use"));  // someFunction
      EXPECT_THAT(
          v.variable_uses,
          ElementsAre(spy_visitor::visited_variable_use{u8"someFunction"}));
    }

    {
      string8 code = u8"export * as " + export_name + u8" from 'other-module';";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(v.visits, IsEmpty());
    }
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

TEST(
    test_parse,
    imported_and_exported_names_can_be_reserved_keywords_with_escape_sequences) {
  for (string8 exported_name : {
           u8"\\u{61}wait",    u8"\\u{62}reak",      u8"\\u{63}ase",
           u8"\\u{63}atch",    u8"\\u{63}lass",      u8"\\u{63}onst",
           u8"\\u{63}ontinue", u8"\\u{64}ebugger",   u8"\\u{64}efault",
           u8"\\u{64}elete",   u8"\\u{64}o",         u8"\\u{65}lse",
           u8"\\u{65}num",     u8"\\u{65}xport",     u8"\\u{65}xtends",
           u8"\\u{66}alse",    u8"\\u{66}inally",    u8"\\u{66}or",
           u8"\\u{66}unction", u8"\\u{69}f",         u8"\\u{69}mport",
           u8"\\u{69}n",       u8"\\u{69}nstanceof", u8"\\u{6e}ew",
           u8"\\u{6e}ull",     u8"\\u{72}eturn",     u8"\\u{73}uper",
           u8"\\u{73}witch",   u8"\\u{74}his",       u8"\\u{74}hrow",
           u8"\\u{74}rue",     u8"\\u{74}ry",        u8"\\u{74}ypeof",
           u8"\\u{76}ar",      u8"\\u{76}oid",       u8"\\u{77}hile",
           u8"\\u{77}ith",     u8"\\u{79}ield",
       }) {
    {
      padded_string code(u8"import {" + exported_name +
                         u8" as someFunction} from 'somewhere';");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // someFunction
    }

    {
      padded_string code(u8"export {someFunction as " + exported_name + u8"};");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_export_use"));  // someFunction
    }

    {
      padded_string code(u8"export * as " + exported_name + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(v.visits, IsEmpty());
    }
  }
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
