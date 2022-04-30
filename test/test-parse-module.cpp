// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
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
                    u8"x", variable_kind::_let, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export let x = 42;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export var x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export var x = 42;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"export const x = null;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_const,
                    variable_init_kind::initialized_with_equals}));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_export_default_variable,  //
                              declaring_token, strlen(u8"export default "),
                              declaration_kind)));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"export {x}"), u8"")));
  }

  {
    padded_string code(u8"export * from 'other' console.log();"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // console
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"export * from 'other'"), u8"")));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"export default x+y"), u8"")));
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
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_semicolon_after_statement,  //
                    where, strlen(u8"export default async () => {}"), u8"")));
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

  {
    spy_visitor v = parse_and_visit_statement(u8"export {myVar as 'name'};"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myVar"}));
  }
}

TEST(test_parse, exporting_by_string_name_is_only_allowed_for_export_from) {
  {
    padded_string code(u8"export {'name'};"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_exporting_string_name_only_allowed_for_export_from,  //
            export_name, strlen(u8"export {"), u8"'name'")));
  }
}

TEST(test_parse, exported_variables_cannot_be_named_reserved_keywords) {
  for (string8 keyword : strict_reserved_keywords) {
    padded_string code(u8"export {" + keyword + u8"};");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_export_variable_named_keyword,  //
                    export_name, strlen(u8"export {"), keyword)));
  }

  for (string8 keyword : strict_reserved_keywords) {
    padded_string code(u8"export {" + keyword + u8" as thing};");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.variable_uses, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_export_variable_named_keyword,  //
                    export_name, strlen(u8"export {"), keyword)));
  }

  // TODO(strager): Test u8"await" and u8"yield".
  // TODO(#73): Disallow 'protected', 'implements', etc.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    string8 exported_variable = escape_first_character_in_keyword(keyword);

    {
      padded_string code(u8"export {" + exported_variable + u8"};");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_uses, IsEmpty());
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"export {"), u8"\\u{??}")));
    }

    {
      padded_string code(u8"export {" + exported_variable + u8" as thing};");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_uses, IsEmpty());
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"export {"), u8"\\u{??}")));
    }
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
    spy_visitor v =
        parse_and_visit_statement(u8"export * as 'mother' from 'other';"_sv);
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

  {
    // Keywords are legal, even if Unicode-escaped.
    spy_visitor v =
        parse_and_visit_statement(u8"export {\\u{76}ar} from 'fs';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    // Keywords are legal, even if Unicode-escaped.
    spy_visitor v = parse_and_visit_statement(
        u8"export {\\u{76}ar as \\u{69}f} from 'fs';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"export {'name'} from 'other';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"export {'name' as 'othername'} from 'other';"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, invalid_export_expression) {
  {
    spy_visitor v;
    padded_string code(u8"export stuff;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_exporting_requires_curlies,  //
                              names, strlen(u8"export "), u8"stuff")));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // stuff
  }

  {
    spy_visitor v;
    padded_string code(u8"export a, b, c;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        // TODO(strager): Report diag_exporting_requires_curlies instead.
        ElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_exporting_requires_default,  //
                              expression, strlen(u8"export "), u8"a, b, c")));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // a
                                      "visit_variable_use",    // b
                                      "visit_variable_use"));  // c
  }

  {
    spy_visitor v;
    padded_string code(u8"export a, b, c+d;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                // TODO(strager): Should we report
                // diag_exporting_requires_curlies instead?
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_exporting_requires_default,  //
                    expression, strlen(u8"export "), u8"a, b, c+d")));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_exporting_requires_default,  //
                              expression, strlen(u8"export "), u8"2 + x")));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }
}

TEST(test_parse, invalid_export) {
  {
    spy_visitor v;
    padded_string code(u8"export ;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_token_after_export,  //
                              export_token, 0, u8"export")));
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"export "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_token_after_export,  //
                              export_token, 0, u8"export")));
    EXPECT_THAT(v.visits, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"export = x"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unexpected_token_after_export,  //
                              unexpected_token, strlen(u8"export "), u8"=")));
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
    spy_visitor v = parse_and_visit_statement(
        u8"import {'read file sync' as readFileSync} from 'fs';"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"readFileSync", variable_kind::_import,
                    variable_init_kind::normal}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"import fs, {readFileSync} from 'fs';"_sv);
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(
            spy_visitor::visited_variable_declaration{
                u8"fs", variable_kind::_import, variable_init_kind::normal},
            spy_visitor::visited_variable_declaration{
                u8"readFileSync", variable_kind::_import,
                variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"import fsDefault, * as fsExports from 'fs';");
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"fsDefault", variable_kind::_import,
                        variable_init_kind::normal},
                    spy_visitor::visited_variable_declaration{
                        u8"fsExports", variable_kind::_import,
                        variable_init_kind::normal}));
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
        ElementsAre(DIAG_TYPE_3_OFFSETS(
            &code,
            diag_expected_as_before_imported_namespace_alias,               //
            star_through_alias_token, strlen(u8"import "), u8"* myExport",  //
            star_token, strlen(u8"import "), u8"*",                         //
            alias, strlen(u8"import * "), u8"myExport")));
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
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_expected_from_before_module_specifier,  //
                    module_specifier, strlen(u8"import { x } "), u8"'other'")));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // x
  }

  {
    padded_string code(u8"import { x } ;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_expected_from_and_module_specifier,  //
                    where, strlen(u8"import { x }"), u8"")));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // x
  }
}

TEST(test_parse, import_as_invalid_token) {
  {
    padded_string code(u8"import {myExport as 'string'} from 'module';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_expected_variable_name_for_import_as,  //
            unexpected_token, strlen(u8"import {myExport as "), u8"'string'")));
  }

  {
    padded_string code(u8"import {'myExport' as 'string'} from 'module';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_expected_variable_name_for_import_as,  //
                    unexpected_token, strlen(u8"import {'myExport' as "),
                    u8"'string'")));
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
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_name_of_exported_function,  //
                    function_keyword, strlen(u8"export "), u8"function")));
  }

  {
    padded_string code(u8"export async function() {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_name_of_exported_function,  //
                              function_keyword, strlen(u8"export async "),
                              u8"function")));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_name_of_exported_class,  //
                              class_keyword, strlen(u8"export "), u8"class")));
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
      spy_visitor v = parse_and_visit_statement(u8"import { exportedName as " +
                                                name + u8" } from 'other';");
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"import { 'exportedName' as " + name + u8" } from 'other';");
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
  for (string8 name : strict_reserved_keywords) {
    {
      padded_string code(u8"import { " + name + u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration"));  // (name)
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_import_variable_named_keyword,  //
                      import_name, strlen(u8"import { "), name)));
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
          ElementsAre(DIAG_TYPE_OFFSETS(
              &code, diag_cannot_import_variable_named_keyword,  //
              import_name, strlen(u8"import { someFunction as "), name)));
    }

    {
      padded_string code(u8"import { 'someFunction' as " + name +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              name, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              &code, diag_cannot_import_variable_named_keyword,  //
              import_name, strlen(u8"import { 'someFunction' as "), name)));
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
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_import_variable_named_keyword,  //
                      import_name, strlen(u8"import "), name)));
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
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_import_variable_named_keyword,  //
                      import_name, strlen(u8"import * as "), name)));
    }
  }

  // TODO(strager): Test u8"await" and u8"yield".
  // TODO(#73): Disallow 'protected', 'implements', etc.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    string8 imported_variable = escape_first_character_in_keyword(keyword);

    {
      padded_string code(u8"import { " + imported_variable +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              keyword, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"import { "), u8"\\u{??}")));
    }

    {
      padded_string code(u8"import { someFunction as " + imported_variable +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              keyword, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"import { someFunction as "),
                      u8"\\u{??}")));
    }

    {
      padded_string code(u8"import { 'someFunction' as " + imported_variable +
                         u8" } from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              keyword, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"import { 'someFunction' as "),
                      u8"\\u{??}")));
    }

    {
      padded_string code(u8"import " + imported_variable + u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              keyword, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"import "), u8"\\u{??}")));
    }

    {
      padded_string code(u8"import * as " + imported_variable +
                         u8" from 'other';");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              keyword, variable_kind::_import, variable_init_kind::normal}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"import * as "), u8"\\u{??}")));
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
                    u8"someFunction", variable_kind::_import,
                    variable_init_kind::normal}));
  }
}

TEST(
    test_parse,
    imported_and_exported_names_can_be_reserved_keywords_with_escape_sequences) {
  for (string8 keyword : keywords) {
    string8 exported_name = escape_first_character_in_keyword(keyword);

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
