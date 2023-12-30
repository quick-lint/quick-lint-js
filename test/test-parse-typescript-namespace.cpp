// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Namespace : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Namespace, not_supported_in_vanilla_javascript) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"namespace ns {}"_sv,  //
      u8"^^^^^^^^^ Diag_TypeScript_Namespaces_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                            "visit_variable_declaration",   // ns
                        }));
}

TEST_F(Test_Parse_TypeScript_Namespace, empty_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"namespace ns {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"module ns {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, missing_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns "_sv,                                               //
        u8"            ` Diag_Missing_Body_For_TypeScript_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // ns
                              "visit_end_of_module",         //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns\nconsole.log('hello');"_sv,                         //
        u8"            ` Diag_Missing_Body_For_TypeScript_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // ns
                              "visit_variable_use",          // console
                              "visit_end_of_module",         //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, incomplete_body) {
  {
    // TODO(strager): Report a namespace-specific diagnostic.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { "_sv,                            //
        u8"             ^ Diag_Unclosed_Code_Block"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // implicit }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export "_sv,                     //
        u8"             ^ Diag_Unclosed_Code_Block"_diag,  //
        u8"Diag_Missing_Token_After_Export"_diag, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // implicit }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}))
        << "the namespace should be empty";
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, namespace_cannot_have_string_name) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace 'my name space' {}"_sv,  //
        u8"          ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"module 'my name space' {}"_sv,  //
        u8"       ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_cannot_have_newline_after_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"namespace\nns {}"_sv,                                              //
        u8"^^^^^^^^^ Diag_Newline_Not_Allowed_After_Namespace_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"module\nns {}"_sv,                                              //
        u8"^^^^^^ Diag_Newline_Not_Allowed_After_Namespace_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       keyword_with_following_newline_is_variable_name) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"namespace\nns\n{}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // namespace
                              "visit_variable_use",       // ns
                              "visit_enter_block_scope",  // {
                              "visit_exit_block_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"namespace", u8"ns"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"module\nns\n{}"_sv, no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // module
                              "visit_variable_use",       // ns
                              "visit_enter_block_scope",  // {
                              "visit_exit_block_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"module", u8"ns"}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_name_can_be_contextual_keyword) {
  for (String8 name :
       contextual_keywords - Dirty_Set<String8>{u8"let", u8"static"}) {
    Padded_String code(concat(u8"namespace "_sv, name, u8" {}"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_name_can_contain_subnamespaces_with_dot) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"namespace ns.subns {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"namespace mainNamespace._.subSubNamespace.yup {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // mainNamespace
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"mainNamespace"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       subnamespace_name_can_be_contextual_keyword_and_more) {
  for (String8 name : contextual_keywords | strict_only_reserved_keywords |
                          Dirty_Set<String8>{u8"await", u8"yield"}) {
    Spy_Visitor p = test_parse_and_visit_module(
        concat(u8"namespace ns."_sv, name, u8" {}"_sv), no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, namespace_can_contain_exports) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export function f() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",      // {
                              "visit_enter_function_scope",       // f
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_exit_namespace_scope",       // }
                              "visit_variable_declaration",       // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv),
                                  non_empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"namespace ns { export class C {} }"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv),
                                  non_empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export var a; export const b = null; export let c; }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // a
                              "visit_variable_declaration",   // b
                              "visit_variable_declaration",   // c
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({var_noinit_decl(u8"a"_sv), const_init_decl(u8"b"_sv),
                          let_noinit_decl(u8"c"_sv),
                          non_empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, namespace_disallows_exporting_default) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export default Z; }"_sv,  //
        u8"                      ^^^^^^^ Diag_TypeScript_Namespace_Cannot_Export_Default.default_keyword\n"_diag
        u8"^^^^^^^^^ .namespace_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",        // {
                              "visit_variable_export_default_use",  // Z
                              "visit_exit_namespace_scope",         // }
                              "visit_variable_declaration",         // ns
                              "visit_end_of_module",                //
                          }));
  }

  test_parse_and_visit_module(
      u8"namespace ns { export default 2+2; }"_sv,               //
      u8"Diag_TypeScript_Namespace_Cannot_Export_Default"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"namespace ns { export default class C {} }"_sv,  //
      u8"                      ^^^^^^^ Diag_TypeScript_Namespace_Cannot_Export_Default.default_keyword\n"_diag
      u8"^^^^^^^^^ .namespace_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"namespace ns { export default function f() {} }"_sv,    //
      u8"Diag_TypeScript_Namespace_Cannot_Export_Default"_diag,  //
      typescript_options);
}

// See NOTE[non-empty-namespace].
TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_with_empty_subnamespaces_is_empty) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"namespace ns { namespace subns {} }"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"subns"_sv),
                                  empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { namespace subns {} module subns2 {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"subns"_sv),
                                  empty_namespace_decl(u8"subns2"_sv),
                                  empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { declare namespace subns {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"subns"_sv),
                                  empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export declare namespace subns {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"subns"_sv),
                                  empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { export namespace subns {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"subns"_sv),
                                  empty_namespace_decl(u8"ns"_sv)}));
  }

  for (String8_View body : {
           u8"export const enum E {}"_sv,          //
           u8"export interface I {}"_sv,           //
           u8"export declare interface I {}"_sv,   //
           u8"export type C = A;"_sv,              //
           u8"export declare type T = null;"_sv,   //
           u8"export declare const enum E {}"_sv,  //
           u8"import myns = ns;"_sv,               //
           u8"import myns = ns.subns;"_sv,         //
           u8"import await = ns.subns;"_sv,        //
           u8"const enum E {}"_sv,                 //
           u8"declare interface I {}"_sv,          //
           u8"declare type T = null;"_sv,          //
           u8"declare const enum E {}"_sv,         //

           // These examples are invalid TypeScript, but the TypeScript compiler
           // still treats them as ambient statements.
           u8"import await from 'mod';"_sv,            //
           u8"import fs from 'fs';"_sv,                //
           u8"import fs = require('fs');"_sv,          //
           u8"import {readFile} from 'fs';"_sv,        //
           u8"import * as fs from 'fs';"_sv,           //
           u8"import 'fs';"_sv,                        //
           u8"import type T from 'module';"_sv,        //
           u8"import type {T} from 'module';"_sv,      //
           u8"import type * as M from 'module';"_sv,   //
           u8"import type from 'module';"_sv,          //
           u8"export declare import a = otherns;"_sv,  //
           u8"declare import a = otherns;"_sv,         //
           u8"default:"_sv,                            //
           u8":"_sv,                                   //
           u8"?"_sv,                                   //
           u8"extends"_sv,                             //

           // NOTE[ambiguous-ambient-statement-in-namespace]: These examples are
           // invalid TypeScript. The TypeScript compiler treats them as either
           // ambient or non-ambient statements depending on what is being
           // exported. quick-lint-js's parser does not have the information
           // needed to determine whether they are ambient or non-ambient. Be
           // conservative and say that such constructs are always empty.
           u8"export * from 'module';"_sv,          //
           u8"export {a, b, c} from 'module';"_sv,  //
           u8"export type {A};"_sv,                 //
       }) {
    Padded_String code(concat(u8"namespace ns { "_sv, body, u8" }"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_module();
    ASSERT_THAT(p.variable_declarations, ::testing::Not(IsEmpty()));
    EXPECT_EQ(p.variable_declarations[p.variable_declarations.size() - 1],
              empty_namespace_decl(u8"ns"_sv))
        << "namespace should have been declared as empty";
    // Ignore p.errors.
  }
}

// See NOTE[non-empty-namespace].
TEST_F(Test_Parse_TypeScript_Namespace, namespace_with_statement_is_non_empty) {
  for (String8_View body : {
           u8";"_sv,                                   //
           u8"x = y;"_sv,                              //
           u8"export async function f() {}"_sv,        //
           u8"export function f() {}"_sv,              //
           u8"export abstract class C {}"_sv,          //
           u8"export class C {}"_sv,                   //
           u8"export let x = 42;"_sv,                  //
           u8"export const x = 42;"_sv,                //
           u8"export var x = 42;"_sv,                  //
           u8"export import A = otherns;"_sv,          //
           u8"export enum E {}"_sv,                    //
           u8"export declare enum E {}"_sv,            //
           u8"export declare const x: any;"_sv,        //
           u8"export declare var x: any;"_sv,          //
           u8"export declare let x: any;"_sv,          //
           u8"export declare abstract class C {}"_sv,  //
           u8"export declare class C {}"_sv,           //
           u8"export declare function f();"_sv,        //
           u8"declare();"_sv,                          //
           u8"import('url')"_sv,                       //
           u8"import.meta"_sv,                         //
           u8"function f() {}"_sv,                     //
           u8"var x: any;"_sv,                         //
           u8"const x: any = null;"_sv,                //
           u8"let x: any;"_sv,                         //
           u8"let();"_sv,                              //
           u8"abstract = null;"_sv,                    //
           u8"abstract class C {}"_sv,                 //
           u8"class C {}"_sv,                          //
           u8"declare enum E {}"_sv,                   //
           u8"declare const x: any;"_sv,               //
           u8"declare var x: any;"_sv,                 //
           u8"declare let x: any;"_sv,                 //
           u8"declare abstract class C {}"_sv,         //
           u8"declare class C {}"_sv,                  //
           u8"declare function f();"_sv,               //
           u8"async function f() {}"_sv,               //
           u8"async => {};"_sv,                        //
           u8"async = null;"_sv,                       //
           u8"async: const enum E {};"_sv,             //
           u8"false;"_sv,                              //
           u8"[1, 2, 3];"_sv,                          //
           u8"'use strict';"_sv,                       //
           u8"await = null;"_sv,                       //
           u8"yield = null;"_sv,                       //
           u8"switch (x) {}"_sv,                       //
           u8"throw new Error();"_sv,                  //
           u8"try {} catch {}"_sv,                     //
           u8"try {} catch (e) {}"_sv,                 //
           u8"try {} finally {}"_sv,                   //
           u8"do {} while (false)"_sv,                 //
           u8"for (;;);"_sv,                           //
           u8"while (true);"_sv,                       //
           u8"with (obj) {}"_sv,                       //
           u8"if (true) {}"_sv,                        //
           u8"if (true) {} else {}"_sv,                //
           u8"debugger;"_sv,                           //
           u8"enum E {}"_sv,                           //
           u8"{}"_sv,                                  //

           // These examples are invalid TypeScript, but our error recovery
           // should correct the code. The corrected code is non-ambient .
           u8"*function f() {}"_sv,  // function* f() {}
           u8"catch (e) {}"_sv,      // try {} catch (e) {}
           u8"finally {}"_sv,        // try {} finally {}
           u8"else {}"_sv,           // if (cond) {} else {}

           // These examples are invalid TypeScript, but the compiler still
           // treats them as non-ambient statements.
           u8"export default class C {}"_sv,           //
           u8"export declare async function f();"_sv,  //
           u8"export 2+2;"_sv,                         //
           u8"export = value;"_sv,                     //
           u8"import var from 'module';"_sv,           //
           u8"import \\u{76}ar from 'module';"_sv,     //
           u8"export;"_sv,                             //
           u8"declare: const enum E {}"_sv,            //
           u8"mylabel: const enum E {}"_sv,            //
           u8"let: const enum E {}"_sv,                //
           u8"abstract: const enum E {}"_sv,           //
           u8"await: const enum E {}"_sv,              //
           u8"yield: const enum E {}"_sv,              //
           u8"interface: const enum E {}"_sv,          //
           u8"type: const enum E {}"_sv,               //
           u8"module: const enum E {}"_sv,             //
           u8"namespace: const enum E {}"_sv,          //
           u8"await null;"_sv,                         //
           u8"yield null;"_sv,                         //
           u8"yield *null;"_sv,                        //
           u8"\\u{69}\\u{66}"_sv,                      //
           u8"protected"_sv,                           //
           u8"return 42;"_sv,                          //
           u8"return;"_sv,                             //
           u8"continue;"_sv,                           //
           u8"break;"_sv,                              //
           u8"case 42:"_sv,

           // TODO(strager):
           // u8"export declare import a from 'b';"_sv,
       }) {
    Padded_String code(concat(u8"namespace ns { "_sv, body, u8" }"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_module();
    ASSERT_THAT(p.variable_declarations, ::testing::Not(IsEmpty()));
    EXPECT_EQ(p.variable_declarations[p.variable_declarations.size() - 1],
              non_empty_namespace_decl(u8"ns"_sv))
        << "namespace should have been declared as non-empty";
    // Ignore p.errors.
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"namespace ns { x = y; }"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({non_empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { namespace\n notANamespace\n {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({non_empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { module\n notANamespace\n {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({non_empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_with_statement_in_subnamespace_is_non_empty) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { namespace subns { ; } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({non_empty_namespace_decl(u8"subns"_sv),
                                  non_empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { namespace subns { x = y; } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({non_empty_namespace_decl(u8"subns"_sv),
                                  non_empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       empty_namespace_after_non_empty_namespace) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"namespace nonempty { ; } namespace empty {}"_sv, no_diags,
      typescript_options);
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({non_empty_namespace_decl(u8"nonempty"_sv),
                                empty_namespace_decl(u8"empty"_sv)}));
}

TEST_F(Test_Parse_TypeScript_Namespace,
       empty_namespace_after_unrelated_statements) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"let x = 42; namespace empty {}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.variable_declarations,
              ElementsAre(::testing::_, empty_namespace_decl(u8"empty"_sv)));
}

TEST_F(Test_Parse_TypeScript_Namespace, namespace_alias) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import A = ns;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(u8"A"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_alias_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import A = ns;"_sv,  //
        u8"^^^^^^ Diag_TypeScript_Import_Alias_Not_Allowed_In_JavaScript.import_keyword\n"_diag
        u8"         ^ .equal"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_alias_cannot_be_used_with_declare_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare import A = ns;"_sv,                             //
        u8"^^^^^^^ Diag_Import_Cannot_Have_Declare_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",           //
                          }));
  }
}

// See NOTE[TypeScript-namespace-alias-name].
TEST_F(
    Test_Parse_TypeScript_Namespace,
    namespace_alias_can_be_named_contextual_keyword_including_await_and_yield) {
  for (String8_View name : contextual_keywords | strict_only_reserved_keywords |
                               Dirty_Set<String8>{u8"await", u8"yield"}) {
    Spy_Visitor p =
        test_parse_and_visit_module(concat(u8"import "_sv, name, u8" = ns;"_sv),
                                    no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_alias_cannot_be_named_most_reserved_keywords) {
  for (String8_View name : disallowed_binding_identifier_keywords) {
    Spy_Visitor p = test_parse_and_visit_module(
        concat(u8"import "_sv, name, u8" = ns;"_sv),
        u8"Diag_Cannot_Import_Variable_Named_Keyword"_diag, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(name)}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace, import_alias_of_namespace_member) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"import A = ns.B;"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(u8"A"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"import A = ns.subns.B;"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  for (String8_View subns_name : contextual_keywords |
                                     strict_only_reserved_keywords |
                                     Dirty_Set<String8>{u8"await", u8"yield"}) {
    Spy_Visitor p = test_parse_and_visit_module(
        concat(u8"import A = ns."_sv, subns_name, u8";"_sv), no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       import_alias_requires_semicolon_or_newline) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import A = ns nextStatement"_sv,                              //
        u8"             ` Diag_Missing_Semicolon_After_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_variable_use",            // nextStatement
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_can_be_contextual_keyword_in_import_alias) {
  for (String8 name : contextual_keywords) {
    Padded_String code(concat(u8"import A = "_sv, name, u8".Member;"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // (name)
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
  }
}

TEST_F(Test_Parse_TypeScript_Namespace,
       namespace_member_can_be_contextual_keyword_in_import_alias) {
  for (String8 name : contextual_keywords) {
    Padded_String code(concat(u8"import A = ns."_sv, name, u8";"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
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
