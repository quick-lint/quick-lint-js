// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// These tests ensure that the parser implements the correct rules for .d.ts
// TypeScript definition files.

#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
TEST(Test_Parse_TypeScript_Definition, const_without_initializer_is_allowed) {
  test_parse_and_visit_statement(u8"export const c;"_sv, no_diags,
                                 typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, import_is_allowed) {
  test_parse_and_visit_module(u8"import {A, B, C} from 'mod';"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, type_alias_is_allowed) {
  test_parse_and_visit_module(u8"type T = null;"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, interface_is_allowed) {
  test_parse_and_visit_module(u8"interface I { }"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, variables_must_have_no_initializer) {
  test_parse_and_visit_module(u8"export const x;"_sv,  //
                              no_diags, typescript_definition_options);
  test_parse_and_visit_module(
      u8"export const x = null;"_sv,  //
      u8"               ^ Diag_DTS_Var_Cannot_Have_Initializer.equal\n"_diag
      u8"       ^^^^^ .declaring_token"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare const x;"_sv,  //
                              no_diags, typescript_definition_options);
  test_parse_and_visit_module(
      u8"declare const x = null;"_sv,  //
      u8"                ^ Diag_DTS_Var_Cannot_Have_Initializer.equal\n"_diag
      u8"        ^^^^^ .declaring_token"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(
      u8"export let x = null;"_sv,  //
      u8"             ^ Diag_DTS_Var_Cannot_Have_Initializer.equal\n"_diag
      u8"       ^^^ .declaring_token"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"export var x = null;"_sv,  //
      u8"             ^ Diag_DTS_Var_Cannot_Have_Initializer.equal\n"_diag
      u8"       ^^^ .declaring_token"_diag,
      typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, variables_require_export_or_declare) {
  test_parse_and_visit_module(
      u8"const x;"_sv,  //
      u8"^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"let x;"_sv,  //
      u8"^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"var x;"_sv,  //
      u8"^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare const x;"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export const x;"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"declare let x;"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export let x;"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"declare var x;"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export var x;"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, enums_require_export_or_declare) {
  test_parse_and_visit_module(
      u8"enum E {}"_sv,  //
      u8"^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"const enum E {}"_sv,  //
      u8"      ^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare enum E {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export enum E {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"declare const enum E {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export const enum E {}"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, classes_require_export_or_declare) {
  test_parse_and_visit_module(
      u8"class C {}"_sv,  //
      u8"^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"abstract class C {}"_sv,  //
      u8"         ^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare class C {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export class C {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"declare abstract class C {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export abstract class C {}"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, namespaces_require_export_or_declare) {
  test_parse_and_visit_module(
      u8"namespace ns {}"_sv,  //
      u8"^^^^^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"module ns {}"_sv,  //
      u8"^^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare namespace ns {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export namespace ns {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"declare module ns {}"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export module ns {}"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, named_module_require_declare) {
  test_parse_and_visit_module(
      u8"module 'mymod' {}"_sv,  //
      u8"       ^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"export module 'mymod' {}"_sv,  //
      u8"              ^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare module 'mymod' {}"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, function_requires_declare_or_export) {
  test_parse_and_visit_module(
      u8"function f();"_sv,  //
      u8"^^^^^^^^ Diag_DTS_Missing_Declare_Or_Export.declaring_token\n"_diag
      u8"` .expected"_diag,
      typescript_definition_options);

  test_parse_and_visit_module(u8"declare function f();"_sv, no_diags,
                              typescript_definition_options);
  test_parse_and_visit_module(u8"export function f();"_sv, no_diags,
                              typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, function_must_have_no_body) {
  test_parse_and_visit_module(
      u8"declare function f() {}"_sv,  //
      u8"                     ^ Diag_DTS_Function_Cannot_Have_Body"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"export function f() {}"_sv,  //
      u8"                    ^ Diag_DTS_Function_Cannot_Have_Body"_diag,
      typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, function_cannot_be_async) {
  test_parse_and_visit_module(
      u8"declare async function f();"_sv,  //
      u8"        ^^^^^ Diag_DTS_Function_Cannot_Be_Async"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"export async function f();"_sv,  //
      u8"       ^^^^^ Diag_DTS_Function_Cannot_Be_Async"_diag,
      typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition, function_cannot_be_generator) {
  test_parse_and_visit_module(
      u8"declare function* f();"_sv,  //
      u8"                ^ Diag_DTS_Function_Cannot_Be_Generator"_diag,
      typescript_definition_options);
  test_parse_and_visit_module(
      u8"export function* f();"_sv,  //
      u8"               ^ Diag_DTS_Function_Cannot_Be_Generator"_diag,
      typescript_definition_options);
}

TEST(Test_Parse_TypeScript_Definition,
     statements_inside_namespace_do_not_require_export_or_declare) {
  for (String8_View statement : {
           u8"const enum E {}"_sv,
           u8"enum E {}"_sv,
           u8"namespace ns {}"_sv,
           u8"module ns {}"_sv,
           u8"const x;"_sv,
           u8"let x;"_sv,
           u8"var x;"_sv,
           u8"interface I {}"_sv,
           u8"type T = null;"_sv,
           u8"function f();"_sv,
       }) {
    test_parse_and_visit_module(
        concat(u8"declare namespace ns { "_sv, statement, u8" }"_sv), no_diags,
        typescript_definition_options);
    test_parse_and_visit_module(
        concat(u8"export namespace ns { "_sv, statement, u8" }"_sv), no_diags,
        typescript_definition_options);
    test_parse_and_visit_module(
        concat(u8"declare module ns { "_sv, statement, u8" }"_sv), no_diags,
        typescript_definition_options);
    test_parse_and_visit_module(
        concat(u8"export module ns { "_sv, statement, u8" }"_sv), no_diags,
        typescript_definition_options);
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
