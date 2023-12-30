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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Module : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Module, type_only_import) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type { T } from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type {T as U} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"U"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type T from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type * as M from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // M
                              "visit_end_of_module",
                          }));
    // TODO(#788): Assert import_module_decl instead.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"M"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       type_only_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (String8 name :
       contextual_keywords - Dirty_Set<String8>{u8"from", u8"let"}) {
    {
      Padded_String code(
          concat(u8"import type "_sv, name, u8" from 'mod';"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                  typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_type_decl(name)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       type_only_import_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type {T} from 'mod';"_sv,  //
        u8"       ^^^^ Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       type_only_import_cannot_import_default_and_named) {
  {
    Test_Parser p(u8"import type A, {B} from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_variable_declaration",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_decl(u8"B"_sv), import_type_decl(u8"A"_sv)}))
        << "B should be imported as an 'import' not an 'import_type' in case "
           "the user thought that the 'type' keyword only applied to 'A'";
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^^^^ Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type A, * as B from 'mod';"_sv,  //
        u8"       ^^^^ Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_variable_declaration",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_decl(u8"B"_sv), import_type_decl(u8"A"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, inline_type_import) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {type T} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"import {type T, type U} from 'mod';"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {import_type_decl(u8"T"_sv), import_type_decl(u8"U"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {type T as U} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"U"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, mixed_inline_type_and_normal_import) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {type T, f} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // f
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_type_decl(u8"T"_sv), import_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {f, type T} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // f
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_decl(u8"f"_sv), import_type_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       inline_type_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (String8 name : contextual_keywords - Dirty_Set<String8>{u8"let"}) {
    {
      Padded_String code(
          concat(u8"import {type "_sv, name, u8"} from 'mod';"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                  typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_type_decl(name)}));
    }

    {
      Padded_String code(
          concat(u8"import {type "_sv, name, u8", other} from 'mod';"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_module(code.string_view(), no_diags,
                                                  typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                                "visit_variable_declaration",  // other
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {import_type_decl(name), import_decl(u8"other"_sv)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       inline_type_import_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {type T} from 'mod';"_sv,  //
        u8"        ^^^^ Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import {type as} from 'mod';"_sv,  //
        u8"        ^^^^ Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // as
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, mixed_inline_type_and_type_only_import) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import type {type T} from 'mod';"_sv,  //
        u8"             ^^^^ Diag_TypeScript_Inline_Type_Import_Not_Allowed_In_Type_Only_Import.inline_type_keyword\n"_diag
        u8"       ^^^^ .type_only_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, type_only_export) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export type { T };"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export type {T as U};"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export type {T} from 'mod';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       type_only_export_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export type {T};"_sv,  //
        u8"       ^^^^ Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {T} from 'othermod';"_sv,  //
        u8"       ^^^^ Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type * from 'othermod';"_sv,  //
        u8"       ^^^^ Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Module, inline_type_export) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export {type T};"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export {type T, type U};"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"U"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export {type T as U};"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       inline_type_export_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export {type T};"_sv,  //
        u8"        ^^^^ Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export {type as};"_sv,  //
        u8"        ^^^^ Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // as
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, mixed_inline_type_and_type_only_export) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export type {type T};"_sv,  //
        u8"             ^^^^ Diag_TypeScript_Inline_Type_Export_Not_Allowed_In_Type_Only_Export.inline_type_keyword\n"_diag
        u8"       ^^^^ .type_only_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_type_from) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type * from 'other';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type * as mother from 'other';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type * as 'mother' from 'other';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {} from 'other';"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {util1, util2, util3} from 'other';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {readFileSync as readFile} from 'fs';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {promises as default} from 'fs';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  for (String8 keyword : keywords) {
    Padded_String code(
        concat(u8"export type {"_sv, keyword, u8"} from 'other';"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_statement(code.string_view(), no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Keywords are legal, even if Unicode-escaped.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {\\u{76}ar} from 'fs';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Keywords are legal, even if Unicode-escaped.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {\\u{76}ar as \\u{69}f} from 'fs';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {'name'} from 'other';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export type {'name' as 'othername'} from 'other';"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Module, import_require) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import fs = require('node:fs');"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv)}));
  }

  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"import type fs = require('node:fs');"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv)}))
        << "See NOTE[TypeScript-type-import-alias].";
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       import_require_variable_cannot_be_await_or_yield) {
  test_parse_and_visit_module(
      u8"import await = require('node:fs');"_sv,
      u8"       ^^^^^ Diag_Cannot_Import_Variable_Named_Keyword"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"import yield = require('node:fs');"_sv,
      u8"       ^^^^^ Diag_Cannot_Import_Variable_Named_Keyword"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Module, export_interface) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export interface I {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       export_interface_disallows_newline_after_interface_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export interface\nI {}"_sv,  //
        u8"       ^^^^^^^^^ Diag_Newline_Not_Allowed_After_Interface_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_default_interface) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default interface I {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv)}));
  }

  // A newline is allowed after 'interface' (unlike with 'export interface').
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default interface\nI {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_abstract_class) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export abstract class C { abstract m(); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_property_declaration",    // m
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default abstract class C { abstract m(); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_property_declaration",    // m
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default abstract class { abstract m(); }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_property_declaration",    // m
                              "visit_exit_class_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       export_abstract_class_cannot_have_newline_after_abstract) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export abstract\nclass C { abstract m(); }"_sv,  //
        u8"       ^^^^^^^^ Diag_Newline_Not_Allowed_After_Abstract_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export abstract\nclass C { abstract m(); }"_sv,  //
        u8"       ^^^^^^^^ Diag_Newline_Not_Allowed_After_Abstract_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default abstract\nclass C { abstract m(); }"_sv,          //
        u8"Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"abstract"}))
        << "'abstract' should be treated as a variable name, not a keyword";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export namespace ns {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export module ns {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       exported_namespace_cannot_have_string_name) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export namespace 'my name space' {}"_sv,  //
        u8"                 ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export module 'my name space' {}"_sv,  //
        u8"              ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       export_namespace_disallows_newline_after_namespace_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export namespace\nns {}"_sv,  //
        u8"       ^^^^^^^^^ Diag_Newline_Not_Allowed_After_Namespace_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_enum) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export enum E {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_const_enum) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export const enum E {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // E
                              "visit_enter_enum_scope",      // {
                              "visit_exit_enum_scope",       // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({enum_decl(u8"E"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_type_alias) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export type T = C;"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // C
                              "visit_exit_type_scope",       //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       export_type_alias_disallows_newline_after_type_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export type\nA = any;"_sv,                                      //
        u8"       ^^^^ Diag_Newline_Not_Allowed_After_Type_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_exit_type_scope",       //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_import_alias) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export import A = B;"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(u8"A"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Module,
       export_import_alias_cannot_be_named_certain_keywords) {
  // TODO[TypeScript-export-namespace-alias-keyword-name]: Disallow 'await',
  // 'implements', etc. (strict_reserved_keywords).
  for (String8_View keyword : disallowed_binding_identifier_keywords) {
    Spy_Visitor p = test_parse_and_visit_module(
        concat(u8"export import "_sv, keyword, u8" = B;"_sv),
        u8"Diag_Cannot_Import_Variable_Named_Keyword"_diag, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // (keyword)
                              "visit_variable_namespace_use",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_alias_decl(keyword)}));
  }
}

// TODO(#795): Report an error for other kinds of 'export import', such as
// 'export import fs from "fs";'.

TEST_F(Test_Parse_TypeScript_Module,
       import_cannot_be_used_with_declare_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare import fs from 'fs';"_sv,                       //
        u8"^^^^^^^ Diag_Import_Cannot_Have_Declare_Keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_end_of_module",         //
                          }));
  }

  test_parse_and_visit_module(
      u8"namespace ns { declare import fs from 'fs'; }"_sv,  //
      u8"Diag_Import_Cannot_Have_Declare_Keyword"_diag,      //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Module,
       export_equal_is_not_allowed_in_javascript) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export = foo;"_sv,  //
        u8"       ^ Diag_TypeScript_Export_Equal_Not_Allowed_In_JavaScript.equal\n"_diag
        u8"^^^^^^ .export_keyword"_diag,  //
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_end_of_module",        //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_equal_with_identifier) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export = foo;"_sv, no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_end_of_module",        //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_equal_with_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export = foo.bar;"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export = new foo();"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"export = (foo);"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Module, export_equal_requires_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export = foo bar"_sv,                                        //
        u8"            ` Diag_Missing_Semicolon_After_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_variable_use",         // bar
                              "visit_end_of_module",        //
                          }));
  }

  {
    // ASI:
    Spy_Visitor p = test_parse_and_visit_module(u8"export = foo\nbar"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_variable_use",         // bar
                              "visit_end_of_module",        //
                          }));
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
