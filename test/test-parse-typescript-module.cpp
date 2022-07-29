// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_module : public test_parse_expression {};

TEST_F(test_parse_typescript_module, type_only_import) {
  {
    test_parser& p = this->errorless_parser(
        u8"import type { T } from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"import type {T as U} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"U")));
  }

  {
    test_parser& p = this->errorless_parser(u8"import type T from 'mod';"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"import type * as M from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // M
                                      "visit_end_of_module"));
    // TODO(#788): Assert import_module_decl instead.
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"M")));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name :
       contextual_keywords - dirty_set<string8>{u8"from", u8"let"}) {
    {
      padded_string code(u8"import type " + name + u8" from 'mod';");
      SCOPED_TRACE(code);
      test_parser& p =
          this->errorless_parser(code.string_view(), typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_end_of_module"));
      EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(name)));
    }
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"import type {T} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(),
                    diag_typescript_type_only_import_not_allowed_in_javascript,
                    type_keyword, strlen(u8"import "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_cannot_import_default_and_named) {
  {
    test_parser p(u8"import type A, {B} from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // A
                                      "visit_variable_declaration",  // B
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_type_decl(u8"A"), import_decl(u8"B")))
        << "B should be imported as an 'import' not an 'import_type' in case "
           "the user thought that the 'type' keyword only applied to 'A'";
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_type_only_import_cannot_import_default_and_named,
            type_keyword, strlen(u8"import "), u8"type")));
  }

  {
    test_parser p(u8"import type A, * as B from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // A
                                      "visit_variable_declaration",  // B
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_type_decl(u8"A"), import_decl(u8"B")));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_type_only_import_cannot_import_default_and_named,
            type_keyword, strlen(u8"import "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, inline_type_import) {
  {
    test_parser& p = this->errorless_parser(u8"import {type T} from 'mod';"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"T")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"import {type T, type U} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_type_decl(u8"T"), import_type_decl(u8"U")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"import {type T as U} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"U")));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_normal_import) {
  {
    test_parser& p = this->errorless_parser(
        u8"import {type T, f} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // f
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_type_decl(u8"T"), import_decl(u8"f")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"import {f, type T} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // f
                                      "visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_decl(u8"f"), import_type_decl(u8"T")));
  }
}

TEST_F(test_parse_typescript_module,
       inline_type_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name : contextual_keywords - dirty_set<string8>{u8"let"}) {
    {
      padded_string code(u8"import {type " + name + u8"} from 'mod';");
      SCOPED_TRACE(code);
      test_parser& p =
          this->errorless_parser(code.string_view(), typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_end_of_module"));
      EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(name)));
    }

    {
      padded_string code(u8"import {type " + name + u8", other} from 'mod';");
      SCOPED_TRACE(code);
      test_parser& p =
          this->errorless_parser(code.string_view(), typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // (name)
                                        "visit_variable_declaration",  // other
                                        "visit_end_of_module"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(import_type_decl(name), import_decl(u8"other")));
    }
  }
}

TEST_F(test_parse_typescript_module,
       inline_type_import_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"import {type T} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_inline_type_import_not_allowed_in_javascript,
            type_keyword, strlen(u8"import {"), u8"type")));
  }

  {
    test_parser p(u8"import {type as} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // as
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_inline_type_import_not_allowed_in_javascript,
            type_keyword, strlen(u8"import {"), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_type_only_import) {
  {
    test_parser p(u8"import type {type T} from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_type_decl(u8"T")));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code(),
            diag_typescript_inline_type_import_not_allowed_in_type_only_import,
            inline_type_keyword, strlen(u8"import type {"), u8"type",  //
            type_only_keyword, strlen(u8"import "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, type_only_export) {
  {
    test_parser& p =
        this->errorless_parser(u8"export type { T };"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser& p = this->errorless_parser(u8"export type {T as U};"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser& p = this->errorless_parser(u8"export type {T} from 'mod';"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_end_of_module"));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_export_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"export type {T};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(),
                    diag_typescript_type_only_export_not_allowed_in_javascript,
                    type_keyword, strlen(u8"export "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, inline_type_export) {
  {
    test_parser& p =
        this->errorless_parser(u8"export {type T};"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser& p = this->errorless_parser(u8"export {type T, type U};"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_variable_type_use",  // U
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T", u8"U"));
  }

  {
    test_parser& p = this->errorless_parser(u8"export {type T as U};"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }
}

TEST_F(test_parse_typescript_module,
       inline_type_export_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"export {type T};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_inline_type_export_not_allowed_in_javascript,
            type_keyword, strlen(u8"export {"), u8"type")));
  }

  {
    test_parser p(u8"export {type as};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // as
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(),
            diag_typescript_inline_type_export_not_allowed_in_javascript,
            type_keyword, strlen(u8"export {"), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_type_only_export) {
  {
    test_parser p(u8"export type {type T};"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code(),
            diag_typescript_inline_type_export_not_allowed_in_type_only_export,
            inline_type_keyword, strlen(u8"export type {"), u8"type",  //
            type_only_keyword, strlen(u8"export "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, import_require) {
  {
    test_parser& p = this->errorless_parser(
        u8"import fs = require('node:fs');"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // fs
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"fs")));
  }
}

TEST_F(test_parse_typescript_module, export_interface) {
  {
    test_parser& p = this->errorless_parser(u8"export interface I {}"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  // {
                                      "visit_exit_interface_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(interface_decl(u8"I")));
  }
}

TEST_F(test_parse_typescript_module,
       export_interface_disallows_newline_after_interface_keyword) {
  {
    test_parser p(u8"export interface\nI {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  // {
                                      "visit_exit_interface_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(interface_decl(u8"I")));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_newline_not_allowed_after_interface_keyword,
                    interface_keyword, strlen(u8"export "), u8"interface")));
  }
}

TEST_F(test_parse_typescript_module, export_default_interface) {
  {
    test_parser& p = this->errorless_parser(
        u8"export default interface I {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  // {
                                      "visit_exit_interface_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(interface_decl(u8"I")));
  }

  // A newline is allowed after 'interface' (unlike with 'export interface').
  {
    test_parser& p = this->errorless_parser(
        u8"export default interface\nI {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  // {
                                      "visit_exit_interface_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(interface_decl(u8"I")));
  }
}

TEST_F(test_parse_typescript_module, export_namespace) {
  {
    test_parser& p = this->errorless_parser(u8"export namespace ns {}"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_namespace_scope",  // {
                                      "visit_exit_namespace_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(namespace_decl(u8"ns")));
  }
}

TEST_F(test_parse_typescript_module,
       export_namespace_disallows_newline_after_namespace_keyword) {
  {
    test_parser p(u8"export namespace\nns {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",   // ns
                                      "visit_enter_namespace_scope",  // {
                                      "visit_exit_namespace_scope",   // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(namespace_decl(u8"ns")));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_newline_not_allowed_after_namespace_keyword,
                    namespace_keyword, strlen(u8"export "), u8"namespace")));
  }
}

TEST_F(test_parse_typescript_module, export_enum) {
  {
    test_parser& p =
        this->errorless_parser(u8"export enum E {}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(enum_decl(u8"E")));
  }
}

TEST_F(test_parse_typescript_module, export_const_enum) {
  {
    test_parser& p = this->errorless_parser(u8"export const enum E {}"_sv,
                                            typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // E
                                      "visit_enter_enum_scope",      // {
                                      "visit_exit_enum_scope",       // }
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(enum_decl(u8"E")));
  }
}

TEST_F(test_parse_typescript_module, export_type_alias) {
  {
    test_parser& p =
        this->errorless_parser(u8"export type T = C;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",    // T
                                      "visit_enter_type_alias_scope",  //
                                      "visit_variable_type_use",       // C
                                      "visit_exit_type_alias_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(type_alias_decl(u8"T")));
  }
}

TEST_F(test_parse_typescript_module,
       export_type_alias_disallows_newline_after_type_keyword) {
  {
    test_parser p(u8"export type\nA = any;"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",    // A
                                      "visit_enter_type_alias_scope",  //
                                      "visit_exit_type_alias_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_newline_not_allowed_after_type_keyword,
                    type_keyword, strlen(u8"export "), u8"type")));
  }
}

TEST_F(test_parse_typescript_module, export_import_alias) {
  {
    test_parser& p =
        this->errorless_parser(u8"export import A = B;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",    // A
                                      "visit_variable_namespace_use",  // B
                                      "visit_end_of_module"));
    // TODO(#793): Emit a import alias declaration instead.
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"A")));
  }
}

// TODO(#795): Report an error for other kinds of 'export import', such as
// 'export import fs from "fs";'.
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
