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
class test_parse_typescript_module : public test_parse_expression {};

TEST_F(test_parse_typescript_module, type_only_import) {
  {
    test_parser p(u8"import type { T } from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    test_parser p(u8"import type {T as U} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"U"_sv)}));
  }

  {
    test_parser p(u8"import type T from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    test_parser p(u8"import type * as M from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // M
                              "visit_end_of_module",
                          }));
    // TODO(#788): Assert import_module_decl instead.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"M"_sv)}));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name :
       contextual_keywords - dirty_set<string8>{u8"from", u8"let"}) {
    {
      padded_string code(
          concat(u8"import type "_sv, name, u8" from 'mod';"_sv));
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_type_decl(name)}));
    }
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"import type {T} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_import_not_allowed_in_javascript,
                type_keyword, strlen(u8"import "), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_import_cannot_import_default_and_named) {
  {
    test_parser p(u8"import type A, {B} from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_variable_declaration",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(import_type_decl(u8"A"_sv), import_decl(u8"B"_sv)))
        << "B should be imported as an 'import' not an 'import_type' in case "
           "the user thought that the 'type' keyword only applied to 'A'";
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_type_only_import_cannot_import_default_and_named,
                type_keyword, strlen(u8"import "), u8"type"_sv),
        }));
  }

  {
    test_parser p(u8"import type A, * as B from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_variable_declaration",  // B
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_type_decl(u8"A"_sv), import_decl(u8"B"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_type_only_import_cannot_import_default_and_named,
                type_keyword, strlen(u8"import "), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, inline_type_import) {
  {
    test_parser p(u8"import {type T} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
  }

  {
    test_parser p(u8"import {type T, type U} from 'mod';"_sv,
                  typescript_options);
    p.parse_and_visit_module();
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
    test_parser p(u8"import {type T as U} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"U"_sv)}));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_normal_import) {
  {
    test_parser p(u8"import {type T, f} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
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
    test_parser p(u8"import {f, type T} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module,
       inline_type_import_can_declare_contextual_keywords) {
  // TODO(strager): This probably allows more contextual keywords than
  // TypeScript allows.
  for (string8 name : contextual_keywords - dirty_set<string8>{u8"let"}) {
    {
      padded_string code(
          concat(u8"import {type "_sv, name, u8"} from 'mod';"_sv));
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_type_decl(name)}));
    }

    {
      padded_string code(
          concat(u8"import {type "_sv, name, u8", other} from 'mod';"_sv));
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module,
       inline_type_import_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"import {type T} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_import_not_allowed_in_javascript,
                type_keyword, strlen(u8"import {"), u8"type"_sv),
        }));
  }

  {
    test_parser p(u8"import {type as} from 'mod';"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // as
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_import_not_allowed_in_javascript,
                type_keyword, strlen(u8"import {"), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_type_only_import) {
  {
    test_parser p(u8"import type {type T} from 'mod';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_type_decl(u8"T"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_inline_type_import_not_allowed_in_type_only_import,
                inline_type_keyword, strlen(u8"import type {"), u8"type"_sv,
                type_only_keyword, strlen(u8"import "), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, type_only_export) {
  {
    test_parser p(u8"export type { T };"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"export type {T as U};"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"export type {T} from 'mod';"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_typescript_module,
       type_only_export_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"export type {T};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_export_not_allowed_in_javascript,
                type_keyword, strlen(u8"export "), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, inline_type_export) {
  {
    test_parser p(u8"export {type T};"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"export {type T, type U};"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"U"}));
  }

  {
    test_parser p(u8"export {type T as U};"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(test_parse_typescript_module,
       inline_type_export_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"export {type T};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_export_not_allowed_in_javascript,
                type_keyword, strlen(u8"export {"), u8"type"_sv),
        }));
  }

  {
    test_parser p(u8"export {type as};"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // as
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_type_export_not_allowed_in_javascript,
                type_keyword, strlen(u8"export {"), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, mixed_inline_type_and_type_only_export) {
  {
    test_parser p(u8"export type {type T};"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_inline_type_export_not_allowed_in_type_only_export,
                inline_type_keyword, strlen(u8"export type {"), u8"type"_sv,
                type_only_keyword, strlen(u8"export "), u8"type"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, import_require) {
  {
    test_parser p(u8"import fs = require('node:fs');"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv)}));
  }
}

TEST_F(test_parse_typescript_module, export_interface) {
  {
    test_parser p(u8"export interface I {}"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module,
       export_interface_disallows_newline_after_interface_keyword) {
  {
    test_parser p(u8"export interface\nI {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_newline_not_allowed_after_interface_keyword,
                interface_keyword, strlen(u8"export "), u8"interface"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, export_default_interface) {
  {
    test_parser p(u8"export default interface I {}"_sv, typescript_options);
    p.parse_and_visit_module();
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
    test_parser p(u8"export default interface\nI {}"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module, export_abstract_class) {
  {
    test_parser p(u8"export abstract class C { abstract m(); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // m
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(u8"export default abstract class C { abstract m(); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // m
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(u8"export default abstract class { abstract m(); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // m
                              "visit_enter_function_scope",    //
                              "visit_exit_function_scope",     //
                              "visit_exit_class_scope",        // }
                          }));
  }
}

TEST_F(test_parse_typescript_module,
       export_abstract_class_cannot_have_newline_after_abstract) {
  {
    test_parser p(u8"export abstract\nclass C { abstract m(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_newline_not_allowed_after_abstract_keyword,
                        abstract_keyword, strlen(u8"export "), u8"abstract"_sv),
                }));
  }

  {
    test_parser p(u8"export abstract\nclass C { abstract m(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_newline_not_allowed_after_abstract_keyword,
                        abstract_keyword, strlen(u8"export "), u8"abstract"_sv),
                }));
  }

  {
    test_parser p(u8"export default abstract\nclass C { abstract m(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"abstract"}))
        << "'abstract' should be treated as a variable name, not a keyword";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(diag_abstract_property_not_allowed_in_non_abstract_class),
        }));
  }
}

TEST_F(test_parse_typescript_module, export_namespace) {
  {
    test_parser p(u8"export namespace ns {}"_sv, typescript_options);
    p.parse_and_visit_module();
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
    test_parser p(u8"export module ns {}"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module,
       exported_namespace_cannot_have_string_name) {
  {
    test_parser p(u8"export namespace 'my name space' {}"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_string_namespace_name_is_only_allowed_with_declare_module,
                module_name, strlen(u8"export namespace "),
                u8"'my name space'"_sv),
        }));
  }

  {
    test_parser p(u8"export module 'my name space' {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_string_namespace_name_is_only_allowed_with_declare_module,
                module_name, strlen(u8"export module "),
                u8"'my name space'"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module,
       export_namespace_disallows_newline_after_namespace_keyword) {
  {
    test_parser p(u8"export namespace\nns {}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_newline_not_allowed_after_namespace_keyword,
                namespace_keyword, strlen(u8"export "), u8"namespace"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, export_enum) {
  {
    test_parser p(u8"export enum E {}"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module, export_const_enum) {
  {
    test_parser p(u8"export const enum E {}"_sv, typescript_options);
    p.parse_and_visit_module();
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

TEST_F(test_parse_typescript_module, export_type_alias) {
  {
    test_parser p(u8"export type T = C;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  //
                              "visit_variable_type_use",       // C
                              "visit_exit_type_alias_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"T"_sv)}));
  }
}

TEST_F(test_parse_typescript_module,
       export_type_alias_disallows_newline_after_type_keyword) {
  {
    test_parser p(u8"export type\nA = any;"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_enter_type_alias_scope",  //
                              "visit_exit_type_alias_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_newline_not_allowed_after_type_keyword,
                        type_keyword, strlen(u8"export "), u8"type"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_module, export_import_alias) {
  {
    test_parser p(u8"export import A = B;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // A
                              "visit_variable_namespace_use",  // B
                              "visit_end_of_module",
                          }));
    // TODO(#793): Emit a import alias declaration instead.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"A"_sv)}));
  }
}

// TODO(#795): Report an error for other kinds of 'export import', such as
// 'export import fs from "fs";'.

TEST_F(test_parse_typescript_module,
       import_cannot_be_used_with_declare_keyword) {
  {
    test_parser p(u8"declare import fs from 'fs';"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_import_cannot_have_declare_keyword,  //
                              declare_keyword, 0, u8"declare"_sv),
        }));
  }

  {
    test_parser p(u8"namespace ns { declare import fs from 'fs'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_import_cannot_have_declare_keyword),
                }));
  }
}

TEST_F(test_parse_typescript_module,
       export_equal_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"export = foo;"_sv, javascript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_end_of_module",        //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_export_equal_not_allowed_in_javascript,  //
                equal, strlen(u8"export "), u8"="_sv, export_keyword, 0,
                u8"export"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_module, export_equal_with_identifier) {
  {
    test_parser p(u8"export = foo;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // foo
                              "visit_end_of_module",        //
                          }));
  }
}

TEST_F(test_parse_typescript_module, export_equal_with_expression) {
  {
    test_parser p(u8"export = foo.bar;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
                          }));
  }

  {
    test_parser p(u8"export = new foo();"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
                          }));
  }

  {
    test_parser p(u8"export = (foo);"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",   // foo
                              "visit_end_of_module",  //
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
