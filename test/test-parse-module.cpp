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
class Test_Parse_Module : public Test_Parse_Expression {};

TEST_F(Test_Parse_Module, export_variable) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export let x;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export let x = 42;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export var x;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export var x = 42;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_init_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export const x = null;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_init_decl(u8"x"_sv)}));
  }
}

TEST_F(Test_Parse_Module, export_default) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default x;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_default_use",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default (x);"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }))
        << "visit_variable_export_default_use is not used because (x) is an "
           "expression, not a name";
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default function f() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default function() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default async function f() {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                              "visit_variable_declaration",  // f
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default async function() {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default (function f() {})"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default class C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default class {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default class A {} export default class B {}"_sv,  //
        u8"       ^^^^^^^ Diag_Multiple_Export_Defaults.first_export_default\n"
        u8"                                 ^^^^^^^ .second_export_default"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // A
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        //
                              "visit_variable_declaration",    //
                              "visit_enter_class_scope",       // B
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        //
                              "visit_variable_declaration",    //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export default async (a) => b;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // a
                              "visit_enter_function_scope_body",
                              "visit_variable_use",  // b
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_Module,
       export_default_with_contextual_keyword_variable_expression) {
  Dirty_Set<String8> variable_names =
      // TODO(#73): Disallow 'interface'.
      // TODO(#73): Disallow 'protected', 'implements', etc.
      // (strict_only_reserved_keywords).
      (contextual_keywords - Dirty_Set<String8>{u8"let", u8"interface"}) |
      Dirty_Set<String8>{u8"await", u8"yield"};
  for (String8_View variable_name : variable_names) {
    Test_Parser p(concat(u8"export default "_sv, variable_name, u8";"_sv));
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_variable_export_default_use",  // (variable_name)
                    "visit_end_of_module",                //
                }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({variable_name}));
  }
}

TEST_F(Test_Parse_Module, export_default_of_variable_is_illegal) {
  for (String8 declaration_kind : {u8"const", u8"let", u8"var"}) {
    Test_Parser p(
        concat(u8"export default "_sv, declaration_kind, u8" x = y;"_sv),
        capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Cannot_Export_Default_Variable,  //
                              declaring_token, u8"export default "_sv.size(),
                              declaration_kind),
        }));
  }
}

TEST_F(Test_Parse_Module, export_sometimes_requires_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export {x} console.log();"_sv,  //
        u8"          ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // x
                              "visit_variable_use",         // console
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export * from 'other' console.log();"_sv,  //
        u8"                     ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // console
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default x+y console.log();"_sv,  //
        u8"                  ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_variable_use",  // console
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default async () => {} console.log();"_sv,  //
        u8"                             ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // console
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Module, export_sometimes_does_not_require_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default async function f() {} console.log();"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // console
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default function() {} console.log();"_sv, no_diags);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // console
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Module,
       export_default_async_function_with_newline_inserts_semicolon) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"export default async\nfunction f() {await;}"_sv, no_diags,
      javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_export_default_use",  // async
                            "visit_enter_function_scope",         // f
                            "visit_enter_function_scope_body",    // {
                            "visit_variable_use",  // await (not an operator)
                            "visit_exit_function_scope",   // }
                            "visit_variable_declaration",  // f
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"async", u8"await"}))
      << "'async' should be treated as a variable name, not a keyword; "
      << "'await' should be treated as a variable name, not a keyword because "
         "the containing function is not async";
}

TEST_F(Test_Parse_Module,
       export_default_async_with_newline_does_not_insert_semicolon) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default async\n[x] = 42;"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"async", u8"x"}))
        << "'async' should be treated as a variable name, not a keyword; "
        << "'x' should be parsed as an index, not as part of a destructured "
           "assignment";
  }
}

TEST_F(Test_Parse_Module, export_list) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {one, two};"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // one
                              "visit_variable_export_use",  // two
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {one as two, three as four};"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_export_use",  // one
                              "visit_variable_export_use",  // three
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"one", u8"three"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {myVar as 'name'};"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myVar"}));
  }
}

TEST_F(Test_Parse_Module,
       exporting_by_string_name_is_only_allowed_for_export_from) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {'name'};"_sv,  //
        u8"        ^^^^^^ Diag_Exporting_String_Name_Only_Allowed_For_Export_From"_diag);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_Module,
       exported_variables_cannot_be_named_reserved_keywords) {
  for (String8 keyword : strict_reserved_keywords) {
    Test_Parser p(concat(u8"export {"_sv, keyword, u8"};"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, Diag_Cannot_Export_Variable_Named_Keyword,  //
                        export_name, u8"export {"_sv.size(), keyword),
                }));
  }

  for (String8 keyword : strict_reserved_keywords) {
    Test_Parser p(concat(u8"export {"_sv, keyword, u8" as thing};"_sv),
                  capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, Diag_Cannot_Export_Variable_Named_Keyword,  //
                        export_name, u8"export {"_sv.size(), keyword),
                }));
  }

  // TODO(strager): Test u8"await" and u8"yield".
  // TODO(#73): Disallow 'protected', 'implements', etc.
  for (String8 keyword : disallowed_binding_identifier_keywords) {
    String8 exported_variable = escape_first_character_in_keyword(keyword);

    {
      Test_Parser p(concat(u8"export {"_sv, exported_variable, u8"};"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, IsEmpty());
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"        ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"export {"_sv, exported_variable, u8" as thing};"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, IsEmpty());
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"        ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }
  }
}

TEST_F(Test_Parse_Module, export_from) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export * from 'other';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export * as mother from 'other';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export * as 'mother' from 'other';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {} from 'other';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {util1, util2, util3} from 'other';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {readFileSync as readFile} from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {promises as default} from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  for (String8 keyword : keywords) {
    Padded_String code(
        concat(u8"export {"_sv, keyword, u8"} from 'other';"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_statement(code.string_view(), no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Keywords are legal, even if Unicode-escaped.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {\\u{76}ar} from 'fs';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Keywords are legal, even if Unicode-escaped.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {\\u{76}ar as \\u{69}f} from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {'name'} from 'other';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export {'name' as 'othername'} from 'other';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_Module, invalid_export_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export stuff;"_sv,  //
        u8"       ^^^^^ Diag_Exporting_Requires_Curlies"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // stuff
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export a, b, c;"_sv,  //
        u8"       ^^^^^^^ Diag_Exporting_Requires_Default"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // b
                              "visit_variable_use",  // c
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export a, b, c+d;"_sv,  //
        u8"       ^^^^^^^^^ Diag_Exporting_Requires_Default"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // b
                              "visit_variable_use",  // c
                              "visit_variable_use",  // d
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export 2 + x;"_sv,  //
        u8"       ^^^^^ Diag_Exporting_Requires_Default"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }
}

TEST_F(Test_Parse_Module, invalid_export) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export ;"_sv,  //
        u8"^^^^^^ Diag_Missing_Token_After_Export"_diag);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export "_sv,  //
        u8"^^^^^^ Diag_Missing_Token_After_Export"_diag);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Test_Parser p(u8"export += x"_sv, capture_diags);
    p.parse_and_visit_statement();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^^ Diag_Unexpected_Token_After_Export"_diag,
        });
    p.parse_and_visit_statement();  // Parse '+= x'.
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }
}

TEST_F(Test_Parse_Module, parse_and_visit_import) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import 'foo';"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import fs from 'fs'"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import * as fs from 'fs'"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv)}));
  }

  {
    Test_Parser p(u8"import fs from 'fs'; import net from 'net';"_sv);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({import_decl(u8"fs"_sv), import_decl(u8"net"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { readFile, writeFile } from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"readFile"_sv),
                                  import_decl(u8"writeFile"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import {readFileSync as rf} from 'fs';"_sv, no_diags,
        javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"rf"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import {'read file sync' as readFileSync} from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"readFileSync"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import fs, {readFileSync} from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fs"_sv),
                                  import_decl(u8"readFileSync"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import fsDefault, * as fsExports from 'fs';"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"fsDefault"_sv),
                                  import_decl(u8"fsExports"_sv)}));
  }
}

TEST_F(Test_Parse_Module, import_star_without_as_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import * myExport from 'other';"_sv,  //
        u8"       ^^^^^^^^^^ Diag_Expected_As_Before_Imported_Namespace_Alias.star_through_alias_token\n"_diag
        u8"       ^ .star_token\n"_diag
        u8"         ^^^^^^^^ .alias"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // myExport
                          }));
  }
}

TEST_F(Test_Parse_Module, import_without_from_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { x } 'other';"_sv,  //
        u8"             ^^^^^^^ Diag_Expected_From_Before_Module_Specifier"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { x } ;"_sv,  //
        u8"            ` Diag_Expected_From_And_Module_Specifier"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                          }));
  }
}

TEST_F(Test_Parse_Module, import_as_invalid_token) {
  test_parse_and_visit_statement(
      u8"import {myExport as 'string'} from 'module';"_sv,  //
      u8"                    ^^^^^^^^ Diag_Expected_Variable_Name_For_Import_As"_diag);

  test_parse_and_visit_statement(
      u8"import {'myExport' as 'string'} from 'module';"_sv,  //
      u8"                      ^^^^^^^^ Diag_Expected_Variable_Name_For_Import_As"_diag);
}

TEST_F(Test_Parse_Module, export_function) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function foo() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"foo"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export async function foo() {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"foo"_sv)}));
  }
}

TEST_F(Test_Parse_Module, export_function_requires_a_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function() {}"_sv,  //
        u8"       ^^^^^^^^ Diag_Missing_Name_Of_Exported_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export async function() {}"_sv,  //
        u8"             ^^^^^^^^ Diag_Missing_Name_Of_Exported_Function"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_Module, export_class) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export class C {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
  }
}

TEST_F(Test_Parse_Module, export_class_requires_a_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export class {}"_sv,  //
        u8"       ^^^^^ Diag_Missing_Name_Of_Exported_Class"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",
                          }));
  }
}

TEST_F(Test_Parse_Module, parse_empty_module) {
  Spy_Visitor p = test_parse_and_visit_module(u8""_sv, no_diags);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_Module, imported_variables_can_be_named_contextual_keywords) {
  for (String8 name : contextual_keywords - Dirty_Set<String8>{u8"let"}) {
    SCOPED_TRACE(out_string8(name));

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"import { "_sv, name, u8" } from 'other';"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"import { exportedName as "_sv, name,
                 u8" } from 'other';"_sv),
          no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"import { 'exportedName' as "_sv, name,
                 u8" } from 'other';"_sv),
          no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"import "_sv, name, u8" from 'other';"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"import * as "_sv, name, u8" from 'other';"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
    }
  }
}

TEST_F(Test_Parse_Module, imported_modules_must_be_quoted) {
  for (String8 import_name : {u8"module", u8"not_a_keyword"}) {
    Test_Parser p(concat(u8"import { test } from "_sv, import_name, u8";"_sv),
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Cannot_Import_From_Unquoted_Module,
                              import_name, u8"import { test } from "_sv.size(),
                              import_name),
        }));
  }
}

TEST_F(Test_Parse_Module,
       imported_variables_cannot_be_named_reserved_keywords) {
  for (String8 name : strict_reserved_keywords) {
    {
      Test_Parser p(concat(u8"import { "_sv, name, u8" } from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Cannot_Import_Variable_Named_Keyword,  //
                          import_name, u8"import { "_sv.size(), name),
                  }));
    }

    {
      Test_Parser p(concat(u8"import { someFunction as "_sv, name,
                           u8" } from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Cannot_Import_Variable_Named_Keyword,  //
                  import_name, u8"import { someFunction as "_sv.size(), name),
          }));
    }

    {
      Test_Parser p(concat(u8"import { 'someFunction' as "_sv, name,
                           u8" } from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(name)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Cannot_Import_Variable_Named_Keyword,  //
                  import_name, u8"import { 'someFunction' as "_sv.size(), name),
          }));
    }

    {
      Test_Parser p(concat(u8"import "_sv, name, u8" from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Cannot_Import_Variable_Named_Keyword,  //
                          import_name, u8"import "_sv.size(), name),
                  }));
    }

    {
      Test_Parser p(concat(u8"import * as "_sv, name, u8" from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Cannot_Import_Variable_Named_Keyword,  //
                          import_name, u8"import * as "_sv.size(), name),
                  }));
    }
  }

  // TODO(strager): Test u8"await" and u8"yield".
  // TODO(#73): Disallow 'protected', 'implements', etc.
  for (String8 keyword : disallowed_binding_identifier_keywords) {
    String8 imported_variable = escape_first_character_in_keyword(keyword);

    {
      Test_Parser p(
          concat(u8"import { "_sv, imported_variable, u8" } from 'other';"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(keyword)}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"         ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"import { someFunction as "_sv, imported_variable,
                           u8" } from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(keyword)}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"                         ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"import { 'someFunction' as "_sv,
                           imported_variable, u8" } from 'other';"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(keyword)}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"                           ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"import "_sv, imported_variable, u8" from 'other';"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(keyword)}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"       ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"import * as "_sv, imported_variable, u8" from 'other';"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({import_decl(keyword)}));
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"            ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
          });
    }
  }
}

TEST_F(Test_Parse_Module, exported_names_can_be_named_keywords) {
  for (String8 export_name : keywords) {
    {
      Test_Parser p(
          concat(u8"export {someFunction as "_sv, export_name, u8"};"_sv));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_export_use",  // someFunction
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"someFunction"}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"export * as "_sv, export_name,
                 u8" from 'other-module';"_sv),
          no_diags, javascript_options);
      EXPECT_THAT(p.visits, IsEmpty());
    }
  }
}

TEST_F(Test_Parse_Module, imported_names_can_be_named_keywords) {
  for (String8 import_name : keywords) {
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(u8"import {"_sv, import_name,
               u8" as someFunction} from 'somewhere';"_sv),
        no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // someFunction
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"someFunction"_sv)}));
  }
}

TEST_F(
    Test_Parse_Module,
    imported_and_exported_names_can_be_reserved_keywords_with_escape_sequences) {
  for (String8 keyword : keywords) {
    String8 exported_name = escape_first_character_in_keyword(keyword);

    {
      Padded_String code(concat(u8"import {"_sv, exported_name,
                                u8" as someFunction} from 'somewhere';"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // someFunction
                            }));
    }

    {
      Padded_String code(
          concat(u8"export {someFunction as "_sv, exported_name, u8"};"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_export_use",  // someFunction
                            }));
    }

    {
      Padded_String code(
          concat(u8"export * as "_sv, exported_name, u8" from 'other';"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.visits, IsEmpty());
    }
  }
}

TEST_F(Test_Parse_Module, import_requires_semicolon_or_newline) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"import fs from 'fs' nextStatement"_sv,  //
        u8"                   ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // fs
                              "visit_variable_use",          // nextStatement
                              "visit_end_of_module",
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
