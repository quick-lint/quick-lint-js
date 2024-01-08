// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Function_Overload : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Function_Overload, function_overload_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f();\n"_sv
        u8"function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f()\n"_sv
        u8"function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f();\n"_sv
        u8"function \\u{66}() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  for (String8_View function_name : contextual_keywords) {
    Test_Parser p(concat(u8"function "_sv, function_name, u8"();\n"_sv,
                         u8"function "_sv, function_name, u8"() {}"_sv),
                  typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(function_name)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function \\u{66}();\n"_sv
        u8"function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f();\n"_sv
        u8"async function f() { await(myPromise); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f();\n"_sv
        u8"function f() { await(myPromise); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await", u8"myPromise"}))
        << "'async' keyword should not apply to implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f();\n"_sv
        u8"function *f() { yield(myValue); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}))
        << "'yield' should be a keyword in the implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f();\n"_sv
        u8"async function *f() { yield(myValue); await(myPromise); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue", u8"myPromise"}))
        << "both 'await' and 'yield' should be keywords in the implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       exported_function_overload_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function f();\n"_sv
        u8"export function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function f()\n"_sv
        u8"export function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function f(a: number);\n"_sv
        u8"export function f(a: string);\n"_sv
        u8"export function f(a: number | string) {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations, ElementsAreArray({
                                             func_param_decl(u8"a"_sv),
                                             func_param_decl(u8"a"_sv),
                                             func_param_decl(u8"a"_sv),
                                             function_decl(u8"f"_sv),
                                         }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function f();\n"_sv
        u8"export async function f() { await(myPromise); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export async function f();\n"_sv
        u8"export function f() { await(myPromise); }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await", u8"myPromise"}))
        << "'async' keyword should not apply to implementation";
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload, mismatched_export_modifier) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function f(); function f() {}"_sv,  //
        u8"                     ` Diag_Missing_Export_For_Function_With_Overload_Signature.expected_export\n"_diag
        u8"^^^^^^ .existing_export",
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(); export function f() {}"_sv,  //
        u8"              ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
        u8"` .expected_export",
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  // -e-

  test_parse_and_visit_statement(
      u8"export function f(); export function f(); function f() {}"_sv,  //
      u8"                                          ` Diag_Missing_Export_For_Function_With_Overload_Signature.expected_export\n"_diag
      u8"^^^^^^ .existing_export",
      typescript_options);
  test_parse_and_visit_statement(
      u8"export function f(); function f(); export function f() {}"_sv,  //
      u8"                     ` Diag_Missing_Export_For_Function_With_Overload_Signature.expected_export\n"_diag
      u8"^^^^^^ .existing_export",
      typescript_options);
  test_parse_and_visit_statement(
      u8"export function f(); function f(); function f() {}"_sv,  //
      u8"                                   ` Diag_Missing_Export_For_Function_With_Overload_Signature.expected_export\n"_diag
      u8"^^^^^^ .existing_export",
      u8"                     ` Diag_Missing_Export_For_Function_With_Overload_Signature.expected_export\n"_diag
      u8"^^^^^^ .existing_export",
      typescript_options);
  test_parse_and_visit_statement(
      u8"function f(); export function f(); export function f() {}"_sv,  //
      u8"              ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
      u8"` .expected_export",
      typescript_options);
  test_parse_and_visit_statement(
      u8"function f(); export function f(); function f() {}"_sv,  //
      u8"              ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
      u8"                                   ` .expected_export",
      u8"              ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
      u8"` .expected_export",
      typescript_options);
  test_parse_and_visit_statement(
      u8"function f(); function f(); export function f() {}"_sv,  //
      u8"                            ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
      u8"              ` .expected_export",
      u8"                            ^^^^^^ Diag_Missing_Export_For_Function_With_Overload_Signature.existing_export\n"_diag
      u8"` .expected_export",
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       function_overload_signature_with_wrong_name) {
  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // g(
                              "visit_enter_function_scope_body",  // ){
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_variable_declaration",       // g
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
            u8"                        ^ .function_name"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function h() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       // f(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // g(
                              "visit_exit_function_scope",        // )
                              "visit_enter_function_scope",       // h(
                              "visit_enter_function_scope_body",  // ){
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_variable_declaration",       // g
                              "visit_variable_declaration",       // h
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations, ElementsAreArray({
                                             function_decl(u8"f"_sv),
                                             function_decl(u8"g"_sv),
                                             function_decl(u8"h"_sv),
                                         }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                        ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
            u8"                                       ^ .function_name"_diag,  //
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
            u8"                                       ^ .function_name"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function f() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"g"_sv), function_decl(u8"f"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                        ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
            u8"                                       ^ .function_name"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
            u8"                                       ^ .function_name"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function g();\n"_sv
        u8"function f();\n"_sv
        u8"function f();\n"_sv
        u8"function f() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"g"_sv), function_decl(u8"f"_sv)}));
    EXPECT_THAT(
        p.legacy_errors(),
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name,
                overload_name,
                u8"function f();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function "_sv.size(),
                u8"g"_sv, function_name,
                u8"function f();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function g();\n"
                u8"function f();\n"
                u8"function f();\n"
                u8"function "_sv.size(),
                u8"g"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"function f()\n"_sv  // ASI
        u8"function g() {}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // g
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    // Missing function body is more likely, so don't report
    // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"            ` Diag_Missing_Function_Body"_diag,
                       });
  }

  {
    Test_Parser p(
        u8"function f()\n"_sv  // ASI
        u8"async\n"_sv
        u8"function g() { await(myPromise); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // f
                              "visit_variable_use",               // async
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // await
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // g
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"async", u8"await", u8"myPromise"}))
        << "'async' should be a variable reference, not a keyword";
    // Missing function body is more likely, so don't report
    // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"            ` Diag_Missing_Function_Body"_diag,
                       });
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       function_overload_signature_with_newline_after_async) {
  {
    // Normally, we would treat 'async' as a variable name here. However, we
    // know that there's a syntax error anyway (missing function body), so
    // Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword seems more
    // helpful.
    Test_Parser p(
        u8"function f()\n"_sv
        u8"async\n"_sv
        u8"function f() { await(myPromise); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"              ^^^^^ Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword.async_keyword\n"_diag
            u8"                     ^^^^^^^^ .function_keyword"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       function_overload_signature_declaration_cannot_have_generator_star) {
  {
    Test_Parser p(
        u8"function *f();"_sv
        u8"function *f() { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_exit_function_scope",        //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_variable_use",               // myValue
                              "visit_exit_function_scope",        // }
                              "visit_variable_declaration",       // f
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function *f(a);"_sv
        u8"function *f(a, b);"_sv
        u8"function *f(...args) { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                        ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,  //
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        });
  }

  {
    Test_Parser p(
        u8"function *f(a);"_sv
        u8"function f(a, b);"_sv
        u8"function f(...args) { yield(myValue); }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield", u8"myValue"}))
        << "'yield' should not be a keyword in the implementation";
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       default_modifier_is_ignored_on_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default function f(); export function f() {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default async function f(); export function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  if ((false)) {  // FIXME(#1127)
    // This should not report Diag_Multiple_Export_Defaults.
    test_parse_and_visit_module(
        u8"export default function f();\n"_sv
        u8"export function f() {}\n"_sv  // Not a default export.
        u8"export default class C {}"_sv,
        no_diags, typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Function_Overload,
       default_modifier_is_optional_on_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export default function f(); export default function f() {}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export function f(); export default function f() {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv)}));
  }

  test_parse_and_visit_module(
      u8"export default function f(); export default function f() {}  export default class C {}"_sv,
      u8"Diag_Multiple_Export_Defaults"_diag, typescript_options);
  if ((false)) {  // FIXME(#1127)
    test_parse_and_visit_module(
        u8"export function f(); export default function f() {}  export default class C {}"_sv,
        u8"Diag_Multiple_Export_Defaults"_diag, typescript_options);
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
