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

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Namespace : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_is_not_allowed_in_javascript) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare namespace ns {}"_sv,  //
      u8"        ^^^^^^^^^ Diag_TypeScript_Namespaces_Not_Allowed_In_JavaScript"_diag,  //
      javascript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",    //
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                            "visit_variable_declaration",   // ns
                            "visit_exit_declare_scope",     //
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace, declare_empty_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_module(u8"declare namespace ns {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"declare module ns {}"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({empty_namespace_decl(u8"ns"_sv)}));
  }
}

TEST_F(
    Test_Parse_TypeScript_Declare_Namespace,
    declaring_namespace_with_string_name_is_not_allowed_with_namespace_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace 'my name space' {}"_sv,  //
        u8"                  ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace, missing_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns "_sv,  //
        u8"                    ` Diag_Missing_Body_For_TypeScript_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // ns
                              "visit_exit_declare_scope",    //
                              "visit_end_of_module",         //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns\nconsole.log('hello');"_sv,  //
        u8"                    ` Diag_Missing_Body_For_TypeScript_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",   //
                              "visit_variable_declaration",  // ns
                              "visit_exit_declare_scope",    //
                              "visit_variable_use",          // console
                              "visit_end_of_module",         //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace, incomplete_body) {
  {
    // TODO(strager): Report a namespace-specific diagnostic.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { "_sv,                            //
        u8"                     ^ Diag_Unclosed_Code_Block"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // implicit }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       newline_is_not_allowed_after_namespace_keyword) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"declare namespace\nns {}"_sv,  //
      u8"        ^^^^^^^^^ Diag_Newline_Not_Allowed_After_Namespace_Keyword"_diag,  //
      typescript_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_declare_scope",    //
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                            "visit_variable_declaration",   // ns
                            "visit_exit_declare_scope",     //
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declares_are_not_allowed_inside_declare_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare enum E { } }"_sv,  //
        u8"                       ^^^^^^^ Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace.declare_keyword\n"_diag
        u8"^^^^^^^ .declare_namespace_declare_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare const enum E { } }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare const myVariable; }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare let myVariable; }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare var myVariable; }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare class C { myMethod(); } }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare abstract class C { } }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_declare_scope",     //
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_declare_scope",      //
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare interface I { } }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare type T = U; }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // T
                              "visit_enter_type_scope",       //
                              "visit_variable_type_use",      // U
                              "visit_exit_type_scope",        //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { declare function f(); }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_enter_function_scope",   //
                              "visit_exit_function_scope",    //
                              "visit_variable_declaration",   // f
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns1 { declare namespace ns2 { } }"_sv,  //
        u8"Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns2
                              "visit_exit_declare_scope",     //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns1
                              "visit_exit_declare_scope",     //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       interface_inside_declare_namespace_is_supported) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { interface I { } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export interface I { } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       type_alias_inside_declare_namespace_is_supported) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"declare namespace ns { type T = U; }"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // T
                              "visit_enter_type_scope",       // {
                              "visit_variable_type_use",      // U
                              "visit_exit_type_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export type T = U; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // T
                              "visit_enter_type_scope",       // {
                              "visit_variable_type_use",      // U
                              "visit_exit_type_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_allows_namespace_alias) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { import a = b; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // a
                              "visit_variable_namespace_use",  // b
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export import a = b; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // a
                              "visit_variable_namespace_use",  // b
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_import_from_module) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { import fs from 'fs'; }"_sv,  //
        u8"                       ^^^^^^ Diag_Declare_Namespace_Cannot_Import_Module.importing_keyword\n"_diag
        u8"^^^^^^^ .declare_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // fs
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  test_parse_and_visit_module(
      u8"declare module ns { import fs from 'fs'; }"_sv,     //
      u8"Diag_Declare_Namespace_Cannot_Import_Module"_diag,  //
      typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { import fs = require('fs'); }"_sv,  //
        u8"                       ^^^^^^ Diag_Declare_Namespace_Cannot_Import_Module.importing_keyword\n"_diag
        u8"^^^^^^^ .declare_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // fs
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_import_from_module_with_export_keyword) {
  test_parse_and_visit_module(
      u8"declare namespace ns { export * from 'module'; }"_sv,  //
      u8"                                ^^^^ Diag_Declare_Namespace_Cannot_Import_Module.importing_keyword\n"_diag
      u8"^^^^^^^ .declare_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare module ns { export * from 'module'; }"_sv,  //
      u8"Diag_Declare_Namespace_Cannot_Import_Module"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export {Z} from 'module'; }"_sv,  //
      u8"                                  ^^^^ Diag_Declare_Namespace_Cannot_Import_Module.importing_keyword\n"_diag
      u8"^^^^^^^ .declare_keyword"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_allows_exporting_variables) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"declare namespace ns { export {Z}; }"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_export_use",    // Z
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export type {T}; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_type_use",      // T
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export {Z as default}; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",          //
                              "visit_enter_namespace_scope",        // {
                              "visit_variable_export_default_use",  // Z
                              "visit_exit_namespace_scope",         // }
                              "visit_variable_declaration",         // ns
                              "visit_exit_declare_scope",           //
                              "visit_end_of_module",                //
                          }))
        << "'default' is treated as the exported name, not as a keyword (see "
           "declare_namespace_disallows_exporting_default)";
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_exporting_default) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export default Z; }"_sv,  //
        u8"                              ^^^^^^^ Diag_TypeScript_Namespace_Cannot_Export_Default.default_keyword\n"_diag
        u8"        ^^^^^^^^^ .namespace_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",          //
                              "visit_enter_namespace_scope",        // {
                              "visit_variable_export_default_use",  // Z
                              "visit_exit_namespace_scope",         // }
                              "visit_variable_declaration",         // ns
                              "visit_exit_declare_scope",           //
                              "visit_end_of_module",                //
                          }));
  }

  test_parse_and_visit_module(
      u8"declare namespace ns { export default 2+2; }"_sv,       //
      u8"Diag_TypeScript_Namespace_Cannot_Export_Default"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export default class C { method(); } }"_sv,  //
      u8"                              ^^^^^^^ Diag_TypeScript_Namespace_Cannot_Export_Default.default_keyword\n"_diag
      u8"        ^^^^^^^^^ .namespace_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export default abstract class C { method(); } }"_sv,  //
      u8"Diag_TypeScript_Namespace_Cannot_Export_Default"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export default function f(); }"_sv,  //
      u8"Diag_TypeScript_Namespace_Cannot_Export_Default"_diag,      //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       enum_inside_declare_namespace_acts_like_declare_enum) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { enum E {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns { const enum E {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                          }));
  }

  // Diag_TypeScript_Enum_Value_Must_Be_Constant should not be reported for
  // normal enums but should be reported for declare enums.
  test_parse_and_visit_module(
      u8"declare namespace ns { enum E { A = f() } }"_sv,  //
      u8"                                    ^^^ Diag_TypeScript_Enum_Value_Must_Be_Constant.expression"_diag
      u8"{.declared_enum_kind=Enum_Kind::declare_enum}"_diag,  //
      typescript_options);

  // Diag_TypeScript_Enum_Value_Must_Be_Constant should not be reported for
  // normal enums but should be reported for declare enums.
  test_parse_and_visit_module(
      u8"declare namespace ns { export enum E { A = f() } }"_sv,  //
      u8"                                           ^^^ Diag_TypeScript_Enum_Value_Must_Be_Constant.expression"_diag
      u8"{.declared_enum_kind=Enum_Kind::declare_enum}"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { const enum E { A = f() } }"_sv,  //
      u8"                                          ^^^ Diag_TypeScript_Enum_Value_Must_Be_Constant.expression"_diag
      u8"{.declared_enum_kind=Enum_Kind::declare_const_enum}"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export const enum E { A = f() } }"_sv,  //
      u8"                                                 ^^^ Diag_TypeScript_Enum_Value_Must_Be_Constant.expression"_diag
      u8"{.declared_enum_kind=Enum_Kind::declare_const_enum}"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       var_inside_declare_namespace_acts_like_declare_var) {
  {
    // Diag_Missing_Initializer_In_Const_Declaration is not reported for declare
    // consts.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { const myVariable; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    // Diag_Missing_Initializer_In_Const_Declaration is not reported for declare
    // consts.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export const myVariable; }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { let myVariable = null; }"_sv,  //
        u8"Diag_Declare_Var_Cannot_Have_Initializer"_diag,       //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export let myVariable = null; }"_sv,  //
        u8"Diag_Declare_Var_Cannot_Have_Initializer"_diag,              //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { var myVariable = null; }"_sv,  //
        u8"Diag_Declare_Var_Cannot_Have_Initializer"_diag,       //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export var myVariable = null; }"_sv,  //
        u8"Diag_Declare_Var_Cannot_Have_Initializer"_diag,              //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_exit_declare_scope",     //
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       function_inside_declare_namespace_acts_like_declare_function) {
  // Diag_Declare_Function_Cannot_Have_Body or Diag_Missing_Function_Body is
  // not reported for declare functions.
  test_parse_and_visit_module(u8"declare namespace ns { function f(); }"_sv,
                              no_diags, typescript_options);

  // Diag_Declare_Function_Cannot_Have_Body or Diag_Missing_Function_Body is
  // not reported for declare functions.
  test_parse_and_visit_module(
      u8"declare namespace ns { export function f(); }"_sv, no_diags,
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { async function f(); }"_sv,  //
      u8"                       ^^^^^ Diag_Declare_Function_Cannot_Be_Async"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { export async function f(); }"_sv,  //
      u8"                              ^^^^^ Diag_Declare_Function_Cannot_Be_Async"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       class_inside_declare_namespace_acts_like_declare_class) {
  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { class C { myMethod(); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // (
                              "visit_exit_function_scope",     // )
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export class C { myMethod(); } }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // (
                              "visit_exit_function_scope",     // )
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { abstract class C { myMethod(); } }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // (
                              "visit_exit_function_scope",     // )
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export abstract class C { myMethod(); } }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",     //
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // (
                              "visit_exit_function_scope",     // )
                              "visit_property_declaration",    // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_exit_declare_scope",      //
                              "visit_end_of_module",           //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       namespace_inside_declare_namespace_acts_like_declare_namespace) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare namespace ns1 { namespace ns2 { } }"_sv,  //
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_declare_scope",    //
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns2
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns1
                              "visit_exit_declare_scope",     //
                          }));
  }

  test_parse_and_visit_module(
      u8"declare namespace ns1 { namespace ns2 { if (true) {} } }"_sv,  //
      u8"                                        ^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token\n"_diag
      u8"^^^^^^^ .declare_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns1 { export namespace ns2 { if (true) {} } }"_sv,  //
      u8"                                               ^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token\n"_diag
      u8"^^^^^^^ .declare_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns1 { module ns2 { if (true) {} } }"_sv,  //
      u8"                                     ^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token\n"_diag
      u8"^^^^^^^ .declare_keyword"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_most_statements) {
  test_parse_and_visit_module(
      u8"declare namespace ns { if (true) { } }"_sv,  //
      u8"                       ^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,  //
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { console.log('hello'); }"_sv,  //
      u8"                       ^^^^^^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,  //
      typescript_options);

  for (String8_View statement : {
           // Expressions:
           u8"!true;"_sv,
           u8"false;"_sv,
           u8"this.method();"_sv,
           u8"void [];"_sv,
           u8"[].map(f);"_sv,
           u8"'use strict';"_sv,
           u8"await myPromise;"_sv,
           u8"yield (null);"_sv,
           u8"implements;"_sv,

           u8";"_sv,
           u8"switch (true) {}"_sv,
           u8"return;"_sv,
           u8"throw null;"_sv,
           u8"try {} catch {}"_sv,
           u8"do {} while (false);"_sv,
           u8"for (;;);"_sv,
           u8"while (true);"_sv,
           u8"with ({}) {}"_sv,
           u8"if (true) {}"_sv,
           u8"debugger;"_sv,
           u8"{ }"_sv,
       }) {
    test_parse_and_visit_module(
        concat(u8"declare namespace ns { "_sv, statement, u8" }"_sv),  //
        u8"Diag_Declare_Namespace_Cannot_Contain_Statement"_diag,      //
        typescript_options);
  }

  test_parse_and_visit_module(
      u8"declare namespace ns { \\u{69}f; }"_sv,  //
      u8"                       ^^^^^^^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,  //
      u8"                       ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
      typescript_options);
}

TEST_F(
    Test_Parse_TypeScript_Declare_Namespace,
    declare_namespace_does_not_report_double_diagnostic_for_certain_statements) {
  test_parse_and_visit_module(
      u8"declare namespace ns { break; }"_sv,  //
      u8"                       ^^^^^ Diag_Invalid_Break"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"for (;;) { declare namespace ns { break; } }"_sv,  //
      u8"                                  ^^^^^ Diag_Invalid_Break"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"switch (true) { default: declare namespace ns { break; } }"_sv,  //
      u8"                                                ^^^^^ Diag_Invalid_Break"_diag,
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { continue; }"_sv,  //
      u8"                       ^^^^^^^^ Diag_Invalid_Continue"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"for (;;) { declare namespace ns { continue; } }"_sv,  //
      u8"                                  ^^^^^^^^ Diag_Invalid_Continue"_diag,
      typescript_options);

  test_parse_and_visit_module(
      u8"declare namespace ns { else { } }"_sv,  //
      u8"                       ^^^^ Diag_Else_Has_No_If"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"declare namespace ns { catch { } }"_sv,  //
      u8"                       ^^^^^ Diag_Catch_Without_Try"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"declare namespace ns { finally { } }"_sv,  //
      u8"                       ^^^^^^^ Diag_Finally_Without_Try"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_is_always_empty) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export class C {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"ns"_sv)));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       subnamespace_in_declare_namespace_is_always_empty) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { namespace subns { export class C { } } }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"subns"_sv),
                            ::testing::_));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       namespace_with_declare_subnamespace_containing_statement_is_not_empty) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"namespace ns { declare namespace subns { export class C { } } }"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"subns"_sv),
                            non_empty_namespace_decl(u8"ns"_sv)));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_cannot_contain_loop_label) {
  test_parse_and_visit_module(
      u8"declare namespace ns { label: export class C {} }"_sv,
      u8"                       ^^^^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,
      typescript_options);
  test_parse_and_visit_module(
      u8"declare namespace ns { label: }"_sv,
      u8"                       ^^^^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,
      typescript_options);
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
