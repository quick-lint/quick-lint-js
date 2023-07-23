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
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                            "visit_variable_declaration",   // ns
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace, declare_empty_namespace) {
  {
    Test_Parser p(u8"declare namespace ns {}"_sv, typescript_options);
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
    Test_Parser p(u8"declare module ns {}"_sv, typescript_options);
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

TEST_F(
    Test_Parse_TypeScript_Declare_Namespace,
    declaring_namespace_with_string_name_is_not_allowed_with_namespace_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace 'my name space' {}"_sv,  //
        u8"                  ^^^^^^^^^^^^^^^ Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module"_diag,  //

        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
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
                              "visit_variable_declaration",  // ns
                              "visit_end_of_module",         //
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns\nconsole.log('hello');"_sv,  //
        u8"                    ` Diag_Missing_Body_For_TypeScript_Namespace"_diag,  //

        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // ns
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
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // implicit }
                              "visit_variable_declaration",   // ns
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
                            "visit_enter_namespace_scope",  // {
                            "visit_exit_namespace_scope",   // }
                            "visit_variable_declaration",   // ns
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declares_are_not_allowed_inside_declare_namespace) {
  {
    Test_Parser p(u8"declare namespace ns { declare enum E { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace,  //
                declare_keyword, u8"declare namespace ns { "_sv.size(),
                u8"declare"_sv,  //
                declare_namespace_declare_keyword, 0, u8"declare"_sv),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare const enum E { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // E
                              "visit_enter_enum_scope",       // {
                              "visit_exit_enum_scope",        // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare const myVariable; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare let myVariable; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare var myVariable; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // myVariable
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(
        u8"declare namespace ns { declare class C { myMethod(); } }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_property_declaration",    // myMethod
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare abstract class C { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare interface I { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare type T = U; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  //
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   //
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { declare function f(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // f
                              "visit_enter_function_scope",   //
                              "visit_exit_function_scope",    //
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns1 { declare namespace ns2 { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_enter_namespace_scope",  // {
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns2
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns1
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(
                Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       interface_inside_declare_namespace_is_supported) {
  {
    Test_Parser p(u8"declare namespace ns { interface I { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export interface I { } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_exit_interface_scope",   // }
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       type_alias_inside_declare_namespace_is_supported) {
  {
    Test_Parser p(u8"declare namespace ns { type T = U; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // {
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // }
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_end_of_module",           //
                          }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export type T = U; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // T
                              "visit_enter_type_alias_scope",  // {
                              "visit_variable_type_use",       // U
                              "visit_exit_type_alias_scope",   // }
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_end_of_module",           //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_allows_namespace_alias) {
  {
    Test_Parser p(u8"declare namespace ns { import a = b; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // a
                              "visit_variable_namespace_use",  // b
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_end_of_module",           //
                          }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export import a = b; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",   // {
                              "visit_variable_declaration",    // a
                              "visit_variable_namespace_use",  // b
                              "visit_exit_namespace_scope",    // }
                              "visit_variable_declaration",    // ns
                              "visit_end_of_module",           //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_import_from_module) {
  {
    Test_Parser p(u8"declare namespace ns { import fs from 'fs'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // fs
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                Diag_Declare_Namespace_Cannot_Import_Module,  //
                                importing_keyword,
                                u8"declare namespace ns { "_sv.size(),
                                u8"import"_sv,  //
                                declare_keyword, 0, u8"declare"_sv),
        }));
  }

  {
    Test_Parser p(u8"declare module ns { import fs from 'fs'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({DIAG_TYPE(
                              Diag_Declare_Namespace_Cannot_Import_Module)}));
  }

  {
    Test_Parser p(u8"declare namespace ns { import fs = require('fs'); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_declaration",   // fs
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                Diag_Declare_Namespace_Cannot_Import_Module,  //
                                importing_keyword,
                                u8"declare namespace ns { "_sv.size(),
                                u8"import"_sv,  //
                                declare_keyword, 0, u8"declare"_sv),
        }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_import_from_module_with_export_keyword) {
  {
    Test_Parser p(u8"declare namespace ns { export * from 'module'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code,
                                Diag_Declare_Namespace_Cannot_Import_Module,  //
                                importing_keyword,
                                u8"declare namespace ns { export * "_sv.size(),
                                u8"from"_sv,  //
                                declare_keyword, 0, u8"declare"_sv),
        }));
  }

  {
    Test_Parser p(u8"declare module ns { export * from 'module'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({DIAG_TYPE(
                              Diag_Declare_Namespace_Cannot_Import_Module)}));
  }

  {
    Test_Parser p(u8"declare namespace ns { export {Z} from 'module'; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Declare_Namespace_Cannot_Import_Module,  //
                        importing_keyword,
                        u8"declare namespace ns { export {Z} "_sv.size(),
                        u8"from"_sv,  //
                        declare_keyword, 0, u8"declare"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_allows_exporting_variables) {
  {
    Test_Parser p(u8"declare namespace ns { export {Z}; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_export_use",    // Z
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export type {T}; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_type_use",      // T
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export {Z as default}; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_export_use",    // Z
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }))
        << "'default' is treated as the exported name, not as a keyword (see "
           "declare_namespace_disallows_exporting_default)";
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_exporting_default) {
  {
    Test_Parser p(u8"declare namespace ns { export default Z; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_namespace_scope",  // {
                              "visit_variable_use",           // Z
                              "visit_exit_namespace_scope",   // }
                              "visit_variable_declaration",   // ns
                              "visit_end_of_module",          //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Namespace_Cannot_Export_Default,  //
                default_keyword, u8"declare namespace ns { export "_sv.size(),
                u8"default"_sv,  //
                namespace_keyword, u8"declare "_sv.size(), u8"namespace"_sv),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export default 2+2; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({DIAG_TYPE(
                    Diag_TypeScript_Namespace_Cannot_Export_Default)}));
  }

  {
    Test_Parser p(
        u8"declare namespace ns { export default class C { method(); } }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_TypeScript_Namespace_Cannot_Export_Default,  //
                default_keyword, u8"declare namespace ns { export "_sv.size(),
                u8"default"_sv,  //
                namespace_keyword, u8"declare "_sv.size(), u8"namespace"_sv),
        }));
  }

  {
    Test_Parser p(
        u8"declare namespace ns { export default abstract class C { method(); } }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({DIAG_TYPE(
                    Diag_TypeScript_Namespace_Cannot_Export_Default)}));
  }

  {
    Test_Parser p(u8"declare namespace ns { export default function f(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({DIAG_TYPE(
                    Diag_TypeScript_Namespace_Cannot_Export_Default)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       enum_inside_declare_namespace_acts_like_declare_enum) {
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
    Test_Parser p(u8"declare namespace ns { const myVariable; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    // Diag_Missing_Initializer_In_Const_Declaration is not reported for declare
    // consts.
    Test_Parser p(u8"declare namespace ns { export const myVariable; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    Test_Parser p(u8"declare namespace ns { let myVariable = null; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Var_Cannot_Have_Initializer),
                }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export let myVariable = null; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Var_Cannot_Have_Initializer),
                }));
  }

  {
    Test_Parser p(u8"declare namespace ns { var myVariable = null; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Var_Cannot_Have_Initializer),
                }));
  }

  {
    Test_Parser p(u8"declare namespace ns { export var myVariable = null; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(Diag_Declare_Var_Cannot_Have_Initializer),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       function_inside_declare_namespace_acts_like_declare_function) {
  {
    // Diag_Declare_Function_Cannot_Have_Body or Diag_Missing_Function_Body is
    // not reported for declare functions.
    Test_Parser p(u8"declare namespace ns { function f(); }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    // Diag_Declare_Function_Cannot_Have_Body or Diag_Missing_Function_Body is
    // not reported for declare functions.
    Test_Parser p(u8"declare namespace ns { export function f(); }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    // TODO(strager): Also link to the 'declare' keyword.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { async function f(); }"_sv,  //
        u8"                       ^^^^^ Diag_Declare_Function_Cannot_Be_Async"_diag,  //

        typescript_options);
  }

  {
    // TODO(strager): Also link to the 'declare' keyword.
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { export async function f(); }"_sv,  //
        u8"                              ^^^^^ Diag_Declare_Function_Cannot_Be_Async"_diag,  //

        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       class_inside_declare_namespace_acts_like_declare_class) {
  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Test_Parser p(u8"declare namespace ns { class C { myMethod(); } }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Test_Parser p(
        u8"declare namespace ns { export class C { myMethod(); } }"_sv,
        typescript_options);
    p.parse_and_visit_module();
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Test_Parser p(
        u8"declare namespace ns { abstract class C { myMethod(); } }"_sv,
        typescript_options);
    p.parse_and_visit_module();
  }

  {
    // Diag_Missing_Function_Body is not reported in declare classes.
    Test_Parser p(
        u8"declare namespace ns { export abstract class C { myMethod(); } }"_sv,
        typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       namespace_inside_declare_namespace_acts_like_declare_namespace) {
  {
    Test_Parser p(
        u8"declare namespace ns1 { namespace ns2 { if (true) {} } }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Declare_Namespace_Cannot_Contain_Statement,  //
                        first_statement_token,
                        u8"declare namespace ns1 { namespace ns2 { "_sv.size(),
                        u8"if"_sv,  //
                        declare_keyword, 0, u8"declare"_sv),
                }));
  }

  {
    Test_Parser p(
        u8"declare namespace ns1 { export namespace ns2 { if (true) {} } }"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Declare_Namespace_Cannot_Contain_Statement,  //
                first_statement_token,
                u8"declare namespace ns1 { export namespace ns2 { "_sv.size(),
                u8"if"_sv,  //
                declare_keyword, 0, u8"declare"_sv),
        }));
  }

  {
    Test_Parser p(u8"declare namespace ns1 { module ns2 { if (true) {} } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code,
                        Diag_Declare_Namespace_Cannot_Contain_Statement,  //
                        first_statement_token,
                        u8"declare namespace ns1 { module ns2 { "_sv.size(),
                        u8"if"_sv,  //
                        declare_keyword, 0, u8"declare"_sv),
                }));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_disallows_most_statements) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { if (true) { } }"_sv,  //
        u8"                       ^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,  //

        typescript_options);
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare namespace ns { console.log('hello'); }"_sv,  //
        u8"                       ^^^^^^^ Diag_Declare_Namespace_Cannot_Contain_Statement.first_statement_token"_diag,  //

        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       declare_namespace_is_always_empty) {
  {
    Test_Parser p(u8"declare namespace ns { export class C {} }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"ns"_sv)));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       subnamespace_in_declare_namespace_is_always_empty) {
  {
    Test_Parser p(
        u8"declare namespace ns { namespace subns { export class C { } } }"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"subns"_sv),
                            ::testing::_));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Namespace,
       namespace_with_declare_subnamespace_containing_statement_is_not_empty) {
  {
    Test_Parser p(
        u8"namespace ns { declare namespace subns { export class C { } } }"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(::testing::_, empty_namespace_decl(u8"subns"_sv),
                            non_empty_namespace_decl(u8"ns"_sv)));
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
