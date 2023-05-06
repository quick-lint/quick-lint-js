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
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class test_parse_typescript_declare_class : public test_parse_expression {};

TEST_F(test_parse_typescript_declare_class,
       declare_class_is_not_allowed_in_javascript) {
  test_parser p(u8"declare class C {}"_sv, javascript_options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(p.code,
                            diag_declare_class_not_allowed_in_javascript,  //
                            declare_keyword, strlen(u8""), u8"declare"_sv),
      }));
}

TEST_F(test_parse_typescript_declare_class,
       declare_abstract_class_is_not_allowed_in_javascript) {
  {
    test_parser p(u8"declare abstract class C {}"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_declare_abstract_class_not_allowed_in_javascript,  //
                declare_keyword, strlen(u8""), u8"declare"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_class, declare_empty_class) {
  test_parser p(u8"declare class C {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(test_parse_typescript_declare_class, declare_empty_abstract_class) {
  test_parser p(u8"declare abstract class C {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(test_parse_typescript_declare_class,
       declare_before_class_keyword_triggers_asi) {
  test_parser p(u8"declare\nclass C {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",            // declare
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(test_parse_typescript_declare_class,
       declare_before_abstract_keyword_triggers_asi) {
  test_parser p(u8"declare\nabstract class C {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",            // declare
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"declare"_sv}));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
}

TEST_F(
    test_parse_typescript_declare_class,
    newline_is_not_allowed_between_abstract_and_class_keyword_in_declare_abstract_class) {
  test_parser p(u8"declare abstract\nclass C {}"_sv, typescript_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({class_decl(u8"C"_sv)}));
  EXPECT_THAT(p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code,
                      diag_newline_not_allowed_after_abstract_keyword,  //
                      abstract_keyword, strlen(u8"declare "), u8"abstract"_sv),
              }));
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_can_extend_and_implement) {
  test_parser p(u8"declare class C extends B implements I {}"_sv,
                typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_variable_use",            // B
                            "visit_variable_type_use",       // I
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"B"_sv, u8"I"_sv}));
}

TEST_F(test_parse_typescript_declare_class, declare_class_can_be_generic) {
  test_parser p(u8"declare class C<T> {}"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_variable_declaration",    // T
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(
      p.variable_declarations,
      ElementsAreArray({generic_param_decl(u8"T"_sv), class_decl(u8"C"_sv)}));
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_can_contain_empty_static_block) {
  test_parser p(u8"declare class C { static { } }"_sv, typescript_options);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_enter_block_scope",       // static {
                            "visit_exit_block_scope",        // }
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module",
                        }));
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_cannot_contain_statements_in_static_block) {
  for (string8_view static_block_body : {
           u8"console.log('hello');",
           u8"{}",
       }) {
    padded_string code(concat(u8"declare class C { static { "_sv,
                              static_block_body, u8" } }"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_declare_class_cannot_contain_static_block_statement,  //
                static_token, strlen(u8"declare class C { "), u8"static"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_can_have_method_signatures) {
  {
    test_parser p(u8"declare class C { myMethod(); }"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    //
                              "visit_enter_function_scope",    // myMethod
                              "visit_exit_function_scope",     // myMethod
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(u8"declare class C { get myProperty(): number; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    //
                              "visit_enter_function_scope",    // myProperty
                              "visit_exit_function_scope",     // myProperty
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(
        u8"declare class C { set myProperty(value: number): void; }"_sv,
        typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    //
                              "visit_enter_function_scope",    // myProperty
                              "visit_variable_declaration",    // value
                              "visit_exit_function_scope",     // myProperty
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_methods_cannot_contain_bodies) {
  {
    test_parser p(u8"declare class C { myMethod() {} }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_declare_class_methods_cannot_contain_bodies,  //
                        body_start, strlen(u8"declare class C { myMethod() "),
                        u8"{"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_methods_must_be_semicolon_terminated) {
  {
    test_parser p(u8"declare class C { myMethod() myOtherMethod() }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_missing_semicolon_after_declare_class_method,  //
                        expected_semicolon,
                        strlen(u8"declare class C { myMethod()"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       non_abstract_declare_class_properties_cannot_be_abstract) {
  {
    test_parser p(u8"declare class C { abstract myMethod(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Should this be a different message? They can just drop the
    // 'abstract' keyword.
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_non_abstract_class,  //
                abstract_keyword, strlen(u8"declare class C { "),
                u8"abstract"_sv, class_keyword, strlen(u8"declare "),
                u8"class"_sv),
        }));
  }

  {
    test_parser p(u8"declare class C { abstract myField: any; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    // TODO(strager): Should this be a different message? They can just drop the
    // 'abstract' keyword.
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_non_abstract_class,  //
                abstract_keyword, strlen(u8"declare class C { "),
                u8"abstract"_sv, class_keyword, strlen(u8"declare "),
                u8"class"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       abstract_declare_class_properties_can_be_abstract) {
  {
    test_parser p(u8"declare abstract class C { abstract myMethod(); }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"declare abstract class C { abstract myField: any; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_can_have_field_signatures) {
  {
    test_parser p(u8"declare class C { myField; myOtherField: any; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_property_declaration",    // myField
                              "visit_property_declaration",    // myOtherField
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(u8"declare class C { 'myField'; }"_sv, typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_cannot_have_field_with_initializer) {
  {
    test_parser p(u8"declare class C { myField = 42; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_declare_class_fields_cannot_have_initializers,  //
                equal, strlen(u8"declare class C { myField "), u8"="_sv),
        }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_cannot_have_assignment_asserted_field) {
  {
    test_parser p(u8"declare class C { myField!: any; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_declare_class,  //
                bang, strlen(u8"declare class C { myField"), u8"!"_sv),
        }));
  }

  {
    test_parser p(u8"declare class C { myField!: any = init; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_declare_class,  //
                bang, strlen(u8"declare class C { myField"), u8"!"_sv),
        }))
        << "diag_declare_class_fields_cannot_have_initializers should not also "
           "be reported";
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_properties_can_have_access_specifiers) {
  for (string8_view keyword :
       {u8"private"_sv, u8"protected"_sv, u8"public"_sv}) {
    {
      padded_string code(
          concat(u8"declare class C { "_sv, keyword, u8" myField; }"_sv));
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_module();
    }

    {
      padded_string code(
          concat(u8"declare class C { "_sv, keyword, u8" myMethod(); }"_sv));
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_module();
    }
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_properties_can_be_optional) {
  {
    test_parser p(u8"declare class C { myField?: any; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"declare class C { myMethod?(); }"_sv, typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_properties_can_be_private) {
  {
    test_parser p(u8"declare class C { #myField; }"_sv, typescript_options);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"declare class C { #myMethod(); }"_sv, typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_properties_can_be_static) {
  {
    test_parser p(u8"declare class C { static myField; }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"declare class C { static myMethod(); }"_sv,
                  typescript_options);
    p.parse_and_visit_module();
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_methods_cannot_be_async_or_generator) {
  {
    test_parser p(u8"declare class C { async myMethod(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_declare_class_methods_cannot_be_async,  //
                              async_keyword, strlen(u8"declare class C { "),
                              u8"async"_sv),
        }));
  }

  {
    test_parser p(u8"declare class C { *myMethod(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code,
                        diag_declare_class_methods_cannot_be_generators,  //
                        star, strlen(u8"declare class C { "), u8"*"_sv),
                }));
  }

  {
    test_parser p(u8"declare class C { async *myMethod(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                UnorderedElementsAreArray({
                    DIAG_TYPE(diag_declare_class_methods_cannot_be_generators),
                    DIAG_TYPE(diag_declare_class_methods_cannot_be_async),
                }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       declare_class_can_have_index_signature) {
  {
    test_parser p(u8"declare class C { [key: KeyType]: ValueType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // C
                              "visit_enter_class_scope_body",       //
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_class_scope",             // C
                              "visit_variable_declaration",         // C
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"KeyType", u8"ValueType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({index_signature_param_decl(u8"key"_sv),
                                  class_decl(u8"C"_sv)}));
  }

  {
    test_parser p(u8"declare class C { static [key: KeyType]: ValueType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
  }
}

TEST_F(test_parse_typescript_declare_class,
       call_signatures_are_disallowed_in_declare_classes) {
  {
    test_parser p(u8"declare class C { (): any; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_class_scope",       // C
                    "visit_enter_class_scope_body",  //
                    "visit_property_declaration",    // (call signature)
                    "visit_enter_function_scope",    // (call signature)
                    "visit_exit_function_scope",     // (call signature)
                    "visit_exit_class_scope",        // C
                    "visit_variable_declaration",    // C
                }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code,
                                      diag_missing_class_method_name,  //
                                      expected_name,
                                      strlen(u8"declare class C { "), u8""_sv),
                }));
  }
}

TEST_F(test_parse_typescript_declare_class,
       parameter_property_is_not_allowed_in_declare_class) {
  for (string8_view keyword :
       {u8"readonly"_sv, u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      test_parser p(concat(u8"declare class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" field);\n"_sv
                           u8"}"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  diag_typescript_parameter_property_not_allowed_in_declare_class,  //
                  property_keyword,
                  u8"declare class C {\n  constructor("_sv.size(), keyword),
          }));
    }
  }

  for (string8_view keyword :
       {u8"public"_sv, u8"protected"_sv, u8"private"_sv}) {
    {
      test_parser p(concat(u8"declare class C {\n"_sv
                           u8"  constructor("_sv,
                           keyword,
                           u8" readonly field);\n"_sv
                           u8"}"_sv),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {func_param_decl(u8"field"_sv), class_decl(u8"C"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  diag_typescript_parameter_property_not_allowed_in_declare_class,  //
                  property_keyword,
                  u8"declare class C {\n  constructor("_sv.size(), keyword),
          }))
          << "only '" << out_string8(keyword)
          << "' should report a diagnostic; 'readonly' should not have its own "
             "diagnostic";
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
