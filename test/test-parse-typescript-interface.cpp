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
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_interface : public test_parse_expression {};

TEST_F(test_parse_typescript_interface, not_supported_in_vanilla_javascript) {
  parser_options options;
  options.typescript = false;
  test_parser p(u8"interface I {}"_sv, options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code,
                      diag_typescript_interfaces_not_allowed_in_javascript,  //
                      interface_keyword, 0, u8"interface"),
              }));
}

TEST_F(test_parse_typescript_interface, empty_interface) {
  test_parser p(u8"interface I {}"_sv, typescript_options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({interface_decl(u8"I")}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_typescript_interface, interface_without_body) {
  {
    test_parser p(u8"interface I"_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_body_for_typescript_interface,  //
                interface_keyword_and_name_and_heritage, 0, u8"interface I"),
        }));
  }

  {
    test_parser p(u8"interface I extends Other"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_type_use",      // J
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_body_for_typescript_interface,  //
                        interface_keyword_and_name_and_heritage, 0,
                        u8"interface I extends Other"),
                }));
  }
}

TEST_F(test_parse_typescript_interface, extends) {
  test_parser p(u8"interface I extends A {}"_sv, typescript_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_variable_type_use",      // A
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_typescript_interface, extends_interface_from_namespace) {
  test_parser p(u8"interface I extends ns.A {}"_sv, typescript_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",    // I
                            "visit_enter_interface_scope",   // I
                            "visit_variable_namespace_use",  // ns
                            "visit_exit_interface_scope",    // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_typescript_interface, extends_multiple_things) {
  test_parser p(u8"interface I extends A, B, C {}"_sv, typescript_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",   // I
                            "visit_enter_interface_scope",  // I
                            "visit_variable_type_use",      // A
                            "visit_variable_type_use",      // B
                            "visit_variable_type_use",      // C
                            "visit_exit_interface_scope",   // I
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_typescript_interface, unclosed_interface_statement) {
  {
    test_parser p(u8"interface I { "_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{"),
        }));
  }

  {
    test_parser p(u8"interface I { property "_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{"),
        }));
  }

  {
    test_parser p(u8"interface I { method() "_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // method
                              "visit_enter_function_scope",   // method
                              "visit_exit_function_scope",    // method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{"),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       interface_can_be_named_contextual_keyword) {
  for (string8 name : contextual_keywords - typescript_builtin_type_keywords -
                          typescript_special_type_keywords -
                          dirty_set<string8>{
                              u8"let",
                              u8"static",
                              u8"yield",
                          }) {
    padded_string code(u8"interface " + name + u8" {}");
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // (name)
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(name)}));
  }
}

TEST_F(test_parse_typescript_interface,
       interface_cannot_have_newline_after_interface_keyword) {
  {
    test_parser p(u8"interface\nI {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_newline_not_allowed_after_interface_keyword,
                              interface_keyword, 0, u8"interface"),
        }));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    test_parser p(u8"interface\nI<T> {}"_sv, typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_declaration",   // T
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_newline_not_allowed_after_interface_keyword,
                              interface_keyword, 0, u8"interface"),
        }));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    test_parser p(u8"interface\nI<T>\n{}"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_declaration",   // T
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_newline_not_allowed_after_interface_keyword,
                              interface_keyword, 0, u8"interface"),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       interface_keyword_with_following_newline_is_variable_name) {
  {
    test_parser p(u8"interface\nI\n{}"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // interface
                              "visit_variable_use",       // I
                              "visit_enter_block_scope",  // {
                              "visit_exit_block_scope",   // }
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"interface", u8"I"}));
  }

  {
    // NOTE(strager): This example is interpreted differently in JavaScript than
    // in TypeScript.
    test_parser p(u8"interface\nI<T> {}"_sv, javascript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // interface
                              "visit_variable_use",  // I
                              "visit_variable_use",  // T
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_typescript_interface, property_without_type) {
  {
    test_parser p(u8"interface I { a;b\nc }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // a
                              "visit_property_declaration",   // b
                              "visit_property_declaration",   // c
                              "visit_exit_interface_scope",   // I
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"a", u8"b", u8"c"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"interface I { 'fieldName'; }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // 'fieldName'
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface I { 3.14; }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // 3.14
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface I { [x + y]; }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   //
                              "visit_enter_interface_scope",  //
                              "visit_variable_use",           // x
                              "visit_variable_use",           // y
                              "visit_property_declaration",   // (x + y)
                              "visit_exit_interface_scope",
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }
}

TEST_F(test_parse_typescript_interface, optional_property) {
  {
    test_parser p(u8"interface I { fieldName?; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // fieldName
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
  }

  {
    // Semicolon is required.
    test_parser p(u8"interface I { fieldName? otherField }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_semicolon_after_field,  //
                              expected_semicolon,
                              strlen(u8"interface I { fieldName?"), u8""),
        }));
  }

  {
    // ASI
    test_parser p(u8"interface I { fieldName?\notherField }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }

  {
    test_parser p(u8"interface I { [2 + 2]?; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface I { 'prop'?; }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface I { method?(param); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // method
                              "visit_enter_function_scope",   // method
                              "visit_variable_declaration",   // param
                              "visit_exit_function_scope",    // method
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    test_parser p(u8"interface I { field?; }"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"field"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(diag_typescript_interfaces_not_allowed_in_javascript),
        }))
        << "should parse optional field but not complain about it";
  }
}

TEST_F(test_parse_typescript_interface,
       assignment_asserted_field_is_disallowed) {
  {
    test_parser p(u8"interface I { fieldName!: any; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces,  //
                bang, strlen(u8"interface I { fieldName"), u8"!"),
        }));
  }

  {
    test_parser p(u8"interface I { fieldName!; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces,  //
                bang, strlen(u8"interface I { fieldName"), u8"!"),
        }))
        << "missing type annotation should not report two errors";
  }

  {
    test_parser p(u8"interface I { fieldName!: any = init; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces,  //
                bang, strlen(u8"interface I { fieldName"), u8"!"),
        }))
        << "initializer should not report two errors";
  }
}

TEST_F(test_parse_typescript_interface, field_with_type) {
  {
    test_parser p(u8"interface I { fieldName: FieldType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_type_use",      // FieldType
                              "visit_property_declaration",   // fieldName
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"fieldName"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"FieldType"}));
  }

  {
    // Semicolon is required.
    test_parser p(u8"interface I { fieldName: FieldType otherField }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_semicolon_after_field,  //
                        expected_semicolon,
                        strlen(u8"interface I { fieldName: FieldType"), u8""),
                }));
  }

  {
    // ASI
    test_parser p(u8"interface I { fieldName: FieldType\notherField }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"fieldName", u8"otherField"}));
  }
}

TEST_F(test_parse_typescript_interface, interface_with_methods) {
  {
    test_parser p(u8"interface Monster { eatMuffins(muffinCount); }",
                  typescript_options);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"Monster");
    EXPECT_EQ(p.variable_declarations[1].name, u8"muffinCount");

    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"eatMuffins"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Monster
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // eatMuffins
                              "visit_enter_function_scope",   //
                              "visit_variable_declaration",   // muffinCount
                              "visit_exit_function_scope",    //
                              "visit_exit_interface_scope",
                          }));
  }

  {
    test_parser p(u8"interface I { get length(); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    test_parser p(u8"interface I { set length(value); }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    test_parser p(u8"interface I { a(); b(); c(); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"a", u8"b", u8"c"}));
  }

  {
    test_parser p(u8"interface I { \"stringKey\"(); }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface I { [x + y](); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    test_parser p(u8"interface Getter<T> { get(): T; }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Getter
                              "visit_enter_interface_scope",  // {
                              "visit_variable_declaration",   // T
                              "visit_property_declaration",   // get
                              "visit_enter_function_scope",   //
                              "visit_variable_type_use",      // T
                              "visit_exit_function_scope",    //
                              "visit_exit_interface_scope",   // }
                          }));
  }
}

TEST_F(test_parse_typescript_interface, interface_with_index_signature) {
  {
    test_parser p(u8"interface I { [key: KeyType]: ValueType; }"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"KeyType", u8"ValueType"}));
    // TODO(strager): We probably should create a new kind of variable instead
    // of 'parameter'.
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({interface_decl(u8"I"),
                                  index_signature_param_decl(u8"key")}));
  }

  {
    test_parser p(u8"interface I { [key: KeyType]: ValueType; }"_sv,
                  javascript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(diag_typescript_interfaces_not_allowed_in_javascript),
        }))
        << "should parse index signature and not complain about it";
  }
}

TEST_F(test_parse_typescript_interface, index_signature_requires_type) {
  {
    test_parser p(u8"interface I { [key: KeyType]; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_exit_index_signature_scope",   //
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_index_signature_needs_type,  //
                expected_type, strlen(u8"interface I { [key: KeyType]"), u8""),
        }));
  }

  {
    // ASI
    test_parser p(u8"interface I { [key: KeyType]\n  method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_exit_index_signature_scope",   //
                              "visit_property_declaration",         // method
                              "visit_enter_function_scope",         // method
                              "visit_exit_function_scope",          // method
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_index_signature_needs_type,  //
                expected_type, strlen(u8"interface I { [key: KeyType]"), u8""),
        }));
  }
}

TEST_F(test_parse_typescript_interface, index_signature_cannot_be_a_method) {
  {
    test_parser p(u8"interface I { [key: KeyType](param); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_variable_declaration",         // I
                    "visit_enter_interface_scope",        // I
                    "visit_enter_index_signature_scope",  //
                    "visit_variable_type_use",            // KeyType
                    "visit_variable_declaration",         // key
                    // TODO(strager): Don't emit visit_property_declaration.
                    "visit_property_declaration",        //
                    "visit_enter_function_scope",        //
                    "visit_variable_declaration",        // param
                    "visit_exit_function_scope",         //
                    "visit_exit_index_signature_scope",  //
                    "visit_exit_interface_scope",        // I
                }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_index_signature_cannot_be_method,  //
                left_paren, strlen(u8"interface I { [key: KeyType]"), u8"("),
        }));
  }
}

TEST_F(test_parse_typescript_interface, index_signature_requires_semicolon) {
  {
    test_parser p(u8"interface I { [key: KeyType]: ValueType method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",         // I
                              "visit_enter_interface_scope",        // I
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // ValueType
                              "visit_exit_index_signature_scope",   //
                              "visit_property_declaration",         // method
                              "visit_enter_function_scope",         // method
                              "visit_exit_function_scope",          // method
                              "visit_exit_interface_scope",         // I
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_semicolon_after_index_signature,  //
                expected_semicolon,
                strlen(u8"interface I { [key: KeyType]: ValueType"), u8""),
        }));
  }
}

TEST_F(test_parse_typescript_interface, interface_methods_cannot_have_bodies) {
  {
    test_parser p(u8"interface I { method() { x } }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // I
                              "visit_enter_interface_scope",      //
                              "visit_property_declaration",       // method
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        // method
                              "visit_exit_interface_scope",       //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_contain_bodies,  //
                body_start, strlen(u8"interface I { method() "), u8"{"),
        }));
  }

  {
    test_parser p(u8"interface I { method() => { x } }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ::testing::UnorderedElementsAre(
            // TODO(strager): Report only one diagnostic:
            // diag_interface_methods_cannot_contain_bodies on the '=>'.
            DIAG_TYPE(diag_functions_or_methods_should_not_have_arrow_operator),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_contain_bodies,  //
                body_start, strlen(u8"interface I { method() => "), u8"{")));
  }
}

TEST_F(test_parse_typescript_interface, interface_with_keyword_property) {
  for (string8_view suffix : {u8""_sv, u8"?"_sv}) {
    for (string8_view keyword : keywords) {
      {
        test_parser p(concat(u8"interface I { ", keyword, suffix, u8"(); }"),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      for (string8_view prefix : {u8"get"_sv, u8"set"_sv}) {
        test_parser p(concat(u8"interface I { ", prefix, u8" ", keyword, suffix,
                             u8"(); }"),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      {
        test_parser p(concat(u8"interface I { ", keyword, suffix, u8" }"),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }

      {
        test_parser p(concat(u8"interface I { ", keyword, suffix, u8"; }"),
                      typescript_options);
        SCOPED_TRACE(p.code);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }
    }

    for (string8_view keyword : strict_reserved_keywords) {
      string8 property = escape_first_character_in_keyword(keyword);
      for (string8_view prefix : {u8""_sv, u8"get"_sv, u8"set"_sv}) {
        padded_string code(concat(u8"interface I { ", prefix, u8" ", property,
                                  suffix, u8"(); }"));
        SCOPED_TRACE(code);
        test_parser p(code.string_view(), typescript_options);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      }
    }
  }
}

TEST_F(test_parse_typescript_interface, interface_with_number_methods) {
  {
    test_parser p(u8"interface Wat { 42.0(); }"_sv, typescript_options);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // Wat
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // 42.0
                              "visit_enter_function_scope",   //
                              "visit_exit_function_scope",    //
                              "visit_exit_interface_scope",
                          }));
  }
}

TEST_F(test_parse_typescript_interface, interface_allows_stray_semicolons) {
  test_parser p(u8"interface I{ ; f() ; ; }"_sv, typescript_options);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"}));
}

TEST_F(test_parse_typescript_interface, private_properties_are_not_allowed) {
  {
    test_parser p(u8"interface I { #method(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #method
                              "visit_enter_function_scope",   // #method
                              "visit_exit_function_scope",    // #method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_private,  //
                        property_name_or_private_keyword,
                        strlen(u8"interface I { "), u8"#method"),
                }));
  }

  {
    test_parser p(u8"interface I { #field; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #field
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_private,  //
                        property_name_or_private_keyword,
                        strlen(u8"interface I { "), u8"#field"),
                }));
  }

  {
    test_parser p(u8"interface I { async static #method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #method
                              "visit_enter_function_scope",   // #method
                              "visit_exit_function_scope",    // #method
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ::testing::UnorderedElementsAre(
                    DIAG_TYPE(diag_interface_methods_cannot_be_async),
                    DIAG_TYPE(diag_interface_properties_cannot_be_static),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_private,  //
                        property_name_or_private_keyword,
                        strlen(u8"interface I { async static "), u8"#method")));
  }

  {
    test_parser p(u8"interface I { readonly static #field; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // #field
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE(diag_interface_properties_cannot_be_static),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_properties_cannot_be_private,  //
                property_name_or_private_keyword,
                strlen(u8"interface I { readonly static "), u8"#field")));
  }
}

TEST_F(test_parse_typescript_interface, static_properties_are_not_allowed) {
  for (string8 property_name : dirty_set<string8>{u8"myProperty"} | keywords) {
    SCOPED_TRACE(out_string8(property_name));

    {
      test_parser p(concat(u8"interface I { static ", property_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_enter_function_scope",   // property
                                "visit_exit_function_scope",    // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      test_parser p(
          concat(u8"interface I { static get ", property_name, u8"(); }"),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_enter_function_scope",   // property
                                "visit_exit_function_scope",    // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      test_parser p(
          concat(u8"interface I { static set ", property_name, u8"(value); }"),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_enter_function_scope",   // property
                                "visit_variable_declaration",   // value
                                "visit_exit_function_scope",    // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      test_parser p(concat(u8"interface I { static ", property_name, u8"; }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    // TODO(#736): Fix 'static readonly static'.
    if (property_name != u8"static") {
      test_parser p(
          concat(u8"interface I { static readonly ", property_name, u8"; }"),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // property
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      test_parser p(
          concat(u8"interface I { static async\n ", property_name, u8"(); }"),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      test_parser p(
          concat(u8"interface I { static\n", property_name, u8"(); }"),
          typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({property_name}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      test_parser p(concat(u8"interface I { static\n", property_name, u8"; }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({property_name}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_properties_cannot_be_static,  //
                  static_keyword, strlen(u8"interface I { "), u8"static"),
          }));
    }
  }

  {
    test_parser p(u8"interface I { static field\n method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_static,  //
                        static_keyword, strlen(u8"interface I { "), u8"static"),
                }));
  }

  {
    test_parser p(u8"interface I { static field\n ['methodName'](); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_static,  //
                        static_keyword, strlen(u8"interface I { "), u8"static"),
                }));
  }

  {
    test_parser p(u8"interface I { static field? method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ::testing::UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_properties_cannot_be_static,  //
                        static_keyword, strlen(u8"interface I { "), u8"static"),
                    DIAG_TYPE(diag_missing_semicolon_after_field)));
  }
}

TEST_F(test_parse_typescript_interface, async_methods_are_not_allowed) {
  for (string8 method_name : dirty_set<string8>{u8"method"} | keywords) {
    SCOPED_TRACE(out_string8(method_name));

    {
      test_parser p(concat(u8"interface I { async ", method_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // method
                                "visit_enter_function_scope",   // method
                                "visit_exit_function_scope",    // method
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_interface_methods_cannot_be_async,  //
                          async_keyword, strlen(u8"interface I { "), u8"async"),
                  }));
    }

    {
      // ASI activates after 'async'.
      test_parser p(concat(u8"interface I { async\n", method_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.property_declarations, ElementsAre(u8"async", method_name));
      EXPECT_THAT(p.errors, IsEmpty());
    }
  }
}

TEST_F(test_parse_typescript_interface, generator_methods_are_not_allowed) {
  for (string8 method_name : dirty_set<string8>{u8"method"} | keywords) {
    SCOPED_TRACE(out_string8(method_name));

    {
      test_parser p(concat(u8"interface I { *", method_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_property_declaration",   // method
                                "visit_enter_function_scope",   // method
                                "visit_exit_function_scope",    // method
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code,
                                diag_interface_methods_cannot_be_generators,  //
                                star, strlen(u8"interface I { "), u8"*"),
          }));
    }

    {
      test_parser p(concat(u8"interface I { static *", method_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_methods_cannot_be_generators,  //
                  star, strlen(u8"interface I { static "), u8"*")));
    }

    {
      test_parser p(concat(u8"interface I { async *", method_name, u8"(); }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_methods_cannot_be_async),
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_methods_cannot_be_generators,  //
                  star, strlen(u8"interface I { async "), u8"*")));
    }
  }
}

TEST_F(test_parse_typescript_interface,
       static_async_methods_are_definitely_not_allowed) {
  {
    test_parser p(u8"interface I { static async method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_be_async,  //
                async_keyword, strlen(u8"interface I { static "), u8"async"),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { "), u8"static")));
  }

  {
    test_parser p(u8"interface I { async static method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_be_async,  //
                async_keyword, strlen(u8"interface I { "), u8"async"),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { async "), u8"static")));
  }

  {
    test_parser p(u8"interface I { async static *method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_be_async,  //
                async_keyword, strlen(u8"interface I { "), u8"async"),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_methods_cannot_be_generators,  //
                star, strlen(u8"interface I { async static "), u8"*"),
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { async "), u8"static")));
  }
}

TEST_F(test_parse_typescript_interface, field_initializers_are_not_allowed) {
  for (string8 field_name : dirty_set<string8>{u8"field"} | keywords) {
    SCOPED_TRACE(out_string8(field_name));

    {
      test_parser p(concat(u8"interface I { ", field_name, u8" = y; }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",   // I
                                "visit_enter_interface_scope",  //
                                "visit_variable_use",           // y
                                "visit_property_declaration",   // field_name
                                "visit_exit_interface_scope",   //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_fields_cannot_have_initializers,  //
                  equal, (u8"interface I { " + field_name + u8" ").size(),
                  u8"="),
          }));
    }

    {
      test_parser p(concat(u8"interface I { static ", field_name, u8" = y; }"),
                    typescript_options, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  p.code, diag_interface_fields_cannot_have_initializers,  //
                  equal,
                  (u8"interface I { static " + field_name + u8" ").size(),
                  u8"=")));
    }
  }

  {
    test_parser p(u8"interface I { 'fieldName' = init; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_fields_cannot_have_initializers,  //
                equal, strlen(u8"interface I { 'fieldName' "), u8"="),
        }));
  }

  {
    test_parser p(u8"interface I { fieldName: typeName = init; }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_interface_fields_cannot_have_initializers,  //
                equal, strlen(u8"interface I { fieldName: typeName "), u8"="),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       interface_named_await_in_async_function) {
  {
    test_parser p(u8"interface await {}", typescript_options);
    p.parse_and_visit_statement();
  }

  {
    test_parser p(
        u8"function f() {"
        u8"interface await {}"
        u8"}",
        typescript_options);
    p.parse_and_visit_statement();
  }

  {
    test_parser p(u8"async function g() { interface await {} }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_cannot_declare_await_in_async_function, name,
                strlen(u8"async function g() { interface "), u8"await"),
        }));
  }
}

TEST_F(test_parse_typescript_interface, call_signature) {
  {
    test_parser p(u8"interface I { (param); }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_property_declaration",  // (call signature)
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
  }
}

TEST_F(test_parse_typescript_interface,
       call_signature_after_invalid_field_with_newline) {
  {
    test_parser p(
        u8"interface I {\n"
        u8"  field!\n"
        u8"  (param);\n"
        u8"}"_sv,
        typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_property_declaration",   // field
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_property_declaration",  // (call signature)
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces,  //
                bang, strlen(u8"interface I {\n  field"), u8"!"),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       call_signature_cannot_have_generator_star) {
  {
    test_parser p(u8"interface I { *(param); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_property_declaration",  // (call signature)
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_interface_methods_cannot_be_generators,  //
                        star, strlen(u8"interface I { "), u8"*"),
                }));
  }
}

TEST_F(test_parse_typescript_interface, generic_call_signature) {
  {
    test_parser p(u8"interface I { <T>(param); }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              // TODO(strager): Emit something other than
                              // visit_property_declaration instead?
                              "visit_property_declaration",  // (call signature)
                              "visit_enter_function_scope",  // (call signature)
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // (call signature)
                              "visit_exit_interface_scope",  // I
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({interface_decl(u8"I"), generic_param_decl(u8"T"),
                          func_param_decl(u8"param")}));
  }
}

TEST_F(test_parse_typescript_interface, generic_interface) {
  {
    test_parser p(u8"interface I<T> { field: T; }", typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // I
                              "visit_variable_declaration",   // T
                              "visit_variable_type_use",      // T
                              "visit_property_declaration",   // field
                              "visit_exit_interface_scope",   // I
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({interface_decl(u8"I"), generic_param_decl(u8"T")}));
  }
}

TEST_F(test_parse_typescript_interface, access_specifiers_are_not_allowed) {
  {
    test_parser p(u8"interface I { public method(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_interface_properties_cannot_be_explicitly_public,  //
                public_keyword, strlen(u8"interface I { "), u8"public"),
        }));
  }

  {
    test_parser p(u8"interface I { protected method(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_interface_properties_cannot_be_protected,  //
                              protected_keyword, strlen(u8"interface I { "),
                              u8"protected"),
        }));
  }

  {
    test_parser p(u8"interface I { private method(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_interface_properties_cannot_be_private,  //
                              property_name_or_private_keyword,
                              strlen(u8"interface I { "), u8"private"),
        }));
  }
}

TEST_F(test_parse_typescript_interface, static_blocks_are_not_allowed) {
  {
    test_parser p(u8"interface I { static { console.log('hello'); } }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.property_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_interfaces_cannot_contain_static_blocks,  //
                static_token, strlen(u8"interface I { "), u8"static"),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       type_annotations_dont_add_extra_diagnostic_in_javascript) {
  {
    test_parser p(u8"interface I<T> { method(): Type; }"_sv, javascript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(diag_typescript_interfaces_not_allowed_in_javascript),
        }))
        << "diag_typescript_type_annotations_not_allowed_in_javascript should "
           "not be reported";
  }
}

TEST_F(test_parse_typescript_interface, method_requires_semicolon_or_asi) {
  {
    test_parser p(
        u8"interface I {\n"
        u8"  f()\n"      // ASI
        u8"  g() }"_sv,  // ASI
        typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // f
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_property_declaration",   // g
                              "visit_enter_function_scope",   // g
                              "visit_exit_function_scope",    // g
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }

  {
    test_parser p(u8"interface I { f() g(); }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // f
                              "visit_enter_function_scope",   // f
                              "visit_exit_function_scope",    // f
                              "visit_property_declaration",   // g
                              "visit_enter_function_scope",   // g
                              "visit_exit_function_scope",    // g
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_semicolon_after_interface_method,  //
                              expected_semicolon, strlen(u8"interface I { f()"),
                              u8""),
        }));
  }
}

TEST_F(test_parse_typescript_interface,
       abstract_properties_are_not_allowed_in_interfaces) {
  {
    test_parser p(u8"interface I { abstract myField; }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // myField
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_interface,  //
                abstract_keyword, strlen(u8"interface I { "), u8"abstract"),
        }));
  }

  {
    test_parser p(u8"interface I { abstract myMethod(); }"_sv,
                  typescript_options, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  // {
                              "visit_property_declaration",   // myMethod
                              "visit_enter_function_scope",   // myMethod
                              "visit_exit_function_scope",    // myMethod
                              "visit_exit_interface_scope",   // }
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_abstract_property_not_allowed_in_interface,  //
                abstract_keyword, strlen(u8"interface I { "), u8"abstract"),
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
