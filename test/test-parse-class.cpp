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
class Test_Parse_Class : public Test_Parse_Expression {};

TEST_F(Test_Parse_Class, super_in_class) {
  test_parse_and_visit_statement(
      u8"class C extends Base { constructor() { super(); } }"_sv, no_diags,
      javascript_options);
}

TEST_F(Test_Parse_Class, parse_class_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"class C {}"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"C"_sv)}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class Derived extends Base {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"Derived"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Base"}));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // Derived
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // Derived
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class FileStream extends fs.ReadStream {}"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"fs"}));
  }

  {
    Test_Parser p(u8"class A {} class B {}"_sv);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"A"_sv), class_decl(u8"B"_sv)}));
  }
}

TEST_F(Test_Parse_Class, class_statement_requires_a_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class {}"_sv,  //
        u8"^^^^^ Diag_Missing_Name_In_Class_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_requires_a_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C "_sv,  //
        u8"       ` Diag_Missing_Body_For_Class"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class Derived extends Base "_sv,  //
        u8"                          ` Diag_Missing_Body_For_Class"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // Derived
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // Derived
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class ;"_sv,                               //
        u8"     ` Diag_Missing_Body_For_Class"_diag,  //
        u8"^^^^^ Diag_Missing_Name_In_Class_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                          }));
  }
}

TEST_F(Test_Parse_Class, unclosed_class_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { "_sv,  //
        u8"        ^ Diag_Unclosed_Class_Block"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        //
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method() {} "_sv,  //
        u8"        ^ Diag_Unclosed_Class_Block"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { property "_sv,  //
        u8"        ^ Diag_Unclosed_Class_Block"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // property
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_with_odd_heritage) {
  {
    // TODO(strager): Should this report errors?
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C extends 0 {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C extends null {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C extends (A, B) {}"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // A
                              "visit_variable_use",            // B
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_extending_class_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C extends class B { x() {} } { y() {} }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C {
                              "visit_enter_class_scope",          // B {
                              "visit_enter_class_scope_body",     // B
                              "visit_enter_function_scope",       // B.x
                              "visit_enter_function_scope_body",  // B.x
                              "visit_exit_function_scope",        // B.x
                              "visit_property_declaration",       // B.x
                              "visit_exit_class_scope",           // } B
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // C.y
                              "visit_enter_function_scope_body",  // C.y
                              "visit_exit_function_scope",        // C.y
                              "visit_property_declaration",       // C.y
                              "visit_exit_class_scope",           // } C
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_with_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class Monster { eatMuffins(muffinCount) { } }"_sv, no_diags,
        javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"muffinCount");
    EXPECT_EQ(p.variable_declarations[1].name, u8"Monster");

    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"eatMuffins"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // muffinCount
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // eatMuffins
                              "visit_exit_class_scope",           //
                              "visit_variable_declaration",       // Monster
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // m
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static async m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { get length() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { set length(value) { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"length"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"
        u8"  static get length() { }\n"
        u8"  static set length(l) { }\n"
        u8"}"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"length", u8"length"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { a(){} b(){} c(){} }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"a", u8"b", u8"c"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { \"stringKey\"() {} }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { [x + y]() {} }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async #m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *#m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async *#m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#m"}));
  }
}

TEST_F(Test_Parse_Class, class_statement_methods_with_arrow_operator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method() => {} }"_sv,  //
        u8"                   ^^ Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }
}

TEST_F(Test_Parse_Class, missing_class_method_name_fails) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class Monster { (muffinCount) { } }"_sv,  //
        u8"                ` Diag_Missing_Class_Method_Name"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // muffinCount
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // (unnamed)
                              "visit_exit_class_scope",           // Monster
                              "visit_variable_declaration",       // Monster
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_with_fields) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class FruitBasket { banana; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // banana
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // FruitBasket
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"banana"}));
  }

  {
    // ASI after field without initializer.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class FruitBasket { banana }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // banana
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // FruitBasket
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"banana"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { prop = init; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // prop
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"prop"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    // ASI after field with initializer.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { prop = init }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // prop
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"prop"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static prop = init }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_variable_use",            // init
                              "visit_property_declaration",    // prop
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }))
        << "visit_enter_class_construct_scope should not be used for static "
           "properties";
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"prop"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #prop = init; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#prop"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #prop = init;\nf() {this.#prop;} }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#prop", u8"f"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    // ASI after field name before private identifier.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #first\n#second }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"#first", u8"#second"}));
  }

  {
    // ASI after initializer before private identifier.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #first = x\n#second }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"#first", u8"#second"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { 'fieldName'; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // 'fieldName'
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    // ASI after field without initializer.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { 'fieldName' }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // 'fieldName'
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { 'fieldName' = init; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",  // 'fieldName'
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"init"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { 3.14 = pi; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // pi
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",  // 'fieldName'
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"pi"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { [x + y]; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_variable_use",            // x
                              "visit_variable_use",            // y
                              "visit_property_declaration",    // (x + y)
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }

  {
    // ASI after field without initializer.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { [x + y] }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_variable_use",            // x
                              "visit_variable_use",            // y
                              "visit_property_declaration",    // (x + y)
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { [x + y] = init; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_variable_use",                 // x
                              "visit_variable_use",                 // y
                              "visit_enter_class_construct_scope",  //
                              "visit_variable_use",                 // init
                              "visit_exit_class_construct_scope",   //
                              "visit_property_declaration",         // (x + y)
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  //
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    // TODO(strager): Is this order correct?
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"y", u8"init"}));
  }

  // TODO(strager): '*field=init' is an error.
  // TODO(strager): 'async field=init' is an error.
  // TODO(strager): 'get field=init' is an error.
  // TODO(strager): 'set field=init' is an error.
}

TEST_F(Test_Parse_Class, class_fields_with_comma_are_not_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { a = 1, b = 2 }"_sv,  //
        u8"               ^ Diag_Unexpected_Comma_After_Class_Field"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            //
                              "visit_enter_class_scope_body",       //
                              "visit_enter_class_construct_scope",  // 1
                              "visit_exit_class_construct_scope",   // 1
                              "visit_property_declaration",         // a
                              "visit_enter_class_construct_scope",  // 2
                              "visit_exit_class_construct_scope",   // 2
                              "visit_property_declaration",         // b
                              "visit_exit_class_scope",             //
                              "visit_variable_declaration",         // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"class C { a, }"_sv,  //
      u8"           ^ Diag_Unexpected_Comma_After_Class_Field"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { a = null, }"_sv,  //
      u8"                  ^ Diag_Unexpected_Comma_After_Class_Field"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { a: any, }"_sv,  //
      u8"                ^ Diag_Unexpected_Comma_After_Class_Field"_diag,
      typescript_options);
}

TEST_F(Test_Parse_Class,
       class_fields_without_initializer_allow_asi_after_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f\ng() {} }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_property_declaration",       // f
                              "visit_enter_function_scope",       // g
                              "visit_enter_function_scope_body",  // g
                              "visit_exit_function_scope",        // g
                              "visit_property_declaration",       // g
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f", u8"g"}));
  }

  std::vector<String8> class_declarations{
      u8"method() {}",   u8"*method() {}", u8"[expr]() {}",
      u8"'method'() {}", u8"3.14() {}",
  };
  for (String8_View keyword : keywords) {
    class_declarations.emplace_back(concat(keyword, u8"() {}"_sv));
  }
  for (const String8& second_member : class_declarations) {
    {
      Padded_String code(
          concat(u8"class C { myField\n"_sv, second_member, u8" }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.property_declarations,
                  ElementsAre(u8"myField", ::testing::_));
    }

    for (String8_View first_member :
         {u8"3.14"_sv, u8"'bananas'"_sv, u8"[expr]"_sv}) {
      Padded_String code(concat(u8"class C { "_sv, first_member, u8"\n"_sv,
                                second_member, u8" }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.property_declarations,
                  ElementsAre(std::nullopt, ::testing::_));
    }
  }
}

TEST_F(Test_Parse_Class, accessor_field) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor myField; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // myField
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor myField = null; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor #myField; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#myField"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor [computedName]; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"computedName"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor 42; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor 'fieldName'; }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static accessor myField; }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myField"}));
  }

  test_parse_and_visit_statement(
      u8"class C { accessor static myField; }"_sv,  //
      u8"                   ^^^^^^ Diag_Class_Modifier_Must_Preceed_Other_Modifier.expected_first_modifier\n"_diag
      u8"          ^^^^^^^^ .expected_second_modifier"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Class, accessor_cannot_be_method) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { accessor myMethod() { } }"_sv,  //
        u8"                           ^ Diag_Class_Accessor_On_Method.method_start\n"_diag
        u8"          ^^^^^^^^ .accessor_keyword"_diag,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // myMethod
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // myMethod
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }))
        << "should be parsed as a method";
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"myMethod"}));
  }

  test_parse_and_visit_statement(
      u8"class C { get accessor myMethod() { } }"_sv,  //
      u8"                               ^ Diag_Class_Accessor_On_Getter_Or_Setter.method_start\n"_diag
      u8"              ^^^^^^^^ .accessor_keyword\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { set accessor myMethod() { } }"_sv,  //
      u8"                               ^ Diag_Class_Accessor_On_Getter_Or_Setter.method_start\n"_diag
      u8"              ^^^^^^^^ .accessor_keyword\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { accessor get myMethod() { } }"_sv,  //
      u8"                               ^ Diag_Class_Accessor_On_Getter_Or_Setter.method_start\n"_diag
      u8"                   ^^^ .getter_setter_keyword\n"_diag
      u8"          ^^^^^^^^ .accessor_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { accessor set myMethod() { } }"_sv,  //
      u8"                               ^ Diag_Class_Accessor_On_Getter_Or_Setter.method_start\n"_diag
      u8"                   ^^^ .getter_setter_keyword\n"_diag
      u8"          ^^^^^^^^ .accessor_keyword"_diag,
      javascript_options);

  test_parse_and_visit_statement(u8"class C { get accessor myField; }"_sv,  //
                                 u8"          ^^^ Diag_Unexpected_Token"_diag,
                                 javascript_options);
  test_parse_and_visit_statement(u8"class C { set accessor myField; }"_sv,  //
                                 u8"          ^^^ Diag_Unexpected_Token"_diag,
                                 javascript_options);
  test_parse_and_visit_statement(
      u8"class C { accessor get myField; }"_sv,  //
      u8"                   ^^^ Diag_Unexpected_Token"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { accessor set myField; }"_sv,  //
      u8"                   ^^^ Diag_Unexpected_Token"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Class, async_keyword_on_getters_and_setters) {
  test_parse_and_visit_statement(
      u8"class C { get async myMethod() { } }"_sv,  //
      u8"              ^^^^^ Diag_Class_Async_On_Getter_Or_Setter.async_keyword\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { set async myMethod() { } }"_sv,  //
      u8"              ^^^^^ Diag_Class_Async_On_Getter_Or_Setter.async_keyword\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { async get myMethod() { } }"_sv,  //
      u8"                ^^^ Diag_Class_Async_On_Getter_Or_Setter.getter_setter_keyword\n"_diag
      u8"          ^^^^^ .async_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { async set myMethod() { } }"_sv,  //
      u8"                ^^^ Diag_Class_Async_On_Getter_Or_Setter.getter_setter_keyword\n"_diag
      u8"          ^^^^^ .async_keyword"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Class, getters_and_setters_cannot_be_generator) {
  test_parse_and_visit_statement(
      u8"class C { get *myMethod() { } }"_sv,  //
      u8"              ^ Diag_Class_Generator_On_Getter_Or_Setter.star_token\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
  test_parse_and_visit_statement(
      u8"class C { set *myMethod() { } }"_sv,  //
      u8"              ^ Diag_Class_Generator_On_Getter_Or_Setter.star_token\n"_diag
      u8"          ^^^ .getter_setter_keyword"_diag,
      javascript_options);
}

TEST_F(Test_Parse_Class, class_methods_should_not_use_function_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { function f() {} }"_sv,  //
        u8"          ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"class C { async function f() {} }"_sv,  //
      u8"                ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag);

  test_parse_and_visit_statement(
      u8"class C { function* f() {} }"_sv,  //
      u8"          ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag);

  test_parse_and_visit_statement(
      u8"class C { static function f() {} }"_sv,  //
      u8"                 ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag);
}

TEST_F(Test_Parse_Class, newline_after_most_modifiers_is_potentially_asi) {
  for (String8_View modifier : {
           u8"abstract"_sv,
           u8"accessor"_sv,
           u8"async"_sv,
           u8"declare"_sv,
           u8"function"_sv,
           u8"override"_sv,
           u8"private"_sv,
           u8"protected"_sv,
           u8"public"_sv,
           u8"readonly"_sv,
       }) {
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, modifier,
                 u8" /*ASI*/\n myMethod() {} }"_sv),
          no_diags, javascript_options);
      EXPECT_THAT(p.property_declarations,
                  ElementsAreArray({modifier, u8"myMethod"_sv}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, modifier,
                 u8" /*ASI*/\n static myMethod() {} }"_sv),
          no_diags, javascript_options);
      EXPECT_THAT(p.property_declarations,
                  ElementsAreArray({modifier, u8"myMethod"_sv}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, modifier, u8" \n = 42; }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({modifier}));
    }
  }
}

TEST_F(Test_Parse_Class, class_statement_with_keyword_property) {
  for (String8_View keyword : keywords) {
    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, keyword, u8"(){} }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { *"_sv, keyword, u8"(){} }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    for (String8_View prefix :
         {u8"async"_sv, u8"get"_sv, u8"set"_sv, u8"static"_sv,
          u8"static async"_sv, u8"static get"_sv, u8"static set"_sv}) {
      Test_Parser p(
          concat(u8"class C { "_sv, prefix, u8" "_sv, keyword, u8"(){} }"_sv));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, keyword, u8" }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, keyword, u8"; }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { "_sv, keyword, u8" = init; }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }

    {
      Spy_Visitor p = test_parse_and_visit_statement(
          concat(u8"class C { accessor "_sv, keyword, u8"; }"_sv), no_diags,
          javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }
  }

  for (String8_View keyword : strict_reserved_keywords) {
    String8 property = escape_first_character_in_keyword(keyword);
    for (String8_View prefix :
         {u8""_sv, u8"*"_sv, u8"async"_sv, u8"async *"_sv, u8"get"_sv,
          u8"set"_sv, u8"static"_sv, u8"static *"_sv, u8"static async"_sv,
          u8"static async *"_sv, u8"static get"_sv, u8"static set"_sv}) {
      Padded_String code(
          concat(u8"class C { "_sv, prefix, u8" "_sv, property, u8"(){} }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_statement(
          code.string_view(), no_diags, javascript_options);
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }
  }
}

TEST_F(Test_Parse_Class,
       typescript_class_statement_with_readonly_keyword_property) {
  for (String8_View keyword : keywords) {
    {
      Test_Parser p(concat(u8"class C { readonly "_sv, keyword, u8"; }"_sv),
                    typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }
  }
}

TEST_F(Test_Parse_Class, typescript_class_with_keyword_generic_method) {
  for (String8_View keyword : keywords) {
    {
      Test_Parser p(concat(u8"class C { "_sv, keyword, u8"<T>(){} }"_sv),
                    typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
    }
  }

  {
    // A generic method named 'async' should not be async.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async<T>() { let await; await(x); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // async
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  // async
                              "visit_variable_declaration",       // await
                              "visit_variable_use",               // await
                              "visit_variable_use",               // x
                              "visit_exit_function_scope",        // async
                              "visit_property_declaration",       // async
                              "visit_exit_class_scope",           // C
                              "visit_variable_declaration",       // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_with_number_methods) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class Wat { 42.0() { } }"_sv, no_diags, javascript_options);
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // 42.0
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // Wat
                          }));
  }
}

TEST_F(Test_Parse_Class, class_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(class C { })"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"(class { })"_sv, no_diags,
                                                   javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(class { a() {} [b]() {} })"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // a
                              "visit_enter_function_scope_body",  // a
                              "visit_exit_function_scope",        // a
                              "visit_property_declaration",       // a
                              "visit_variable_use",               // b
                              "visit_enter_function_scope",       // [b]
                              "visit_enter_function_scope_body",  // [b]
                              "visit_exit_function_scope",        // [b]
                              "visit_property_declaration",       // [b]
                              "visit_exit_class_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(class A extends B {})"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // B
                              "visit_enter_class_scope_body",  // A
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(class extends C {})"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_variable_use",            // C
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // }
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(class C {#x = 10; m() {this.#x;}})"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",            // {
                              "visit_enter_class_scope_body",       // C
                              "visit_enter_class_construct_scope",  // 10
                              "visit_exit_class_construct_scope",   // 10
                              "visit_property_declaration",         // #x
                              "visit_enter_function_scope",         //
                              "visit_enter_function_scope_body",    //
                              "visit_exit_function_scope",          //
                              "visit_property_declaration",         // m
                              "visit_exit_class_scope",             // }
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_allows_stray_semicolons) {
  Spy_Visitor p = test_parse_and_visit_statement(u8"class C{ ; f(){} ; }"_sv,
                                                 no_diags, javascript_options);
  EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"}));
}

TEST_F(Test_Parse_Class, class_method_without_parameter_list) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method { body; } }"_sv,  //
        u8"                ` Diag_Missing_Function_Parameter_List"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_enter_function_scope_body",  // method
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }

  test_parse_and_visit_statement(
      u8"class C { [method+name] { body; } }"_sv,  //
      u8"                       ` Diag_Missing_Function_Parameter_List"_diag);

  test_parse_and_visit_statement(
      u8"class C { 'method name' { body; } }"_sv,  //
      u8"                       ` Diag_Missing_Function_Parameter_List"_diag);
}

TEST_F(Test_Parse_Class, stray_identifier_before_class_method) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { junkIdentifier method(arg) { body; } }"_sv,  //
        u8"          ^^^^^^^^^^^^^^ Diag_Unexpected_Token"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // arg
                              "visit_enter_function_scope_body",  // method
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { #junkIdentifier #method(arg) { body; } }"_sv,  //
        u8"          ^^^^^^^^^^^^^^^ Diag_Unexpected_Token"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"#method"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { junkIdentifier *method(arg) { body; } }"_sv,  //
        u8"          ^^^^^^^^^^^^^^ Diag_Unexpected_Token"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // method
                              "visit_variable_declaration",       // arg
                              "visit_enter_function_scope_body",  // method
                              "visit_variable_use",               // body
                              "visit_exit_function_scope",        // method
                              "visit_property_declaration",       // method
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(Test_Parse_Class, stray_left_curly_in_class_is_ignored) {
  // TODO(strager): Is this the right approach? What about 'class C { { } }'?
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { { method() {} }"_sv,  //
        u8"          ^ Diag_Unexpected_Token"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"}));
  }
}

TEST_F(Test_Parse_Class, stray_keyword_in_class_body) {
  test_parse_and_visit_statement(
      u8"class C { if method(arg) { body; } instanceof myField; }"_sv,  //
      u8"                                   ^^^^^^^^^^ Diag_Unexpected_Token"_diag,  //
      u8"          ^^ Diag_Unexpected_Token"_diag);
}

TEST_F(Test_Parse_Class,
       class_statement_as_do_while_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"do class C {} while (cond);"_sv,  //
        u8"  ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::do_while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                              "visit_variable_use",            // cond
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_as_if_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"if (cond) class C {} after"_sv,  //
        u8"         ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // cond
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_variable_use",            // after
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"if (cond) class C {} else {}"_sv,  //
        u8"         ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // cond
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_enter_block_scope",       // else
                              "visit_exit_block_scope",        // else
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"if (cond) {} else class C {}"_sv,  //
        u8"                 ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"                  ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // cond
                              "visit_enter_block_scope",       // if
                              "visit_exit_block_scope",        // if
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_as_for_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"for (;cond;) class C {}"_sv,  //
        u8"            ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::for_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // cond
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_Class,
       class_statement_as_while_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while (cond) class C {}"_sv,  //
        u8"            ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // cond
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_as_with_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (obj) class C {}"_sv,  //
        u8"          ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"           ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::with_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // obj
                              "visit_enter_with_scope",        // with
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_exit_with_scope",
                          }));
  }
}

TEST_F(Test_Parse_Class, class_statement_as_label_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"l: class C {}"_sv,  //
        u8"  ` Diag_Class_Statement_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^^^ .class_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::labelled_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
  }
}

TEST_F(Test_Parse_Class, class_in_async_function_is_allowed) {
  test_parse_and_visit_statement(
      u8"async function f() {"
      u8"  class C {}"
      u8"}"_sv,
      no_diags, javascript_options);
}

TEST_F(Test_Parse_Class, class_named_await_in_async_function) {
  {
    test_parse_and_visit_statement(u8"class await {}"_sv, no_diags,
                                   javascript_options);
  }

  test_parse_and_visit_statement(
      u8"function f() {"
      u8"class await {}"
      u8"}"_sv,
      no_diags, javascript_options);

  test_parse_and_visit_module(
      u8"async function g() { class await {} }"_sv,  //
      u8"                           ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag);
}

TEST_F(Test_Parse_Class, async_static_method_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async static m() { await myPromise; } }"_sv,  //
        u8"          ^^^^^^^^^^^^ Diag_Async_Static_Method"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
                              "visit_enter_function_scope",       // m
                              "visit_enter_function_scope_body",  // m
                              "visit_variable_use",               // myPromise
                              "visit_exit_function_scope",        // m
                              "visit_property_declaration",       // m
                              "visit_exit_class_scope",
                              "visit_variable_declaration",  // C
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async static static() { await myPromise; } }"_sv,  //
        u8"          ^^^^^^^^^^^^ Diag_Async_Static_Method"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"static"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async static *m() { await myPromise; yield 42; } }"_sv,  //
        u8"          ^^^^^^^^^^^^ Diag_Async_Static_Method"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }
}

TEST_F(Test_Parse_Class, static_method_allows_newline_after_static_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static\n m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static\n *m() { } }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static\n async *m() { } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"m"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static\n async\n *m() { } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"async", u8"m"}));
  }
}

TEST_F(Test_Parse_Class, typescript_style_const_field) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { const f = null }"_sv,  //
        u8"          ^^^^^ Diag_TypeScript_Style_Const_Field"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"}));
  }
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { const f }"_sv,  //
        u8"          ^^^^^ Diag_TypeScript_Style_Const_Field"_diag);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"}));
  }
}

TEST_F(Test_Parse_Class, class_expression_body_is_visited_first_in_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"[before, class C { m() { inside; } }, after];"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C {
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // m
                              "visit_enter_function_scope_body",  // m
                              "visit_variable_use",               // inside
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // m
                              "visit_exit_class_scope",           // } C
                              "visit_variable_use",               // before
                              "visit_variable_use",               // after
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"inside", u8"before", u8"after"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"[before, class C { m() { inside; } }.prop, after] = [1,2,3];"_sv,
        no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C {
                              "visit_enter_class_scope_body",     // C
                              "visit_enter_function_scope",       // m
                              "visit_enter_function_scope_body",  // m
                              "visit_variable_use",               // inside
                              "visit_exit_function_scope",        //
                              "visit_property_declaration",       // m
                              "visit_exit_class_scope",           // }
                              "visit_variable_assignment",        // before
                              "visit_variable_assignment",        // after
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"inside"}));
    EXPECT_THAT(p.variable_assignments,
                ElementsAreArray({u8"before", u8"after"}));
  }
}

TEST_F(Test_Parse_Class, static_blocks) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { static #private; static { C.#private; } }"_sv, no_diags,
        javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  //
                              "visit_property_declaration",    // #private
                              "visit_enter_block_scope",       // static
                              "visit_variable_use",            // C
                              "visit_exit_block_scope",        // static
                              "visit_exit_class_scope",        // C
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C"}));
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
