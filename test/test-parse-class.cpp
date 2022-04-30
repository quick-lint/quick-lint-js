// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse, super_in_class) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C extends Base { constructor() { super(); } }");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_class_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class C {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class Derived extends Base {}"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Derived");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);

    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"Base");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class FileStream extends fs.ReadStream {}");
    ASSERT_EQ(v.variable_uses.size(), 1);
    EXPECT_EQ(v.variable_uses[0].name, u8"fs");
  }

  {
    spy_visitor v;
    padded_string code(u8"class A {} class B {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(
            spy_visitor::visited_variable_declaration{
                u8"A", variable_kind::_class, variable_init_kind::normal},
            spy_visitor::visited_variable_declaration{
                u8"B", variable_kind::_class, variable_init_kind::normal}));
  }
}

TEST(test_parse, class_statement_requires_a_name) {
  {
    spy_visitor v;
    padded_string code(u8"class {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_name_in_class_statement,  //
                              class_keyword, 0, u8"class")));
  }
}

TEST(test_parse, class_statement_requires_a_body) {
  {
    spy_visitor v;
    padded_string code(u8"class C "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // C
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_body_for_class,  //
                              class_keyword_and_name_and_heritage,
                              strlen(u8"class C"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class ;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_missing_name_in_class_statement,  //
                              class_keyword, 0, u8"class"),
            DIAG_TYPE_OFFSETS(&code, diag_missing_body_for_class,  //
                              class_keyword_and_name_and_heritage,
                              strlen(u8"class"), u8"")));
  }
}

TEST(test_parse, unclosed_class_statement) {
  {
    spy_visitor v;
    padded_string code(u8"class C { "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_class_block,  //
                              block_open, strlen(u8"class C "), u8"{")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { method() {} "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          // C
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope"));         // C
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_class_block,  //
                              block_open, strlen(u8"class C "), u8"{")));
  }
}

TEST(test_parse, class_statement_with_odd_heritage) {
  {
    // TODO(strager): Should this report errors?
    spy_visitor v = parse_and_visit_statement(u8"class C extends 0 {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C extends null {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C extends (A, B) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // A
                                      "visit_variable_use",          // B
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     //
                                      "visit_exit_class_scope"));
  }
}

TEST(test_parse, class_statement_extending_class_expression) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C extends class B { x() {} } { y() {} }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",          // B
                                      "visit_variable_declaration",       // B
                                      "visit_property_declaration",       // B.x
                                      "visit_enter_function_scope",       // B.x
                                      "visit_enter_function_scope_body",  // B.x
                                      "visit_exit_function_scope",        // B.x
                                      "visit_exit_class_scope",           // B
                                      "visit_variable_declaration",       // C
                                      "visit_enter_class_scope",          // C
                                      "visit_property_declaration",       // C.y
                                      "visit_enter_function_scope",       // C.y
                                      "visit_enter_function_scope_body",  // C.y
                                      "visit_exit_function_scope",        // C.y
                                      "visit_exit_class_scope"));         // C
  }
}

TEST(test_parse, class_statement_with_methods) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"class Monster { eatMuffins(muffinCount) { } }");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Monster");
    EXPECT_EQ(v.variable_declarations[1].name, u8"muffinCount");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, u8"eatMuffins");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // Monster
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // eatMuffins
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       // muffinCount
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static m() { } }"_sv);

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, u8"m");

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // C
                                      "visit_enter_class_scope",          //
                                      "visit_property_declaration",       // m
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { async m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static async m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { *m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { get length() { } }"_sv);
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C {\n"
        u8"  static get length() { }\n"
        u8"  static set length(l) { }\n"
        u8"}");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"},
                    spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { a(){} b(){} c(){} }"_sv);
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, u8"a");
    EXPECT_EQ(v.property_declarations[1].name, u8"b");
    EXPECT_EQ(v.property_declarations[2].name, u8"c");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { \"stringKey\"() {} }");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { [x + y]() {} }"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { #m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"#m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async #m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"#m"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { *#m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"#m"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async *#m() { } }"_sv);
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"#m"}));
  }
}

TEST(test_parse, class_statement_methods_with_arrow_operator) {
  {
    spy_visitor v;
    padded_string code(u8"class C { method() => {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code,
            diag_functions_or_methods_should_not_have_arrow_operator,  //
            arrow_operator, strlen(u8"class C { method() "), u8"=>")));
  }
}

TEST(test_parse, class_statement_with_fields) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"class FruitBasket { banana; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // FruitBasket
                            "visit_enter_class_scope",     //
                            "visit_property_declaration",  // banana
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"banana"}));
  }

  {
    // ASI after field without initializer.
    spy_visitor v = parse_and_visit_statement(u8"class FruitBasket { banana }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // FruitBasket
                            "visit_enter_class_scope",     //
                            "visit_property_declaration",  // banana
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"banana"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { prop = init; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // init
                            "visit_property_declaration",  // prop
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"prop"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    // ASI after field with initializer.
    spy_visitor v = parse_and_visit_statement(u8"class C { prop = init }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // init
                            "visit_property_declaration",  // prop
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"prop"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static prop = init }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // init
                            "visit_property_declaration",  // prop
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"prop"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { #prop = init; }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"#prop"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"class C { #prop = init;\nf() {this.#prop;} }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"#prop"},
                    spy_visitor::visited_property_declaration{u8"f"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    // ASI after field name before private identifier.
    spy_visitor v = parse_and_visit_statement(u8"class C { #first\n#second }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"#first"},
                    spy_visitor::visited_property_declaration{u8"#second"}));
  }

  {
    // ASI after initializer before private identifier.
    spy_visitor v =
        parse_and_visit_statement(u8"class C { #first = x\n#second }");
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"#first"},
                    spy_visitor::visited_property_declaration{u8"#second"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { 'fieldName'; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_property_declaration",  // 'fieldName'
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
  }

  {
    // ASI after field without initializer.
    spy_visitor v = parse_and_visit_statement(u8"class C { 'fieldName' }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_property_declaration",  // 'fieldName'
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { 'fieldName' = init; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // init
                            "visit_property_declaration",  // 'fieldName'
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { 3.14 = pi; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // pi
                            "visit_property_declaration",  // 'fieldName'
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"pi"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { [x + y]; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // x
                            "visit_variable_use",          // y
                            "visit_property_declaration",  // (x + y)
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"y"}));
  }

  {
    // ASI after field without initializer.
    spy_visitor v = parse_and_visit_statement(u8"class C { [x + y] }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // x
                            "visit_variable_use",          // y
                            "visit_property_declaration",  // (x + y)
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"y"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { [x + y] = init; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  //
                            "visit_enter_class_scope",     //
                            "visit_variable_use",          // x
                            "visit_variable_use",          // y
                            "visit_variable_use",          // init
                            "visit_property_declaration",  // (x + y)
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    // TODO(strager): Is this order correct?
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"y"},
                            spy_visitor::visited_variable_use{u8"init"}));
  }

  // TODO(strager): '*field=init' is an error.
  // TODO(strager): 'async field=init' is an error.
  // TODO(strager): 'get field=init' is an error.
  // TODO(strager): 'set field=init' is an error.
}

TEST(test_parse, class_fields_without_initializer_allow_asi_after_name) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class C { f\ng() {} }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          // C
                            "visit_property_declaration",       // f
                            "visit_property_declaration",       // g
                            "visit_enter_function_scope",       // g
                            "visit_enter_function_scope_body",  // g
                            "visit_exit_function_scope",        // g
                            "visit_exit_class_scope"));         // C
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"f"},
                            spy_visitor::visited_property_declaration{u8"g"}));
  }

  std::vector<string8> class_declarations{
      u8"method() {}",   u8"*method() {}", u8"[expr]() {}",
      u8"'method'() {}", u8"3.14() {}",
  };
  for (string8 keyword : keywords) {
    class_declarations.emplace_back(keyword + u8"() {}");
  }
  for (const string8& second_member : class_declarations) {
    {
      padded_string code(u8"class C { myField\n" + second_member + u8" }");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{u8"myField"},
                      ::testing::_));
    }

    for (string8 first_member : {u8"3.14", u8"'bananas'", u8"[expr]"}) {
      padded_string code(u8"class C { " + first_member + u8"\n" +
                         second_member + u8" }");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{std::nullopt},
                      ::testing::_));
    }
  }
}

TEST(test_parse, class_methods_should_not_use_function_keyword) {
  {
    spy_visitor v;
    padded_string code(u8"class C { function f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // C
                                      "visit_enter_class_scope",          //
                                      "visit_property_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_exit_class_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_methods_should_not_use_function_keyword,  //
                    function_token, strlen(u8"class C { "), u8"function")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { async function f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_methods_should_not_use_function_keyword,  //
            function_token, strlen(u8"class C { async "), u8"function")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { function* f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_methods_should_not_use_function_keyword,  //
                    function_token, strlen(u8"class C { "), u8"function")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { static function f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &code, diag_methods_should_not_use_function_keyword,  //
            function_token, strlen(u8"class C { static "), u8"function")));
  }
}

TEST(test_parse, class_statement_with_keyword_property) {
  for (string8 keyword : keywords) {
    {
      string8 code = u8"class C { " + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    {
      string8 code = u8"class C { *" + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    for (string8 prefix : {u8"async", u8"get", u8"set", u8"static",
                           u8"static async", u8"static get", u8"static set"}) {
      string8 code = u8"class C { " + prefix + u8" " + keyword + u8"(){} }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    {
      string8 code = u8"class C { " + keyword + u8" }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }

    {
      string8 code = u8"class C { " + keyword + u8"; }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }

    {
      string8 code = u8"class C { " + keyword + u8" = init; }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_statement(code.c_str());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }
  }

  for (string8 keyword : strict_reserved_keywords) {
    string8 property = escape_first_character_in_keyword(keyword);
    for (string8 prefix :
         {u8"", u8"*", u8"async", u8"async *", u8"get", u8"set", u8"static",
          u8"static *", u8"static async", u8"static async *", u8"static get",
          u8"static set"}) {
      padded_string code(u8"class C { " + prefix + u8" " + property +
                         u8"(){} }");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_statement(code.string_view());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }
  }
}

TEST(test_parse, class_statement_with_number_methods) {
  {
    spy_visitor v = parse_and_visit_statement(u8"class Wat { 42.0() { } }"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // Wat
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // 42.0
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
  }
}

TEST(test_parse, class_expression) {
  {
    spy_visitor v = parse_and_visit_statement(u8"(class C { })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",     //
                                      "visit_variable_declaration",  // C
                                      "visit_exit_class_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"C");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class { })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(class { a() {} [b]() {} })"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",          //
                                      "visit_property_declaration",       // a
                                      "visit_enter_function_scope",       // a
                                      "visit_enter_function_scope_body",  // a
                                      "visit_exit_function_scope",        // a
                                      "visit_variable_use",               // b
                                      "visit_property_declaration",       // [b]
                                      "visit_enter_function_scope",       // [b]
                                      "visit_enter_function_scope_body",  // [b]
                                      "visit_exit_function_scope",        // [b]
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class A extends B {})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",     //
                                      "visit_variable_use",          // B
                                      "visit_variable_declaration",  // A
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(class extends C {})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",  //
                                      "visit_variable_use",       // C
                                      "visit_exit_class_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"(class C {#x = 10; m() {this.#x;}})"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",
                                      "visit_variable_declaration",       // C
                                      "visit_property_declaration",       // x
                                      "visit_property_declaration",       // m
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_exit_class_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, class_statement_allows_stray_semicolons) {
  spy_visitor v = parse_and_visit_statement(u8"class C{ ; f(){} ; }"_sv);
  ASSERT_EQ(v.property_declarations.size(), 1);
  EXPECT_EQ(v.property_declarations[0].name, u8"f");
}

TEST(test_parse, class_method_without_parameter_list) {
  {
    spy_visitor v;
    padded_string code(u8"class C { method { body; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_variable_use",               // body
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_function_parameter_list,  //
                              expected_parameter_list,
                              strlen(u8"class C { method"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { [method+name] { body; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_function_parameter_list,  //
                              expected_parameter_list,
                              strlen(u8"class C { [method+name]"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { 'method name' { body; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_function_parameter_list,  //
                              expected_parameter_list,
                              strlen(u8"class C { 'method name'"), u8"")));
  }
}

TEST(test_parse, stray_identifier_before_class_method) {
  {
    spy_visitor v;
    padded_string code(u8"class C { junkIdentifier method(arg) { body; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // arg
                            "visit_enter_function_scope_body",  // method
                            "visit_variable_use",               // body
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"method"}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                              token, strlen(u8"class C { "),
                                              u8"junkIdentifier")));
  }

  {
    padded_string code(
        u8"class C { #junkIdentifier #method(arg) { body; } }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"#method"}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                              token, strlen(u8"class C { "),
                                              u8"#junkIdentifier")));
  }

  {
    spy_visitor v;
    padded_string code(
        u8"class C { junkIdentifier *method(arg) { body; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_variable_declaration",       // arg
                            "visit_enter_function_scope_body",  // method
                            "visit_variable_use",               // body
                            "visit_exit_function_scope",        // method
                            "visit_exit_class_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"method"}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                              token, strlen(u8"class C { "),
                                              u8"junkIdentifier")));
  }
}

TEST(test_parse, stray_left_curly_in_class_is_ignored) {
  // TODO(strager): Is this the right approach? What about 'class C { { } }'?
  {
    spy_visitor v;
    padded_string code(u8"class C { { method() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"method"}));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unexpected_token,  //
                              token, strlen(u8"class C { "), u8"{")));
  }
}

TEST(test_parse, stray_keyword_in_class_body) {
  {
    spy_visitor v;
    padded_string code(
        u8"class C { if method(arg) { body; } instanceof myField; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                      token, strlen(u8"class C { "), u8"if"),
                    DIAG_TYPE_OFFSETS(
                        &code, diag_unexpected_token,  //
                        token, strlen(u8"class C { if method(arg) { body; } "),
                        u8"instanceof")));
  }
}

TEST(test_parse, class_statement_as_do_while_statement_body_is_disallowed) {
  {
    padded_string code(u8"do class C {} while (cond);"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope",      // C
                                      "visit_variable_use"));        // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::do_while_loop,                                //
            expected_body, offsets_matcher(&code, strlen(u8"do"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"do "), u8"class"))));
  }
}

TEST(test_parse, class_statement_as_if_statement_body_is_disallowed) {
  {
    padded_string code(u8"if (cond) class C {} after"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope",      // C
                                      "visit_variable_use",          // after
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::if_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"if (cond)"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"if (cond) "), u8"class"))));
  }

  {
    padded_string code(u8"if (cond) class C {} else {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope",      // C
                                      "visit_enter_block_scope",     // else
                                      "visit_exit_block_scope",      // else
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::if_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"if (cond)"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"if (cond) "), u8"class"))));
  }

  {
    padded_string code(u8"if (cond) {} else class C {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_enter_block_scope",     // if
                                      "visit_exit_block_scope",      // if
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope",      // C
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::if_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"if (cond) {} else"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"if (cond) {} else "),
                            u8"class"))));
  }
}

TEST(test_parse, class_statement_as_for_statement_body_is_disallowed) {
  {
    padded_string code(u8"for (;cond;) class C {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope"));    // C
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::for_loop,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"for (;cond;)"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"for (;cond;) "), u8"class"))));
  }
}

TEST(test_parse, class_statement_as_while_statement_body_is_disallowed) {
  {
    padded_string code(u8"while (cond) class C {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope"));    // C
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::while_loop,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"while (cond)"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"while (cond) "), u8"class"))));
  }
}

TEST(test_parse, class_statement_as_with_statement_body_is_disallowed) {
  {
    padded_string code(u8"with (obj) class C {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // obj
                                      "visit_enter_with_scope",      // with
                                      "visit_variable_declaration",  // C
                                      "visit_enter_class_scope",     // C
                                      "visit_exit_class_scope",      // C
                                      "visit_exit_with_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_class_statement_not_allowed_in_body, kind_of_statement,
            statement_kind::with_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"with (obj)"), u8""),  //
            class_keyword,
            offsets_matcher(&code, strlen(u8"with (obj) "), u8"class"))));
  }
}

TEST(test_parse, async_static_method_is_disallowed) {
  {
    spy_visitor v;
    padded_string code(
        u8"class C { async static m() { await myPromise; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));

    EXPECT_EQ(v.property_declarations[0].name, u8"m");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // C
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       // m
                            "visit_enter_function_scope",       // m
                            "visit_enter_function_scope_body",  // m
                            "visit_variable_use",               // myPromise
                            "visit_exit_function_scope",        // m
                            "visit_exit_class_scope"));

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_async_static_method,  //
                    async_static, strlen(u8"class C { "), u8"async static")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { async static static() { } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.property_declarations[0].name, u8"static");
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_async_static_method,  //
                    async_static, strlen(u8"class C { "), u8"async static")));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { async static *m() { } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.property_declarations[0].name, u8"m");
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_async_static_method,  //
                    async_static, strlen(u8"class C { "), u8"async static")));
  }
}

TEST(test_parse, static_method_allows_newline_after_static_keyword) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static\n m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"m");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static\n *m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"m");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static\n async *m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"m");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { static\n async\n *m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"async");
    EXPECT_EQ(v.property_declarations[1].name, u8"m");
  }
}

TEST(test_parse, async_method_prohibits_newline_after_async_keyword) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async\n m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"async");
    EXPECT_EQ(v.property_declarations[1].name, u8"m");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async\n static m() { } }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"async");
    EXPECT_EQ(v.property_declarations[1].name, u8"m");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"class C { async\n = 42 }"_sv);
    EXPECT_EQ(v.property_declarations[0].name, u8"async");
  }
}
TEST(test_parse, typescript_style_const_field) {
  {
    spy_visitor v;
    padded_string code(u8"class C { const f = null }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"f"}));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_typescript_style_const_field,  //
                              const_token, strlen(u8"class C { "), u8"const")));
  }
  {
    spy_visitor v;
    padded_string code(u8"class C { const f }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"f"}));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_typescript_style_const_field,  //
                              const_token, strlen(u8"class C { "), u8"const")));
  }
}

TEST(test_parse, class_expression_body_is_visited_first_in_expression) {
  {
    padded_string code(u8"[before, class C { m() { inside; } }, after];"sv);
    spy_visitor v = parse_and_visit_statement(&code);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",          // C
                                      "visit_variable_declaration",       // C
                                      "visit_property_declaration",       // m
                                      "visit_enter_function_scope",       // m
                                      "visit_enter_function_scope_body",  // m
                                      "visit_variable_use",         // inside
                                      "visit_exit_function_scope",  //
                                      "visit_exit_class_scope",     // C
                                      "visit_variable_use",         // before
                                      "visit_variable_use"));       // after
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"inside"},
                            spy_visitor::visited_variable_use{u8"before"},
                            spy_visitor::visited_variable_use{u8"after"}));
  }

  {
    padded_string code(
        u8"[before, class C { m() { inside; } }.prop, after] = [1,2,3];"sv);
    spy_visitor v = parse_and_visit_statement(&code);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",          // C
                                      "visit_variable_declaration",       // C
                                      "visit_property_declaration",       // m
                                      "visit_enter_function_scope",       // m
                                      "visit_enter_function_scope_body",  // m
                                      "visit_variable_use",           // inside
                                      "visit_exit_function_scope",    //
                                      "visit_exit_class_scope",       // C
                                      "visit_variable_assignment",    // before
                                      "visit_variable_assignment"));  // after
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"inside"}));
    EXPECT_THAT(
        v.variable_assignments,
        ElementsAre(spy_visitor::visited_variable_assignment{u8"before"},
                    spy_visitor::visited_variable_assignment{u8"after"}));
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
