// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
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
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"A", variable_kind::_class},
                    spy_visitor::visited_variable_declaration{
                        u8"B", variable_kind::_class}));
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
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_name_in_class_statement, class_keyword,
                    offsets_matcher(&code, 0, u8"class"))));
  }
}

TEST(test_parse, class_statement_requires_a_body) {
  {
    spy_visitor v;
    padded_string code(u8"class C "_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));  // C
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_body_for_class,
                              class_keyword_and_name_and_heritage,
                              offsets_matcher(&code, 0, u8"class C"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"class ;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    ERROR_TYPE_FIELD(error_missing_name_in_class_statement,
                                     class_keyword,
                                     offsets_matcher(&code, 0, u8"class")),
                    ERROR_TYPE_FIELD(error_missing_body_for_class,
                                     class_keyword_and_name_and_heritage,
                                     offsets_matcher(&code, 0, u8"class"))));
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
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(&code, strlen(u8"class C { "), u8"function"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { async function f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(&code, strlen(u8"class C { async "),
                            u8"function"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { function* f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(&code, strlen(u8"class C { "), u8"function"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"class C { static function f() {} }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(&code, strlen(u8"class C { static "),
                            u8"function"))));
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
}

TEST(test_parse, class_statement_allows_stray_semicolons) {
  spy_visitor v = parse_and_visit_statement(u8"class C{ ; f(){} ; }"_sv);
  ASSERT_EQ(v.property_declarations.size(), 1);
  EXPECT_EQ(v.property_declarations[0].name, u8"f");
}

}
}
