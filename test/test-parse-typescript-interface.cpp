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

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_interface, not_supported_in_vanilla_javascript) {
  padded_string code(u8"interface I {}"_sv);
  spy_visitor v;
  parser_options options;
  options.typescript = false;
  parser p(&code, &v, options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                    "visit_enter_interface_scope",  // I
                                    "visit_exit_interface_scope",   // I
                                    "visit_end_of_module"));
  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  &code,
                  diag_typescript_interfaces_not_allowed_in_javascript,  //
                  interface_keyword, 0, u8"interface")));
}

TEST(test_parse_typescript_interface, empty_interface) {
  padded_string code(u8"interface I {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                    "visit_enter_interface_scope",  // I
                                    "visit_exit_interface_scope",   // I
                                    "visit_end_of_module"));
  EXPECT_THAT(
      v.variable_declarations,
      ElementsAre(spy_visitor::visited_variable_declaration{
          u8"I", variable_kind::_interface, variable_init_kind::normal}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_typescript_interface, extends) {
  padded_string code(u8"interface I extends A {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                    "visit_variable_type_use",      // A
                                    "visit_enter_interface_scope",  // I
                                    "visit_exit_interface_scope",   // I
                                    "visit_end_of_module"));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"A"}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_typescript_interface, extends_interface_from_namespace) {
  padded_string code(u8"interface I extends ns.A {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // I
                                    "visit_variable_namespace_use",  // ns
                                    "visit_enter_interface_scope",   // I
                                    "visit_exit_interface_scope",    // I
                                    "visit_end_of_module"));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"ns"}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_typescript_interface, extends_multiple_things) {
  padded_string code(u8"interface I extends A, B, C {}"_sv);
  spy_visitor v;
  parser p(&code, &v, typescript_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                    "visit_variable_type_use",      // A
                                    "visit_variable_type_use",      // B
                                    "visit_variable_type_use",      // C
                                    "visit_enter_interface_scope",  // I
                                    "visit_exit_interface_scope",   // I
                                    "visit_end_of_module"));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"A"},
                          spy_visitor::visited_variable_use{u8"B"},
                          spy_visitor::visited_variable_use{u8"C"}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, unclosed_interface_statement) {
  {
    padded_string code(u8"interface I { "_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_exit_interface_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{")));
  }

  {
    padded_string code(u8"interface I { property "_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_property_declaration",  // property
                                      "visit_exit_interface_scope",  //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{")));
  }

  {
    padded_string code(u8"interface I { method() "_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_property_declaration",   // method
                                      "visit_enter_function_scope",   // method
                                      "visit_exit_function_scope",    // method
                                      "visit_exit_interface_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_interface_block,  //
                              block_open, strlen(u8"interface I "), u8"{")));
  }
}

TEST(test_parse_typescript_interface, property_without_type) {
  {
    padded_string code(u8"interface I { a;b\nc }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  // I
                                      "visit_property_declaration",   // a
                                      "visit_property_declaration",   // b
                                      "visit_property_declaration",   // c
                                      "visit_exit_interface_scope",   // I
                                      "visit_end_of_module"));
    EXPECT_THAT(v.property_declarations,
                ElementsAre(spy_visitor::visited_property_declaration{u8"a"},
                            spy_visitor::visited_property_declaration{u8"b"},
                            spy_visitor::visited_property_declaration{u8"c"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"interface I { 'fieldName'; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",   //
                            "visit_enter_interface_scope",  //
                            "visit_property_declaration",   // 'fieldName'
                            "visit_exit_interface_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"interface I { 3.14; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",   //
                            "visit_enter_interface_scope",  //
                            "visit_property_declaration",   // 3.14
                            "visit_exit_interface_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"interface I { [x + y]; }");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",   //
                            "visit_enter_interface_scope",  //
                            "visit_variable_use",           // x
                            "visit_variable_use",           // y
                            "visit_property_declaration",   // (x + y)
                            "visit_exit_interface_scope"));
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{std::nullopt}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"y"}));
  }
}

TEST(test_parse_typescript_interface, interface_with_methods) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface Monster { eatMuffins(muffinCount); }");

    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Monster");
    EXPECT_EQ(v.variable_declarations[1].name, u8"muffinCount");

    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, u8"eatMuffins");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",   // Monster
                            "visit_enter_interface_scope",  //
                            "visit_property_declaration",   // eatMuffins
                            "visit_enter_function_scope",   //
                            "visit_variable_declaration",   // muffinCount
                            "visit_exit_function_scope",    //
                            "visit_exit_interface_scope"));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface I { get length(); }"_sv);
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface I { set length(value); }"_sv);
    EXPECT_THAT(
        v.property_declarations,
        ElementsAre(spy_visitor::visited_property_declaration{u8"length"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface I { a(); b(); c(); }"_sv);
    ASSERT_EQ(v.property_declarations.size(), 3);
    EXPECT_EQ(v.property_declarations[0].name, u8"a");
    EXPECT_EQ(v.property_declarations[1].name, u8"b");
    EXPECT_EQ(v.property_declarations[2].name, u8"c");
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"interface I { \"stringKey\"(); }");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"interface I { [x + y](); }"_sv);
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"x");
    EXPECT_EQ(v.variable_uses[1].name, u8"y");
    ASSERT_EQ(v.property_declarations.size(), 1);
    EXPECT_EQ(v.property_declarations[0].name, std::nullopt);
  }
}

TEST(test_parse_typescript_interface, interface_methods_cannot_have_bodies) {
  {
    padded_string code(u8"interface I { method() { x } }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // I
                            "visit_enter_interface_scope",      //
                            "visit_property_declaration",       // method
                            "visit_enter_function_scope",       // method
                            "visit_enter_function_scope_body",  // method
                            "visit_variable_use",               // x
                            "visit_exit_function_scope",        // method
                            "visit_exit_interface_scope",       //
                            "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_methods_cannot_contain_bodies,  //
                    body_start, strlen(u8"interface I { method() "), u8"{")));
  }

  {
    padded_string code(u8"interface I { method() => { x } }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ::testing::UnorderedElementsAre(
            // TODO(strager): Report only one diagnostic:
            // diag_interface_methods_cannot_contain_bodies on the '=>'.
            DIAG_TYPE(diag_functions_or_methods_should_not_have_arrow_operator),
            DIAG_TYPE_OFFSETS(
                &code, diag_interface_methods_cannot_contain_bodies,  //
                body_start, strlen(u8"interface I { method() => "), u8"{")));
  }
}

TEST(test_parse_typescript_interface, interface_with_keyword_property) {
  for (string8 keyword : keywords) {
    {
      string8 code = u8"interface I { " + keyword + u8"(); }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_typescript_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    for (string8 prefix : {u8"get", u8"set"}) {
      string8 code = u8"interface I { " + prefix + u8" " + keyword + u8"(); }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_typescript_statement(code.c_str());
      ASSERT_EQ(v.property_declarations.size(), 1);
      EXPECT_EQ(v.property_declarations[0].name, keyword);
    }

    {
      string8 code = u8"interface I { " + keyword + u8" }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_typescript_statement(code.c_str());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }

    {
      string8 code = u8"interface I { " + keyword + u8"; }";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v = parse_and_visit_typescript_statement(code.c_str());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }
  }

  for (string8 keyword : strict_reserved_keywords) {
    string8 property = escape_first_character_in_keyword(keyword);
    for (string8 prefix : {u8"", u8"get", u8"set"}) {
      padded_string code(u8"interface I { " + prefix + u8" " + property +
                         u8"(); }");
      SCOPED_TRACE(code);
      spy_visitor v = parse_and_visit_typescript_statement(code.string_view());
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
    }
  }
}

TEST(test_parse_typescript_interface, interface_with_number_methods) {
  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"interface Wat { 42.0(); }"_sv);

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"Wat");

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",   // Wat
                            "visit_enter_interface_scope",  //
                            "visit_property_declaration",   // 42.0
                            "visit_enter_function_scope",   //
                            "visit_exit_function_scope",    //
                            "visit_exit_interface_scope"));
  }
}

TEST(test_parse_typescript_interface, interface_allows_stray_semicolons) {
  spy_visitor v =
      parse_and_visit_typescript_statement(u8"interface I{ ; f() ; ; }"_sv);
  ASSERT_EQ(v.property_declarations.size(), 1);
  EXPECT_EQ(v.property_declarations[0].name, u8"f");
}

TEST(test_parse_typescript_interface, private_properties_are_not_allowed) {
  {
    padded_string code(u8"interface I { #method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_property_declaration",   // #method
                                      "visit_enter_function_scope",   // #method
                                      "visit_exit_function_scope",    // #method
                                      "visit_exit_interface_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_properties_cannot_be_private,  //
                    property_name, strlen(u8"interface I { "), u8"#method")));
  }

  {
    padded_string code(u8"interface I { #field; }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_property_declaration",   // #field
                                      "visit_exit_interface_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_properties_cannot_be_private,  //
                    property_name, strlen(u8"interface I { "), u8"#field")));
  }

  {
    padded_string code(u8"interface I { async static #method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                      "visit_enter_interface_scope",  //
                                      "visit_property_declaration",   // #method
                                      "visit_enter_function_scope",   // #method
                                      "visit_exit_function_scope",    // #method
                                      "visit_exit_interface_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ::testing::UnorderedElementsAre(
                    DIAG_TYPE(diag_interface_methods_cannot_be_async),
                    DIAG_TYPE(diag_interface_properties_cannot_be_static),
                    DIAG_TYPE_OFFSETS(
                        &code, diag_interface_properties_cannot_be_private,  //
                        property_name, strlen(u8"interface I { async static "),
                        u8"#method")));
  }
}

TEST(test_parse_typescript_interface, static_properties_are_not_allowed) {
  for (string8 property_name : concat(make_array(u8"myProperty"), keywords)) {
    SCOPED_TRACE(out_string8(property_name));

    {
      padded_string code(u8"interface I { static " + property_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_enter_function_scope",   // property
                              "visit_exit_function_scope",    // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      padded_string code(u8"interface I { static get " + property_name +
                         u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_enter_function_scope",   // property
                              "visit_exit_function_scope",    // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      padded_string code(u8"interface I { static set " + property_name +
                         u8"(value); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_enter_function_scope",   // property
                              "visit_variable_declaration",   // value
                              "visit_exit_function_scope",    // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      padded_string code(u8"interface I { static " + property_name + u8"; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_property_declaration",   // property
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      padded_string code(u8"interface I { static async\n " + property_name +
                         u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      padded_string code(u8"interface I { static\n" + property_name +
                         u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.property_declarations,
                  ElementsAre(spy_visitor::visited_property_declaration{
                      property_name}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }

    {
      // ASI doesn't activate after 'static'.
      // TODO(strager): Is this a bug in the TypeScript compiler?
      padded_string code(u8"interface I { static\n" + property_name + u8"; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.property_declarations,
                  ElementsAre(spy_visitor::visited_property_declaration{
                      property_name}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_properties_cannot_be_static,  //
                      static_keyword, strlen(u8"interface I { "), u8"static")));
    }
  }

  {
    padded_string code(u8"interface I { static field\n method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_properties_cannot_be_static,  //
                    static_keyword, strlen(u8"interface I { "), u8"static")));
  }

  {
    padded_string code(u8"interface I { static field\n ['methodName'](); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_properties_cannot_be_static,  //
                    static_keyword, strlen(u8"interface I { "), u8"static")));
  }
}

TEST(test_parse_typescript_interface, async_methods_are_not_allowed) {
  for (string8 method_name : concat(make_array(u8"method"), keywords)) {
    SCOPED_TRACE(out_string8(method_name));

    {
      padded_string code(u8"interface I { async " + method_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                        "visit_enter_interface_scope",  //
                                        "visit_property_declaration",  // method
                                        "visit_enter_function_scope",  // method
                                        "visit_exit_function_scope",   // method
                                        "visit_exit_interface_scope",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_methods_cannot_be_async,  //
                      async_keyword, strlen(u8"interface I { "), u8"async")));
    }

    {
      // ASI activates after 'async'.
      padded_string code(u8"interface I { async\n" + method_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{u8"async"},
                      spy_visitor::visited_property_declaration{method_name}));
      EXPECT_THAT(v.errors, IsEmpty());
    }
  }
}

TEST(test_parse_typescript_interface, generator_methods_are_not_allowed) {
  for (string8 method_name : concat(make_array(u8"method"), keywords)) {
    SCOPED_TRACE(out_string8(method_name));

    {
      padded_string code(u8"interface I { *" + method_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",   // I
                                        "visit_enter_interface_scope",  //
                                        "visit_property_declaration",  // method
                                        "visit_enter_function_scope",  // method
                                        "visit_exit_function_scope",   // method
                                        "visit_exit_interface_scope",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_methods_cannot_be_generators,  //
                      star, strlen(u8"interface I { "), u8"*")));
    }

    {
      padded_string code(u8"interface I { static *" + method_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  &code, diag_interface_methods_cannot_be_generators,  //
                  star, strlen(u8"interface I { static "), u8"*")));
    }

    {
      padded_string code(u8"interface I { async *" + method_name + u8"(); }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_methods_cannot_be_async),
              DIAG_TYPE_OFFSETS(
                  &code, diag_interface_methods_cannot_be_generators,  //
                  star, strlen(u8"interface I { async "), u8"*")));
    }
  }
}

TEST(test_parse_typescript_interface,
     static_async_methods_are_definitely_not_allowed) {
  {
    padded_string code(u8"interface I { static async method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_interface_methods_cannot_be_async,  //
                              async_keyword, strlen(u8"interface I { static "),
                              u8"async"),
            DIAG_TYPE_OFFSETS(
                &code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { "), u8"static")));
  }

  {
    padded_string code(u8"interface I { async static method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_interface_methods_cannot_be_async,  //
                              async_keyword, strlen(u8"interface I { "),
                              u8"async"),
            DIAG_TYPE_OFFSETS(
                &code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { async "), u8"static")));
  }

  {
    padded_string code(u8"interface I { async static *method(); }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ::testing::UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_interface_methods_cannot_be_async,  //
                              async_keyword, strlen(u8"interface I { "),
                              u8"async"),
            DIAG_TYPE_OFFSETS(
                &code, diag_interface_methods_cannot_be_generators,  //
                star, strlen(u8"interface I { async static "), u8"*"),
            DIAG_TYPE_OFFSETS(
                &code, diag_interface_properties_cannot_be_static,  //
                static_keyword, strlen(u8"interface I { async "), u8"static")));
  }
}

TEST(test_parse_typescript_interface, field_initializers_are_not_allowed) {
  for (string8 field_name : concat(make_array(u8"field"), keywords)) {
    SCOPED_TRACE(out_string8(field_name));

    {
      padded_string code(u8"interface I { " + field_name + u8" = y; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_use",           // y
                              "visit_property_declaration",   // field_name
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              &code, diag_interface_fields_cannot_have_initializers,  //
              equal, (u8"interface I { " + field_name + u8" ").size(), u8"=")));
    }

    {
      padded_string code(u8"interface I { static " + field_name + u8" = y; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  &code, diag_interface_fields_cannot_have_initializers,  //
                  equal,
                  (u8"interface I { static " + field_name + u8" ").size(),
                  u8"=")));
    }
  }

  {
    padded_string code(u8"interface I { 'fieldName' = init; }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_fields_cannot_have_initializers,  //
                    equal, strlen(u8"interface I { 'fieldName' "), u8"=")));
  }
}

TEST(test_parse_typescript_interface, const_fields_are_not_allowed) {
  for (string8 field_name : concat(make_array(u8"field"), keywords)) {
    SCOPED_TRACE(out_string8(field_name));

    {
      padded_string code(u8"interface I { const " + field_name +
                         u8" = init; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",   // I
                              "visit_enter_interface_scope",  //
                              "visit_variable_use",           // init
                              "visit_property_declaration",   // field_name
                              "visit_exit_interface_scope",   //
                              "visit_end_of_module"));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_fields_cannot_be_const,  //
                      const_token, strlen(u8"interface I { "), u8"const")));
    }

    {
      padded_string code(u8"interface I { static const " + field_name +
                         u8" = init; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  &code, diag_interface_fields_cannot_be_const,  //
                  const_token, strlen(u8"interface I { static "), u8"const")));
    }

    {
      padded_string code(u8"interface I { const " + field_name + u8"; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_interface_fields_cannot_be_const,  //
                      const_token, strlen(u8"interface I { "), u8"const")));
    }

    {
      padded_string code(u8"interface I { static const " + field_name +
                         u8"; }");
      spy_visitor v;
      parser p(&code, &v, typescript_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ::testing::UnorderedElementsAre(
              DIAG_TYPE(diag_interface_properties_cannot_be_static),
              DIAG_TYPE_OFFSETS(
                  &code, diag_interface_fields_cannot_be_const,  //
                  const_token, strlen(u8"interface I { static "), u8"const")));
    }
  }

  {
    padded_string code(u8"interface I { const 'fieldName' = init; }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_fields_cannot_have_initializers,  //
                    equal, strlen(u8"interface I { 'fieldName' "), u8"=")));
  }

  {
    padded_string code(u8"interface I { const 'fieldName'; }"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_interface_fields_cannot_have_initializers,  //
                    equal, strlen(u8"interface I { 'fieldName' "), u8"=")));
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
