// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
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
#include <quick-lint-js/string-view.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_parse, parse_simple_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[0].init_kind, variable_init_kind::normal);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"a");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[0].init_kind, variable_init_kind::normal);
    EXPECT_EQ(v.variable_declarations[1].name, u8"b");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_let);
    EXPECT_EQ(v.variable_declarations[1].init_kind, variable_init_kind::normal);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let a, b, c, d, e, f, g"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 7);
    EXPECT_EQ(v.variable_declarations[0].name, u8"a");
    EXPECT_EQ(v.variable_declarations[1].name, u8"b");
    EXPECT_EQ(v.variable_declarations[2].name, u8"c");
    EXPECT_EQ(v.variable_declarations[3].name, u8"d");
    EXPECT_EQ(v.variable_declarations[4].name, u8"e");
    EXPECT_EQ(v.variable_declarations[5].name, u8"f");
    EXPECT_EQ(v.variable_declarations[6].name, u8"g");
    for (const auto& declaration : v.variable_declarations) {
      EXPECT_EQ(declaration.kind, variable_kind::_let);
    }
  }

  {
    spy_visitor v;
    padded_string code(u8"let first; let second"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"first");
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"first");
    EXPECT_EQ(v.variable_declarations[1].name, u8"second");
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_simple_var) {
  spy_visitor v;
  padded_string code(u8"var x"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  EXPECT_EQ(v.variable_declarations[0].init_kind, variable_init_kind::normal);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_simple_const) {
  spy_visitor v;
  padded_string code(u8"const x = null"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_EQ(v.variable_declarations[0].init_kind,
            variable_init_kind::initialized_with_equals);
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_const_with_no_initializers) {
  spy_visitor v;
  padded_string code(u8"const x;"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
  EXPECT_EQ(v.variable_declarations[0].init_kind, variable_init_kind::normal);
  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  &code, diag_missing_initializer_in_const_declaration,  //
                  variable_name, strlen(u8"const "), u8"x")));
}

TEST(test_parse, let_asi) {
  {
    spy_visitor v = parse_and_visit_module(u8"let x\ny"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // y
                            "visit_end_of_module"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let, variable_init_kind::normal}));
  }
}

TEST(test_parse, parse_let_with_initializers) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].init_kind,
              variable_init_kind::initialized_with_equals);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = 2, y = 3"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].init_kind,
              variable_init_kind::initialized_with_equals);
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[1].init_kind,
              variable_init_kind::initialized_with_equals);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = other, y = x"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"other");
    EXPECT_EQ(v.variable_uses[1].name, u8"x");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let x = y in z;"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[0].name, u8"y");
    EXPECT_EQ(v.variable_uses[1].name, u8"z");
  }
}

TEST(test_parse, parse_let_with_object_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let {x} = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[0].init_kind,
              variable_init_kind::initialized_with_equals);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {x, y, z} = 2"_sv);
    ASSERT_EQ(v.variable_declarations.size(), 3);
    EXPECT_EQ(v.variable_declarations[0].name, u8"x");
    EXPECT_EQ(v.variable_declarations[1].name, u8"y");
    EXPECT_EQ(v.variable_declarations[2].name, u8"z");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {key: variable} = 2"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"variable", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {} = x;"_sv);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    ASSERT_EQ(v.variable_uses.size(), 1);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"let {key = defaultValue} = x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // defaultValue
                                      "visit_variable_declaration"));  // key
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"key", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
    EXPECT_THAT(
        v.variable_uses,
        ElementsAre(spy_visitor::visited_variable_use{u8"x"},  //
                    spy_visitor::visited_variable_use{u8"defaultValue"}));
  }
}

TEST(test_parse, parse_let_with_array_destructuring) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let [first, second] = xs;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // x
                                      "visit_variable_declaration",    // first
                                      "visit_variable_declaration"));  // second
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"first", variable_kind::_let,
                        variable_init_kind::initialized_with_equals},
                    spy_visitor::visited_variable_declaration{
                        u8"second", variable_kind::_let,
                        variable_init_kind::initialized_with_equals}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
  }
}

TEST(test_parse, let_does_not_insert_semicolon_after_let_keyword) {
  {
    spy_visitor v = parse_and_visit_statement(u8"let\nx = y;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }
}

TEST(test_parse,
     variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  spy_visitor v;
  padded_string code(u8"let x = x"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));

  EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                    "visit_variable_declaration"));

  ASSERT_EQ(v.variable_declarations.size(), 1);
  EXPECT_EQ(v.variable_declarations[0].name, u8"x");
  ASSERT_EQ(v.variable_uses.size(), 1);
  EXPECT_EQ(v.variable_uses[0].name, u8"x");
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse, parse_valid_let) {
  {
    spy_visitor v;
    padded_string code(u8"let x\nclass C{}"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_declaration",  // C
                            "visit_enter_class_scope",     //
                            "visit_exit_class_scope",      //
                            "visit_end_of_module"));

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"let x\nnew Array()"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // Array
                            "visit_end_of_module"));

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"let x\ntypeof Array"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_typeof_use",   // Array
                            "visit_end_of_module"));

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"let x\nclass C{}\nx = new C();"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_declaration",  // C
                            "visit_enter_class_scope",     //
                            "visit_exit_class_scope",      //
                            "visit_variable_use",          // C
                            "visit_variable_assignment",   // x
                            "visit_end_of_module"));

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, parse_invalid_let) {
  {
    spy_visitor v;
    padded_string code(u8"let a,"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_stray_comma_in_let_statement,  //
                              where, strlen(u8"let a"), u8",")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let a,;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_stray_comma_in_let_statement,  //
                              where, strlen(u8"let a"), u8",")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x, 42"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 1);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"42")));
  }

  // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    {
      padded_string code(u8"var " + keyword);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations, IsEmpty());
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_declare_variable_with_keyword_name,  //
                      keyword, strlen(u8"var "), keyword)));
    }

    {
      padded_string code(u8"var " + keyword + u8";");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations, IsEmpty());
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_declare_variable_with_keyword_name,  //
                      keyword, strlen(u8"var "), keyword)));
    }

    {
      padded_string code(u8"var " + keyword + u8" = x;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.variable_declarations, IsEmpty());
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_cannot_declare_variable_with_keyword_name,  //
                      keyword, strlen(u8"var "), keyword)));
    }
  }

  {
    padded_string code(u8"let while (x) { break; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"while")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let 42*69"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"42")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x, `hello`;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"`hello`")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x, `hello${world}`;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // world
                            "visit_end_of_module"));
    // TODO(strager): Improve the span.
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"`hello${")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let {debugger}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_value_for_object_literal_entry,  //
                    key, strlen(u8"let {"), u8"debugger")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let {42}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 0);
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_invalid_lone_literal_in_object_literal,  //
                    where, strlen(u8"let {"), u8"42")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let true, true, y\nlet x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",          // y
                            "visit_variable_declaration",  // x
                            "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"y"}));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"true")));
  }

  for (string8 prefix_operator : {u8"--", u8"++"}) {
    padded_string code(u8"var " + prefix_operator + u8"x;");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // x
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(&code, diag_let_with_no_bindings,  //
                              where, 0, u8"let"),
            DIAG_TYPE_OFFSETS(&code, diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"let"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"const = y, z = w, = x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_use",          // w
                                      "visit_variable_declaration",  // z
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        &code, diag_missing_variable_name_in_declaration,  //
                        equal_token, strlen(u8"const "), u8"="),
                    DIAG_TYPE_OFFSETS(
                        &code, diag_missing_variable_name_in_declaration,  //
                        equal_token, strlen(u8"const = y, z = w, "), u8"=")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x y = z w"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // z
                                      "visit_variable_declaration",  // y
                                      "visit_variable_declaration",  // z
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                &code, diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x"), u8""),
            DIAG_TYPE_OFFSETS(
                &code, diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x y = z"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x [y]=ys {z}=zs"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // ys
                                      "visit_variable_declaration",  // y
                                      "visit_variable_use",          // zs
                                      "visit_variable_declaration",  // z
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                &code, diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x"), u8""),
            DIAG_TYPE_OFFSETS(
                &code, diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x [y]=ys"), u8"")));
  }

  for (string8 compound_assignment_operator : {
           u8"%=",
           u8"&=",
           u8"**=",
           u8"*=",
           u8"+=",
           u8"-=",
           u8"/=",
           u8"<<=",
           u8">>=",
           u8">>>=",
           u8"^=",
           u8"|=",
       }) {
    padded_string code(u8"let x " + compound_assignment_operator + u8" y, z");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    // TODO(strager): We should signal to the linter that duplicate-definition
    // errors should be ignored for 'x'.
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",          // y
                            "visit_variable_declaration",  // x
                            "visit_variable_declaration",  // z
                            "visit_end_of_module"));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(
            spy_visitor::visited_variable_declaration{
                u8"x", variable_kind::_let,
                variable_init_kind::initialized_with_equals},
            spy_visitor::visited_variable_declaration{
                u8"z", variable_kind::_let, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    &code, diag_cannot_update_variable_during_declaration,  //
                    updating_operator, strlen(u8"let x "),
                    compound_assignment_operator,  //
                    declaring_token, 0, u8"let")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let [42] = x;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_EQ(v.variable_declarations.size(), 0);
    // TODO(strager): Report a better message. We should say 'let statement',
    // not 'parameter'.
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_literal_in_parameter_list,  //
                    literal, strlen(u8"let ["), u8"42")));
  }
}

TEST(test_parse, parse_let_with_missing_equal) {
  {
    spy_visitor v;
    padded_string code(u8"async function f() {return 1;}\nlet x await f()"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_variable_declaration",       // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_equal_after_variable,  //
                    expected_equal,
                    strlen(u8"async function f() {return 1;}\nlet x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x class C{}"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_class_scope",     //
                            "visit_variable_declaration",  // C
                            "visit_exit_class_scope",      //
                            "visit_variable_declaration",  // x
                            "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x function f() {}"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x null"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x new Array()"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // Array
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x this"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x typeof Array"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use",   // Array
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(
        u8"async function f() {return 1;}\nlet x await f(), y = x"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_variable_declaration",       // x
                                      "visit_variable_use",               // x
                                      "visit_variable_declaration",       // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_missing_equal_after_variable,  //
                    expected_equal,
                    strlen(u8"async function f() {return 1;}\nlet x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x class C{}, y = x"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_class_scope",     //
                                      "visit_variable_declaration",  // C
                                      "visit_exit_class_scope",      //
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x function f() {}, y = x"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_variable_use",                // x
                                      "visit_variable_declaration",        // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x null, y = x"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x new Array(), y = x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // Array
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x this, y = x"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"let x typeof Array, y = x;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_typeof_use",   // Array
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }
}

TEST(test_parse, parse_invalid_var) {
  {
    spy_visitor v;
    padded_string code(u8"var"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_let_with_no_bindings,  //
                              where, 0, u8"var")));
  }
}

TEST(test_parse, parse_invalid_const) {
  {
    spy_visitor v;
    padded_string code(u8"const"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations, IsEmpty());
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_let_with_no_bindings,  //
                              where, 0, u8"const")));
  }
}

TEST(test_parse, report_missing_semicolon_for_declarations) {
  {
    spy_visitor v;
    padded_string code(u8"let x = 2 for (;;) { console.log(); }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"}));
    cli_source_position::offset_type end_of_let_statement =
        strlen(u8"let x = 2");
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, end_of_let_statement, u8"")));
  }
  {
    spy_visitor v;
    padded_string code(u8"let x debugger"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let, variable_init_kind::normal}));
    cli_source_position::offset_type end_of_let_statement = strlen(u8"let x");
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, end_of_let_statement, u8"")));
  }
}

TEST(test_parse, old_style_variables_can_be_named_let) {
  {
    spy_visitor v = parse_and_visit_statement(u8"var let = initial;");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",            // initial
                            "visit_variable_declaration"));  // let
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"function let(let) {}");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // let (function)
                            "visit_enter_function_scope",
                            "visit_variable_declaration",  // let (parameter)
                            "visit_enter_function_scope_body",
                            "visit_exit_function_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
    EXPECT_EQ(v.variable_declarations[1].name, u8"let");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(function let() {})");
    EXPECT_THAT(
        v.visits,
        ElementsAre("visit_enter_named_function_scope",  // let (function)
                    "visit_enter_function_scope_body",
                    "visit_exit_function_scope"));
    EXPECT_THAT(
        v.enter_named_function_scopes,
        ElementsAre(spy_visitor::visited_enter_named_function_scope{u8"let"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try { } catch (let) { }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // let
                                      "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let {x = let} = o;");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // o
                                      "visit_variable_use",            // let
                                      "visit_variable_declaration"));  // x
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[1].name, u8"let");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"console.log(let);");
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // console
                                      "visit_variable_use"));  // let
    ASSERT_EQ(v.variable_uses.size(), 2);
    EXPECT_EQ(v.variable_uses[1].name, u8"let");
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let.method();");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use"));  // let
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"}));
  }

  for (string8 code : {
           u8"(async let => null)",
           u8"(async (let) => null)",
           u8"(let => null)",
           u8"((let) => null)",
       }) {
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_statement(code);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // let
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_parameter);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (let in xs) ;");
    EXPECT_THAT(v.visits,
                // TODO(strager): A for scope shouldn't be introduced by
                // this syntax. (No variable is being declared.)
                ElementsAre("visit_enter_for_scope",      //
                            "visit_variable_use",         // xs
                            "visit_variable_assignment",  // let
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"let"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (let.prop in xs) ;");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"let"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let;");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let in other;");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"},
                            spy_visitor::visited_variable_use{u8"other"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let instanceof MyClass;");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"let"},
                            spy_visitor::visited_variable_use{u8"MyClass"}));
  }
}

TEST(test_parse, new_style_variables_cannot_be_named_let) {
  for (string8 declaration_kind : {u8"const", u8"let"}) {
    spy_visitor v;
    padded_string code(declaration_kind + u8" let = null;");
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_variable_named_let_with_let,  //
                    name, declaration_kind.size() + 1, u8"let")));

    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
  }

  {
    spy_visitor v;
    padded_string code(u8"let {other, let} = stuff;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_variable_named_let_with_let,  //
                    name, strlen(u8"let {other, "), u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import let from 'weird';"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_import_let,  //
                              import_name, strlen(u8"import "), u8"let")));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import * as let from 'weird';"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_import_let,  //
                              import_name, strlen(u8"import * as "), u8"let")));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // import implies strict mode (because modules imply strict mode).
  {
    spy_visitor v;
    padded_string code(u8"import { let } from 'weird';"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_import_let,  //
                              import_name, strlen(u8"import { "), u8"let")));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_import);
  }

  // import implies strict mode (because modules imply strict mode).
  {
    padded_string code(u8"import { someName as let } from 'weird';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_import_let,  //
                    import_name, strlen(u8"import { someName as "), u8"let")));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"let", variable_kind::_import, variable_init_kind::normal}));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    padded_string code(u8"import { 'someName' as let } from 'weird';"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_import_let,  //
                              import_name, strlen(u8"import { 'someName' as "),
                              u8"let")));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"let", variable_kind::_import, variable_init_kind::normal}));
  }

  {
    spy_visitor v;
    padded_string code(u8"export function let() {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_export_let,  //
                    export_name, strlen(u8"export function "), u8"let")));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
  }

  // class implies strict mode.
  {
    spy_visitor v;
    padded_string code(u8"class let {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_cannot_declare_class_named_let,  //
                              name, strlen(u8"class "), u8"let")));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"let");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_class);
  }
}

TEST(test_parse, use_await_in_non_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"await(x);"_sv,
                                              function_attributes::normal);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"async function f() {\n"
        u8"  function g() { await(x); }\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function f() {\n"
        u8"  async function g() {}\n"
        u8"  await();\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(() => {\n"
        u8"  async () => {};\n"
        u8"  await();\n"
        u8"})");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"(async => { await(); })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"({ async() { await(); } })"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"class C { async() { await(); } }"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }
}

TEST(test_parse, declare_await_in_non_async_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function await() { }"_sv,
                                              function_attributes::normal);
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_function, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let await = 42;"_sv,
                                              function_attributes::normal);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() {\n"
        u8"  (function(await) { })\n"
        u8"})");
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_parameter, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(function() {\n"
        u8"  async function await() { }\n"
        u8"})");
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_function, variable_init_kind::normal}));
  }
}

TEST(test_parse, declare_await_in_async_function) {
  {
    spy_visitor v;
    padded_string code(u8"function await() { }"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_function, variable_init_kind::normal}));
    // TODO(strager): Include a note referencing the origin of the async
    // function.
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_await_in_async_function,  //
                    name, strlen(u8"function "), u8"await")));
  }

  {
    spy_visitor v;
    padded_string code(u8"var await;"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_var, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_await_in_async_function,  //
                    name, strlen(u8"var "), u8"await")));
  }

  {
    spy_visitor v;
    padded_string code(u8"try {} catch (await) {}"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_catch, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_await_in_async_function,  //
                    name, strlen(u8"try {} catch ("), u8"await")));
  }

  {
    spy_visitor v;
    padded_string code(u8"async function f(await) {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"f", variable_kind::_function,
                        variable_init_kind::normal},  //
                    spy_visitor::visited_variable_declaration{
                        u8"await", variable_kind::_parameter,
                        variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        &code, diag_cannot_declare_await_in_async_function,  //
                        name, strlen(u8"async function f("), u8"await"),
                    // TODO(strager): Drop the
                    // diag_missing_operand_for_operator error.
                    DIAG_TYPE(diag_missing_operand_for_operator)));
  }
}

TEST(test_parse, declare_await_at_top_level) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function await() { }"_sv);
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"await", variable_kind::_function, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let await = 42;"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"await", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }
}

TEST(test_parse, use_await_at_top_level_as_operator) {
  {
    spy_visitor v = parse_and_visit_module(u8"await x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await(x);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await +x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await -x;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await[x]"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await`template`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await`template${x}`"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, use_await_at_top_level_as_variable) {
  {
    spy_visitor v = parse_and_visit_module(u8"await;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"(await)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await = x"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // x
                                      "visit_variable_assignment",  // await
                                      "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await.prop"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await?.prop"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"await"}));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"await ? x : y"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_variable_use",  // x
                                      "visit_variable_use",  // y
                                      "visit_end_of_module"));
  }

  for (string8 op : {
           u8"!=",  u8"!==", u8"%",          u8"&",  u8"&&",  u8"*",
           u8"**",  u8",",   u8"<",          u8"<<", u8"<=",  u8"==",
           u8"===", u8">",   u8">=",         u8">>", u8">>>", u8"??",
           u8"^",   u8"in",  u8"instanceof", u8"|",  u8"||",
       }) {
    padded_string code(u8"await " + op + u8" x;");
    SCOPED_TRACE(code);
    spy_visitor v = parse_and_visit_module(code.string_view());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  for (string8 op : {
           u8"%=",
           u8"&&=",
           u8"&=",
           u8"**=",
           u8"*=",
           u8"+=",
           u8"-=",
           u8"/=",
           u8"<<=",
           u8">>=",
           u8">>>=",
           u8"?\x3f=",
           u8"^=",
           u8"|=",
           u8"||=",
       }) {
    padded_string code(u8"await " + op + u8" x;");
    SCOPED_TRACE(code);
    spy_visitor v = parse_and_visit_module(code.string_view());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // await
                                      "visit_variable_use",         // x
                                      "visit_variable_assignment",  // await
                                      "visit_end_of_module"));
  }

  // TODO(#464): Interpret / as divide, not a regular expression.
  if ((false)) {
    spy_visitor v =
        parse_and_visit_module(u8"await / await / await / await"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, forced_top_level_await_operator) {
  {
    padded_string code(u8"await p;"_sv);
    spy_visitor v;
    parser p(
        &code, &v,
        parser_options{
            .top_level_await_mode = parser_top_level_await_mode::await_operator,
        });
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // p
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"await;"_sv);
    spy_visitor v;
    parser p(
        &code, &v,
        parser_options{
            .top_level_await_mode = parser_top_level_await_mode::await_operator,
        });
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_operand_for_operator,  //
                              where, 0, u8"await")));
  }
}

TEST(
    test_parse,
    declare_await_in_async_function_is_allowed_for_named_function_expressions) {
  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() {\n"
        u8"  (function await() { await; })(); \n"
        u8"})();");
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_function_scope",        //
                            "visit_enter_function_scope_body",   //
                            "visit_enter_named_function_scope",  // await
                            "visit_enter_function_scope_body",   //
                            "visit_variable_use",                // await
                            "visit_exit_function_scope",         //
                            "visit_exit_function_scope"));
    EXPECT_THAT(v.enter_named_function_scopes,
                ElementsAre(spy_visitor::visited_enter_named_function_scope{
                    u8"await"}));
  }
}

TEST(test_parse, use_yield_in_non_generator_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"yield(x);"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function* f() {\n"
        u8"  function g() { yield(x); }\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"},  //
                            spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"function f() {\n"
        u8"  function* g() {}\n"
        u8"  yield();\n"
        u8"}");
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"yield"}));
  }
}

TEST(test_parse, declare_yield_in_non_generator_function) {
  {
    spy_visitor v = parse_and_visit_statement(u8"function yield() { }"_sv);
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_function, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"let yield = 42;"_sv);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"yield", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(async function() {\n"
        u8"  (function(yield) { })\n"
        u8"})");
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_parameter, variable_init_kind::normal}));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"(function() {\n"
        u8"  function* yield() { }\n"
        u8"})");
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_function, variable_init_kind::normal}));
  }
}

TEST(test_parse, declare_yield_in_generator_function) {
  {
    spy_visitor v;
    padded_string code(u8"function yield() { }"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_function, variable_init_kind::normal}));
    // TODO(strager): Include a note referencing the origin of the generator
    // function.
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_yield_in_generator_function,  //
                    name, strlen(u8"function "), u8"yield")));
  }

  {
    spy_visitor v;
    padded_string code(u8"var yield;"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_var, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_yield_in_generator_function,  //
                    name, strlen(u8"var "), u8"yield")));
  }

  {
    spy_visitor v;
    padded_string code(u8"try {} catch (yield) {}"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"yield", variable_kind::_catch, variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_yield_in_generator_function,  //
                    name, strlen(u8"try {} catch ("), u8"yield")));
  }

  {
    spy_visitor v;
    padded_string code(u8"function* f(yield) {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"f", variable_kind::_function,
                        variable_init_kind::normal},  //
                    spy_visitor::visited_variable_declaration{
                        u8"yield", variable_kind::_parameter,
                        variable_init_kind::normal}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_yield_in_generator_function,  //
                    name, strlen(u8"function* f("), u8"yield")));
  }
}

TEST(test_parse, variables_can_be_named_contextual_keywords) {
  std::vector<const char8*> variable_names;
  std::copy_if(contextual_keywords.begin(), contextual_keywords.end(),
               std::back_inserter(variable_names),
               [](const char8* keyword) { return keyword != u8"let"_sv; });
  variable_names.push_back(u8"await");
  variable_names.push_back(u8"yield");
  // TODO(#73): Disallow these ('protected', 'implements', etc.) in strict mode.
  for (const char8* keyword : strict_only_reserved_keywords) {
    variable_names.push_back(keyword);
  }

  for (string8 name : variable_names) {
    SCOPED_TRACE(out_string8(name));

    {
      spy_visitor v = parse_and_visit_statement(
          u8"var " + name + u8" = initial;", function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_var);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"let " + name + u8" = initial;", function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    }

    {
      spy_visitor v =
          parse_and_visit_statement(u8"let {" + name + u8" = 10 } = initial;",
                                    function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_let);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"const " + name + u8" = initial;", function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_const);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"function " + name + u8"(" + name + u8") {}",
          function_attributes::normal);
      EXPECT_THAT(
          v.visits,
          ElementsAre("visit_variable_declaration",       // (name) (function)
                      "visit_enter_function_scope",       //
                      "visit_variable_declaration",       // (name) (parameter)
                      "visit_enter_function_scope_body",  //
                      "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 2);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_function);
      EXPECT_EQ(v.variable_declarations[1].name, name);
      EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_parameter);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"(function " + name + u8"() {})", function_attributes::normal);
      EXPECT_THAT(
          v.visits,
          ElementsAre("visit_enter_named_function_scope",  // (name) (function)
                      "visit_enter_function_scope_body",   //
                      "visit_exit_function_scope"));
      EXPECT_THAT(
          v.enter_named_function_scopes,
          ElementsAre(spy_visitor::visited_enter_named_function_scope{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(u8"class " + name + u8" {}",
                                                function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_declaration",  // (name)
                              "visit_enter_class_scope",     //
                              "visit_exit_class_scope"));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              name, variable_kind::_class, variable_init_kind::normal}));
    }

    {
      spy_visitor v = parse_and_visit_statement(u8"(class " + name + u8" {})",
                                                function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_enter_class_scope",     //
                              "visit_variable_declaration",  // (name)
                              "visit_exit_class_scope"));
      EXPECT_THAT(
          v.variable_declarations,
          ElementsAre(spy_visitor::visited_variable_declaration{
              name, variable_kind::_class, variable_init_kind::normal}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"try { } catch (" + name + u8") { }", function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                        "visit_exit_block_scope",      //
                                        "visit_enter_block_scope",     //
                                        "visit_variable_declaration",  // (name)
                                        "visit_exit_block_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"let {x = " + name + u8"} = o;", function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // o
                                        "visit_variable_use",  // (name)
                                        "visit_variable_declaration"));  // x
      ASSERT_EQ(v.variable_uses.size(), 2);
      EXPECT_EQ(v.variable_uses[1].name, name);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"console.log(" + name + u8");", function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // console
                                        "visit_variable_use"));  // (name)
      ASSERT_EQ(v.variable_uses.size(), 2);
      EXPECT_EQ(v.variable_uses[1].name, name);
    }

    {
      string8 code = name;
      SCOPED_TRACE(out_string8(code));
      spy_visitor v =
          parse_and_visit_statement(code.c_str(), function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    {
      string8 code = name + u8";";
      SCOPED_TRACE(out_string8(code));
      spy_visitor v =
          parse_and_visit_statement(code.c_str(), function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // (name)
      ASSERT_EQ(v.variable_uses.size(), 1);
      EXPECT_EQ(v.variable_uses[0].name, name);
    }

    {
      spy_visitor v = parse_and_visit_statement(name + u8".method();",
                                                function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name}));
    }

    for (string8 code : {
             u8"(async " + name + u8" => null)",
             u8"(async (" + name + u8") => null)",
             u8"(" + name + u8" => null)",
             u8"((" + name + u8") => null)",
         }) {
      if (name == u8"await" &&
          quick_lint_js::starts_with(string8_view(code), u8"(async"sv)) {
        // NOTE(erlliam): await parameter isn't allowed in async functions. See
        // test_parse.disallow_await_parameter_in_async_arrow_function.
        continue;
      }
      SCOPED_TRACE(out_string8(code));
      spy_visitor v =
          parse_and_visit_statement(code, function_attributes::normal);
      EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",  //
                                        "visit_variable_declaration",  // (name)
                                        "visit_enter_function_scope_body",  //
                                        "visit_exit_function_scope"));
      ASSERT_EQ(v.variable_declarations.size(), 1);
      EXPECT_EQ(v.variable_declarations[0].name, name);
      EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_parameter);
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8" in xs) ;", function_attributes::normal);
      EXPECT_THAT(v.visits,
                  ElementsAre("visit_variable_use",           // xs
                              "visit_variable_assignment"));  // (name)
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8".prop in xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},  //
                              spy_visitor::visited_variable_use{u8"xs"}));
    }

    if (name != u8"async") {
      // NOTE(strager): async isn't allowed here. See
      // test_parse.cannot_assign_to_variable_named_async_in_for_of.
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8" of xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for ((" + name + u8") of xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_assignments,
                  ElementsAre(spy_visitor::visited_variable_assignment{name}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8".prop of xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_assignments, IsEmpty());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"xs"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (let " + name + u8" of xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      name, variable_kind::_let, variable_init_kind::normal}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (var " + name + u8" of xs) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      name, variable_kind::_var, variable_init_kind::normal}));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8"; cond;) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_assignments, IsEmpty());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"cond"}));
    }

    {
      spy_visitor v = parse_and_visit_statement(
          u8"for (" + name + u8".prop; cond;) ;", function_attributes::normal);
      EXPECT_THAT(v.variable_assignments, IsEmpty());
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{name},
                              spy_visitor::visited_variable_use{u8"cond"}));
    }
  }
}

TEST(test_parse, lexical_declaration_as_do_while_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    padded_string code(u8"do " + variable_kind + u8" x = y; while (cond);");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use"));        // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::do_while_loop,                                //
            expected_body, offsets_matcher(&code, strlen(u8"do"), u8""),  //
            declaring_keyword,
            offsets_matcher(&code, strlen(u8"do "), variable_kind))));
  }
}

TEST(test_parse, lexical_declaration_as_for_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    padded_string code(u8"for (;cond;) " + variable_kind + u8" x = y;");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::for_loop,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"for (;cond;)"), u8""),  //
            declaring_keyword,
            offsets_matcher(&code, strlen(u8"for (;cond;) "), variable_kind))));
  }
}

TEST(test_parse, lexical_declaration_as_if_statement_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    {
      padded_string code(u8"if (cond) " + variable_kind + u8" x = y;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // cond
                                        "visit_variable_use",            // y
                                        "visit_variable_declaration"));  // x
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(&code, strlen(u8"if (cond)"), u8""),  //
              declaring_keyword,
              offsets_matcher(&code, strlen(u8"if (cond) "), variable_kind))));
    }

    {
      padded_string code(u8"if (cond) " + variable_kind + u8" x = y; else {}");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // cond
                                        "visit_variable_use",          // y
                                        "visit_variable_declaration",  // x
                                        "visit_enter_block_scope",     // else
                                        "visit_exit_block_scope"));    // else
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(&code, strlen(u8"if (cond)"), u8""),  //
              declaring_keyword,
              offsets_matcher(&code, strlen(u8"if (cond) "), variable_kind))));
    }

    {
      padded_string code(u8"if (cond) {} else " + variable_kind + u8" x = y;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      EXPECT_TRUE(p.parse_and_visit_statement(v));
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // cond
                                        "visit_enter_block_scope",       // if
                                        "visit_exit_block_scope",        // if
                                        "visit_variable_use",            // y
                                        "visit_variable_declaration"));  // x
      EXPECT_THAT(
          v.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(&code, strlen(u8"if (cond) {} else"), u8""),  //
              declaring_keyword,
              offsets_matcher(&code, strlen(u8"if (cond) {} else "),
                              variable_kind))));
    }
  }
}

TEST(test_parse, lexical_declaration_as_while_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    padded_string code(u8"while (cond) " + variable_kind + u8" x = y;");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::while_loop,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"while (cond)"), u8""),  //
            declaring_keyword,
            offsets_matcher(&code, strlen(u8"while (cond) "), variable_kind))));
  }
}

TEST(test_parse, lexical_declaration_as_with_statement_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    padded_string code(u8"with (obj) " + variable_kind + u8" x = y;");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // obj
                                      "visit_enter_with_scope",      // with
                                      "visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_exit_with_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::with_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"with (obj)"), u8""),  //
            declaring_keyword,
            offsets_matcher(&code, strlen(u8"with (obj) "), variable_kind))));
  }
}

TEST(test_parse, let_as_statement_body_does_not_allow_asi_before_left_square) {
  {
    padded_string code(u8"if (cond) let\n[x] = xs;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // xs
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::if_statement,  //
            expected_body,
            offsets_matcher(&code, strlen(u8"if (cond)"), u8""),  //
            declaring_keyword,
            offsets_matcher(&code, strlen(u8"if (cond) "), u8"let"))));
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
