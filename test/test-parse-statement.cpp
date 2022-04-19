// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock-more-matchers.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
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
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
TEST(test_parse, return_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"return a;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"return a\nreturn b"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"b"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"return a; return b;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use", "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"b"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (true) return; x;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"if (true) { return } else { other }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_exit_block_scope",   //
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // other
                            "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"other"}));
  }
}

TEST(test_parse, return_statement_disallows_newline) {
  {
    padded_string code(u8"return\nx"_sv);
    spy_visitor v;
    parser p(&code, &v);

    // Parse 'return'.
    EXPECT_TRUE(p.parse_and_visit_statement(
        v, parser::parse_statement_type::any_statement_in_block));
    EXPECT_THAT(v.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_return_statement_returns_nothing,  //
                              return_keyword, 0, u8"return")));
  }

  {
    padded_string code(u8"if (true) return\nx"_sv);
    spy_visitor v;
    parser p(&code, &v);

    // Parse 'if (true) return'.
    EXPECT_TRUE(p.parse_and_visit_statement(
        v, parser::parse_statement_type::any_statement));
    EXPECT_THAT(v.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));

    EXPECT_THAT(v.errors, IsEmpty());
  }

  // TODO(strager): These cases might be dead code instead (e.g. a method call).
  // Report a different error for potentially dead code.
  // TODO(strager): This list is incomplete.
  for (const char8* second_line : {
           u8"!true",
           u8"'string'",
           u8"() => {}",
           u8"(2 + 2)",
           u8"+42",
           u8"-42",
           u8"/=pattern/",
           u8"/pattern/",
           u8"42",
           u8"['a', 'b', 'c']",
           u8"`template${withSubstitution}`",
           u8"`template`",
           u8"await myPromise",
           u8"false",
           u8"function f() { }",
           u8"myVariable",
           u8"new Promise()",
           u8"null",
           u8"super.method()",
           u8"this",
           u8"true",
           u8"typeof banana",
           u8"{}",
           u8"~bits",
           u8"<div>hi</div>",
           u8"<p></p>",
           // TODO(strager): Contextual keywords (let, from, yield, etc.).
           // TODO(strager): Function without name. (Must be an expression, not
           // a statement.)
       }) {
    {
      padded_string code(u8"return\n"s + second_line);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, jsx_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_OFFSETS(
                      &code, diag_return_statement_returns_nothing,  //
                      return_keyword, 0, u8"return")));
    }

    {
      padded_string code(u8"{ return\n"s + second_line + u8"}");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, jsx_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_OFFSETS(
                      &code, diag_return_statement_returns_nothing,  //
                      return_keyword, strlen(u8"{ "), u8"return")));
    }

    {
      padded_string code(u8"async function f() { return\n"s + second_line +
                         u8"}");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, jsx_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_OFFSETS(
              &code, diag_return_statement_returns_nothing,  //
              return_keyword, strlen(u8"async function f() { "), u8"return")));
    }

    {
      padded_string code(
          u8"switch (cond) {\n"s
          u8"default:\n"s
          u8"return\n"s +
          second_line + u8"}");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v, jsx_options);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.errors,
                  ElementsAre(ERROR_TYPE_OFFSETS(
                      &code, diag_return_statement_returns_nothing,  //
                      return_keyword, strlen(u8"switch (cond) {\ndefault:\n"),
                      u8"return")));
    }
  }
}

TEST(test_parse, return_statement_disallows_newline_in_block) {
  {
    spy_visitor v = parse_and_visit_module(u8"for (let x of []) return\nx"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_exit_for_scope",        //
                            "visit_variable_use",          // x
                            "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"if (cond) return\nx"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  // cond
                            "visit_variable_use",  // x
                            "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"if (cond) {} else return\nx"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_block_scope",  // (if)
                            "visit_exit_block_scope",   // (if)
                            "visit_variable_use",       // x
                            "visit_end_of_module"));
  }

  {
    spy_visitor v = parse_and_visit_module(u8"while (cond) return\nx"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",  // cond
                            "visit_variable_use",  // x
                            "visit_end_of_module"));
  }
}

TEST(test_parse, throw_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"throw new Error('ouch');"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"Error"}));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_expression_before_semicolon,  //
                    where, strlen(u8"throw"), u8";")));
  }

  {
    spy_visitor v;
    padded_string code(u8"throw\nnew Error();"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_expression_before_newline,  //
                    where, strlen(u8"throw"), u8"")));
  }
}

TEST(test_parse, parse_and_visit_try) {
  {
    spy_visitor v = parse_and_visit_statement(u8"try {} finally {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try {} catch (e) {}"_sv);

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"try {} catch {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // try
                                      "visit_exit_block_scope",    // try
                                      "visit_enter_block_scope",   // catch
                                      "visit_exit_block_scope"));  // catch
    EXPECT_THAT(v.variable_declarations, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch (e) {} finally {}"_sv);

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_declarations.size(), 1);
    EXPECT_EQ(v.variable_declarations[0].name, u8"e");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"try {f();} catch (e) {g();} finally {h();}");

    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));

    ASSERT_EQ(v.variable_uses.size(), 3);
    EXPECT_EQ(v.variable_uses[0].name, u8"f");
    EXPECT_EQ(v.variable_uses[1].name, u8"g");
    EXPECT_EQ(v.variable_uses[2].name, u8"h");
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch ({message, code}) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     // (catch)
                                      "visit_variable_declaration",  // message
                                      "visit_variable_declaration",  // code
                                      "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"message");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    EXPECT_EQ(v.variable_declarations[1].name, u8"code");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_catch);
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"try {} catch ([message, code]) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     // (catch)
                                      "visit_variable_declaration",  // message
                                      "visit_variable_declaration",  // code
                                      "visit_exit_block_scope"));
    ASSERT_EQ(v.variable_declarations.size(), 2);
    EXPECT_EQ(v.variable_declarations[0].name, u8"message");
    EXPECT_EQ(v.variable_declarations[0].kind, variable_kind::_catch);
    EXPECT_EQ(v.variable_declarations[1].name, u8"code");
    EXPECT_EQ(v.variable_declarations[1].kind, variable_kind::_catch);
  }
}

TEST(test_parse, catch_without_try) {
  {
    padded_string code(u8"catch (e) { body; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // e
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope",      //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_catch_without_try,  //
                              catch_token, 0, u8"catch")));
  }

  {
    padded_string code(u8"catch (e) { body; } finally { body; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // e
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope",      //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_catch_without_try,  //
                              catch_token, 0, u8"catch")));
  }
}

TEST(test_parse, finally_without_try) {
  {
    padded_string code(u8"finally { body; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_finally_without_try,  //
                              finally_token, 0, u8"finally")));
  }
}

TEST(test_parse, try_without_catch_or_finally) {
  {
    padded_string code(u8"try { tryBody; }\nlet x = 3;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // (try)
                                      "visit_variable_use",          // tryBody
                                      "visit_exit_block_scope",      // (try)
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_OFFSETS(
            &code, diag_missing_catch_or_finally_for_try_statement,  //
            try_token, 0, u8"try",                                   //
            expected_catch_or_finally, strlen(u8"try { tryBody; }"), u8"")));
  }
}

TEST(test_parse, try_without_body) {
  {
    padded_string code(u8"try\nlet x = 3;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_body_for_try_statement,  //
                              try_token, 0, u8"try")));
  }
}

TEST(test_parse, catch_without_body) {
  {
    padded_string code(u8"try {} catch\nlet x = 3;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // (try)
                                      "visit_exit_block_scope",      // (try)
                                      "visit_enter_block_scope",     // (catch)
                                      "visit_exit_block_scope",      // (catch)
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_body_for_catch_clause,  //
                              catch_token, strlen(u8"try {} catch"), u8"")));
  }
}

TEST(test_parse, finally_without_body) {
  {
    padded_string code(u8"try {} finally\nlet x = 3;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     // (try)
                                      "visit_exit_block_scope",      // (try)
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_body_for_finally_clause,  //
                    finally_token, strlen(u8"try {} "), u8"finally")));
  }
}

TEST(test_parse, catch_without_variable_name_in_parentheses) {
  {
    padded_string code(u8"try {} catch () { body; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  // (try)
                                      "visit_exit_block_scope",   // (try)
                                      "visit_enter_block_scope",  // (catch)
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope",   // (catch)
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_3_FIELDS(
            diag_missing_catch_variable_between_parentheses,
            left_paren_to_right_paren,
            offsets_matcher(&code, strlen(u8"try {} catch "), u8"()"),  //
            left_paren,
            offsets_matcher(&code, strlen(u8"try {} catch "), u8"("),  //
            right_paren,
            offsets_matcher(&code, strlen(u8"try {} catch ("), u8")"))));
  }

  {
    padded_string code(u8"try {} catch ('ball') { body; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  // (try)
                            "visit_exit_block_scope",   // (try)
                            "visit_enter_block_scope",  // (catch)
                            "visit_variable_use",       // body
                            "visit_exit_block_scope",   // (catch)
                            "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_variable_name_for_catch,  //
                    unexpected_token, strlen(u8"try {} catch ("), u8"'ball'")));
  }
}

TEST(test_parse, if_without_else) {
  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_with_else) {
  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) { b; } else { c; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"if (a) b; else c;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                      "visit_variable_use",  //
                                      "visit_variable_use"));
  }
}

TEST(test_parse, if_without_body) {
  {
    spy_visitor v;
    padded_string code(u8"if (a)\nelse e;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // a
                                      "visit_variable_use"));  // e
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"if (a)"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"{\nif (a)\n} b;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"{\nif (a)"), u8"")));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // b
  }

  {
    spy_visitor v;
    padded_string code(u8"if (a)"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // a
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"if (a)"), u8"")));
  }
}

TEST(test_parse, if_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"if cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_parentheses_around_if_condition,  //
                    condition, strlen(u8"if "), u8"cond")));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_expected_parenthesis_around_if_condition,              //
            where, offsets_matcher(&code, strlen(u8"if (cond"), u8""),  //
            token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"if cond) { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    diag_expected_parenthesis_around_if_condition,         //
                    where, offsets_matcher(&code, strlen(u8"if "), u8""),  //
                    token, u8'(')));
  }
}

TEST(test_parse, if_without_condition) {
  {
    spy_visitor v;
    padded_string code(u8"if { yay(); } else { nay(); }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // (if)
                                      "visit_variable_use",        // yay
                                      "visit_exit_block_scope",    // (if)
                                      "visit_enter_block_scope",   // (else)
                                      "visit_variable_use",        // nay
                                      "visit_exit_block_scope"));  // (else)
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_condition_for_if_statement,  //
                    if_keyword, 0, u8"if")));
  }
}

TEST(test_parse, else_without_if) {
  {
    spy_visitor v;
    padded_string code(u8"else { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(&code, diag_else_has_no_if,  //
                                               else_token, 0, u8"else")));
  }
}

TEST(test_parse, missing_if_after_else) {
  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else (true) {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // if
                                      "visit_exit_block_scope",    // if
                                      "visit_enter_block_scope",   // else
                                      "visit_exit_block_scope"));  // else
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_if_after_else,  //
                    expected_if, strlen(u8"if (false) {} else"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (x) {} else (y) {} else {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",        // x
                            "visit_enter_block_scope",   // if
                            "visit_exit_block_scope",    // if
                            "visit_variable_use",        // y
                            "visit_enter_block_scope",   // first else
                            "visit_exit_block_scope",    // first else
                            "visit_enter_block_scope",   // second else
                            "visit_exit_block_scope"));  // second else
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_if_after_else,  //
                              expected_if, strlen(u8"if (x) {} else"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else true {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
    ElementsAre(
        ERROR_TYPE_OFFSETS(&code, diag_missing_semicolon_after_statement,  //
                           where, strlen(u8"if (false) {} else true"), u8""));
  }

  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else (true)\n{}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else (true); {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else () {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // if
                                      "visit_exit_block_scope",    // if
                                      "visit_enter_block_scope",   // else
                                      "visit_exit_block_scope"));  // else
    EXPECT_THAT(v.errors,
                UnorderedElementsAre(
                    ERROR_TYPE_OFFSETS(
                        &code, diag_missing_expression_between_parentheses,  //
                        left_paren_to_right_paren,
                        strlen(u8"if (false) {} else "), u8"()"),
                    ERROR_TYPE_OFFSETS(&code, diag_missing_if_after_else,  //
                                       expected_if,
                                       strlen(u8"if (false) {} else"), u8"")))
        << "should not report diag_missing_arrow_operator_in_arrow_function";
  }

  {
    spy_visitor v;
    padded_string code(u8"if (false) {} else (x, y) {}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   // if
                                      "visit_exit_block_scope",    // if
                                      "visit_variable_use",        // x
                                      "visit_variable_use",        // y
                                      "visit_enter_block_scope",   // else
                                      "visit_exit_block_scope"));  // else
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_if_after_else,  //
                    expected_if, strlen(u8"if (false) {} else"), u8"")))
        << "should not report diag_missing_arrow_operator_in_arrow_function";
  }
}

TEST(test_parse, block_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"{ }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"{ first; second; third; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"first"},   //
                            spy_visitor::visited_variable_use{u8"second"},  //
                            spy_visitor::visited_variable_use{u8"third"}));
  }
}

TEST(test_parse, incomplete_block_statement) {
  {
    padded_string code(u8"{ a; "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_unclosed_code_block,  //
                              block_open, 0, u8"{")));
  }
}

TEST(test_parse, switch_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"switch (x) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {case y:}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // y
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"switch (true) {default:}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"switch (true) {case x: case y: default: case z:}");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // x
                                      "visit_variable_use",       // y
                                      "visit_variable_use",       // z
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"switch (true) { case true: x; let y; z; }");
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_variable_use",          // z
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, switch_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"switch cond { case ONE: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // ONE
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_OFFSETS(
            &code, diag_expected_parentheses_around_switch_condition,  //
            condition, strlen(u8"switch "), u8"cond")));
  }

  {
    spy_visitor v;
    padded_string code(u8"switch (cond { case ONE: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // ONE
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_expected_parenthesis_around_switch_condition,              //
            where, offsets_matcher(&code, strlen(u8"switch (cond"), u8""),  //
            token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"switch cond) { case ONE: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // ONE
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_expected_parenthesis_around_switch_condition,         //
            where, offsets_matcher(&code, strlen(u8"switch "), u8""),  //
            token, u8'(')));
  }
}

TEST(test_parse, switch_without_condition) {
  {
    spy_visitor v;
    padded_string code(u8"switch { case ONE: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // ONE
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_condition_for_switch_statement,  //
                    switch_keyword, 0, u8"switch")));
  }
}

TEST(test_parse, switch_without_body) {
  {
    spy_visitor v;
    padded_string code(u8"switch (cond);"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // cond
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_missing_body_for_switch_statement,  //
                    switch_and_condition, strlen(u8"switch (cond)"), u8"")));
  }
}

TEST(test_parse, switch_without_body_curlies) {
  {
    spy_visitor v;
    padded_string code(u8"switch (cond) case a: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_left_curly,  //
                    expected_left_curly, strlen(u8"switch (cond)"), u8"")));
  }

  {
    spy_visitor v;
    padded_string code(u8"switch (cond) default: body; break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_left_curly,  //
                    expected_left_curly, strlen(u8"switch (cond)"), u8"")));
  }
}

TEST(test_parse, switch_case_without_expression) {
  {
    padded_string code(u8"switch (cond) { case: banana; break; }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // banana
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_expression_for_switch_case,  //
                    case_token, strlen(u8"switch (cond) { "), u8"case")));
  }
}

TEST(test_parse, switch_clause_outside_switch_statement) {
  {
    padded_string code(u8"case x:"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_unexpected_case_outside_switch_statement,  //
                    case_token, 0, u8"case")));
  }

  {
    padded_string code(u8"case\nif (y) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // y
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_unexpected_case_outside_switch_statement,  //
                    case_token, 0, u8"case")));
  }

  {
    padded_string code(u8"default: next;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // next
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_unexpected_default_outside_switch_statement,  //
                    default_token, 0, u8"default")));
  }

  {
    padded_string code(u8"default\nif (x) body;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // body
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_unexpected_default_outside_switch_statement,  //
                    default_token, 0, u8"default")));
  }
}

TEST(test_parse, with_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) body;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",      // cond
                                      "visit_enter_with_scope",  // with
                                      "visit_variable_use",      // body
                                      "visit_exit_with_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"with (cond) { body; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_with_scope",   // with
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // body
                            "visit_exit_block_scope",   //
                            "visit_exit_with_scope"));
  }
}

TEST(test_parse, statement_before_first_switch_case) {
  {
    spy_visitor v;
    padded_string code(
        u8"switch (cond) { console.log('hi'); case ONE: break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // console
                            "visit_variable_use",       // ONE
                            "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_statement_before_first_switch_case,
                              unexpected_statement,
                              strlen(u8"switch (cond) { "), u8"console")));
  }
}

TEST(test_parse, with_statement_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"with cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_with_scope",   // with
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // body
                            "visit_exit_block_scope",   //
                            "visit_exit_with_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_expected_parentheses_around_with_expression,  //
                    expression, strlen(u8"with "), u8"cond")));
  }

  {
    spy_visitor v;
    padded_string code(u8"with (cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_with_scope",   // with
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // body
                            "visit_exit_block_scope",   //
                            "visit_exit_with_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_expected_parenthesis_around_with_expression,             //
            where, offsets_matcher(&code, strlen(u8"with (cond"), u8""),  //
            token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"with cond) { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",       // cond
                            "visit_enter_with_scope",   // with
                            "visit_enter_block_scope",  //
                            "visit_variable_use",       // body
                            "visit_exit_block_scope",   //
                            "visit_exit_with_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    diag_expected_parenthesis_around_with_expression,        //
                    where, offsets_matcher(&code, strlen(u8"with "), u8""),  //
                    token, u8'(')));
  }
}

TEST(test_parse, debugger_statement) {
  {
    spy_visitor v;
    padded_string code(u8"debugger; x;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
  }
}

TEST(test_parse, labelled_statement) {
  {
    spy_visitor v;
    padded_string code(u8"some_label: ; x;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"foob: for (;;) body"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"one: two: three: while (false) body;"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
    // TODO(strager): Announce the labels with a visit or visits.
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // body
  }
}

TEST(test_parse, statement_label_can_be_a_contextual_keyword) {
  for (string8_view keyword : contextual_keywords) {
    padded_string code(string8(keyword) + u8": x;");
    SCOPED_TRACE(code);

    {
      // Top-level.
      spy_visitor v = parse_and_visit_statement(code.string_view());
      // TODO(strager): Announce the label with a visit?
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
    }

    {
      spy_visitor v = parse_and_visit_statement(code.string_view(),
                                                function_attributes::normal);
      // TODO(strager): Announce the label with a visit?
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // x
    }
  }
}

TEST(test_parse, disallow_label_named_await_in_async_function) {
  spy_visitor v;
  padded_string code(u8"async function f() {await:}"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                    "visit_enter_function_scope",       //
                                    "visit_enter_function_scope_body",  //
                                    "visit_exit_function_scope"));
  EXPECT_THAT(
      v.errors,
      ElementsAre(ERROR_TYPE_2_OFFSETS(
          &code, diag_label_named_await_not_allowed_in_async_function,  //
          await, strlen(u8"async function f() {"), u8"await",           //
          colon, strlen(u8"async function f() {await"), u8":")));
}

TEST(test_parse, disallow_label_named_yield_in_generator_function) {
  spy_visitor v;
  padded_string code(u8"function *f() {yield:}"_sv);
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // f
                                    "visit_enter_function_scope",       //
                                    "visit_enter_function_scope_body",  //
                                    "visit_exit_function_scope"));
  EXPECT_THAT(
      v.errors,
      ElementsAre(
          ERROR_TYPE_OFFSETS(&code, diag_missing_semicolon_after_statement,  //
                             where, strlen(u8"function *f() {yield"), u8""),
          ERROR_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                             token, strlen(u8"function *f() {yield"), u8":")));
}

TEST(test_parse, enum_statement_not_yet_implemented) {
  {
    spy_visitor v;
    padded_string code(u8"enum\nlet x = y;"_sv);
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_typescript_enum_not_implemented,  //
                              enum_keyword, 0, u8"enum")));
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
