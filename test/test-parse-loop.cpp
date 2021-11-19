// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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
TEST(test_parse, do_while) {
  {
    spy_visitor v = parse_and_visit_statement(u8"do { a; } while (b)"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"do do {a;} while(b) while(c);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use",       // b
                                      "visit_variable_use"));     // c
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"do do {a;} while(b); while(c);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // a
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use",       // b
                                      "visit_variable_use"));     // c
  }
}

TEST(test_parse, do_while_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"do {} while cond"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_parentheses_around_do_while_condition, condition,
            offsets_matcher(&code, strlen(u8"do {} while "), u8"cond"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"do {} while cond;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_parentheses_around_do_while_condition, condition,
            offsets_matcher(&code, strlen(u8"do {} while "), u8"cond"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"{ do {} while cond }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   //
                                      "visit_enter_block_scope",   //
                                      "visit_exit_block_scope",    //
                                      "visit_variable_use",        // cond
                                      "visit_exit_block_scope"));  //
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_parentheses_around_do_while_condition, condition,
            offsets_matcher(&code, strlen(u8"{ do {} while "), u8"cond"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"do {} while (cond"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_parenthesis_around_do_while_condition,  //
            where,
            offsets_matcher(&code, strlen(u8"do {} while (cond"), u8""),  //
            token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"do {} while cond)"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // cond
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_parenthesis_around_do_while_condition,           //
            where, offsets_matcher(&code, strlen(u8"do {} while "), u8""),  //
            token, u8'(')));
  }
}

TEST(test_parse, do_while_without_body) {
  {
    padded_string code(u8"do\nwhile (cond);"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // cond
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_body_for_do_while_statement,
                              do_token, offsets_matcher(&code, 0, u8"do"))));
  }
}

TEST(test_parse, do_while_without_while_and_condition) {
  {
    padded_string code(u8"do {} "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_while_and_condition_for_do_while_statement, do_token,
            offsets_matcher(&code, 0, u8"do"),  //
            expected_while, offsets_matcher(&code, strlen(u8"do {}"), u8""))));
  }

  {
    padded_string code(u8"do {}; while (x);"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use",       // x
                                      "visit_end_of_module"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_while_and_condition_for_do_while_statement, do_token,
            offsets_matcher(&code, 0, u8"do"),  //
            expected_while, offsets_matcher(&code, strlen(u8"do {}"), u8""))));
  }
}

TEST(test_parse, c_style_for_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (;;) { a; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (init; cond; after) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       //
                                      "visit_variable_use",       //
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"},  //
                            spy_visitor::visited_variable_use{u8"cond"},  //
                            spy_visitor::visited_variable_use{u8"body"},  //
                            spy_visitor::visited_variable_use{u8"after"}));
  }

  for (const char8* variable_kind : {u8"const", u8"let"}) {
    SCOPED_TRACE(out_string8(variable_kind));
    string8 code =
        string8(u8"for (") + variable_kind + u8" i = 0; cond; after) { body; }";
    spy_visitor v = parse_and_visit_statement(code.c_str());
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_declaration",  //
                                      "visit_variable_use",          //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use",          //
                                      "visit_exit_for_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var i = 0; ; ) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (i = 0, j = 0; ; ) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_assignment",  // i
                                      "visit_variable_assignment",  // j
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, c_style_for_loop_with_in_operator) {
  {
    padded_string code(u8"for (a in b; c; d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_in_disallowed_in_c_style_for_loop, in_token,
                    offsets_matcher(&code, strlen(u8"for (a "), u8"in"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // b
                                      "visit_variable_assignment",  // a
                                      "visit_variable_use",         // c
                                      "visit_enter_block_scope",    //
                                      "visit_exit_block_scope",     //
                                      "visit_variable_use"));       // d
  }

  {
    padded_string code(u8"for (let x = a in b; c; d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_in_disallowed_in_c_style_for_loop, in_token,
            offsets_matcher(&code, strlen(u8"for (let x = a "), u8"in"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // a
                                      "visit_variable_use",          // b
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // c
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use",          // d
                                      "visit_exit_for_scope"));
  }

  {
    padded_string code(u8"for (var x = a in b; c; d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_in_disallowed_in_c_style_for_loop, in_token,
            offsets_matcher(&code, strlen(u8"for (var x = a "), u8"in"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // a
                                      "visit_variable_use",          // b
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // c
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_variable_use"));        // d
  }
}

TEST(test_parse, for_loop_with_missing_component) {
  {
    padded_string code(u8"for () {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_header_of_for_loop, where,
                    offsets_matcher(&code, strlen(u8"for "), u8"()"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",   //
                                      "visit_exit_block_scope"));  //
  }

  {
    padded_string code(u8"for (myVar) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_for_loop_rhs_or_components_after_expression, header,
            offsets_matcher(&code, strlen(u8"for "), u8"(myVar)"),  //
            for_token, offsets_matcher(&code, 0, u8"for"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // myVar
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    padded_string code(u8"for (let myVar) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_for_loop_rhs_or_components_after_declaration, header,
            offsets_matcher(&code, strlen(u8"for "), u8"(let myVar)"),  //
            for_token, offsets_matcher(&code, 0, u8"for"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_declaration",  // myVar
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
  }

  {
    padded_string code(u8"for (init; cond) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_c_style_for_loop_is_missing_third_component,
            existing_semicolon,
            offsets_matcher(&code, strlen(u8"for (init"), u8";"),  //
            expected_last_component,
            offsets_matcher(&code, strlen(u8"for (init; cond"), u8")"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",        // init
                                      "visit_variable_use",        // cond
                                      "visit_enter_block_scope",   //
                                      "visit_exit_block_scope"));  //
  }
}

TEST(test_parse, for_loop_with_missing_semicolons) {
  {
    padded_string code(u8"for (a b; c) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_semicolon_between_for_loop_init_and_condition,
                    expected_semicolon,
                    offsets_matcher(&code, strlen(u8"for (a"), u8""))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // c
  }

  {
    padded_string code(u8"for (a; b c) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_semicolon_between_for_loop_condition_and_update,
            expected_semicolon,
            offsets_matcher(&code, strlen(u8"for (a; b"), u8""))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // c
  }
}

TEST(test_parse, for_loop_with_extra_semicolons) {
  {
    padded_string code(u8"for (;;;) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_semicolon_in_c_style_for_loop, semicolon,
                    offsets_matcher(&code, strlen(u8"for (;;"), u8";"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_exit_block_scope"));
  }

  {
    padded_string code(u8"for (;; ;;;) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_c_style_for_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (;; "), u8";")),
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_c_style_for_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (;; ;"), u8";")),
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_c_style_for_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (;; ;;"), u8";"))));
  }

  {
    padded_string code(u8"for (a;b;c;d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_semicolon_in_c_style_for_loop, semicolon,
                    offsets_matcher(&code, strlen(u8"for (a;b;c"), u8";"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // a
                                      "visit_variable_use",       // b
                                      "visit_variable_use",       // d
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_variable_use"));     // c
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"a"},  //
                            spy_visitor::visited_variable_use{u8"b"},  //
                            spy_visitor::visited_variable_use{u8"d"},  //
                            spy_visitor::visited_variable_use{u8"c"}));
  }

  {
    padded_string code(u8"for (a of b; c; d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_for_of_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (a of b"), u8";")),
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_for_of_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (a of b; c"), u8";"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // b
                                      "visit_variable_assignment",  // a
                                      "visit_variable_use",         // c
                                      "visit_variable_use",         // d
                                      "visit_enter_block_scope",    //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"b"},  //
                            spy_visitor::visited_variable_use{u8"c"},  //
                            spy_visitor::visited_variable_use{u8"d"}));
  }

  {
    padded_string code(u8"for (var a of b; c) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_unexpected_semicolon_in_for_of_loop, semicolon,
            offsets_matcher(&code, strlen(u8"for (var a of b"), u8";"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // b
                                      "visit_variable_declaration",  // a
                                      "visit_variable_use",          // c
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }

  {
    padded_string code(u8"for (var a in b; c; d) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_for_in_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (var a of b"), u8";")),
            ERROR_TYPE_FIELD(
                error_unexpected_semicolon_in_for_in_loop, semicolon,
                offsets_matcher(&code, strlen(u8"for (var a of b; c"),
                                u8";"))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // a
                                      "visit_variable_use",          // b
                                      "visit_variable_use",          // c
                                      "visit_variable_use",          // d
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, for_in_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (x in xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (let x in xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x in xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // xs
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    padded_string code(u8"for (const x in []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, for_in_loop_with_var_initializer) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = init in xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // init
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // xs
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"},  //
                            spy_visitor::visited_variable_use{u8"xs"},    //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    padded_string code(u8"for (var x = 10 in []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = ++y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_assignment",   // y
                                      "visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (var x = -y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = y + z in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_use",          // z
                                      "visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = () => y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_block_scope",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = (z) => y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // z
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_block_scope",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = async () => y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_block_scope",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = async (z) => y in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // z
                                      "visit_enter_function_scope_body",  //
                                      "visit_variable_use",               // y
                                      "visit_exit_function_scope",        //
                                      "visit_variable_declaration",       // x
                                      "visit_enter_block_scope",          //
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x = y ? z : w in []) {}"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_use",          // z
                                      "visit_variable_use",          // w
                                      "visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
  }

  {
    padded_string code(u8"for (var x = yield y in []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, invalid_for_in_loop) {
  {
    padded_string code(u8"for (const x = 10 in []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_assign_to_loop_variable_in_for_of_or_in_loop,
                    equal_token,
                    offsets_matcher(&code, strlen(u8"for (const x "), u8"="))));
  }

  {
    padded_string code(u8"for (let x = 10 in []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_assign_to_loop_variable_in_for_of_or_in_loop,
                    equal_token,
                    offsets_matcher(&code, strlen(u8"for (let x "), u8"="))));
  }
}

TEST(test_parse, for_of_loop) {
  {
    spy_visitor v = parse_and_visit_statement(u8"for (x of xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         //
                                      "visit_variable_assignment",  //
                                      "visit_enter_block_scope",    //
                                      "visit_variable_use",         //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (let x of xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (var x of xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_var}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"xs"},  //
                            spy_visitor::visited_variable_use{u8"body"}));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for await (let x of xs) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          //
                                      "visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_use",          //
                                      "visit_exit_block_scope",      //
                                      "visit_exit_for_scope"));
  }

  {
    padded_string code(u8"for (let of myArray) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",    //
                                      "visit_variable_use",       // myArray
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_let_with_no_bindings, where,
                    offsets_matcher(&code, strlen(u8"for ("), u8"let"))));
  }

  {
    padded_string code(u8"for (const x of []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, invalid_for_of_loop) {
  {
    padded_string code(u8"for (const x = 10 of []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_assign_to_loop_variable_in_for_of_or_in_loop,
                    equal_token,
                    offsets_matcher(&code, strlen(u8"for (const x "), u8"="))));
  }

  {
    padded_string code(u8"for (let x = 10 of []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_for_scope",       //
                            "visit_variable_declaration",  // x
                            "visit_enter_block_scope",     //
                            "visit_exit_block_scope",      //
                            "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_assign_to_loop_variable_in_for_of_or_in_loop,
                    equal_token,
                    offsets_matcher(&code, strlen(u8"for (let x "), u8"="))));
  }

  {
    padded_string code(u8"for (var x = 10 of []) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                      "visit_enter_block_scope",     //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_assign_to_loop_variable_in_for_of_or_in_loop,
                    equal_token,
                    offsets_matcher(&code, strlen(u8"for (let x "), u8"="))));
  }
}

TEST(test_parse, for_loop_without_body) {
  {
    padded_string code(u8"for (let x of myArray) "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_for_scope",       //
                                      "visit_variable_use",          // myArray
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_body_for_for_statement, for_and_header,
                    offsets_matcher(&code, 0, u8"for (let x of myArray)"))));
  }

  {
    padded_string code(u8"{ for (let x of myArray) }"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_enter_for_scope",       //
                                      "visit_variable_use",          // myArray
                                      "visit_variable_declaration",  // x
                                      "visit_exit_for_scope",        //
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_body_for_for_statement, for_and_header,
                    offsets_matcher(&code, strlen(u8"{ "),
                                    u8"for (let x of myArray)"))));
  }
}

TEST(test_parse, for_loop_without_header) {
  {
    padded_string code(u8"for x = y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // y
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_missing_for_loop_header, for_token,
                              offsets_matcher(&code, 0, u8"for"))));
  }

  {
    padded_string code(u8"{ for } x = y;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",    //
                                      "visit_exit_block_scope",     //
                                      "visit_variable_use",         // y
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_for_loop_header, for_token,
                    offsets_matcher(&code, strlen(u8"{ "), u8"for"))));
  }
}

TEST(test_parse, while_statement) {
  {
    spy_visitor v = parse_and_visit_statement(u8"while (cond) body;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // cond
                                      "visit_variable_use"));  // body
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"while (cond) { body; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
  }
}

TEST(test_parse, while_without_parens) {
  {
    spy_visitor v;
    padded_string code(u8"while cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_parentheses_around_while_condition, condition,
            offsets_matcher(&code, strlen(u8"while "), u8"cond"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"while (cond { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_expected_parenthesis_around_while_condition,             //
            where, offsets_matcher(&code, strlen(u8"while (cond"), u8""),  //
            token, u8')')));
  }

  {
    spy_visitor v;
    padded_string code(u8"while cond) { body; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",       // cond
                                      "visit_enter_block_scope",  //
                                      "visit_variable_use",       // body
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_expected_parenthesis_around_while_condition,        //
                    where, offsets_matcher(&code, strlen(u8"while "), u8""),  //
                    token, u8'(')));
  }
}

TEST(test_parse, while_without_condition) {
  {
    spy_visitor v;
    padded_string code(u8"while { go(); break; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",  //
                                      "visit_variable_use",       // go
                                      "visit_exit_block_scope"));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_condition_for_while_statement, while_keyword,
                    offsets_matcher(&code, 0, u8"while"))));
  }
}

TEST(test_parse, while_without_body) {
  {
    padded_string code(u8"while (cond) "_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use"));  // cond
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_body_for_while_statement, while_and_condition,
                    offsets_matcher(&code, 0, u8"while (cond)"))));
  }
}

TEST(test_parse, break_statement) {
  {
    spy_visitor v;
    padded_string code(u8"break;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_break, break_statement,
                              offsets_matcher(&code, 0, u8"break"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"for (;;) { } break;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_invalid_break, break_statement,
            offsets_matcher(&code, strlen(u8"for (;;) { } "), u8"break"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"for (;;) { function f() { break; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_invalid_break, break_statement,
            offsets_matcher(&code, strlen(u8"for (;;) { function f() { "),
                            u8"break"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"for (;;) { () => { break; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_break, break_statement,
                    offsets_matcher(&code, strlen(u8"for (;;) { () => { "),
                                    u8"break"))));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"switch (0) { default: break; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"do { break; } while (0);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (;;) { break; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"while (0) { break; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"for (;;) { for (;;) { break; } break; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_enter_block_scope",  //
                            "visit_exit_block_scope",   //
                            "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"switch (0) { default: switch(0) { default: break; } break; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_enter_block_scope",  //
                            "visit_exit_block_scope",   //
                            "visit_exit_block_scope"));
  }

  // TODO(#72): Visit the label.
  {
    spy_visitor v = parse_and_visit_statement(u8"break label;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse, continue_statement) {
  {
    spy_visitor v;
    padded_string code(u8"continue;"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_invalid_continue, continue_statement,
                              offsets_matcher(&code, 0, u8"continue"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"switch (0) { default: continue; }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    ASSERT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_continue, continue_statement,
                    offsets_matcher(&code, strlen(u8"switch (0) { default: "),
                                    u8"continue"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"for (;;) { function f() { continue; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_invalid_continue, continue_statement,
            offsets_matcher(&code, strlen(u8"for (;;) { function f() { "),
                            u8"continue"))));
  }

  {
    spy_visitor v;
    padded_string code(u8"for (;;) { () => { continue; } }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_continue, continue_statement,
                    offsets_matcher(&code, strlen(u8"for (;;) { () => { "),
                                    u8"continue"))));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"do { continue; } while (0);"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"for (;;) { continue; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(u8"while (0) { continue; }"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_block_scope",
                                      "visit_exit_block_scope"));
  }

  {
    spy_visitor v = parse_and_visit_statement(
        u8"for (;;) { for (;;) { continue; } continue; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_enter_block_scope",  //
                            "visit_exit_block_scope",   //
                            "visit_exit_block_scope"));
  }

  // TODO(#72): Visit the label.
  {
    spy_visitor v = parse_and_visit_statement(u8"continue label;"_sv);
    EXPECT_THAT(v.visits, IsEmpty());
  }
}

TEST(test_parse,
     break_and_continue_statements_do_not_allow_newline_before_label) {
  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (;;) { break\nnotALabel; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_variable_use",       // notALabel
                            "visit_exit_block_scope"));
  }

  {
    spy_visitor v =
        parse_and_visit_statement(u8"for (;;) { continue\nnotALabel; }"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_enter_block_scope",  //
                            "visit_variable_use",       // notALabel
                            "visit_exit_block_scope"));
  }
}

TEST(test_parse,
     break_and_continue_statements_allows_contextual_keyword_as_label) {
  for (const char8* statement : {u8"break", u8"continue"}) {
    for (string8 keyword : contextual_keywords) {
      padded_string code(keyword + u8": for (;;) { " + statement + u8" " +
                         keyword + u8"; }");
      SCOPED_TRACE(code);

      {
        // Top-level.
        spy_visitor v = parse_and_visit_statement(code.string_view());
        EXPECT_THAT(v.errors, IsEmpty());
      }

      {
        spy_visitor v = parse_and_visit_statement(code.string_view(),
                                                  function_attributes::normal);
        EXPECT_THAT(v.errors, IsEmpty());
      }
    }
  }

  // TODO(#214): Disallow labels named 'await' in async functions.
  // TODO(#214): Disallow labels named 'yield' in generator functions.
}

TEST(test_parse, for_loop_async_arrow_with_of_parameter_is_init_expression) {
  spy_visitor v = parse_and_visit_statement(u8"for (async of => x; y; z);"_sv);
  EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                    "visit_variable_declaration",       // of
                                    "visit_enter_function_scope_body",  //
                                    "visit_variable_use",               // x
                                    "visit_exit_function_scope",        //
                                    "visit_variable_use",               // y
                                    "visit_variable_use"));             // z
}

TEST(test_parse,
     cannot_assign_to_variable_named_async_without_parentheses_in_for_of) {
  padded_string code(u8"for (async of xs) ;"_sv);
  spy_visitor v;
  parser p(&code, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.variable_assignments,
              ElementsAre(spy_visitor::visited_variable_assignment{u8"async"}));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"xs"}));
  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_FIELD(
                  error_cannot_assign_to_variable_named_async_in_for_of_loop,
                  async_identifier,
                  offsets_matcher(&code, strlen(u8"for ("), u8"async"))));
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
