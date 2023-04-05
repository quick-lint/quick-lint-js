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
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
class test_parse_loop : public test_parse_expression {};

TEST_F(test_parse_loop, do_while) {
  {
    test_parser p(u8"do { a; } while (b)"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",
                          }));
  }

  {
    test_parser p(u8"do do {a;} while(b) while(c);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // b
                              "visit_variable_use",       // c
                          }));
  }

  {
    test_parser p(u8"do do {a;} while(b); while(c);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // b
                              "visit_variable_use",       // c
                          }));
  }

  {
    // 'while(a)' is the body of 'do'-'while(b)'.
    test_parser p(u8"do while(a) {b;} while(c);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_enter_block_scope",  // {
                              "visit_variable_use",       // b
                              "visit_exit_block_scope",   // }
                              "visit_variable_use",       // c
                          }));
  }
}

TEST_F(test_parse_loop, do_while_without_parens) {
  {
    test_parser p(u8"do {} while cond"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // cond
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_do_while_condition,  //
                condition, strlen(u8"do {} while "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"do {} while cond;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // cond
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_do_while_condition,  //
                condition, strlen(u8"do {} while "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"{ do {} while cond }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // cond
                              "visit_exit_block_scope",   //
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_do_while_condition,  //
                condition, strlen(u8"{ do {} while "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"do {} while (cond"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // cond
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        diag_expected_parenthesis_around_do_while_condition,  //
                        where,
                        offsets_matcher(p.code, strlen(u8"do {} while (cond"),
                                        u8""_sv),  //
                        token, u8')'),
                }));
  }

  {
    test_parser p(u8"do {} while cond)"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // cond
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_do_while_condition,  //
                where,
                offsets_matcher(p.code, strlen(u8"do {} while "), u8""_sv),  //
                token, u8'('),
        }));
  }
}

TEST_F(test_parse_loop, do_while_without_body) {
  {
    test_parser p(u8"do\nwhile (cond);"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_body_for_do_while_statement,  //
                        do_token, 0, u8"do"_sv),
                }));
  }

  {
    test_parser p(u8"{ do while (cond); }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // {
                              "visit_variable_use",       // cond
                              "visit_exit_block_scope",   // }
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_body_for_do_while_statement,  //
                        do_token, strlen(u8"{ "), u8"do"_sv),
                }));
  }
}

TEST_F(test_parse_loop, do_while_without_while_and_condition) {
  {
    test_parser p(u8"do {} "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_while_and_condition_for_do_while_statement,  //
                do_token, 0, u8"do"_sv, expected_while, strlen(u8"do {}"),
                u8""_sv),
        }));
  }

  {
    test_parser p(u8"do {}; while (x);"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_while_and_condition_for_do_while_statement,  //
                do_token, 0, u8"do"_sv, expected_while, strlen(u8"do {}"),
                u8""_sv),
        }));
  }
}

TEST_F(test_parse_loop, c_style_for_loop) {
  {
    test_parser p(u8"for (;;) { a; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (init; cond; after) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       //
                              "visit_variable_use",       //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"init", u8"cond", u8"body", u8"after"}));
  }

  for (string8_view variable_kind : {u8"const"_sv, u8"let"_sv}) {
    test_parser p(concat(u8"for ("_sv, variable_kind,
                         u8" i = 0; cond; after) { body; }"_sv));
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  //
                              "visit_variable_use",          //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_variable_use",          //
                              "visit_exit_for_scope",
                          }));
  }

  {
    test_parser p(u8"for (var i = 0; ; ) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (i = 0, j = 0; ; ) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_assignment",  // i
                              "visit_variable_assignment",  // j
                              "visit_enter_block_scope",    //
                              "visit_variable_use",         // body
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(test_parse_loop, c_style_for_loop_with_in_operator) {
  {
    test_parser p(u8"for (a in b; c; d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_in_disallowed_in_c_style_for_loop,  //
                        in_token, strlen(u8"for (a "), u8"in"_sv),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // b
                              "visit_variable_assignment",  // a
                              "visit_variable_use",         // c
                              "visit_enter_block_scope",    //
                              "visit_exit_block_scope",     //
                              "visit_variable_use",         // d
                          }));
  }

  {
    test_parser p(u8"for (let x = a in b; c; d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_in_disallowed_in_c_style_for_loop,  //
                        in_token, strlen(u8"for (let x = a "), u8"in"_sv),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // a
                              "visit_variable_use",          // b
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // c
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_variable_use",          // d
                              "visit_exit_for_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = a in b; c; d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_in_disallowed_in_c_style_for_loop,  //
                        in_token, strlen(u8"for (var x = a "), u8"in"_sv),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // a
                              "visit_variable_use",          // b
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // c
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_variable_use",          // d
                          }));
  }
}

TEST_F(test_parse_loop, for_loop_with_missing_component) {
  {
    test_parser p(u8"for () {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_header_of_for_loop,  //
                              where, strlen(u8"for "), u8"()"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                          }));
  }

  {
    test_parser p(u8"for (myVar) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_for_loop_rhs_or_components_after_expression,  //
                header, strlen(u8"for "), u8"(myVar)"_sv, for_token, 0,
                u8"for"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // myVar
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (let myVar) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_for_loop_rhs_or_components_after_declaration,  //
                header, strlen(u8"for "), u8"(let myVar)"_sv, for_token, 0,
                u8"for"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // myVar
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"myVar"_sv)}));
  }

  {
    test_parser p(u8"for (init; cond) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_c_style_for_loop_is_missing_third_component,  //
                existing_semicolon, strlen(u8"for (init"), u8";"_sv,
                expected_last_component, strlen(u8"for (init; cond"), u8")"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // init
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                          }));
  }
}

TEST_F(test_parse_loop, for_loop_with_missing_semicolons) {
  {
    test_parser p(u8"for (a b; c) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_missing_semicolon_between_for_loop_init_and_condition,  //
                expected_semicolon, strlen(u8"for (a"), u8""_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // c
                          }));
  }

  {
    test_parser p(u8"for (a; b c) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_missing_semicolon_between_for_loop_condition_and_update,  //
                expected_semicolon, strlen(u8"for (a; b"), u8""_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // c
                          }));
  }
}

TEST_F(test_parse_loop, for_loop_with_extra_semicolons) {
  {
    test_parser p(u8"for (;;;) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_semicolon_in_c_style_for_loop,  //
                              semicolon, strlen(u8"for (;;"), u8";"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;; ;;;) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_semicolon_in_c_style_for_loop,  //
                              semicolon, strlen(u8"for (;; "), u8";"_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_semicolon_in_c_style_for_loop,  //
                              semicolon, strlen(u8"for (;; ;"), u8";"_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_semicolon_in_c_style_for_loop,  //
                              semicolon, strlen(u8"for (;; ;;"), u8";"_sv)));
  }

  {
    test_parser p(u8"for (a;b;c;d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_semicolon_in_c_style_for_loop,  //
                              semicolon, strlen(u8"for (a;b;c"), u8";"_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // a
                              "visit_variable_use",       // b
                              "visit_variable_use",       // d
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // c
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"a", u8"b", u8"d", u8"c"}));
  }

  {
    test_parser p(u8"for (a of b; c; d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_semicolon_in_for_of_loop,  //
                        semicolon, strlen(u8"for (a of b"), u8";"_sv),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_semicolon_in_for_of_loop,  //
                        semicolon, strlen(u8"for (a of b; c"), u8";"_sv)));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // b
                              "visit_variable_assignment",  // a
                              "visit_variable_use",         // c
                              "visit_variable_use",         // d
                              "visit_enter_block_scope",    //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"b", u8"c", u8"d"}));
  }

  {
    test_parser p(u8"for (var a of b; c) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_semicolon_in_for_of_loop,  //
                        semicolon, strlen(u8"for (var a of b"), u8";"_sv),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // b
                              "visit_variable_declaration",  // a
                              "visit_variable_use",          // c
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var a in b; c; d) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_semicolon_in_for_in_loop,  //
                        semicolon, strlen(u8"for (var a of b"), u8";"_sv),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_semicolon_in_for_in_loop,  //
                        semicolon, strlen(u8"for (var a of b; c"), u8";"_sv)));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // a
                              "visit_variable_use",          // b
                              "visit_variable_use",          // c
                              "visit_variable_use",          // d
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(test_parse_loop, for_in_loop) {
  {
    test_parser p(u8"for (x in xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         //
                              "visit_variable_assignment",  //
                              "visit_enter_block_scope",    //
                              "visit_variable_use",         //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (let x in xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          //
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (var x in xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // xs
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (const x in []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_loop, for_in_loop_with_destructuring) {
  {
    test_parser p(u8"for ([x] in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"for ({x} in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"for (let [x] in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (let {x} in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (const [x] in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (const {x} in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (var [x] in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (var {x} in xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }
}

TEST_F(test_parse_loop, for_in_loop_with_var_initializer) {
  {
    test_parser p(u8"for (var x = init in xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // init
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // xs
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"init", u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (var x = 10 in []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"for (var x = ++y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_assignment",   // y
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = -y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = y + z in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_use",          // z
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = () => y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_enter_block_scope",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = (z) => y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // z
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_enter_block_scope",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = async () => y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_enter_block_scope",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = async (z) => y in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // z
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_enter_block_scope",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = <T,>() => y in []) {}"_sv,
                  typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // T
                              "visit_enter_function_scope_body",  //
                              "visit_variable_use",               // y
                              "visit_exit_function_scope",        //
                              "visit_variable_declaration",       // x
                              "visit_enter_block_scope",          //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = y ? z : w in []) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_use",          // z
                              "visit_variable_use",          // w
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (var x = yield y in []) {}"_sv);
    auto guard = p.enter_function(function_attributes::generator);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
  }

  // Previously, there was a bug which caused errors in parse_expression after
  // 'in' to be reported twice.
  {
    test_parser p(u8"for (var x = 0 in ()) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_missing_expression_between_parentheses),
                }));
  }
}

TEST_F(test_parse_loop, invalid_for_in_loop) {
  {
    test_parser p(u8"for (const x = 10 in []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop,  //
                equal_token, strlen(u8"for (const x "), u8"="_sv),
        }));
  }

  {
    test_parser p(u8"for (let x = 10 in []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop,  //
                equal_token, strlen(u8"for (let x "), u8"="_sv),
        }));
  }
}

TEST_F(test_parse_loop, for_of_loop) {
  {
    test_parser p(u8"for (x of xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         //
                              "visit_variable_assignment",  //
                              "visit_enter_block_scope",    //
                              "visit_variable_use",         //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (let x of xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          //
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for (var x of xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          //
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"body"}));
  }

  {
    test_parser p(u8"for await (let x of xs) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          //
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
  }

  {
    test_parser p(u8"for (let of myArray) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",    //
                              "visit_variable_use",       // myArray
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_let_with_no_bindings,  //
                                      where, strlen(u8"for ("), u8"let"_sv),
                }));
  }

  {
    test_parser p(u8"for (const x of []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_loop, for_of_loop_with_destructuring) {
  {
    test_parser p(u8"for ([x] of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"for ({x} of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"for (let [x] of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (let {x} of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (const [x] of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (const {x} of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (var [x] of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }

  {
    test_parser p(u8"for (var {x} of xs) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  }
}

TEST_F(test_parse_loop, invalid_for_of_loop) {
  {
    test_parser p(u8"for (const x = 10 of []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop,  //
                equal_token, strlen(u8"for (const x "), u8"="_sv),
        }));
  }

  {
    test_parser p(u8"for (let x = 10 of []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop,  //
                equal_token, strlen(u8"for (let x "), u8"="_sv),
        }));
  }

  {
    test_parser p(u8"for (var x = 10 of []) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  //
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop,  //
                equal_token, strlen(u8"for (let x "), u8"="_sv),
        }));
  }
}

TEST_F(test_parse_loop, for_loop_without_body) {
  {
    test_parser p(u8"for (let x of myArray) "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // myArray
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_for_statement,  //
                              for_and_header,
                              strlen(u8"for (let x of myArray)"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{ for (let x of myArray) }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_enter_for_scope",       //
                              "visit_variable_use",          // myArray
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",        //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_for_statement,  //
                              for_and_header,
                              strlen(u8"{ for (let x of myArray)"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_loop, for_loop_without_header) {
  {
    test_parser p(u8"for x = y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_missing_for_loop_header,  //
                                      for_token, 0, u8"for"_sv),
                }));
  }

  {
    test_parser p(u8"{ for } x = y;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",    //
                              "visit_exit_block_scope",     //
                              "visit_variable_use",         // y
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_missing_for_loop_header,  //
                                      for_token, strlen(u8"{ "), u8"for"_sv),
                }));
  }
}

TEST_F(test_parse_loop, while_statement) {
  {
    test_parser p(u8"while (cond) body;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                              "visit_variable_use",  // body
                          }));
  }

  {
    test_parser p(u8"while (cond) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(test_parse_loop, while_without_parens) {
  {
    test_parser p(u8"while cond { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_while_condition,  //
                condition, strlen(u8"while "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"while (cond { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_while_condition,  //
                where,
                offsets_matcher(p.code, strlen(u8"while (cond"), u8""_sv),  //
                token, u8')'),
        }));
  }

  {
    test_parser p(u8"while cond) { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_while_condition,             //
                where, offsets_matcher(p.code, strlen(u8"while "), u8""_sv),  //
                token, u8'('),
        }));
  }
}

TEST_F(test_parse_loop, while_without_condition) {
  {
    test_parser p(u8"while { go(); break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // go
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_condition_for_while_statement,  //
                        while_keyword, 0, u8"while"_sv),
                }));
  }
}

TEST_F(test_parse_loop, while_without_body) {
  {
    test_parser p(u8"while (cond) "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_body_for_while_statement,  //
                        while_and_condition, strlen(u8"while (cond)"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_loop, break_statement) {
  {
    test_parser p(u8"break;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_invalid_break,  //
                                      break_statement, 0, u8"break"_sv),
                }));
  }

  {
    test_parser p(u8"for (;;) { } break;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_invalid_break,  //
                                      break_statement,
                                      strlen(u8"for (;;) { } "), u8"break"_sv),
                }));
  }

  {
    test_parser p(u8"for (;;) { function f() { break; } }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_invalid_break,  //
                                      break_statement,
                                      strlen(u8"for (;;) { function f() { "),
                                      u8"break"_sv),
                }));
  }

  {
    test_parser p(u8"for (;;) { () => { break; } }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_OFFSETS(p.code, diag_invalid_break,  //
                                                break_statement,
                                                strlen(u8"for (;;) { () => { "),
                                                u8"break"_sv),
                          }));
  }

  {
    test_parser p(u8"switch (0) { default: break; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"do { break; } while (0);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;;) { break; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"while (0) { break; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;;) { for (;;) { break; } break; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(
        u8"switch (0) { default: switch(0) { default: break; } break; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_exit_block_scope",
                          }));
  }

  // TODO(#72): Visit the label.
  {
    test_parser p(u8"break label;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(test_parse_loop, continue_statement) {
  {
    test_parser p(u8"continue;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_invalid_continue,  //
                                      continue_statement, 0, u8"continue"_sv),
                }));
  }

  {
    test_parser p(u8"switch (0) { default: continue; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    ASSERT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_invalid_continue,  //
                    continue_statement, strlen(u8"switch (0) { default: "),
                    u8"continue"_sv)));
  }

  {
    test_parser p(u8"for (;;) { function f() { continue; } }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_invalid_continue,  //
                                      continue_statement,
                                      strlen(u8"for (;;) { function f() { "),
                                      u8"continue"_sv),
                }));
  }

  {
    test_parser p(u8"for (;;) { () => { continue; } }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_invalid_continue,  //
                              continue_statement,
                              strlen(u8"for (;;) { () => { "), u8"continue"_sv),
        }));
  }

  {
    test_parser p(u8"do { continue; } while (0);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;;) { continue; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"while (0) { continue; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;;) { for (;;) { continue; } continue; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_exit_block_scope",
                          }));
  }

  // TODO(#72): Visit the label.
  {
    test_parser p(u8"continue label;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(test_parse_loop,
       break_and_continue_statements_do_not_allow_newline_before_label) {
  {
    test_parser p(u8"for (;;) { break\nnotALabel; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // notALabel
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"for (;;) { continue\nnotALabel; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // notALabel
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(test_parse_loop,
       break_and_continue_statements_allows_contextual_keyword_as_label) {
  for (string8_view statement : {u8"break"_sv, u8"continue"_sv}) {
    for (string8_view keyword : contextual_keywords) {
      padded_string code(concat(keyword, u8": for (;;) { "_sv, statement,
                                u8" "_sv, keyword, u8"; }"_sv));
      SCOPED_TRACE(code);

      {
        // Top-level.
        test_parser p(code.string_view());
        p.parse_and_visit_statement();
      }

      {
        test_parser p(code.string_view());
        auto guard = p.enter_function(function_attributes::normal);
        p.parse_and_visit_statement();
      }
    }
  }

  // TODO(#214): Disallow labels named 'await' in async functions.
  // TODO(#214): Disallow labels named 'yield' in generator functions.
}

TEST_F(test_parse_loop,
       for_loop_async_arrow_with_of_parameter_is_init_expression) {
  test_parser p(u8"for (async of => x; y; z);"_sv);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       // of
                            "visit_enter_function_scope_body",  //
                            "visit_variable_use",               // x
                            "visit_exit_function_scope",        //
                            "visit_variable_use",               // y
                            "visit_variable_use",               // z
                        }));
}

TEST_F(test_parse_loop,
       cannot_assign_to_variable_named_async_without_parentheses_in_for_of) {
  test_parser p(u8"for (async of xs) ;"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"async"}));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(
              p.code,
              diag_cannot_assign_to_variable_named_async_in_for_of_loop,  //
              async_identifier, strlen(u8"for ("), u8"async"_sv),
      }));
}

TEST_F(test_parse_loop, for_loop_in_for_loop_header_crash) {
  // There used to be a use-after-free bug caused by a buffering_visitor copying
  // memory into another buffering_visitor, then the parser's
  // buffering_visitor_memory_ being rewind-ed. This test makes sure a
  // regression doesn't happen again (assuming Address Sanitizer catches the
  // use-after-free).
  test_parser p(
      u8R"(
        for (var f = () => {
          for (var xs = [x, x, x, x, x, x, x, x, x, x, x, x, x, x];;) {}
        };;) {}
      )"_sv,
      capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_uses, Not(IsEmpty()));
  EXPECT_THAT(p.variable_uses, ::testing::Each(u8"x"));
  EXPECT_THAT(p.errors, IsEmpty());
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
