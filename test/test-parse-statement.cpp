// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock-more-matchers.h>
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
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class test_parse_statement : public test_parse_expression {};

TEST_F(test_parse_statement, return_statement) {
  {
    test_parser p(u8"return a;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"a"}));
  }

  {
    test_parser p(u8"return a\nreturn b"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"a", u8"b"}));
  }

  {
    test_parser p(u8"return a; return b;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"a", u8"b"}));
  }

  {
    test_parser p(u8"if (true) return; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    test_parser p(u8"if (true) { return } else { other }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // other
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other"}));
  }
}

TEST_F(test_parse_statement, return_statement_disallows_newline) {
  {
    test_parser p(u8"return\nx"_sv, capture_diags);

    // Parse 'return'.
    p.parse_and_visit_statement(
        parser::parse_statement_type::any_statement_in_block);
    EXPECT_THAT(p.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));

    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_return_statement_returns_nothing,  //
                              return_keyword, 0, u8"return"_sv),
        }));
  }

  {
    test_parser p(u8"if (true) return\nx"_sv);

    // Parse 'if (true) return'.
    p.parse_and_visit_statement(parser::parse_statement_type::any_statement);
    EXPECT_THAT(p.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  // TODO(strager): These cases might be dead code instead (e.g. a method call).
  // Report a different error for potentially dead code.
  // TODO(strager): This list is incomplete.
  for (string8_view second_line : {
           u8"!true"_sv,
           u8"'string'"_sv,
           u8"() => {}"_sv,
           u8"(2 + 2)"_sv,
           u8"+42"_sv,
           u8"-42"_sv,
           u8"/=pattern/"_sv,
           u8"/pattern/"_sv,
           u8"42"_sv,
           u8"['a', 'b', 'c']"_sv,
           u8"`template${withSubstitution}`"_sv,
           u8"`template`"_sv,
           u8"await myPromise"_sv,
           u8"false"_sv,
           u8"function f() { }"_sv,
           u8"myVariable"_sv,
           u8"new Promise()"_sv,
           u8"null"_sv,
           u8"super.method()"_sv,
           u8"this"_sv,
           u8"true"_sv,
           u8"typeof banana"_sv,
           u8"{}"_sv,
           u8"~bits"_sv,
           u8"<div>hi</div>"_sv,
           u8"<p></p>"_sv,
           // TODO(strager): Contextual keywords (let, from, yield, etc.).
           // TODO(strager): Function without name. (Must be an expression, not
           // a statement.)
       }) {
    {
      test_parser p(concat(u8"return\n"_sv, second_line), jsx_options,
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_return_statement_returns_nothing,  //
                          return_keyword, 0, u8"return"_sv),
                  }));
    }

    {
      test_parser p(concat(u8"{ return\n"_sv, second_line, u8"}"_sv),
                    jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_return_statement_returns_nothing,  //
                          return_keyword, strlen(u8"{ "), u8"return"_sv),
                  }));
    }

    {
      test_parser p(
          concat(u8"async function f() { return\n"_sv, second_line, u8"}"_sv),
          jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_return_statement_returns_nothing,  //
                          return_keyword, strlen(u8"async function f() { "),
                          u8"return"_sv),
                  }));
    }

    {
      test_parser p(concat(u8"switch (cond) {\n"_sv, u8"default:\n"_sv,
                           u8"return\n"_sv, second_line, u8"}"_sv),
                    jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_return_statement_returns_nothing,  //
                  return_keyword, strlen(u8"switch (cond) {\ndefault:\n"),
                  u8"return"_sv),
          }));
    }
  }
}

TEST_F(test_parse_statement, return_statement_disallows_newline_in_block) {
  {
    test_parser p(u8"for (let x of []) return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",       //
                              "visit_variable_declaration",  // x
                              "visit_exit_for_scope",        //
                              "visit_variable_use",          // x
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(u8"if (cond) return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(u8"if (cond) {} else return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  // (if)
                              "visit_exit_block_scope",   // (if)
                              "visit_variable_use",       // x
                              "visit_end_of_module",
                          }));
  }

  {
    test_parser p(u8"while (cond) return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse_statement, throw_statement) {
  {
    test_parser p(u8"throw new Error('ouch');"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Error"}));
  }

  {
    test_parser p(u8"throw;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_expected_expression_before_semicolon,  //
                        where, strlen(u8"throw"), u8";"_sv),
                }));
  }

  {
    test_parser p(u8"throw\nnew Error();"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_expected_expression_before_newline,  //
                        where, strlen(u8"throw"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_statement, parse_and_visit_try) {
  {
    test_parser p(u8"try {} finally {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"try {} catch (e) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({catch_decl(u8"e"_sv)}));
  }

  {
    test_parser p(u8"try {} catch {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // try
                              "visit_exit_block_scope",   // try
                              "visit_enter_block_scope",  // catch
                              "visit_exit_block_scope",   // catch
                          }));
    EXPECT_THAT(p.variable_declarations, IsEmpty());
  }

  {
    test_parser p(u8"try {} catch (e) {} finally {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({catch_decl(u8"e"_sv)}));
  }

  {
    test_parser p(u8"try {f();} catch (e) {g();} finally {h();}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"f", u8"g", u8"h"}));
  }

  {
    test_parser p(u8"try {} catch ({message, code}) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     // (catch)
                              "visit_variable_declaration",  // message
                              "visit_variable_declaration",  // code
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {catch_decl(u8"message"_sv), catch_decl(u8"code"_sv)}));
  }

  {
    test_parser p(u8"try {} catch ([message, code]) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     // (catch)
                              "visit_variable_declaration",  // message
                              "visit_variable_declaration",  // code
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {catch_decl(u8"message"_sv), catch_decl(u8"code"_sv)}));
  }
}

TEST_F(test_parse_statement, catch_without_try) {
  {
    test_parser p(u8"catch (e) { body; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  // e
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",      //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_catch_without_try,  //
                                      catch_token, 0, u8"catch"_sv),
                }));
  }

  {
    test_parser p(u8"catch (e) { body; } finally { body; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  // e
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",      //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_catch_without_try,  //
                                      catch_token, 0, u8"catch"_sv),
                }));
  }
}

TEST_F(test_parse_statement, finally_without_try) {
  {
    test_parser p(u8"finally { body; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_finally_without_try,  //
                                      finally_token, 0, u8"finally"_sv),
                }));
  }
}

TEST_F(test_parse_statement, try_without_catch_or_finally) {
  {
    test_parser p(u8"try { tryBody; }\nlet x = 3;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_variable_use",          // tryBody
                              "visit_exit_block_scope",      // (try)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_missing_catch_or_finally_for_try_statement,  //
                try_token, 0, u8"try"_sv, expected_catch_or_finally,
                strlen(u8"try { tryBody; }"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_statement, try_without_body) {
  {
    test_parser p(u8"try\nlet x = 3;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_try_statement,  //
                              try_token, 0, u8"try"_sv),
        }));
  }
}

TEST_F(test_parse_statement, catch_without_body) {
  {
    test_parser p(u8"try {} catch\nlet x = 3;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_exit_block_scope",      // (try)
                              "visit_enter_block_scope",     // (catch)
                              "visit_exit_block_scope",      // (catch)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_catch_clause,  //
                              catch_token, strlen(u8"try {} catch"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_statement, finally_without_body) {
  {
    test_parser p(u8"try {} finally\nlet x = 3;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_exit_block_scope",      // (try)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_body_for_finally_clause,  //
                        finally_token, strlen(u8"try {} "), u8"finally"_sv),
                }));
  }
}

TEST_F(test_parse_statement, catch_without_variable_name_in_parentheses) {
  {
    test_parser p(u8"try {} catch () { body; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (try)
                              "visit_exit_block_scope",   // (try)
                              "visit_enter_block_scope",  // (catch)
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   // (catch)
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_FIELDS(
                diag_missing_catch_variable_between_parentheses,
                left_paren_to_right_paren,
                offsets_matcher(p.code, strlen(u8"try {} catch "),
                                u8"()"_sv),  //
                left_paren,
                offsets_matcher(p.code, strlen(u8"try {} catch "),
                                u8"("_sv),  //
                right_paren,
                offsets_matcher(p.code, strlen(u8"try {} catch ("), u8")"_sv)),
        }));
  }

  {
    test_parser p(u8"try {} catch ('ball') { body; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (try)
                              "visit_exit_block_scope",   // (try)
                              "visit_enter_block_scope",  // (catch)
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   // (catch)
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_expected_variable_name_for_catch,  //
                              unexpected_token, strlen(u8"try {} catch ("),
                              u8"'ball'"_sv),
        }));
  }
}

TEST_F(test_parse_statement, if_without_else) {
  {
    test_parser p(u8"if (a) { b; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"if (a) b;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_use",
                          }));
  }
}

TEST_F(test_parse_statement, if_with_else) {
  {
    test_parser p(u8"if (a) { b; } else { c; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",   //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"if (a) b; else c;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_use",  //
                              "visit_variable_use",
                          }));
  }
}

TEST_F(test_parse_statement, if_without_body) {
  {
    test_parser p(u8"if (a)\nelse e;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // e
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"if (a)"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{\nif (a)\n} b;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"{\nif (a)"), u8""_sv),
        }));
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // b
                          }));
  }

  {
    test_parser p(u8"if (a)"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_if_statement,  //
                              expected_body, strlen(u8"if (a)"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_statement, if_without_parens) {
  {
    test_parser p(u8"if cond { body; }"_sv, capture_diags);
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
            DIAG_TYPE_OFFSETS(p.code,
                              diag_expected_parentheses_around_if_condition,  //
                              condition, strlen(u8"if "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"if (cond { body; }"_sv, capture_diags);
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
                diag_expected_parenthesis_around_if_condition,  //
                where,
                offsets_matcher(p.code, strlen(u8"if (cond"), u8""_sv),  //
                token, u8')'),
        }));
  }

  {
    test_parser p(u8"if cond) { body; }"_sv, capture_diags);
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
                diag_expected_parenthesis_around_if_condition,             //
                where, offsets_matcher(p.code, strlen(u8"if "), u8""_sv),  //
                token, u8'('),
        }));
  }
}

TEST_F(test_parse_statement, if_without_condition) {
  {
    test_parser p(u8"if { yay(); } else { nay(); }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (if)
                              "visit_variable_use",       // yay
                              "visit_exit_block_scope",   // (if)
                              "visit_enter_block_scope",  // (else)
                              "visit_variable_use",       // nay
                              "visit_exit_block_scope",   // (else)
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_condition_for_if_statement,  //
                        if_keyword, 0, u8"if"_sv),
                }));
  }
}

TEST_F(test_parse_statement, else_without_if) {
  {
    test_parser p(u8"else { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_OFFSETS(p.code, diag_else_has_no_if,  //
                                                else_token, 0, u8"else"_sv),
                          }));
  }
}

TEST_F(test_parse_statement, missing_if_after_else) {
  {
    test_parser p(u8"if (false) {} else (true) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_missing_if_after_else,  //
                                      expected_if,
                                      strlen(u8"if (false) {} else"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"if (x) {} else (y) {} else {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_variable_use",       // y
                              "visit_enter_block_scope",  // first else
                              "visit_exit_block_scope",   // first else
                              "visit_enter_block_scope",  // second else
                              "visit_exit_block_scope",   // second else
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_if_after_else,  //
                              expected_if, strlen(u8"if (x) {} else"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"if (false) {} else true {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    ElementsAre(
        DIAG_TYPE_OFFSETS(p.code, diag_missing_semicolon_after_statement,  //
                          where, strlen(u8"if (false) {} else true"), u8""_sv));
  }

  {
    test_parser p(u8"if (false) {} else (true)\n{}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"if (false) {} else (true); {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"if (false) {} else () {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_expression_between_parentheses,  //
                              left_paren_to_right_paren,
                              strlen(u8"if (false) {} else "), u8"()"_sv),
            DIAG_TYPE_OFFSETS(p.code, diag_missing_if_after_else,  //
                              expected_if, strlen(u8"if (false) {} else"),
                              u8""_sv)))
        << "should not report diag_missing_arrow_operator_in_arrow_function";
  }

  {
    test_parser p(u8"if (false) {} else (x, y) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_variable_use",       // x
                              "visit_variable_use",       // y
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_missing_if_after_else,  //
                                      expected_if,
                                      strlen(u8"if (false) {} else"), u8""_sv),
                }))
        << "should not report diag_missing_arrow_operator_in_arrow_function";
  }
}

TEST_F(test_parse_statement, block_statement) {
  {
    test_parser p(u8"{ }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"{ first; second; third; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_variable_use",       //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"first",   //
                                                   u8"second",  //
                                                   u8"third"}));
  }
}

TEST_F(test_parse_statement, incomplete_block_statement) {
  {
    test_parser p(u8"{ a; "_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unclosed_code_block,  //
                                      block_open, 0, u8"{"_sv),
                }));
  }
}

TEST_F(test_parse_statement, switch_statement) {
  {
    test_parser p(u8"switch (x) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"switch (true) {case y:}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // y
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"switch (true) {default:}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"switch (true) {case x: case y: default: case z:}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // x
                              "visit_variable_use",       // y
                              "visit_variable_use",       // z
                              "visit_exit_block_scope",
                          }));
  }

  {
    test_parser p(u8"switch (true) { case true: x; let y; z; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // y
                              "visit_variable_use",          // z
                              "visit_exit_block_scope",
                          }));
  }

  {
    SCOPED_TRACE("':' should not be treated as a type annotation");
    test_parser p(u8"switch (true) { case x: Type }"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // x
                              "visit_variable_use",       // Type
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"Type"}));
  }
}

TEST_F(test_parse_statement, switch_without_parens) {
  {
    test_parser p(u8"switch cond { case ONE: break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_switch_condition,  //
                condition, strlen(u8"switch "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"switch (cond { case ONE: break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_switch_condition,  //
                where,
                offsets_matcher(p.code, strlen(u8"switch (cond"), u8""_sv),  //
                token, u8')'),
        }));
  }

  {
    test_parser p(u8"switch cond) { case ONE: break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_switch_condition,  //
                where,
                offsets_matcher(p.code, strlen(u8"switch "), u8""_sv),  //
                token, u8'('),
        }));
  }
}

TEST_F(test_parse_statement, switch_without_condition) {
  {
    test_parser p(u8"switch { case ONE: break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_condition_for_switch_statement,  //
                        switch_keyword, 0, u8"switch"_sv),
                }));
  }
}

TEST_F(test_parse_statement, switch_without_body) {
  {
    test_parser p(u8"switch (cond);"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_body_for_switch_statement,  //
                switch_and_condition, strlen(u8"switch (cond)"), u8""_sv),
        }));
  }
}

TEST_F(test_parse_statement, switch_without_body_curlies) {
  {
    test_parser p(u8"switch (cond) case a: break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_expected_left_curly,  //
                                      expected_left_curly,
                                      strlen(u8"switch (cond)"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"switch (cond) default: body; break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_expected_left_curly,  //
                                      expected_left_curly,
                                      strlen(u8"switch (cond)"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_statement, switch_case_without_expression) {
  {
    test_parser p(u8"switch (cond) { case: banana; break; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // banana
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_expected_expression_for_switch_case,  //
                        case_token, strlen(u8"switch (cond) { "), u8"case"_sv),
                }));
  }
}

TEST_F(test_parse_statement, switch_case_with_duplicated_cases) {
  {
    test_parser p(u8"switch (cond) {case x: case y: case y:}"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_duplicated_cases_in_switch_statement,
                first_switch_case, strlen(u8"switch (cond) {case x: case "),
                u8"y"_sv, duplicated_switch_case,
                strlen(u8"switch (cond) {case x: case y: case "), u8"y"_sv),
        }));
  }
  {
    test_parser p(
        u8"switch (cond) {case MyEnum.A: break; case MyEnum.A: break;}"_sv,
        capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_duplicated_cases_in_switch_statement,
                        first_switch_case, strlen(u8"switch (cond) {case "),
                        u8"MyEnum.A"_sv, duplicated_switch_case,
                        strlen(u8"switch (cond) {case MyEnum.A: break; case "),
                        u8"MyEnum.A"_sv),
                }));
  }
}

TEST_F(test_parse_statement, switch_clause_outside_switch_statement) {
  {
    test_parser p(u8"case x:"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_case_outside_switch_statement,  //
                              case_token, 0, u8"case"_sv),
        }));
  }

  {
    SCOPED_TRACE("':' should not be treated as a type annotation");
    test_parser p(u8"case x: Type"_sv, typescript_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // Type
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_case_outside_switch_statement,  //
                              case_token, 0, u8"case"_sv),
        }));
  }

  {
    test_parser p(u8"case\nif (y) {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // y
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_case_outside_switch_statement,  //
                              case_token, 0, u8"case"_sv),
        }));
  }

  {
    test_parser p(u8"default: next;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // next
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_unexpected_default_outside_switch_statement,  //
                default_token, 0, u8"default"_sv),
        }));
  }

  {
    test_parser p(u8"default\nif (x) body;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // body
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_unexpected_default_outside_switch_statement,  //
                default_token, 0, u8"default"_sv),
        }));
  }
}

TEST_F(test_parse_statement, with_statement) {
  {
    test_parser p(u8"with (cond) body;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",      // cond
                              "visit_enter_with_scope",  // with
                              "visit_variable_use",      // body
                              "visit_exit_with_scope",
                          }));
  }

  {
    test_parser p(u8"with (cond) { body; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
  }
}

TEST_F(test_parse_statement, statement_before_first_switch_case) {
  {
    test_parser p(u8"switch (cond) { console.log('hi'); case ONE: break; }"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // console
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_statement_before_first_switch_case,
                              unexpected_statement,
                              strlen(u8"switch (cond) { "), u8"console"_sv),
        }));
  }
}

TEST_F(test_parse_statement, with_statement_without_parens) {
  {
    test_parser p(u8"with cond { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_expected_parentheses_around_with_expression,  //
                expression, strlen(u8"with "), u8"cond"_sv),
        }));
  }

  {
    test_parser p(u8"with (cond { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_with_expression,  //
                where,
                offsets_matcher(p.code, strlen(u8"with (cond"), u8""_sv),  //
                token, u8')'),
        }));
  }

  {
    test_parser p(u8"with cond) { body; }"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(
                diag_expected_parenthesis_around_with_expression,            //
                where, offsets_matcher(p.code, strlen(u8"with "), u8""_sv),  //
                token, u8'('),
        }));
  }
}

TEST_F(test_parse_statement, debugger_statement) {
  {
    test_parser p(u8"debugger; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(test_parse_statement, labelled_statement) {
  {
    test_parser p(u8"some_label: ; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }

  {
    test_parser p(u8"foob: for (;;) body"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // body
                          }));
  }

  {
    test_parser p(u8"one: two: three: while (false) body;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // body
                          }));
  }
}

TEST_F(test_parse_statement, statement_label_can_be_a_contextual_keyword) {
  for (string8_view keyword : contextual_keywords) {
    padded_string code(string8(keyword) + u8": x;");
    SCOPED_TRACE(code);

    {
      // Top-level.
      test_parser p(code.string_view());
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // x
                            }));
    }

    {
      test_parser p(code.string_view());
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // x
                            }));
    }
  }
}

TEST_F(test_parse_statement, disallow_label_named_await_in_async_function) {
  test_parser p(u8"async function f() {await:}"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",
                        }));
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_2_OFFSETS(
              p.code, diag_label_named_await_not_allowed_in_async_function,  //
              await, strlen(u8"async function f() {"), u8"await"_sv, colon,
              strlen(u8"async function f() {await"), u8":"_sv),
      }));
}

TEST_F(test_parse_statement, disallow_label_named_yield_in_generator_function) {
  test_parser p(u8"function *f() {yield:}"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",
                        }));
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(p.code,
                            diag_missing_semicolon_after_statement,  //
                            where, strlen(u8"function *f() {yield"), u8""_sv),
          DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                            token, strlen(u8"function *f() {yield"), u8":"_sv),
      }));
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
