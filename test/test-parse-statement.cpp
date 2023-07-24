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

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Statement : public Test_Parse_Expression {};

TEST_F(Test_Parse_Statement, return_statement) {
  {
    Test_Parser p(u8"return a;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"a"}));
  }

  {
    Test_Parser p(u8"return a\nreturn b"_sv, capture_diags);
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
    Test_Parser p(u8"return a; return b;"_sv, capture_diags);
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
    Test_Parser p(u8"if (true) return; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Test_Parser p(u8"if (true) { return } else { other }"_sv);
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

TEST_F(Test_Parse_Statement, return_statement_disallows_newline) {
  {
    Test_Parser p(u8"return\nx"_sv, capture_diags);

    // Parse 'return'.
    p.parse_and_visit_statement(
        Parser::Parse_Statement_Type::any_statement_in_block);
    EXPECT_THAT(p.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));

    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^^^^^ Diag_Return_Statement_Returns_Nothing"_diag,
        });
  }

  {
    Test_Parser p(u8"if (true) return\nx"_sv);

    // Parse 'if (true) return'.
    p.parse_and_visit_statement(Parser::Parse_Statement_Type::any_statement);
    EXPECT_THAT(p.variable_uses, IsEmpty());

    // Parse 'x' (separate statement from 'return')
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  // TODO(strager): These cases might be dead code instead (e.g. a method call).
  // Report a different error for potentially dead code.
  // TODO(strager): This list is incomplete.
  for (String8_View second_line : {
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
      Test_Parser p(concat(u8"return\n"_sv, second_line), jsx_options,
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^^ Diag_Return_Statement_Returns_Nothing"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"{ return\n"_sv, second_line, u8"}"_sv),
                    jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"  ^^^^^^ Diag_Return_Statement_Returns_Nothing"_diag,
          });
    }

    {
      Test_Parser p(
          concat(u8"async function f() { return\n"_sv, second_line, u8"}"_sv),
          jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"                     ^^^^^^ Diag_Return_Statement_Returns_Nothing"_diag,
          });
    }

    {
      Test_Parser p(concat(u8"switch (cond) {\n"_sv, u8"default:\n"_sv,
                           u8"return\n"_sv, second_line, u8"}"_sv),
                    jsx_options, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"                           ^^^^^^ Diag_Return_Statement_Returns_Nothing"_diag,
          });
    }
  }
}

TEST_F(Test_Parse_Statement, return_statement_disallows_newline_in_block) {
  {
    Test_Parser p(u8"for (let x of []) return\nx"_sv);
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
    Test_Parser p(u8"if (cond) return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"if (cond) {} else return\nx"_sv);
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
    Test_Parser p(u8"while (cond) return\nx"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, empty_paren_after_control_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if(){}"_sv,  //
        u8"   ` Diag_Empty_Paren_After_Control_Statement.expected_expression\n"_diag
        u8"^^ .token"_diag);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch(){}"_sv,  //
        u8"       ` Diag_Empty_Paren_After_Control_Statement.expected_expression\n"_diag
        u8"^^^^^^ .token"_diag);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while(){}"_sv,  //
        u8"      ` Diag_Empty_Paren_After_Control_Statement.expected_expression\n"_diag
        u8"^^^^^ .token"_diag);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with(){}"_sv,  //
        u8"     ` Diag_Empty_Paren_After_Control_Statement.expected_expression\n"_diag
        u8"^^^^ .token"_diag);
  }
}

TEST_F(Test_Parse_Statement, throw_statement) {
  {
    Test_Parser p(u8"throw new Error('ouch');"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Error"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"throw;"_sv,  //
        u8"     ^ Diag_Expected_Expression_Before_Semicolon"_diag);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"throw\nnew Error();"_sv,  //
        u8"     ` Diag_Expected_Expression_Before_Newline"_diag);
  }
}

TEST_F(Test_Parse_Statement, parse_and_visit_try) {
  {
    Test_Parser p(u8"try {} finally {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"try {} catch (e) {}"_sv);
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
    Test_Parser p(u8"try {} catch {}"_sv);
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
    Test_Parser p(u8"try {} catch (e) {} finally {}"_sv);
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
    Test_Parser p(u8"try {f();} catch (e) {g();} finally {h();}"_sv);
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
    Test_Parser p(u8"try {} catch ({message, code}) {}"_sv);
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
    Test_Parser p(u8"try {} catch ([message, code]) {}"_sv);
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

TEST_F(Test_Parse_Statement, catch_without_try) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"catch (e) { body; }"_sv,  //
                                    u8"^^^^^ Diag_Catch_Without_Try"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  // e
                              "visit_variable_use",          // body
                              "visit_exit_block_scope",      //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"catch (e) { body; } finally { body; }"_sv,  //
        u8"^^^^^ Diag_Catch_Without_Try"_diag);
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
  }
}

TEST_F(Test_Parse_Statement, finally_without_try) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"finally { body; }"_sv,  //
                                    u8"^^^^^^^ Diag_Finally_Without_Try"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, try_without_catch_or_finally) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try { tryBody; }\nlet x = 3;"_sv,  //
        u8"^^^ Diag_Missing_Catch_Or_Finally_For_Try_Statement.try_token\n"_diag
        u8"                ` .expected_catch_or_finally"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_variable_use",          // tryBody
                              "visit_exit_block_scope",      // (try)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, try_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try\nlet x = 3;"_sv,  //
        u8"^^^ Diag_Missing_Body_For_Try_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, catch_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try {} catch\nlet x = 3;"_sv,  //
        u8"            ` Diag_Missing_Body_For_Catch_Clause"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_exit_block_scope",      // (try)
                              "visit_enter_block_scope",     // (catch)
                              "visit_exit_block_scope",      // (catch)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, finally_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try {} finally\nlet x = 3;"_sv,  //
        u8"       ^^^^^^^ Diag_Missing_Body_For_Finally_Clause"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     // (try)
                              "visit_exit_block_scope",      // (try)
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, catch_without_variable_name_in_parentheses) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try {} catch () { body; }"_sv,  //
        u8"             ^^ Diag_Missing_Catch_Variable_Between_Parentheses.left_paren_to_right_paren\n"_diag
        u8"             ^ .left_paren\n"_diag
        u8"              ^ .right_paren"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (try)
                              "visit_exit_block_scope",   // (try)
                              "visit_enter_block_scope",  // (catch)
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   // (catch)
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"try {} catch ('ball') { body; }"_sv,  //
        u8"              ^^^^^^ Diag_Expected_Variable_Name_For_Catch"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (try)
                              "visit_exit_block_scope",   // (try)
                              "visit_enter_block_scope",  // (catch)
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   // (catch)
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, if_without_else) {
  {
    Test_Parser p(u8"if (a) { b; }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       //
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"if (a) b;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_use",
                          }));
  }
}

TEST_F(Test_Parse_Statement, if_with_else) {
  {
    Test_Parser p(u8"if (a) { b; } else { c; }"_sv);
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
    Test_Parser p(u8"if (a) b; else c;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  //
                              "visit_variable_use",  //
                              "visit_variable_use",
                          }));
  }

  {
    Test_Parser p(u8"if (a) async () => {}; else b;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",               // a
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_variable_use",               // b
                          }));
  }
}

TEST_F(Test_Parse_Statement, if_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (a)\nelse e;"_sv,  //
        u8"      ` Diag_Missing_Body_For_If_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_variable_use",  // e
                          }));
  }

  {
    Test_Parser p(u8"{\nif (a)\n} b;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ` Diag_Missing_Body_For_If_Statement"_diag,
        });
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",   //
                              "visit_variable_use",       // b
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"if (a)"_sv,  //
        u8"      ` Diag_Missing_Body_For_If_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // a
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, if_without_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if cond { body; }"_sv,  //
        u8"   ^^^^ Diag_Expected_Parentheses_Around_If_Condition"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond { body; }"_sv,  //
        u8"        ` Diag_Expected_Parenthesis_Around_If_Condition.where{.token=)}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if cond) { body; }"_sv,  //
        u8"   ` Diag_Expected_Parenthesis_Around_If_Condition.where{.token=(}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, if_without_condition) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if { yay(); } else { nay(); }"_sv,  //
        u8"^^ Diag_Missing_Condition_For_If_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // (if)
                              "visit_variable_use",       // yay
                              "visit_exit_block_scope",   // (if)
                              "visit_enter_block_scope",  // (else)
                              "visit_variable_use",       // nay
                              "visit_exit_block_scope",   // (else)
                          }));
  }
}

TEST_F(Test_Parse_Statement, else_without_if) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"else { body; }"_sv,  //
                                       u8"^^^^ Diag_Else_Has_No_If"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, missing_if_after_else) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (false) {} else (true) {}"_sv,  //
        u8"                  ` Diag_Missing_If_After_Else"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (x) {} else (y) {} else {}"_sv,  //
        u8"              ` Diag_Missing_If_After_Else"_diag);
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
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (false) {} else true {}"_sv,  //
        u8"                       ` Diag_Missing_Semicolon_After_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"if (false) {} else (true)\n{}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"if (false) {} else (true); {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    // Should not report Diag_Missing_Arrow_Operator_In_Arrow_Function.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (false) {} else () {}"_sv,  //
        u8"                   ^^ Diag_Missing_Expression_Between_Parentheses.left_paren_to_right_paren"_diag,  //
        u8"                  ` Diag_Missing_If_After_Else"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
  }

  {
    // Should not report Diag_Missing_Arrow_Operator_In_Arrow_Function.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (false) {} else (x, y) {}"_sv,  //
        u8"                  ` Diag_Missing_If_After_Else"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  // if
                              "visit_exit_block_scope",   // if
                              "visit_variable_use",       // x
                              "visit_variable_use",       // y
                              "visit_enter_block_scope",  // else
                              "visit_exit_block_scope",   // else
                          }));
  }
}

TEST_F(Test_Parse_Statement, block_statement) {
  {
    Test_Parser p(u8"{ }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"{ first; second; third; }"_sv);
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

TEST_F(Test_Parse_Statement, incomplete_block_statement) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"{ a; "_sv,  //
                                       u8"^ Diag_Unclosed_Code_Block"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_statement) {
  {
    Test_Parser p(u8"switch (x) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"switch (true) {case y:}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // y
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"switch (true) {default:}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",
                          }));
  }

  {
    Test_Parser p(u8"switch (true) {case x: case y: default: case z:}"_sv);
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
    Test_Parser p(u8"switch (true) { case true: x; let y; z; }"_sv);
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
    Test_Parser p(u8"switch (true) { case x: Type }"_sv, typescript_options);
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

TEST_F(Test_Parse_Statement, switch_without_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch cond { case ONE: break; }"_sv,  //
        u8"       ^^^^ Diag_Expected_Parentheses_Around_Switch_Condition"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond { case ONE: break; }"_sv,  //
        u8"            ` Diag_Expected_Parenthesis_Around_Switch_Condition.where{.token=)}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch cond) { case ONE: break; }"_sv,  //
        u8"       ` Diag_Expected_Parenthesis_Around_Switch_Condition.where{.token=(}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_without_condition) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch { case ONE: break; }"_sv,  //
        u8"^^^^^^ Diag_Missing_Condition_For_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_without_body) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond);"_sv,  //
        u8"             ` Diag_Missing_Body_For_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // cond
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_without_body_curlies) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) case a: break; }"_sv,  //
        u8"             ` Diag_Expected_Left_Curly"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // a
                              "visit_exit_block_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) default: body; break; }"_sv,  //
        u8"             ` Diag_Expected_Left_Curly"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_case_without_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) { case: banana; break; }"_sv,  //
        u8"                ^^^^ Diag_Expected_Expression_For_Switch_Case"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // banana
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, switch_case_with_duplicated_cases) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) {case x: case y: case y:}"_sv,  //
        u8"                            ^ Diag_Duplicated_Cases_In_Switch_Statement.first_switch_case\n"_diag
        u8"                                    ^ .duplicated_switch_case"_diag);
  }
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) {case MyEnum.A: break; case MyEnum.A: break;}"_sv,  //
        u8"                    ^^^^^^^^ Diag_Duplicated_Cases_In_Switch_Statement.first_switch_case\n"_diag
        u8"                                          ^^^^^^^^ .duplicated_switch_case"_diag);
  }
}

TEST_F(Test_Parse_Statement, switch_clause_outside_switch_statement) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"case x:"_sv,  //
        u8"^^^^ Diag_Unexpected_Case_Outside_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    SCOPED_TRACE("':' should not be treated as a type annotation");
    Spy_Visitor p = test_parse_and_visit_module(
        u8"case x: Type"_sv,                                          //
        u8"^^^^ Diag_Unexpected_Case_Outside_Switch_Statement"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // Type
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"case\nif (y) {}"_sv,  //
        u8"^^^^ Diag_Unexpected_Case_Outside_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // y
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"default: next;"_sv,  //
        u8"^^^^^^^ Diag_Unexpected_Default_Outside_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // next
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"default\nif (x) body;"_sv,  //
        u8"^^^^^^^ Diag_Unexpected_Default_Outside_Switch_Statement"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_variable_use",  // body
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Statement, with_statement) {
  {
    Test_Parser p(u8"with (cond) body;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",      // cond
                              "visit_enter_with_scope",  // with
                              "visit_variable_use",      // body
                              "visit_exit_with_scope",
                          }));
  }

  {
    Test_Parser p(u8"with (cond) { body; }"_sv);
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

TEST_F(Test_Parse_Statement, statement_before_first_switch_case) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"switch (cond) { console.log('hi'); case ONE: break; }"_sv,  //
        u8"                ^^^^^^^ Diag_Statement_Before_First_Switch_Case"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // console
                              "visit_variable_use",       // ONE
                              "visit_exit_block_scope",
                          }));
  }
}

TEST_F(Test_Parse_Statement, with_statement_without_parens) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with cond { body; }"_sv,  //
        u8"     ^^^^ Diag_Expected_Parentheses_Around_With_Expression"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (cond { body; }"_sv,  //
        u8"          ` Diag_Expected_Parenthesis_Around_With_Expression.where{.token=)}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // cond
                              "visit_enter_with_scope",   // with
                              "visit_enter_block_scope",  //
                              "visit_variable_use",       // body
                              "visit_exit_block_scope",   //
                              "visit_exit_with_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with cond) { body; }"_sv,  //
        u8"     ` Diag_Expected_Parenthesis_Around_With_Expression.where{.token=(}"_diag);
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

TEST_F(Test_Parse_Statement, debugger_statement) {
  {
    Test_Parser p(u8"debugger; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Statement, labelled_statement) {
  {
    Test_Parser p(u8"some_label: ; x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    // TODO(strager): Announce the label with a visit?
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                          }));
  }

  {
    Test_Parser p(u8"foob: for (;;) body"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // body
                          }));
  }

  {
    Test_Parser p(u8"one: two: three: while (false) body;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // body
                          }));
  }
}

TEST_F(Test_Parse_Statement, statement_label_can_be_a_contextual_keyword) {
  for (String8_View keyword : contextual_keywords) {
    Padded_String code(String8(keyword) + u8": x;");
    SCOPED_TRACE(code);

    {
      // Top-level.
      Test_Parser p(code.string_view());
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // x
                            }));
    }

    {
      Test_Parser p(code.string_view());
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // x
                            }));
    }
  }
}

TEST_F(Test_Parse_Statement, disallow_label_named_await_in_async_function) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"async function f() {await:}"_sv,  //
      u8"                    ^^^^^ Diag_Label_Named_Await_Not_Allowed_In_Async_Function.await\n"_diag
      u8"                         ^ .colon"_diag);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",
                        }));
}

TEST_F(Test_Parse_Statement, disallow_label_named_yield_in_generator_function) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"function *f() {yield:}"_sv,                                          //
      u8"                    ` Diag_Missing_Semicolon_After_Statement"_diag,  //
      u8"                    ^ Diag_Unexpected_Token"_diag);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_declaration",       // f
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",
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
