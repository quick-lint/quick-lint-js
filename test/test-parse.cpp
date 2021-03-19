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

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
TEST(test_parse, statement_starting_with_invalid_token) {
  for (string8_view token : {
           u8":",
           u8"?",
       }) {
    padded_string code(string8(token) + u8" x");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_token, token,
                              offsets_matcher(&code, 0, token))));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, asi_for_statement_at_right_curly) {
  {
    spy_visitor v;
    padded_string code(
        u8"function f() { console.log(\"hello\") } function g() { }"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(
                    spy_visitor::visited_variable_declaration{
                        u8"f", variable_kind::_function},
                    spy_visitor::visited_variable_declaration{
                        u8"g", variable_kind::_function}));
  }
}

TEST(test_parse, asi_for_statement_at_newline) {
  {
    spy_visitor v;
    padded_string code(u8"console.log('hello')\nconsole.log('world')\n"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
  }

  for (string8_view second_statement : {
           u8"break; cond;"_sv,
           u8"continue; cond;"_sv,
           u8"do {} while (cond)"_sv,
           u8"for (; cond; ) {}"_sv,
           u8"if (cond) {}"_sv,
           u8"switch (cond) {}"_sv,
           u8"while (cond) {}"_sv,
       }) {
    spy_visitor v;
    padded_string code(string8(u8"let x = 2\n"_sv) + string8(second_statement));
    SCOPED_TRACE(code);
    parser p(&code, &v);
    auto loop_guard = p.enter_loop();  // Allow 'break' and 'continue'.
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"cond"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    // This code should emit an error, but also use ASI for error recovery.
    spy_visitor v;
    padded_string code(u8"console.log('hello') console.log('world');"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"console"},
                            spy_visitor::visited_variable_use{u8"console"}));
    cli_source_position::offset_type end_of_first_expression =
        strlen(u8"console.log('hello')");
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_semicolon_after_statement, where,
                    offsets_matcher(&code, end_of_first_expression, u8""))));
  }

  for (string8 variable_kind : {u8"const", u8"let", u8"var"}) {
    padded_string code(variable_kind + u8" a = 1\n" + variable_kind +
                       u8" b = 2\n");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",    // a
                            "visit_variable_declaration"));  // b
  }

  {
    spy_visitor v;
    padded_string code(u8"let a = 1\n!b\n"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",  // a
                            "visit_variable_use"));        // b
  }

  {
    spy_visitor v;
    padded_string code(u8"a + b\nimport {x} from 'module'\n"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",            // a
                            "visit_variable_use",            // b
                            "visit_variable_declaration"));  // x
  }
}

TEST(test_parse, asi_between_expression_statements) {
  {
    padded_string code(u8"false\nfalse"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"true\ntrue"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"true\nvoid x;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, asi_for_statement_at_end_of_file) {
  {
    spy_visitor v = parse_and_visit_statement(u8"console.log(2+2)"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, utter_garbage) {
  {
    spy_visitor v;
    padded_string code(u8"if :\nkjaslkjd;kjaslkjd"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",    // kjaslkjd
                                      "visit_variable_use"));  // kjaslkjd
    EXPECT_THAT(
        v.errors,
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(error_expected_parentheses_around_if_condition,
                             condition,
                             offsets_matcher(&code, strlen(u8"if "), u8":")),
            ERROR_TYPE_FIELD(error_unexpected_token, token,
                             offsets_matcher(&code, strlen(u8"if "), u8":"))));
  }
}

TEST(test_parse, statement_starting_with_extends) {
  {
    padded_string code(u8"extends Base"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // Base
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_token, token,
                              offsets_matcher(&code, 0, u8"extends"))));
  }
}

TEST(test_parse, stray_right_curly_at_top_level) {
  {
    padded_string code(u8"}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unmatched_right_curly, right_curly,
                              offsets_matcher(&code, 0, u8"}"))));
  }
}

TEST(test_parse,
     reserved_keywords_except_await_and_yield_cannot_contain_escape_sequences) {
  struct test_case {
    string8 code;
    string8 expected_identifier;
  };

  for (test_case tc : {
           test_case{u8"\\u{62}reak", u8"break"},
           test_case{u8"\\u{63}ase", u8"case"},
           test_case{u8"\\u{63}atch", u8"catch"},
           test_case{u8"\\u{63}lass", u8"class"},
           test_case{u8"\\u{63}onst", u8"const"},
           test_case{u8"\\u{63}ontinue", u8"continue"},
           test_case{u8"\\u{64}ebugger", u8"debugger"},
           test_case{u8"\\u{64}efault", u8"default"},
           test_case{u8"\\u{64}elete", u8"delete"},
           test_case{u8"\\u{64}o", u8"do"},
           test_case{u8"\\u{65}lse", u8"else"},
           test_case{u8"\\u{65}num", u8"enum"},
           test_case{u8"\\u{65}xport", u8"export"},
           test_case{u8"\\u{65}xtends", u8"extends"},
           test_case{u8"\\u{66}alse", u8"false"},
           test_case{u8"\\u{66}inally", u8"finally"},
           test_case{u8"\\u{66}or", u8"for"},
           test_case{u8"\\u{66}unction", u8"function"},
           test_case{u8"\\u{69}f", u8"if"},
           test_case{u8"\\u{69}mport", u8"import"},
           test_case{u8"\\u{69}n", u8"in"},
           test_case{u8"\\u{69}nstanceof", u8"instanceof"},
           test_case{u8"\\u{6e}ew", u8"new"},
           test_case{u8"\\u{6e}ull", u8"null"},
           test_case{u8"\\u{72}eturn", u8"return"},
           test_case{u8"\\u{73}uper", u8"super"},
           test_case{u8"\\u{73}witch", u8"switch"},
           test_case{u8"\\u{74}his", u8"this"},
           test_case{u8"\\u{74}hrow", u8"throw"},
           test_case{u8"\\u{74}rue", u8"true"},
           test_case{u8"\\u{74}ry", u8"try"},
           test_case{u8"\\u{74}ypeof", u8"typeof"},
           test_case{u8"\\u{76}ar", u8"var"},
           test_case{u8"\\u{76}oid", u8"void"},
           test_case{u8"\\u{77}hile", u8"while"},
           test_case{u8"\\u{77}ith", u8"with"},
       }) {
    {
      padded_string code(tc.code);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{
                      tc.expected_identifier}));
      EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_keywords_cannot_contain_escape_sequences,
                                escape_sequence,
                                offsets_matcher(&code, 0, u8"\\u{??}"))));
    }

    {
      padded_string code(u8"(" + tc.code + u8")");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{
                      tc.expected_identifier}));
      EXPECT_THAT(
          v.errors,
          ElementsAre(ERROR_TYPE_FIELD(
              error_keywords_cannot_contain_escape_sequences, escape_sequence,
              offsets_matcher(&code, strlen(u8"("), u8"\\u{??}"))));
    }
  }
}

TEST(test_parse,
     contextual_keywords_and_await_and_yield_can_contain_escape_sequences) {
  struct test_case {
    string8 code;
    string8 expected_identifier;
  };

  for (test_case tc : {
           test_case{u8"\\u{61}s", u8"as"},
           test_case{u8"\\u{61}sync", u8"async"},
           test_case{u8"\\u{61}wait", u8"await"},
           test_case{u8"\\u{66}rom", u8"from"},
           test_case{u8"\\u{67}et", u8"get"},
           test_case{u8"\\u{6c}et", u8"let"},
           test_case{u8"\\u{6f}f", u8"of"},
           test_case{u8"\\u{73}et", u8"set"},
           test_case{u8"\\u{73}tatic", u8"static"},
           test_case{u8"\\u{79}ield", u8"yield"},
       }) {
    SCOPED_TRACE(out_string8(tc.expected_identifier));

    {
      padded_string code(tc.code);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{
                      tc.expected_identifier}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"({ " + tc.code + u8"() {} })");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                        "visit_enter_function_scope_body",  //
                                        "visit_exit_function_scope",        //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"({ " + tc.code + u8": null })");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"var " + tc.code + u8" = null;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      tc.expected_identifier, variable_kind::_var}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"class C { " + tc.code + u8"() {} }");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // C
                                        "visit_enter_class_scope",          //
                                        "visit_property_declaration",       //
                                        "visit_enter_function_scope",       //
                                        "visit_enter_function_scope_body",  //
                                        "visit_exit_function_scope",        //
                                        "visit_exit_class_scope",           //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.property_declarations,
                  ElementsAre(spy_visitor::visited_property_declaration{
                      tc.expected_identifier}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }
  }
}
}
}
