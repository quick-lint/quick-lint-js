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
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

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
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                              token, 0, token)));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }
}

TEST(test_parse, comma_not_allowed_between_class_methods) {
  {
    spy_visitor v;
    padded_string code(
        u8"class f { constructor() { this._a = false; }, ontext(text) { if (this._a) { process.stdout.write(text);}}}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_comma_not_allowed_between_class_methods,  //
                    unexpected_comma, 44, u8",")));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       //
                            "visit_enter_class_scope",          //
                            "visit_property_declaration",       //
                            "visit_enter_function_scope",       //
                            "visit_enter_function_scope_body",  //
                            "visit_exit_function_scope",        //
                            "visit_property_declaration",       //
                            "visit_enter_function_scope",       //
                            "visit_variable_declaration",       //
                            "visit_enter_function_scope_body",  //
                            "visit_enter_block_scope",          //
                            "visit_variable_use",               //
                            "visit_variable_use",               //
                            "visit_exit_block_scope",           //
                            "visit_exit_function_scope",        //
                            "visit_exit_class_scope"));
  }
}

TEST(test_parse, commas_not_allowed_between_class_methods) {
  {
    spy_visitor v;
    padded_string code(
        u8"class f { ,,, constructor() { this._a = false; },,, ontext(text) { if (this._a) { process.stdout.write(text);}},,,}"_sv);
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));

    EXPECT_THAT(
        v.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 10, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 11, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 12, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 48, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 49, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 50, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 111, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 112, u8","),
            DIAG_TYPE_OFFSETS(&code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 113, u8",")));

    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",       // class f
                            "visit_enter_class_scope",          // {
                            "visit_property_declaration",       // constructor
                            "visit_enter_function_scope",       // ()
                            "visit_enter_function_scope_body",  // {
                            "visit_exit_function_scope",        // }
                            "visit_property_declaration",       // ontext
                            "visit_enter_function_scope",       // (
                            "visit_variable_declaration",       // text)
                            "visit_enter_function_scope_body",  // { if
                            "visit_enter_block_scope",          // {
                            "visit_variable_use",               // this._a
                            "visit_variable_use",               // text
                            "visit_exit_block_scope",           // }
                            "visit_exit_function_scope",        // }
                            "visit_exit_class_scope"            // }
                            ));
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
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(
            spy_visitor::visited_variable_declaration{
                u8"f", variable_kind::_function, variable_init_kind::normal},
            spy_visitor::visited_variable_declaration{
                u8"g", variable_kind::_function, variable_init_kind::normal}));
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
                    u8"x", variable_kind::_let,
                    variable_init_kind::initialized_with_equals}));
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
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_missing_semicolon_after_statement,  //
                              where, end_of_first_expression, u8"")));
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

  {
    spy_visitor v = parse_and_visit_module(u8"true\nnew Animal();"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_module(u8"true\nsuper();"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    spy_visitor v = parse_and_visit_module(u8"true\ntypeof x;"_sv);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"true\nawait myPromise;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myPromise"}));
  }

  {
    padded_string code(u8"true\nyield myValue;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"myValue"}));
  }

  for (string8 keyword : contextual_keywords) {
    padded_string code(u8"true\n" + keyword);
    SCOPED_TRACE(code);
    spy_visitor v = parse_and_visit_module(code.string_view());
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"one\n#two\nthree"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"one"},
                            spy_visitor::visited_variable_use{u8"three"}));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE(
                    diag_cannot_refer_to_private_variable_without_object)));
  }
}

TEST(test_parse, asi_between_expression_statement_and_switch_label) {
  {
    spy_visitor v = parse_and_visit_module(
        u8R"(
      switch (x) {
        case a:
          f()
        case b:
          g()
      }
    )"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"f"},
                            spy_visitor::visited_variable_use{u8"b"},
                            spy_visitor::visited_variable_use{u8"g"}));
  }

  {
    spy_visitor v = parse_and_visit_module(
        u8R"(
      switch (x) {
        case a:
          f()
        default:
          g()
      }
    )"_sv);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"},
                            spy_visitor::visited_variable_use{u8"a"},
                            spy_visitor::visited_variable_use{u8"f"},
                            spy_visitor::visited_variable_use{u8"g"}));
  }
}

TEST(test_parse, asi_between_expression_statement_and_declaration) {
  {
    spy_visitor v = parse_and_visit_module(u8"f()\nclass C {}"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_use",          // f
                            "visit_variable_declaration",  // C
                            "visit_enter_class_scope",     //
                            "visit_exit_class_scope",      //
                            "visit_end_of_module"));
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
            DIAG_TYPE_OFFSETS(&code,
                              diag_expected_parentheses_around_if_condition,  //
                              condition, strlen(u8"if "), u8":"),
            DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                              token, strlen(u8"if "), u8":")));
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
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&code, diag_unexpected_token,  //
                                              token, 0, u8"extends")));
  }
}

TEST(test_parse, stray_right_curly_at_top_level) {
  {
    padded_string code(u8"}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unmatched_right_curly,  //
                              right_curly, 0, u8"}")));
  }
}

TEST(test_parse,
     reserved_keywords_except_await_and_yield_cannot_contain_escape_sequences) {
  // TODO(#73): Test 'protected', 'implements', etc. in strict mode.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    string8 escaped_keyword = escape_first_character_in_keyword(keyword);

    {
      padded_string code(escaped_keyword);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_keyword_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{keyword}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, 0, u8"\\u{??}")));
    }

    {
      padded_string code(u8"(" + escaped_keyword + u8")");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_keyword_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{keyword}));
      EXPECT_THAT(v.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_keywords_cannot_contain_escape_sequences,  //
                      escape_sequence, strlen(u8"("), u8"\\u{??}")));
    }
  }
}

TEST(
    test_parse,
    reserved_keywords_with_escape_sequences_are_treated_as_identifiers_in_variable_declarations) {
  {
    padded_string code(u8"const \\u{69}f = 42;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences)));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"if", variable_kind::_const,
                    variable_init_kind::initialized_with_equals}));
  }

  {
    padded_string code(u8"let \\u{69}f;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences)));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"if", variable_kind::_let, variable_init_kind::normal}));
  }

  {
    padded_string code(u8"var \\u{69}f;"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences)));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"if", variable_kind::_var, variable_init_kind::normal}));
  }

  {
    padded_string code(u8"function g(\\u{69}f) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences)));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",       // g
                                      "visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // if
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(
            spy_visitor::visited_variable_declaration{
                u8"g", variable_kind::_function, variable_init_kind::normal},
            spy_visitor::visited_variable_declaration{
                u8"if", variable_kind::_parameter,
                variable_init_kind::normal}));
  }

  {
    padded_string code(u8"((\\u{69}f) => {})()"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences)));
    EXPECT_THAT(v.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // if
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(spy_visitor::visited_variable_declaration{
            u8"if", variable_kind::_parameter, variable_init_kind::normal}));
  }
}

TEST(test_parse,
     contextual_keywords_and_await_and_yield_can_contain_escape_sequences) {
  for (string8 keyword : contextual_keywords) {
    string8 escaped_keyword = escape_first_character_in_keyword(keyword);
    SCOPED_TRACE(out_string8(keyword));

    {
      padded_string code(escaped_keyword);
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{keyword}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"({ " + escaped_keyword + u8" })");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_uses,
                  ElementsAre(spy_visitor::visited_variable_use{keyword}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"({ " + escaped_keyword + u8"() {} })");
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
      padded_string code(u8"({ " + escaped_keyword + u8": null })");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_end_of_module"));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"var " + escaped_keyword + u8" = null;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      keyword, variable_kind::_var,
                      variable_init_kind::initialized_with_equals}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"var { " + escaped_keyword + u8" = a } = b;");
      SCOPED_TRACE(code);
      spy_visitor v;
      parser p(&code, &v);
      p.parse_and_visit_module(v);
      EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // a
                                        "visit_variable_use",          // b
                                        "visit_variable_declaration",  //
                                        "visit_end_of_module"));
      EXPECT_THAT(v.variable_declarations,
                  ElementsAre(spy_visitor::visited_variable_declaration{
                      keyword, variable_kind::_var,
                      variable_init_kind::initialized_with_equals}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      padded_string code(u8"class C { " + escaped_keyword + u8"() {} }");
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
      EXPECT_THAT(
          v.property_declarations,
          ElementsAre(spy_visitor::visited_property_declaration{keyword}));
      EXPECT_THAT(v.errors, IsEmpty()) << "escaped character is legal";
    }
  }
}

// Update this with different JavaScript if tests start failing because the
// syntax is now implemented. (Or delete this and related tests altogether if
// QLJS_PARSER_UNIMPLEMENTED disappears.)
padded_string unimplemented_token_code(u8"]"_sv);

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
TEST(test_parse, unimplemented_token_crashes) {
  auto check = [] {
    spy_visitor v;
    parser p(&unimplemented_token_code, &v);
    p.parse_and_visit_module(v);
  };
  EXPECT_DEATH(check(), "token not implemented");
}
#endif

#if QLJS_HAVE_SETJMP
TEST(test_parse, unimplemented_token_doesnt_crash_if_caught) {
  {
    spy_visitor v;
    parser p(&unimplemented_token_code, &v);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(&unimplemented_token_code,
                                              diag_unexpected_token,  //
                                              token, 0, u8"]")));
  }
}
#endif

TEST(test_escape_first_character_in_keyword,
     escaping_escapes_single_character) {
  EXPECT_EQ(escape_first_character_in_keyword(u8"a"_sv), u8"\\u{61}");
  EXPECT_EQ(escape_first_character_in_keyword(u8"b"_sv), u8"\\u{62}");
  EXPECT_EQ(escape_first_character_in_keyword(u8"z"_sv), u8"\\u{7a}");
}

TEST(test_escape_first_character_in_keyword,
     escaping_escapes_first_of_many_characters) {
  EXPECT_EQ(escape_first_character_in_keyword(u8"abcde"_sv), u8"\\u{61}bcde");
  EXPECT_EQ(escape_first_character_in_keyword(u8"b1n z"_sv), u8"\\u{62}1n z");
  EXPECT_EQ(escape_first_character_in_keyword(u8"ZYXW"_sv), u8"\\u{5a}YXW");
}

string8 repeated_str(string8_view before, string8_view inner,
                     string8_view after, size_t depth) {
  string8 reps;
  reps.reserve((before.size() + after.size()) * depth + inner.size());
  auto append_str_to_reps = [&](string8_view str) {
    for (size_t i = 0; i < depth; i++) {
      reps.append(str);
    }
  };
  append_str_to_reps(before);
  reps.append(inner);
  append_str_to_reps(after);
  return reps;
}

#if QLJS_HAVE_SETJMP
TEST(test_no_overflow, parser_depth_limit_not_exceeded) {
  {
    for (const string8 &exps : {
             repeated_str(u8"(", u8"10", u8")", parser::stack_limit - 2),
             repeated_str(u8"[", u8"10", u8"]", parser::stack_limit - 2),
             repeated_str(u8"{", u8"10", u8"}", parser::stack_limit - 2),
             repeated_str(u8"while(true) ", u8"10", u8"",
                          parser::stack_limit - 2),
             repeated_str(u8"for(;;) ", u8"10", u8"", parser::stack_limit - 2),
             repeated_str(u8"await ", u8"10", u8"", parser::stack_limit - 2),
             repeated_str(u8"if(true) ", u8"10", u8"", parser::stack_limit - 2),
             repeated_str(u8"function f() { ", u8"", u8"}",
                          parser::stack_limit - 1),
             repeated_str(u8"() => { ", u8"", u8"}",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"if(true) { ", u8"", u8"}",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"while(true) { ", u8"", u8"}",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"for(;;) { ", u8"", u8"}",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"with({}) { ", u8"", u8"}",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"do{ ", u8"", u8"} while (true);",
                          (parser::stack_limit / 2) - 1),
             repeated_str(u8"try{ ", u8"", u8"} catch(e) {}",
                          parser::stack_limit - 1),
             repeated_str(u8"class C { m() { ", u8"", u8"} }",
                          parser::stack_limit - 1),
         }) {
      padded_string code(exps);
      spy_visitor v;
      parser p(&code, &v);
      bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
      EXPECT_TRUE(ok);
      EXPECT_THAT(v.errors, IsEmpty());
    }
  }

  {
    padded_string code(
        u8"(" + repeated_str(u8"{x:", u8"", u8"}", parser::stack_limit - 3) +
        u8")");
    spy_visitor v;
    parser p(&code, &v);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_TRUE(ok);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}
#endif

#if QLJS_HAVE_SETJMP
TEST(test_overflow, parser_depth_limit_exceeded) {
  for (const string8 &exps : {
           repeated_str(u8"(", u8"10", u8")", parser::stack_limit + 1),
           repeated_str(u8"[", u8"10", u8"]", parser::stack_limit + 1),
           repeated_str(u8"{", u8"10", u8"}", parser::stack_limit + 1),
           repeated_str(u8"while(true) ", u8"10", u8"",
                        parser::stack_limit + 1),
           repeated_str(u8"for(;;) ", u8"10", u8"", parser::stack_limit + 1),
           repeated_str(u8"await ", u8"10", u8"", parser::stack_limit + 1),
           repeated_str(u8"if(true) ", u8"10", u8"", parser::stack_limit + 1),
           repeated_str(u8"function f() { ", u8"", u8"}",
                        parser::stack_limit + 1),
           repeated_str(u8"() => { ", u8"", u8"}", parser::stack_limit + 1),
           repeated_str(u8"if(true) { ", u8"", u8"}", parser::stack_limit + 1),
           repeated_str(u8"while(true) { ", u8"", u8"}",
                        parser::stack_limit + 1),
           repeated_str(u8"for(;;) { ", u8"", u8"}", parser::stack_limit + 1),
           repeated_str(u8"with({}) { ", u8"", u8"}", parser::stack_limit + 1),
           repeated_str(u8"do{ ", u8"", u8"} while (true);",
                        parser::stack_limit + 1),
           repeated_str(u8"try{ ", u8"", u8"} catch(e) {}",
                        parser::stack_limit + 1),
           repeated_str(u8"class C { m() { ", u8"", u8"} }",
                        parser::stack_limit + 1),
           u8"return " + repeated_str(u8"<div>", u8"", u8"</div>",
                                      parser::stack_limit + 1),
           u8"return <>" + repeated_str(u8"<div>", u8"", u8"</div></>",
                                        parser::stack_limit + 1),
           u8"return " + repeated_str(u8"<div>{", u8"", u8"}</div>",
                                      parser::stack_limit + 1),
           u8"return " + repeated_str(u8"<div attr={", u8"'value'", u8"} />",
                                      parser::stack_limit + 1),
       }) {
    padded_string code(exps);
    SCOPED_TRACE(code);
    spy_visitor v;
    parser_options p_options;
    p_options.jsx = true;
    parser p(&code, &v, p_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    ElementsAre(DIAG_TYPE(diag_depth_limit_exceeded));
  }

  {
    padded_string code(
        u8"(" + repeated_str(u8"{x:", u8"", u8"}", parser::stack_limit + 1) +
        u8")");
    spy_visitor v;
    parser p(&code, &v);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    ElementsAre(DIAG_TYPE(diag_depth_limit_exceeded));
  }
}
#endif
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
