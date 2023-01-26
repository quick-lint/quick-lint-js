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
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
class test_parse : public test_parse_expression {};

// TODO(strager): Put test_escape_first_character_in_keyword tests into their
// own test file.
class test_escape_first_character_in_keyword : public ::testing::Test {};

// TODO(strager): Put test_no_overflow and test_overflow tests into their own
// test file.
class test_no_overflow : public test_parse_expression {};
class test_overflow : public test_parse_expression {};

TEST_F(test_parse, statement_starting_with_invalid_token) {
  for (string8_view token : {
           u8":"_sv,
           u8"?"_sv,
       }) {
    test_parser p(concat(token, u8" x"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                                      token, 0, token),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse, comma_not_allowed_between_class_methods) {
  {
    test_parser p(
        u8"class f { constructor() { this._a = false; }, ontext(text) { if (this._a) { process.stdout.write(text);}}}"_sv,
        capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 44, u8","_sv),
        }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          //
                              "visit_enter_class_scope_body",     //
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
                              "visit_exit_class_scope",           //
                              "visit_variable_declaration",       // f
                          }));
  }
}

TEST_F(test_parse, commas_not_allowed_between_class_methods) {
  {
    test_parser p(
        u8"class f { ,,, constructor() { this._a = false; },,, ontext(text) { if (this._a) { process.stdout.write(text);}},,,}"_sv,
        capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 10, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 11, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 12, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 48, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 49, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 50, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 111, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 112, u8","_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_comma_not_allowed_between_class_methods,  //
                              unexpected_comma, 113, u8","_sv),
        }));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // {
                              "visit_enter_class_scope_body",     //
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
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // class f
                          }));
  }
}

TEST_F(test_parse, asi_for_statement_at_right_curly) {
  {
    test_parser p(
        u8"function f() { console.log(\"hello\") } function g() { }"_sv,
        capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({function_decl(u8"f"_sv), function_decl(u8"g"_sv)}));
  }
}

TEST_F(test_parse, asi_for_statement_at_newline) {
  {
    test_parser p(u8"console.log('hello')\nconsole.log('world')\n"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console", u8"console"}));
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
    test_parser p(concat(u8"let x = 2\n"_sv, second_statement));
    SCOPED_TRACE(p.code);
    auto loop_guard = p.enter_loop();  // Allow 'break' and 'continue'.
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"cond"}));
  }

  {
    // This code should emit an error, but also use ASI for error recovery.
    test_parser p(u8"console.log('hello') console.log('world');"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console", u8"console"}));
    cli_source_position::offset_type end_of_first_expression =
        strlen(u8"console.log('hello')");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_semicolon_after_statement,  //
                        where, end_of_first_expression, u8""_sv),
                }));
  }

  for (string8_view variable_kind : {u8"const"_sv, u8"let"_sv, u8"var"_sv}) {
    test_parser p(
        concat(variable_kind, u8" a = 1\n"_sv, variable_kind, u8" b = 2\n"_sv),
        capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // a
                              "visit_variable_declaration",  // b
                          }));
  }

  {
    test_parser p(u8"let a = 1\n!b\n"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // a
                              "visit_variable_use",          // b
                          }));
  }

  {
    test_parser p(u8"a + b\nimport {x} from 'module'\n"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // a
                              "visit_variable_use",          // b
                              "visit_variable_declaration",  // x
                          }));
  }
}

TEST_F(test_parse, asi_between_expression_statements) {
  {
    test_parser p(u8"false\nfalse"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"true\ntrue"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"true\nvoid x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"true\nnew Animal();"_sv);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"true\nsuper();"_sv);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"true\ntypeof x;"_sv);
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"true\nawait myPromise;"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"}));
  }

  {
    test_parser p(u8"true\nyield myValue;"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  for (string8 keyword : contextual_keywords) {
    padded_string code(u8"true\n" + keyword);
    SCOPED_TRACE(code);
    test_parser p(code.string_view());
    p.parse_and_visit_module();
  }

  {
    test_parser p(u8"one\n#two\nthree"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"one", u8"three"}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE(diag_cannot_refer_to_private_variable_without_object),
        }));
  }
}

TEST_F(test_parse, asi_between_expression_statement_and_switch_label) {
  {
    test_parser p(
        u8R"(
      switch (x) {
        case a:
          f()
        case b:
          g()
      }
    )"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"x", u8"a", u8"f", u8"b", u8"g"}));
  }

  {
    test_parser p(
        u8R"(
      switch (x) {
        case a:
          f()
        default:
          g()
      }
    )"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"x", u8"a", u8"f", u8"g"}));
  }
}

TEST_F(test_parse, asi_between_expression_statement_and_declaration) {
  {
    test_parser p(u8"f()\nclass C {}"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // f
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(test_parse, asi_for_statement_at_end_of_file) {
  {
    test_parser p(u8"console.log(2+2)"_sv);
    p.parse_and_visit_statement();
  }
}

TEST_F(test_parse, utter_garbage) {
  {
    test_parser p(u8"if :\nkjaslkjd;kjaslkjd"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // kjaslkjd
                              "visit_variable_use",  // kjaslkjd
                          }));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_expected_parentheses_around_if_condition,  //
                              condition, strlen(u8"if "), u8":"_sv),
            DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                              token, strlen(u8"if "), u8":"_sv)));
  }
}

TEST_F(test_parse, statement_starting_with_extends) {
  {
    test_parser p(u8"extends Base"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // Base
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                                      token, 0, u8"extends"_sv),
                }));
  }
}

TEST_F(test_parse, stray_right_curly_at_top_level) {
  {
    test_parser p(u8"}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_right_curly,  //
                                      right_curly, 0, u8"}"_sv),
                }));
  }
}

TEST_F(
    test_parse,
    reserved_keywords_except_await_and_yield_cannot_contain_escape_sequences) {
  // TODO(#73): Test 'protected', 'implements', etc. in strict mode.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    string8 escaped_keyword = escape_first_character_in_keyword(keyword);

    {
      test_parser p(escaped_keyword, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_keyword_variable_use",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_keywords_cannot_contain_escape_sequences,  //
                  escape_sequence, 0, u8"\\u{??}"_sv),
          }));
    }

    {
      test_parser p(concat(u8"("_sv, escaped_keyword, u8")"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_keyword_variable_use",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, diag_keywords_cannot_contain_escape_sequences,  //
                  escape_sequence, strlen(u8"("), u8"\\u{??}"_sv),
          }));
    }
  }
}

TEST_F(
    test_parse,
    reserved_keywords_with_escape_sequences_are_treated_as_identifiers_in_variable_declarations) {
  {
    test_parser p(u8"const \\u{69}f = 42;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({const_init_decl(u8"if"_sv)}));
  }

  {
    test_parser p(u8"let \\u{69}f;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"if"_sv)}));
  }

  {
    test_parser p(u8"var \\u{69}f;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"if"_sv)}));
  }

  {
    test_parser p(u8"function g(\\u{69}f) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // g
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // if
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {function_decl(u8"g"_sv), func_param_decl(u8"if"_sv)}));
  }

  {
    test_parser p(u8"((\\u{69}f) => {})()"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_keywords_cannot_contain_escape_sequences),
                }));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // if
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"if"_sv)}));
  }
}

TEST_F(test_parse,
       contextual_keywords_and_await_and_yield_can_contain_escape_sequences) {
  for (string8 keyword : contextual_keywords) {
    string8 escaped_keyword = escape_first_character_in_keyword(keyword);
    SCOPED_TRACE(out_string8(keyword));

    {
      test_parser p(escaped_keyword, capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"({ "_sv, escaped_keyword, u8" })"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"({ "_sv, escaped_keyword, u8"() {} })"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_enter_function_scope_body",  //
                                "visit_exit_function_scope",        //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"({ "_sv, escaped_keyword, u8": null })"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"var "_sv, escaped_keyword, u8" = null;"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({var_init_decl(keyword)}));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"var { "_sv, escaped_keyword, u8" = a } = b;"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // a
                                "visit_variable_use",          // b
                                "visit_variable_declaration",  //
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({var_init_decl(keyword)}));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }

    {
      test_parser p(concat(u8"class C { "_sv, escaped_keyword, u8"() {} }"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",          //
                                "visit_enter_class_scope_body",     //
                                "visit_property_declaration",       //
                                "visit_enter_function_scope",       //
                                "visit_enter_function_scope_body",  //
                                "visit_exit_function_scope",        //
                                "visit_exit_class_scope",           //
                                "visit_variable_declaration",       // C
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.property_declarations, ElementsAreArray({keyword}));
      EXPECT_THAT(p.errors, IsEmpty()) << "escaped character is legal";
    }
  }
}

// Update this with different JavaScript if tests start failing because the
// syntax is now implemented. (Or delete this and related tests altogether if
// QLJS_PARSER_UNIMPLEMENTED disappears.)
padded_string unimplemented_token_code(u8"]"_sv);

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
TEST_F(test_parse, unimplemented_token_crashes_SLOW) {
  auto check = [] {
    spy_visitor v;
    parser p(&unimplemented_token_code, &v, javascript_options);
    p.parse_and_visit_module(v);
  };
  EXPECT_DEATH(check(), "token not implemented");
}
#endif

TEST_F(test_parse, unimplemented_token_doesnt_crash_if_caught) {
  {
    spy_visitor v;
    parser p(&unimplemented_token_code, &v, javascript_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    EXPECT_THAT(v.visits, IsEmpty());
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE_OFFSETS(&unimplemented_token_code,
                                                diag_unexpected_token,  //
                                                token, 0, u8"]"_sv),
                          }));
  }
}

TEST_F(test_parse, unimplemented_token_returns_to_innermost_handler) {
  {
    padded_string code(u8"hello world"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    volatile bool inner_catch_returned = false;
    bool outer_ok = p.catch_fatal_parse_errors([&] {
      bool inner_ok = p.catch_fatal_parse_errors(
          [&] { QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(&p); });
      inner_catch_returned = true;
      EXPECT_FALSE(inner_ok);
    });
    EXPECT_TRUE(outer_ok);
    EXPECT_TRUE(inner_catch_returned);
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_unexpected_token),
                          }));
  }
}

TEST_F(test_parse,
       unimplemented_token_after_handler_ends_returns_to_outer_handler) {
  {
    padded_string code(u8"hello world"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    volatile bool inner_catch_returned = false;
    bool outer_ok = p.catch_fatal_parse_errors([&] {
      bool inner_ok = p.catch_fatal_parse_errors([] {
        // Do nothing.
      });
      inner_catch_returned = true;
      EXPECT_TRUE(inner_ok);
      QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(&p);
    });
    EXPECT_FALSE(outer_ok);
    EXPECT_TRUE(inner_catch_returned);
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_unexpected_token),
                          }));
  }
}

TEST_F(test_parse, unimplemented_token_rolls_back_parser_depth) {
  {
    padded_string code(u8"hello world"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    volatile bool inner_catch_returned = false;
    bool outer_ok = p.catch_fatal_parse_errors([&] {
      parser::depth_guard outer_g(&p);
      int depth_before_inner = p.depth_;
      bool inner_ok = p.catch_fatal_parse_errors([&p] {
        parser::depth_guard inner_g(&p);
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(&p);
      });
      inner_catch_returned = true;
      int depth_after_inner = p.depth_;
      EXPECT_FALSE(inner_ok);
      EXPECT_EQ(depth_after_inner, depth_before_inner);
    });
    EXPECT_TRUE(outer_ok);
    EXPECT_TRUE(inner_catch_returned);
  }
}

TEST_F(test_parse, unimplemented_token_is_reported_on_outer_diag_reporter) {
  {
    padded_string code(u8"hello world"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);

    parser_transaction transaction = p.begin_transaction();
    bool ok = p.catch_fatal_parse_errors(
        [&] { QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(&p); });
    EXPECT_FALSE(ok);

    EXPECT_THAT(v.errors, IsEmpty())
        << "diag_unexpected_token should be buffered in the transaction";
    p.commit_transaction(std::move(transaction));
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_unexpected_token),
                          }))
        << "diag_unexpected_token should be reported when committing the "
           "transaction";
  }
}

TEST_F(test_escape_first_character_in_keyword,
       escaping_escapes_single_character) {
  EXPECT_EQ(escape_first_character_in_keyword(u8"a"_sv), u8"\\u{61}");
  EXPECT_EQ(escape_first_character_in_keyword(u8"b"_sv), u8"\\u{62}");
  EXPECT_EQ(escape_first_character_in_keyword(u8"z"_sv), u8"\\u{7a}");
}

TEST_F(test_escape_first_character_in_keyword,
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

TEST_F(test_no_overflow, parser_depth_limit_not_exceeded) {
  for (const string8& exps : {
           repeated_str(u8"("_sv, u8"10"_sv, u8")"_sv, parser::stack_limit - 2),
           repeated_str(u8"["_sv, u8"10"_sv, u8"]"_sv, parser::stack_limit - 2),
           repeated_str(u8"{"_sv, u8"10"_sv, u8"}"_sv, parser::stack_limit - 2),
           repeated_str(u8"while(true) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit - 2),
           repeated_str(u8"for(;;) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit - 2),
           repeated_str(u8"await "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit - 2),
           repeated_str(u8"if(true) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit - 2),
           repeated_str(u8"function f() { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit - 1),
           repeated_str(u8"() => { "_sv, u8""_sv, u8"}"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"if(true) { "_sv, u8""_sv, u8"}"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"while(true) { "_sv, u8""_sv, u8"}"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"for(;;) { "_sv, u8""_sv, u8"}"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"with({}) { "_sv, u8""_sv, u8"}"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"do{ "_sv, u8""_sv, u8"} while (true);"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"try{ "_sv, u8""_sv, u8"} catch(e) {}"_sv,
                        parser::stack_limit - 1),
           repeated_str(u8"class C { m() { "_sv, u8""_sv, u8"} }"_sv,
                        parser::stack_limit - 1),
       }) {
    test_parser p(exps, capture_diags);
    SCOPED_TRACE(p.code);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_TRUE(ok);
    EXPECT_THAT(p.errors, ::testing::Not(::testing::Contains(
                              DIAG_TYPE(diag_depth_limit_exceeded))));
  }

  {
    test_parser p(concat(u8"("_sv,
                         repeated_str(u8"{x:"_sv, u8""_sv, u8"}"_sv,
                                      parser::stack_limit - 3),
                         u8")"_sv),
                  capture_diags);
    SCOPED_TRACE(p.code);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_TRUE(ok);
    EXPECT_THAT(p.errors, IsEmpty());
  }

  for (const string8& jsx : {
           repeated_str(u8"<div>"_sv, u8""_sv, u8"</div>"_sv,
                        parser::stack_limit - 2),
           concat(u8"<>"_sv,
                  repeated_str(u8"<div>"_sv, u8""_sv, u8"</div>"_sv,
                               parser::stack_limit - 3),
                  u8"</>"_sv),
           repeated_str(u8"<div>{"_sv, u8""_sv, u8"}</div>"_sv,
                        (parser::stack_limit / 2) - 1),
           repeated_str(u8"<div attr={"_sv, u8"'value'"_sv, u8"} />"_sv,
                        (parser::stack_limit / 2) - 1),
       }) {
    padded_string code(u8"return " + jsx);
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_TRUE(ok);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  for (const string8& type : {
           repeated_str(u8"("_sv, u8"T"_sv, u8")"_sv, parser::stack_limit - 2),
       }) {
    padded_string code(concat(u8"let x: "_sv, type, u8";"_sv));
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_TRUE(ok);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST_F(test_overflow, parser_depth_limit_exceeded) {
  for (const string8& exps : {
           repeated_str(u8"("_sv, u8"10"_sv, u8")"_sv, parser::stack_limit + 1),
           repeated_str(u8"["_sv, u8"10"_sv, u8"]"_sv, parser::stack_limit + 1),
           repeated_str(u8"{"_sv, u8"10"_sv, u8"}"_sv, parser::stack_limit + 1),
           repeated_str(u8"while(true) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"for(;;) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"if(true) "_sv, u8"10"_sv, u8""_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"function f() { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"() => { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"if(true) { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"while(true) { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"for(;;) { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"with({}) { "_sv, u8""_sv, u8"}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"do{ "_sv, u8""_sv, u8"} while (true);"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"try{ "_sv, u8""_sv, u8"} catch(e) {}"_sv,
                        parser::stack_limit + 1),
           repeated_str(u8"class C { m() { "_sv, u8""_sv, u8"} }"_sv,
                        parser::stack_limit + 1),
       }) {
    padded_string code(exps);
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_depth_limit_exceeded),
                          }));
  }

  {
    test_parser p(repeated_str(u8"await "_sv, u8"10"_sv, u8""_sv,
                               parser::stack_limit + 1),
                  capture_diags);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_FALSE(ok);
    EXPECT_THAT(p.errors,
                ::testing::Contains(DIAG_TYPE(diag_depth_limit_exceeded)));
  }

  {
    test_parser p(concat(u8"("_sv,
                         repeated_str(u8"{x:"_sv, u8""_sv, u8"}"_sv,
                                      parser::stack_limit + 1),
                         u8")"_sv),
                  capture_diags);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors();
    EXPECT_FALSE(ok);
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_depth_limit_exceeded),
                          }));
  }

  for (const string8& jsx : {
           repeated_str(u8"<div>"_sv, u8""_sv, u8"</div>"_sv,
                        parser::stack_limit + 1),
           concat(u8"<>"_sv,
                  repeated_str(u8"<div>"_sv, u8""_sv, u8"</div>"_sv,
                               parser::stack_limit + 1),
                  u8"</>"_sv),
           repeated_str(u8"<div>{"_sv, u8""_sv, u8"}</div>"_sv,
                        (parser::stack_limit / 2) + 1),
           repeated_str(u8"<div attr={"_sv, u8"'value'"_sv, u8"} />"_sv,
                        (parser::stack_limit / 2) + 1),
       }) {
    padded_string code(concat(u8"return "_sv, jsx));
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_depth_limit_exceeded),
                          }));
  }

  for (const string8& type : {
           repeated_str(u8"("_sv, u8"T"_sv, u8")"_sv, parser::stack_limit + 1),
       }) {
    padded_string code(concat(u8"let x: "_sv, type, u8";"_sv));
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
    EXPECT_FALSE(ok);
    EXPECT_THAT(v.errors, ElementsAreArray({
                              DIAG_TYPE(diag_depth_limit_exceeded),
                          }));
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
