// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
class test_parse_var : public test_parse_expression {};

TEST_F(test_parse_var, parse_simple_let) {
  {
    test_parser& p = this->errorless_parser(u8"let x"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_noinit_decl(u8"x")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let a, b"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(let_noinit_decl(u8"a"), let_noinit_decl(u8"b")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let a, b, c, d, e, f, g"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 7);
    EXPECT_EQ(p.variable_declarations[0].name, u8"a");
    EXPECT_EQ(p.variable_declarations[1].name, u8"b");
    EXPECT_EQ(p.variable_declarations[2].name, u8"c");
    EXPECT_EQ(p.variable_declarations[3].name, u8"d");
    EXPECT_EQ(p.variable_declarations[4].name, u8"e");
    EXPECT_EQ(p.variable_declarations[5].name, u8"f");
    EXPECT_EQ(p.variable_declarations[6].name, u8"g");
    for (const auto& declaration : p.variable_declarations) {
      EXPECT_EQ(declaration.kind, variable_kind::_let);
    }
  }

  {
    test_parser p(u8"let first; let second"_sv, capture_diags);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"first");
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"first");
    EXPECT_EQ(p.variable_declarations[1].name, u8"second");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_var, parse_simple_var) {
  test_parser p(u8"var x"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_declarations, ElementsAre(var_noinit_decl(u8"x")));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_var, parse_simple_const) {
  test_parser p(u8"const x = null"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_declarations, ElementsAre(const_init_decl(u8"x")));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_var, parse_const_with_no_initializers) {
  test_parser p(u8"const x;"_sv, capture_diags);
  p.parse_and_visit_statement();
  ASSERT_EQ(p.variable_declarations.size(), 1);
  EXPECT_THAT(p.variable_declarations, ElementsAre(const_noinit_decl(u8"x")));
  EXPECT_THAT(p.errors,
              ElementsAre(DIAG_TYPE_OFFSETS(
                  p.code(), diag_missing_initializer_in_const_declaration,  //
                  variable_name, strlen(u8"const "), u8"x")));
}

TEST_F(test_parse_var, let_asi) {
  {
    test_parser& p = this->errorless_parser(u8"let x\ny"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // y
                            "visit_end_of_module"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_noinit_decl(u8"x")));
  }
}

TEST_F(test_parse_var, parse_let_with_initializers) {
  {
    test_parser& p = this->errorless_parser(u8"let x = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"x")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let x = 2, y = 3"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(let_init_decl(u8"x"), let_init_decl(u8"y")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let x = other, y = x"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"other", u8"x"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let x = y in z;"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"y", u8"z"));
  }
}

TEST_F(test_parse_var, parse_let_with_object_destructuring) {
  {
    test_parser& p = this->errorless_parser(u8"let {x} = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"x")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let {x, y, z} = 2"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"z");
  }

  {
    test_parser& p = this->errorless_parser(u8"let {key: variable} = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(let_init_decl(u8"variable")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let {} = x;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
  }

  {
    test_parser& p =
        this->errorless_parser(u8"let {key = defaultValue} = x;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_variable_use",  // defaultValue
                                      "visit_variable_declaration"));  // key
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"key")));
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"x",  //
                            u8"defaultValue"));
  }
}

TEST_F(test_parse_var, parse_let_with_array_destructuring) {
  {
    test_parser& p = this->errorless_parser(u8"let [first, second] = xs;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // x
                                      "visit_variable_declaration",    // first
                                      "visit_variable_declaration"));  // second
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(let_init_decl(u8"first"), let_init_decl(u8"second")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs"));
  }
}

TEST_F(test_parse_var, let_does_not_insert_semicolon_after_let_keyword) {
  {
    test_parser& p = this->errorless_parser(u8"let\nx = y;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"x")));
  }
}

TEST_F(test_parse_var,
       variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  test_parser p(u8"let x = x"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  //
                                    "visit_variable_declaration"));

  ASSERT_EQ(p.variable_declarations.size(), 1);
  EXPECT_EQ(p.variable_declarations[0].name, u8"x");
  EXPECT_THAT(p.variable_uses, ElementsAre(u8"x"));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_var, parse_valid_let) {
  {
    test_parser p(u8"let x\nclass C{}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",    // x
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_end_of_module"));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"let x\nnew Array()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // Array
                            "visit_end_of_module"));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"let x\ntypeof Array"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_typeof_use",   // Array
                            "visit_end_of_module"));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"let x\nclass C{}\nx = new C();"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",    // x
                            "visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // C
                            "visit_variable_use",            // C
                            "visit_variable_assignment",     // x
                            "visit_end_of_module"));

    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_var, parse_invalid_let) {
  {
    test_parser p(u8"let a,"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_stray_comma_in_let_statement,  //
                              where, strlen(u8"let a"), u8",")));
  }

  {
    test_parser p(u8"let a,;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_stray_comma_in_let_statement,  //
                              where, strlen(u8"let a"), u8",")));
  }

  {
    test_parser p(u8"let x, 42"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 1);
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"42")));
  }

  // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    {
      string8 code = u8"var " + keyword;
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code(), diag_cannot_declare_variable_with_keyword_name,  //
              keyword, strlen(u8"var "), keyword)));
    }

    {
      string8 code = u8"var " + keyword + u8";";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code(), diag_cannot_declare_variable_with_keyword_name,  //
              keyword, strlen(u8"var "), keyword)));
    }

    {
      string8 code = u8"var " + keyword + u8" = x;";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // x
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_OFFSETS(
              p.code(), diag_cannot_declare_variable_with_keyword_name,  //
              keyword, strlen(u8"var "), keyword)));
    }
  }

  {
    test_parser p(u8"let while (x) { break; }"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",       // x
                                      "visit_enter_block_scope",  //
                                      "visit_exit_block_scope",   //
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"while")));
  }

  {
    test_parser p(u8"let 42*69"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_EQ(p.variable_declarations.size(), 0);
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"42")));
  }

  {
    test_parser p(u8"let x, `hello`;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"`hello`")));
  }

  {
    test_parser p(u8"let x, `hello${world}`;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",  // x
                            "visit_variable_use",          // world
                            "visit_end_of_module"));
    // TODO(strager): Improve the span.
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let x, "), u8"`hello${")));
  }

  {
    test_parser p(u8"let {debugger}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 0);
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_missing_value_for_object_literal_entry,  //
                    key, strlen(u8"let {"), u8"debugger")));
  }

  {
    test_parser p(u8"let {42}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 0);
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_invalid_lone_literal_in_object_literal,  //
                    where, strlen(u8"let {"), u8"42")));
  }

  {
    test_parser p(u8"let true, true, y\nlet x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_use",          // y
                            "visit_variable_declaration",  // x
                            "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"y"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_noinit_decl(u8"x")));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_token_in_variable_declaration,  //
                    unexpected_token, strlen(u8"let "), u8"true")));
  }

  for (string8 prefix_operator : {u8"--", u8"++"}) {
    string8 code = u8"var " + prefix_operator + u8"x;";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // x
                                      "visit_variable_assignment",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(p.code(), diag_let_with_no_bindings,  //
                                      where, 0, u8"let"),
                    DIAG_TYPE_OFFSETS(
                        p.code(), diag_missing_semicolon_after_statement,  //
                        where, strlen(u8"let"), u8"")));
  }

  {
    test_parser p(u8"const = y, z = w, = x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_use",          // w
                                      "visit_variable_declaration",  // z
                                      "visit_variable_use",          // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors,
                UnorderedElementsAre(
                    DIAG_TYPE_OFFSETS(
                        p.code(), diag_missing_variable_name_in_declaration,  //
                        equal_token, strlen(u8"const "), u8"="),
                    DIAG_TYPE_OFFSETS(
                        p.code(), diag_missing_variable_name_in_declaration,  //
                        equal_token, strlen(u8"const = y, z = w, "), u8"=")));
  }

  {
    test_parser p(u8"let x y = z w"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // z
                                      "visit_variable_declaration",  // y
                                      "visit_variable_declaration",  // z
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code(), diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x"), u8""),
            DIAG_TYPE_OFFSETS(
                p.code(), diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x y = z"), u8"")));
  }

  {
    test_parser p(u8"let x [y]=ys {z}=zs"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // ys
                                      "visit_variable_declaration",  // y
                                      "visit_variable_use",          // zs
                                      "visit_variable_declaration",  // z
                                      "visit_end_of_module"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code(), diag_missing_comma_between_variable_declarations,  //
                expected_comma, strlen(u8"let x"), u8""),
            DIAG_TYPE_OFFSETS(
                p.code(), diag_missing_comma_between_variable_declarations,  //
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
    {
      string8 code = u8"let x " + compound_assignment_operator + u8" y, z";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_variable_declaration",  // z
                              "visit_end_of_module"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(let_init_decl(u8"x"), let_noinit_decl(u8"z")));
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_2_OFFSETS(
              p.code(), diag_cannot_update_variable_during_declaration,  //
              updating_operator, strlen(u8"let x "),
              compound_assignment_operator,  //
              declaring_token, 0, u8"let")));
    }

    {
      string8 code =
          u8"const [x, y] " + compound_assignment_operator + u8" init;";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",          // init
                              "visit_variable_declaration",  // x
                              "visit_variable_declaration",  // y
                              "visit_end_of_module"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(const_init_decl(u8"x"), const_init_decl(u8"y")));
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_2_OFFSETS(
              p.code(), diag_cannot_update_variable_during_declaration,  //
              updating_operator, strlen(u8"const [x, y] "),
              compound_assignment_operator,  //
              declaring_token, 0, u8"const")));
    }
  }

  {
    test_parser p(u8"let [42] = x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 0);
    // TODO(strager): Report a better message. We should say 'let statement',
    // not 'parameter'.
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_unexpected_literal_in_parameter_list,  //
                    literal, strlen(u8"let ["), u8"42")));
  }
}

TEST_F(test_parse_var, parse_let_with_missing_equal) {
  {
    test_parser p(u8"async function f() {return 1;}\nlet x await f()"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_variable_declaration",       // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_missing_equal_after_variable,  //
                    expected_equal,
                    strlen(u8"async function f() {return 1;}\nlet x"), u8"")));
  }

  {
    test_parser p(u8"let x class C{}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_class_scope",       // {
                            "visit_enter_class_scope_body",  // C
                            "visit_exit_class_scope",        // }
                            "visit_variable_declaration",    // x
                            "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x function f() {}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x null"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x new Array()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // Array
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x this"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x typeof Array"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_typeof_use",   // Array
                                      "visit_variable_declaration",  // x
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"async function f() {return 1;}\nlet x await f(), y = x"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",       // f
                                      "visit_enter_function_scope",       //
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope",        //
                                      "visit_variable_use",               // f
                                      "visit_variable_declaration",       // x
                                      "visit_variable_use",               // x
                                      "visit_variable_declaration",       // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_missing_equal_after_variable,  //
                    expected_equal,
                    strlen(u8"async function f() {return 1;}\nlet x"), u8"")));
  }

  {
    test_parser p(u8"let x class C{}, y = x"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_class_scope",       // {
                                      "visit_enter_class_scope_body",  // C
                                      "visit_exit_class_scope",        // }
                                      "visit_variable_declaration",    // x
                                      "visit_variable_use",            // x
                                      "visit_variable_declaration",    // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x function f() {}, y = x"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_named_function_scope",  // f
                                      "visit_enter_function_scope_body",   //
                                      "visit_exit_function_scope",         //
                                      "visit_variable_declaration",        // x
                                      "visit_variable_use",                // x
                                      "visit_variable_declaration",        // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x null, y = x"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x new Array(), y = x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // Array
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x this, y = x"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }

  {
    test_parser p(u8"let x typeof Array, y = x;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_typeof_use",   // Array
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use",          // x
                                      "visit_variable_declaration",  // y
                                      "visit_end_of_module"));

    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_equal_after_variable,  //
                              expected_equal, strlen(u8"let x"), u8"")));
  }
}

TEST_F(test_parse_var, parse_invalid_var) {
  {
    test_parser p(u8"var"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_let_with_no_bindings,  //
                              where, 0, u8"var")));
  }
}

TEST_F(test_parse_var, parse_invalid_const) {
  {
    test_parser p(u8"const"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_let_with_no_bindings,  //
                              where, 0, u8"const")));
  }
}

TEST_F(test_parse_var, report_missing_semicolon_for_declarations) {
  {
    test_parser p(u8"let x = 2 for (;;) { console.log(); }"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"x")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"console"));
    cli_source_position::offset_type end_of_let_statement =
        strlen(u8"let x = 2");
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_missing_semicolon_after_statement,  //
                    where, end_of_let_statement, u8"")));
  }
  {
    test_parser p(u8"let x debugger"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_noinit_decl(u8"x")));
    cli_source_position::offset_type end_of_let_statement = strlen(u8"let x");
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_missing_semicolon_after_statement,  //
                    where, end_of_let_statement, u8"")));
  }
}

TEST_F(test_parse_var, old_style_variables_can_be_named_let) {
  {
    test_parser& p = this->errorless_parser(u8"var let = initial;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_use",            // initial
                            "visit_variable_declaration"));  // let
    EXPECT_THAT(p.variable_declarations, ElementsAre(var_init_decl(u8"let")));
  }

  {
    test_parser& p = this->errorless_parser(u8"function let(let) {}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_declaration",  // let (function)
                            "visit_enter_function_scope",
                            "visit_variable_declaration",  // let (parameter)
                            "visit_enter_function_scope_body",
                            "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"let"), param_decl(u8"let")));
  }

  {
    test_parser& p = this->errorless_parser(u8"(function let() {})");
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.visits,
        ElementsAre("visit_enter_named_function_scope",  // let (function)
                    "visit_enter_function_scope_body",
                    "visit_exit_function_scope"));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAre(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"try { } catch (let) { }");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_block_scope",     //
                                      "visit_exit_block_scope",      //
                                      "visit_enter_block_scope",     //
                                      "visit_variable_declaration",  // let
                                      "visit_exit_block_scope"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(catch_decl(u8"let")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let {x = let} = o;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // o
                                      "visit_variable_use",            // let
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(p.variable_uses, ::testing::Contains(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"console.log(let);");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",    // console
                                      "visit_variable_use"));  // let
    EXPECT_THAT(p.variable_uses, ::testing::Contains(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let.method();");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_use"));  // let
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"let"));
  }

  for (string8 code : {
           u8"(async let => null)",
           u8"(async (let) => null)",
           u8"(let => null)",
           u8"((let) => null)",
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser& p = this->errorless_parser(code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",       //
                                      "visit_variable_declaration",       // let
                                      "visit_enter_function_scope_body",  //
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"let")));
  }

  {
    test_parser& p = this->errorless_parser(u8"for (let in xs) ;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                // TODO(strager): A for scope shouldn't be introduced by
                // this syntax. (No variable is being declared.)
                ElementsAre("visit_enter_for_scope",      //
                            "visit_variable_use",         // xs
                            "visit_variable_assignment",  // let
                            "visit_exit_for_scope"));
    EXPECT_THAT(p.variable_assignments, ElementsAre(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"for (let.prop in xs) ;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs", u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"let"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let in other;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"let", u8"other"));
  }

  {
    test_parser& p = this->errorless_parser(u8"let instanceof MyClass;");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"let", u8"MyClass"));
  }
}

TEST_F(test_parse_var, new_style_variables_cannot_be_named_let) {
  for (string8 declaration_kind : {u8"const", u8"let"}) {
    test_parser p(declaration_kind + u8" let = null;", capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(), diag_cannot_declare_variable_named_let_with_let,  //
            name, declaration_kind.size() + 1, u8"let")));

    EXPECT_THAT(p.visits, ElementsAre("visit_variable_declaration"));
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"let");
  }

  {
    test_parser p(u8"let {other, let} = stuff;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(), diag_cannot_declare_variable_named_let_with_let,  //
            name, strlen(u8"let {other, "), u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    test_parser p(u8"import let from 'weird';"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_cannot_import_let,  //
                              import_name, strlen(u8"import "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    test_parser p(u8"import * as let from 'weird';"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_cannot_import_let,  //
                              import_name, strlen(u8"import * as "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    test_parser p(u8"import { let } from 'weird';"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_cannot_import_let,  //
                              import_name, strlen(u8"import { "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    test_parser p(u8"import { someName as let } from 'weird';"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_cannot_import_let,  //
                    import_name, strlen(u8"import { someName as "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"let")));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    test_parser p(u8"import { 'someName' as let } from 'weird';"_sv,
                  capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_cannot_import_let,  //
                              import_name, strlen(u8"import { 'someName' as "),
                              u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(import_decl(u8"let")));
  }

  {
    test_parser p(u8"export function let() {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code(), diag_cannot_export_let,  //
                    export_name, strlen(u8"export function "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"let")));
  }

  // class implies strict mode.
  {
    test_parser p(u8"class let {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_cannot_declare_class_named_let,  //
                              name, strlen(u8"class "), u8"let")));
    EXPECT_THAT(p.variable_declarations, ElementsAre(class_decl(u8"let")));
  }
}

TEST_F(test_parse_var, use_await_in_non_async_function) {
  {
    test_parser& p = this->errorless_parser(u8"await(x);"_sv);
    auto guard = p.enter_function(function_attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"await",  //
                            u8"x"));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"async function f() {\n"
        u8"  function g() { await(x); }\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"await",  //
                            u8"x"));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"function f() {\n"
        u8"  async function g() {}\n"
        u8"  await();\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"(() => {\n"
        u8"  async () => {};\n"
        u8"  await();\n"
        u8"})");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p = this->errorless_parser(u8"(async => { await(); })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p = this->errorless_parser(u8"({ async() { await(); } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p =
        this->errorless_parser(u8"class C { async() { await(); } }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }
}

TEST_F(test_parse_var, declare_await_in_non_async_function) {
  {
    test_parser& p = this->errorless_parser(u8"function await() { }"_sv);
    auto guard = p.enter_function(function_attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"await")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let await = 42;"_sv);
    auto guard = p.enter_function(function_attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"await")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"(async function() {\n"
        u8"  (function(await) { })\n"
        u8"})");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"await")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"(function() {\n"
        u8"  async function await() { }\n"
        u8"})");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"await")));
  }
}

TEST_F(test_parse_var, declare_await_in_async_function) {
  {
    spy_visitor v;
    padded_string code(u8"function await() { }"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::async);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations, ElementsAre(function_decl(u8"await")));
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
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(var_noinit_decl(u8"await")));
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
    EXPECT_THAT(v.variable_declarations, ElementsAre(catch_decl(u8"await")));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_await_in_async_function,  //
                    name, strlen(u8"try {} catch ("), u8"await")));
  }

  {
    test_parser p(u8"async function f(await) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"f"),  //
                            param_decl(u8"await")));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code(),
                              diag_cannot_declare_await_in_async_function,  //
                              name, strlen(u8"async function f("), u8"await"),
            // TODO(strager): Drop the
            // diag_missing_operand_for_operator error.
            DIAG_TYPE(diag_missing_operand_for_operator)));
  }
}

TEST_F(test_parse_var, declare_await_at_top_level) {
  {
    test_parser& p = this->errorless_parser(u8"function await() { }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"await")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let await = 42;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"await")));
  }
}

TEST_F(test_parse_var, use_await_at_top_level_as_operator) {
  {
    test_parser& p = this->errorless_parser(u8"await x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await(x);"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await +x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await -x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await[x]"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await`template`"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await`template${x}`"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
  }
}

TEST_F(test_parse_var, use_await_at_top_level_as_variable) {
  {
    test_parser& p = this->errorless_parser(u8"await;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"(await)"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await = x"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // x
                                      "visit_variable_assignment",  // await
                                      "visit_end_of_module"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await.prop"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await?.prop"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // x
                                      "visit_end_of_module"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"await"));
  }

  {
    test_parser& p = this->errorless_parser(u8"await ? x : y"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
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
    test_parser& p = this->errorless_parser(code.string_view());
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
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
    test_parser& p = this->errorless_parser(code.string_view());
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // await
                                      "visit_variable_use",         // x
                                      "visit_variable_assignment",  // await
                                      "visit_end_of_module"));
  }

  // TODO(#464): Interpret / as divide, not a regular expression.
  if ((false)) {
    test_parser& p =
        this->errorless_parser(u8"await / await / await / await"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_variable_use",  // await
                                      "visit_end_of_module"));
  }
}

TEST_F(test_parse_var, forced_top_level_await_operator) {
  {
    test_parser p(
        u8"await p;"_sv,
        parser_options{
            .top_level_await_mode = parser_top_level_await_mode::await_operator,
        },
        capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // p
                                      "visit_end_of_module"));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(
        u8"await;"_sv,
        parser_options{
            .top_level_await_mode = parser_top_level_await_mode::await_operator,
        },
        capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAre("visit_end_of_module"));
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code(), diag_missing_operand_for_operator,  //
                              where, 0, u8"await")));
  }
}

TEST_F(
    test_parse_var,
    declare_await_in_async_function_is_allowed_for_named_function_expressions) {
  {
    test_parser& p = this->errorless_parser(
        u8"(async function() {\n"
        u8"  (function await() { await; })(); \n"
        u8"})();");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",        //
                            "visit_enter_function_scope_body",   //
                            "visit_enter_named_function_scope",  // await
                            "visit_enter_function_scope_body",   //
                            "visit_variable_use",                // await
                            "visit_exit_function_scope",         //
                            "visit_exit_function_scope"));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAre(u8"await"));
  }
}

TEST_F(test_parse_var, use_yield_in_non_generator_function) {
  {
    test_parser& p = this->errorless_parser(u8"yield(x);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"yield", u8"x"));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"function* f() {\n"
        u8"  function g() { yield(x); }\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"yield", u8"x"));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"function f() {\n"
        u8"  function* g() {}\n"
        u8"  yield();\n"
        u8"}");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"yield"));
  }
}

TEST_F(test_parse_var, declare_yield_in_non_generator_function) {
  {
    test_parser& p = this->errorless_parser(u8"function yield() { }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"yield")));
  }

  {
    test_parser& p = this->errorless_parser(u8"let yield = 42;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(u8"yield")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"(async function() {\n"
        u8"  (function(yield) { })\n"
        u8"})");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(u8"yield")));
  }

  {
    test_parser& p = this->errorless_parser(
        u8"(function() {\n"
        u8"  function* yield() { }\n"
        u8"})");
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, ElementsAre(function_decl(u8"yield")));
  }
}

TEST_F(test_parse_var, declare_yield_in_generator_function) {
  {
    spy_visitor v;
    padded_string code(u8"function yield() { }"_sv);
    parser p(&code, &v);
    auto guard = p.enter_function(function_attributes::generator);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_declarations, ElementsAre(function_decl(u8"yield")));
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
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(var_noinit_decl(u8"yield")));
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
    EXPECT_THAT(v.variable_declarations, ElementsAre(catch_decl(u8"yield")));
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_cannot_declare_yield_in_generator_function,  //
                    name, strlen(u8"try {} catch ("), u8"yield")));
  }

  {
    test_parser p(u8"function* f(yield) {}"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(function_decl(u8"f"),  //
                            param_decl(u8"yield")));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code(), diag_cannot_declare_yield_in_generator_function,  //
            name, strlen(u8"function* f("), u8"yield")));
  }
}

TEST_F(test_parse_var, variables_can_be_named_contextual_keywords) {
  dirty_set<string8> variable_names =
      (contextual_keywords - dirty_set<string8>{u8"let"}) |
      dirty_set<string8>{u8"await", u8"yield"} |
      // TODO(#73): Disallow these ('protected', 'implements', etc.) in strict
      // mode.
      strict_only_reserved_keywords;

  for (string8 name : variable_names) {
    SCOPED_TRACE(out_string8(name));

    {
      test_parser& p =
          this->errorless_parser(u8"var " + name + u8" = initial;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      EXPECT_THAT(p.variable_declarations, ElementsAre(var_init_decl(name)));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"let " + name + u8" = initial;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(name)));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"let {" + name + u8" = 10 } = initial;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      EXPECT_THAT(p.variable_declarations, ElementsAre(let_init_decl(name)));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"const " + name + u8" = initial;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",            // initial
                              "visit_variable_declaration"));  // (name)
      EXPECT_THAT(p.variable_declarations, ElementsAre(const_init_decl(name)));
    }

    {
      test_parser& p = this->errorless_parser(u8"function " + name + u8"(" +
                                              name + u8") {}");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.visits,
          ElementsAre("visit_variable_declaration",       // (name) (function)
                      "visit_enter_function_scope",       //
                      "visit_variable_declaration",       // (name) (parameter)
                      "visit_enter_function_scope_body",  //
                      "visit_exit_function_scope"));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(function_decl(name), param_decl(name)));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"(function " + name + u8"() {})");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.visits,
          ElementsAre("visit_enter_named_function_scope",  // (name) (function)
                      "visit_enter_function_scope_body",   //
                      "visit_exit_function_scope"));
      EXPECT_THAT(p.enter_named_function_scopes, ElementsAre(name));
    }

    {
      test_parser& p = this->errorless_parser(u8"class " + name + u8" {}");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_enter_class_scope",       //
                              "visit_enter_class_scope_body",  //
                              "visit_exit_class_scope",
                              "visit_variable_declaration"));  // (name)
      EXPECT_THAT(p.variable_declarations, ElementsAre(class_decl(name)));
    }

    {
      test_parser& p = this->errorless_parser(u8"(class " + name + u8" {})");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // (name)
                              "visit_exit_class_scope"));      // }
    }

    {
      test_parser& p =
          this->errorless_parser(u8"try { } catch (" + name + u8") { }");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_block_scope",     //
                                        "visit_exit_block_scope",      //
                                        "visit_enter_block_scope",     //
                                        "visit_variable_declaration",  // (name)
                                        "visit_exit_block_scope"));
      EXPECT_THAT(p.variable_declarations, ElementsAre(catch_decl(name)));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"let {x = " + name + u8"} = o;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",  // o
                                        "visit_variable_use",  // (name)
                                        "visit_variable_declaration"));  // x
      EXPECT_THAT(p.variable_uses, ::testing::Contains(name));
    }

    {
      test_parser& p = this->errorless_parser(u8"console.log(" + name + u8");");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",    // console
                                        "visit_variable_use"));  // (name)
      EXPECT_THAT(p.variable_uses, ::testing::Contains(name));
    }

    {
      string8 code = name;
      SCOPED_TRACE(out_string8(code));
      test_parser& p = this->errorless_parser(code.c_str());
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(p.variable_uses, ElementsAre(name));
    }

    {
      string8 code = name + u8";";
      SCOPED_TRACE(out_string8(code));
      test_parser& p = this->errorless_parser(code.c_str());
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(p.variable_uses, ElementsAre(name));
    }

    {
      test_parser& p = this->errorless_parser(name + u8".method();");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use"));  // (name)
      EXPECT_THAT(p.variable_uses, ElementsAre(name));
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
      test_parser& p = this->errorless_parser(code);
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                        "visit_variable_declaration",  // (name)
                                        "visit_enter_function_scope_body",  //
                                        "visit_exit_function_scope"));
      EXPECT_THAT(p.variable_declarations, ElementsAre(param_decl(name)));
    }

    {
      test_parser& p = this->errorless_parser(u8"for (" + name + u8" in xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_use",           // xs
                              "visit_variable_assignment"));  // (name)
      EXPECT_THAT(p.variable_assignments, ElementsAre(name));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (" + name + u8".prop in xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"xs"));
    }

    if (name != u8"async") {
      // NOTE(strager): async isn't allowed here. See
      // test_parse.cannot_assign_to_variable_named_async_in_for_of.
      test_parser& p = this->errorless_parser(u8"for (" + name + u8" of xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, ElementsAre(name));
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for ((" + name + u8") of xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, ElementsAre(name));
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (" + name + u8".prop of xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"xs"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (let " + name + u8" of xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, ElementsAre(let_noinit_decl(name)));
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (var " + name + u8" of xs) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, ElementsAre(var_noinit_decl(name)));
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"xs"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (" + name + u8"; cond;) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"cond"));
    }

    {
      test_parser& p =
          this->errorless_parser(u8"for (" + name + u8".prop; cond;) ;");
      auto guard = p.enter_function(function_attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"cond"));
    }
  }
}

TEST_F(test_parse_var,
       lexical_declaration_as_do_while_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    string8 code = u8"do " + variable_kind + u8" x = y; while (cond);";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_variable_use"));        // cond
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::do_while_loop,                                   //
            expected_body, offsets_matcher(p.code(), strlen(u8"do"), u8""),  //
            declaring_keyword,
            offsets_matcher(p.code(), strlen(u8"do "), variable_kind))));
  }
}

TEST_F(test_parse_var, lexical_declaration_as_for_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    string8 code = u8"for (;cond;) " + variable_kind + u8" x = y;";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::for_loop,  //
            expected_body,
            offsets_matcher(p.code(), strlen(u8"for (;cond;)"), u8""),  //
            declaring_keyword,
            offsets_matcher(p.code(), strlen(u8"for (;cond;) "),
                            variable_kind))));
  }
}

TEST_F(test_parse_var, lexical_declaration_as_if_statement_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    {
      string8 code = u8"if (cond) " + variable_kind + u8" x = y;";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // cond
                                        "visit_variable_use",            // y
                                        "visit_variable_declaration"));  // x
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(p.code(), strlen(u8"if (cond)"), u8""),  //
              declaring_keyword,
              offsets_matcher(p.code(), strlen(u8"if (cond) "),
                              variable_kind))));
    }

    {
      string8 code = u8"if (cond) " + variable_kind + u8" x = y; else {}";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // cond
                                        "visit_variable_use",          // y
                                        "visit_variable_declaration",  // x
                                        "visit_enter_block_scope",     // else
                                        "visit_exit_block_scope"));    // else
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(p.code(), strlen(u8"if (cond)"), u8""),  //
              declaring_keyword,
              offsets_matcher(p.code(), strlen(u8"if (cond) "),
                              variable_kind))));
    }

    {
      string8 code = u8"if (cond) {} else " + variable_kind + u8" x = y;";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code, capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // cond
                                        "visit_enter_block_scope",       // if
                                        "visit_exit_block_scope",        // if
                                        "visit_variable_use",            // y
                                        "visit_variable_declaration"));  // x
      EXPECT_THAT(
          p.errors,
          ElementsAre(DIAG_TYPE_3_FIELDS(
              diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
              statement_kind::if_statement,  //
              expected_body,
              offsets_matcher(p.code(), strlen(u8"if (cond) {} else"),
                              u8""),  //
              declaring_keyword,
              offsets_matcher(p.code(), strlen(u8"if (cond) {} else "),
                              variable_kind))));
    }
  }
}

TEST_F(test_parse_var, lexical_declaration_as_while_loop_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    string8 code = u8"while (cond) " + variable_kind + u8" x = y;";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::while_loop,  //
            expected_body,
            offsets_matcher(p.code(), strlen(u8"while (cond)"), u8""),  //
            declaring_keyword,
            offsets_matcher(p.code(), strlen(u8"while (cond) "),
                            variable_kind))));
  }
}

TEST_F(test_parse_var,
       lexical_declaration_as_with_statement_body_is_disallowed) {
  for (string8 variable_kind : {u8"const", u8"let"}) {
    string8 code = u8"with (obj) " + variable_kind + u8" x = y;";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",          // obj
                                      "visit_enter_with_scope",      // with
                                      "visit_variable_use",          // y
                                      "visit_variable_declaration",  // x
                                      "visit_exit_with_scope"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::with_statement,  //
            expected_body,
            offsets_matcher(p.code(), strlen(u8"with (obj)"), u8""),  //
            declaring_keyword,
            offsets_matcher(p.code(), strlen(u8"with (obj) "),
                            variable_kind))));
  }
}

TEST_F(test_parse_var,
       let_as_statement_body_does_not_allow_asi_before_left_square) {
  {
    test_parser p(u8"if (cond) let\n[x] = xs;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",            // cond
                                      "visit_variable_use",            // xs
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_3_FIELDS(
            diag_lexical_declaration_not_allowed_in_body, kind_of_statement,
            statement_kind::if_statement,  //
            expected_body,
            offsets_matcher(p.code(), strlen(u8"if (cond)"), u8""),  //
            declaring_keyword,
            offsets_matcher(p.code(), strlen(u8"if (cond) "), u8"let"))));
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
