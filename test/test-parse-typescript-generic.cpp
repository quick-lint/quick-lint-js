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
class test_parse_typescript_generic : public test_parse_expression {};

TEST_F(test_parse_typescript_generic, single_basic_generic_parameter) {
  {
    test_parser p(u8"<T>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_typescript_generic, multiple_basic_generic_parameter) {
  {
    test_parser p(u8"<T1, T2, T3>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                              "visit_variable_declaration",  // T3
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T1"_sv),
                                  generic_param_decl(u8"T2"_sv),
                                  generic_param_decl(u8"T3"_sv)}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"<T1, T2, T3,>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                              "visit_variable_declaration",  // T3
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_typescript_generic, parameters_require_commas_between) {
  {
    test_parser p(u8"<T1 T2>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T1"_sv),
                                  generic_param_decl(u8"T2"_sv)}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_comma_between_generic_parameters,
                        expected_comma, strlen(u8"<T1"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_typescript_generic,
       parameter_list_does_not_allow_leading_comma) {
  {
    test_parser p(u8"<, T>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<"), u8","_sv),
        }));
  }

  {
    test_parser p(u8"<,,, T>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<"), u8","_sv),
            DIAG_TYPE_OFFSETS(
                p.code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<,"), u8","_sv),
            DIAG_TYPE_OFFSETS(
                p.code, diag_comma_not_allowed_before_first_generic_parameter,
                unexpected_comma, strlen(u8"<,,"), u8","_sv),
        }));
  }
}

TEST_F(test_parse_typescript_generic,
       parameter_list_must_contain_at_least_one_parameter) {
  {
    test_parser p(u8"<>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_typescript_generic_parameter_list_is_empty,
                        expected_parameter, strlen(u8"<"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"<,>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_typescript_generic_parameter_list_is_empty,
                        expected_parameter, strlen(u8"<"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"<,,>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_typescript_generic_parameter_list_is_empty,
                        expected_parameter, strlen(u8"<"), u8""_sv),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<,"), u8","_sv),
                }));
  }
}

TEST_F(test_parse_typescript_generic,
       parameter_list_does_not_allow_multiple_trailing_commas) {
  {
    test_parser p(u8"<T,,>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T,"), u8","_sv),
                }));
  }

  {
    test_parser p(u8"<T , , ,>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T , "), u8","_sv),
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T , , "), u8","_sv),
                }));
  }
}

TEST_F(test_parse_typescript_generic,
       parameter_list_does_not_allow_consecutive_interior_commas) {
  {
    test_parser p(u8"<T,,U>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // U
                          }));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_multiple_commas_in_generic_parameter_list,
                        unexpected_comma, strlen(u8"<T,"), u8","_sv),
                }));
  }
}

TEST_F(test_parse_typescript_generic, parameter_list_extends) {
  {
    test_parser p(u8"<T extends U>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // U
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_typescript_generic, unexpected_colon_in_parameter_extends) {
  {
    test_parser p(u8"<T: U>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // U
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_unexpected_colon_after_generic_definition,
                        colon, strlen(u8"<T"), u8":"_sv),
                }));
  }
}

TEST_F(test_parse_typescript_generic, type_parameter_default) {
  {
    test_parser p(u8"<T = U>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // U
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_typescript_generic, type_parameter_default_with_extends) {
  {
    test_parser p(u8"<T extends U = Def>"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // U
                              "visit_variable_type_use",     // Def
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U", u8"Def"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_typescript_generic, variance_specifiers) {
  {
    test_parser p(u8"<in T>"_sv, typescript_options);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }

  {
    test_parser p(u8"<out T>"_sv, typescript_options);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }

  {
    test_parser p(u8"<in out T>"_sv, typescript_options);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(test_parse_typescript_generic, variance_specifiers_in_wrong_order) {
  {
    test_parser p(u8"<out in T>"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_generic_parameters();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_typescript_variance_keywords_in_wrong_order,
                in_keyword, strlen(u8"<out "), u8"in"_sv,  //
                out_keyword, strlen(u8"<"), u8"out"_sv),
        }));
  }
}

TEST_F(test_parse_typescript_generic,
       parameters_can_be_named_contextual_keywords) {
  for (string8 name :
       dirty_set<string8>{
           u8"await",
           u8"undefined",
       } | (contextual_keywords - typescript_builtin_type_keywords -
            typescript_special_type_keywords - typescript_type_only_keywords -
            dirty_set<string8>{
                u8"let",
                u8"static",
                u8"yield",
            })) {
    {
      test_parser p(concat(u8"<"_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      test_parser p(concat(u8"<in "_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      test_parser p(concat(u8"<out "_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      test_parser p(concat(u8"<in out "_sv, name, u8">"_sv),
                    typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }
  }
}

TEST_F(test_parse_typescript_generic, function_call_with_generic_arguments) {
  {
    test_parser p(u8"foo<T>(p)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    test_parser p(u8"foo<<Param>() => ReturnType>(p)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // Param
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
  }

  {
    test_parser p(u8"foo?.<T>(p)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    test_parser p(u8"foo?.<<Param>() => ReturnType>(p)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
  }

  {
    test_parser p(u8"foo<T>`bar`"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"foo<T>`bar${baz}`"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var foo, var baz)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }
}

TEST_F(test_parse_typescript_generic, new_with_generic_arguments) {
  {
    test_parser p(u8"new Foo<T>;"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"new Foo<T>"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    test_parser p(u8"new Foo<T>(p)"_sv, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    test_parser p(u8"new Foo<<Param>() => ReturnType>()"_sv,
                  typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // Param
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(test_parse_typescript_generic,
       variable_reference_with_generic_arguments) {
  struct test_case {
    string8_view code;
    const char* expected_ast;
    const char8* variable_type_use;
  };

  for (const test_case& tc : {
           // clang-format off
           test_case
           {u8"foo<T> /*EOF*/"_sv,     "var foo", u8"T"},
           {u8"foo<T>;"_sv,            "var foo", u8"T"},
           {u8"[foo<T>]"_sv,           "array(var foo)", u8"T"},
           {u8"(foo<T>)"_sv,           "paren(var foo)", u8"T"},
           {u8"{k: foo<T>}"_sv,        "object(literal: var foo)", u8"T"},
           {u8"foo<T>.prop"_sv,        "dot(var foo, prop)", u8"T"},
           {u8"foo<T>, other"_sv,      "binary(var foo, var other)", u8"T"},
           {u8"f(foo<T>)"_sv,          "call(var f, var foo)", u8"T"},
           {u8"f(foo<T>, other)"_sv,   "call(var f, var foo, var other)", u8"T"},
           {u8"foo<T> ? t : f"_sv,     "cond(var foo, var t, var f)", u8"T"},
           {u8"cond ? foo<T> : f"_sv,  "cond(var cond, var foo, var f)", u8"T"},
           {u8"foo<T> = rhs"_sv,       "assign(var foo, var rhs)", u8"T"},

           {u8"foo<T> ||= rhs"_sv,     "condassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> &&= rhs"_sv,     "condassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> ?\x3f= rhs"_sv,  "condassign(var foo, var rhs)", u8"T"},

           {u8"foo<T> %= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> &= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> **= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> *= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> += rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> -= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> /= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> <<= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> >>= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> >>>= rhs"_sv,    "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> ^= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> |= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},

           // In the following examples, the final keyword is part of the next
           // statement. We're only parsing the expression, and expression
           // parsing stops before the keyword.
           {u8"foo<T> break"_sv,     "var foo", u8"T"},
           {u8"foo<T> case"_sv,      "var foo", u8"T"},
           {u8"foo<T> const"_sv,     "var foo", u8"T"},
           {u8"foo<T> continue"_sv,  "var foo", u8"T"},
           {u8"foo<T> debugger"_sv,  "var foo", u8"T"},
           {u8"foo<T> default"_sv,   "var foo", u8"T"},
           {u8"foo<T> do"_sv,        "var foo", u8"T"},
           {u8"foo<T> else"_sv,      "var foo", u8"T"},
           {u8"foo<T> enum"_sv,      "var foo", u8"T"},
           {u8"foo<T> export"_sv,    "var foo", u8"T"},
           {u8"foo<T> for"_sv,       "var foo", u8"T"},
           {u8"foo<T> if"_sv,        "var foo", u8"T"},
           {u8"foo<T> import"_sv,    "var foo", u8"T"},
           {u8"foo<T> return"_sv,    "var foo", u8"T"},
           {u8"foo<T> switch"_sv,    "var foo", u8"T"},
           {u8"foo<T> throw"_sv,     "var foo", u8"T"},
           {u8"foo<T> try"_sv,       "var foo", u8"T"},
           {u8"foo<T> var"_sv,       "var foo", u8"T"},
           {u8"foo<T> while"_sv,     "var foo", u8"T"},
           {u8"foo<T> with"_sv,      "var foo", u8"T"},
           // clang-format on
       }) {
    SCOPED_TRACE(out_string8(tc.code));
    test_parser p(tc.code, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), tc.expected_ast);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({tc.variable_type_use}));
  }
}

TEST_F(test_parse_typescript_generic,
       generic_arguments_less_and_greater_are_operators_in_javascript) {
  {
    test_parser p(u8"foo<T>(p)"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, paren(var p))");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"foo<<T>()=>{}>(p)"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var foo, var T, arrowfunc(), paren(var p))");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"foo<T>`bar`"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, literal)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"foo<T>`bar${baz}`"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, template(var baz))");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"foo<<T>() => number>`bar${baz}`"_sv, javascript_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, arrowfunc())");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"new Foo<T>;"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    // FIXME(#557): Precedence is incorrect.
    EXPECT_EQ(summarize(ast), "new(binary(var Foo, var T, missing))");
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_missing_operand_for_operator),
                          }));
  }

  {
    test_parser p(u8"new Foo<T>(p);"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    // FIXME(#557): Precedence is incorrect.
    EXPECT_EQ(summarize(ast), "new(binary(var Foo, var T, paren(var p)))");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

// FIXME(#690): On second thought, I think treating less-greater as operators by
// default is a bad plan. TypeScript parses foo<T>{} as < and > operations, but
// also has type errors when using > with an object literal or when mixing < and
// >.
TEST_F(test_parse_typescript_generic,
       less_and_greater_are_operators_by_default) {
  struct test_case {
    string8_view code;
    const char* expected_ast;
  };

  for (const test_case& tc : {
           // clang-format off
           test_case
           {u8"foo<T> rhs"_sv,           "binary(var foo, var T, var rhs)"},
           {u8"foo<T> delete x"_sv,      "binary(var foo, var T, delete(var x))"},
           {u8"foo<T> class {}"_sv,      "binary(var foo, var T, class)"},
           {u8"foo<T> function(){}"_sv,  "binary(var foo, var T, function)"},
           {u8"foo<T> {}"_sv,            "binary(var foo, var T, object())"},
           {u8"foo<T> []"_sv,            "binary(var foo, var T, array())"},
           {u8"foo<T> /regexp/"_sv,      "binary(var foo, var T, literal)"},

           // The 'x' is part of the next statement.
           {u8"foo<T>\n let\n x"_sv,             "binary(var foo, var T, var let)"},
           {u8"foo<T>\n interface\n x\n {}"_sv,  "binary(var foo, var T, var interface)"},
           // clang-format on
       }) {
    SCOPED_TRACE(out_string8(tc.code));
    test_parser p(tc.code, typescript_options);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), tc.expected_ast);
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.visits, ::testing::AnyOf(
                              IsEmpty(),
                              ElementsAre("visit_enter_class_scope",          //
                                          "visit_enter_class_scope_body",     //
                                          "visit_exit_class_scope"),          //
                              ElementsAre("visit_enter_function_scope",       //
                                          "visit_enter_function_scope_body",  //
                                          "visit_exit_function_scope")))
        << "there should be no generic arguments (visit_variable_type_use)";
  }
}

TEST_F(
    test_parse_typescript_generic,
    greater_equal_ending_generic_argument_list_requires_space_in_expression) {
  // TypeScript does not split '>=' into '>' and '='. This will always result in
  // an error:
  //
  // * (A<B >= Z) is always a type error because booleans ('A<B' and 'Z') cannot
  //   be compared using >= in TypeScript.
  // * (A<B<C >>= Z) is always a type error because 'A<B' and 'C' cannot be
  //   compared using '<', and is always an error because you cannot assign to
  //   'A<B<C'.
  // * (A<B<C<D >>>= Z) is always an error like with (A<B<C >>= Z).
  //
  // quick-lint-js does split '>=', but it should report a helpful diagnostic
  // (instead of ugly type errors like TypeScript emits).
  //
  // See NOTE[typescript-generic-expression-token-splitting].

  {
    test_parser p(u8"foo<T>= rhs"_sv, typescript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_requires_space_between_greater_and_equal,
                greater_equal, strlen(u8"foo<T"), u8">="_sv),
        }));
  }

  {
    test_parser p(u8"foo<T<U>>= rhs"_sv, typescript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv, u8"U"_sv}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_requires_space_between_greater_and_equal,
                greater_equal, strlen(u8"foo<T<U>"), u8">="_sv),
        }));
  }

  {
    test_parser p(u8"foo<T<U<V>>>= rhs"_sv, typescript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                              "visit_variable_type_use",  // V
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"T"_sv, u8"U"_sv, u8"V"_sv}));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_typescript_requires_space_between_greater_and_equal,
                greater_equal, strlen(u8"foo<T<U<V>>"), u8">="_sv),
        }));
  }
}

TEST_F(test_parse_typescript_generic,
       unambiguous_generic_arguments_are_parsed_in_javascript) {
  {
    test_parser p(u8"foo?.<T>(p)"_sv, javascript_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_generics_not_allowed_in_javascript,
                opening_less, strlen(u8"foo?."), u8"<"_sv),
        }));
  }

  {
    test_parser p(u8"foo?.<<T>() => void>(p)"_sv, javascript_options,
                  capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_typescript_generics_not_allowed_in_javascript,
                opening_less, strlen(u8"foo?."), u8"<"_sv),
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
