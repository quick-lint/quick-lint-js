// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <string>
#include <string_view>

using ::testing::ElementsAreArray;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Angle_Type_Assertion
    : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion, angle_type_assertion) {
  {
    Test_Parser p(u8"<Type>expr"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::Angle_Type_Assertion);
    EXPECT_EQ(summarize(ast->child_0()), "var expr");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"<Type>expr"_sv));
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Test_Parser p(u8"<Type>(expr)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeassert(paren(var expr))");
  }

  {
    Test_Parser p(u8"<Type>expr;\n// </Type>;"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeassert(var expr)")
        << "'<Type>' shouldn't be confused as an opening JSX tag";
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"f(<T>x);"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // f
                              "visit_variable_use",       // x
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f", u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"(<T>lhs) = rhs;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",     // <
                              "visit_variable_type_use",    // T
                              "visit_exit_type_scope",      // >
                              "visit_variable_use",         // rhs
                              "visit_variable_assignment",  // lhs
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"rhs"}));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"lhs"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type1 | Type2>(expr);"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Type1", u8"Type2", u8"expr"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type1 & Type2>(expr);"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Type1", u8"Type2", u8"expr"}));
  }

  for (String8_View code : {
           u8"<Type>(expr);"_sv,
           u8"<(Type)>(expr);"_sv,
           u8"< | Type>(expr);"_sv,
           u8"< & Type>(expr);"_sv,
           u8"<[Type]>(expr);"_sv,
           u8"<Type[]>(expr);"_sv,
           u8"<readonly Type[]>(expr);"_sv,
           u8"<keyof Type>(expr);"_sv,
           u8"<{k: Type}>(expr);"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Type
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type", u8"expr"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"< <T>() => RT>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // <
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // RT
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",   //
                              "visit_exit_type_scope",       // >
                              "visit_variable_use",          // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Array<T>>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Array
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Array<<T>() => RT>>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // <
                              "visit_variable_type_use",     // Array
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // RT
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",   //
                              "visit_exit_type_scope",       // >
                              "visit_variable_use",          // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<ns.Type>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",        // <
                              "visit_variable_namespace_use",  // ns
                              "visit_exit_type_scope",         // >
                              "visit_variable_use",            // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<typeof v>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_variable_use",      // v
                              "visit_exit_type_scope",   // >
                              "visit_variable_use",      // expr
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"v"_sv, u8"expr"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<unique symbol>expr;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_exit_type_scope",   // >
                              "visit_variable_use",      // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"<this>expr;"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_exit_type_scope",   // >
                              "visit_variable_use",      // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(u8"<42>expr;"_sv, no_diags,
                                                   typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_exit_type_scope",   // >
                              "visit_variable_use",      // expr
                          }));
  }

  for (const String8& type :
       typescript_builtin_type_keywords | typescript_special_type_keywords) {
    Test_Parser p(concat(u8"<"_sv, type, u8">expr;"_sv), typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_exit_type_scope",   // >
                              "visit_variable_use",      // expr
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"expr"}));
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_type_assertion_with_parenthesized_arrow) {
  // NOTE(strager): The following syntax is a generic arrow function, not an
  // angle type assertion: <Type>(param) => body

  {
    Test_Parser p(u8"<Type>(param => body)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "typeassert(paren(arrowfunc(var param)))");
  }

  {
    Test_Parser p(u8"<Type>((param) => body)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast), "typeassert(paren(arrowfunc(var param)))");
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_type_assertion_in_complex_expression) {
  {
    Test_Parser p(u8"<Type>x ? y : z"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(typeassert(var x), var y, var z)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_bracketed_type_assertion_is_jsx_tag_in_typescript_jsx_mode) {
  {
    Test_Parser p(u8"<Component>text;\n// </Component>;"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(Component)");
  }

  for (const String8& tag : keywords) {
    Test_Parser p(concat(u8"<"_sv, tag, u8">text;\n// </"_sv, tag, u8">;"_sv),
                  typescript_jsx_options);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(" + to_string(tag) + ")");
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_type_assertion_with_complex_type_is_error_in_typescript_jsx_mode) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Type1 | Type2>(expr);"_sv,  //
        u8"^^^^^^^^^^^^^^^ Diag_TypeScript_Angle_Type_Assertion_Not_Allowed_In_Tsx.bracketed_type\n"_diag
        u8"                     ` .expected_as"_diag,  //
        typescript_jsx_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<(Type)>expr;"_sv,  //
        u8"^^^^^^^^ Diag_TypeScript_Angle_Type_Assertion_Not_Allowed_In_Tsx.bracketed_type\n"_diag
        u8"            ` .expected_as"_diag,  //
        typescript_jsx_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // Type
                              "visit_exit_type_scope",    // >
                              "visit_variable_use",       // expr
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_type_assertion_is_not_allowed_in_function_parameter_list) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"(<T>x) => {}"_sv,                    //
                                    u8" ^^^^ Diag_Invalid_Parameter"_diag,  //
                                    typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"x"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"function f(<T>x) {}"_sv,                       //
        u8"           ^^^^ Diag_Invalid_Parameter"_diag,  //
        typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({func_param_decl(u8"x"_sv), function_decl(u8"f"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Angle_Type_Assertion,
       angle_type_assertion_without_expression_is_invalid) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"<Type> ;"_sv,                                                      //
        u8"      ` Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"_sv}));
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
