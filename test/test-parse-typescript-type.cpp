// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
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

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Type : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Type, direct_type_reference) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, direct_type_reference_with_keyword_name) {
  for (String8 keyword :
       ((contextual_keywords - typescript_builtin_type_keywords -
         typescript_special_type_keywords - typescript_type_only_keywords) |
        strict_only_reserved_keywords |
        Dirty_Set<String8>{u8"await", u8"yield"}) -
           Dirty_Set<String8>{
               // NOTE(strager): keyof is omitted on purpose because of
               // ambiguities in the grammar:
               // https://github.com/microsoft/TypeScript/issues/49724
               u8"keyof",
               // NOTE(strager): readonly is omitted on purpose because
               // TypeScript complains about it, even though there is no
               // ambiguity in this case.
               u8"readonly",
               // NOTE(strager): unique is omitted on purpose because of
               // ambiguities in the grammar.
               u8"unique",
           }) {
    {
      Padded_String code(keyword);
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_type_use",  // (keyword)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
    }

    {
      Padded_String code(concat(u8"["_sv, keyword, u8"]"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_type_use",  // (keyword)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Type, direct_generic_type_reference) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type", u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"C<'hello', number, Banana>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // C
                              "visit_variable_type_use",  // Banana
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C", u8"Banana"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"ns.C<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_namespace_use",  // ns
                              "visit_variable_type_use",       // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns", u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, less_less_token_is_split) {
  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"C<<T>() => ReturnType>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // C
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C", u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, greater_greater_token_is_split) {
  {
    SCOPED_TRACE("'>>' should be split into two tokens");
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"A<B<C>>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                              "visit_variable_type_use",  // C
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }

  {
    SCOPED_TRACE("'>>>' should be split into three tokens");
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"A<B<C<D>>>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                              "visit_variable_type_use",  // C
                              "visit_variable_type_use",  // D
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"A", u8"B", u8"C", u8"D"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, greater_equal_token_is_split) {
  {
    SCOPED_TRACE("'>=' should be split into two tokens");
    Spy_Visitor p = test_parse_and_visit_module(u8"let x: A<B>= y"_sv, no_diags,
                                                typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // A
                              "visit_variable_type_use",     // B
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"y"}));
  }

  {
    SCOPED_TRACE("'>>=' should be split into three tokens");
    Spy_Visitor p = test_parse_and_visit_module(u8"let x: A<B<C>>= y"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // A
                              "visit_variable_type_use",     // B
                              "visit_variable_type_use",     // C
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"A", u8"B", u8"C", u8"y"}));
  }

  {
    SCOPED_TRACE("'>>>=' should be split into four tokens");
    Spy_Visitor p = test_parse_and_visit_module(u8"let x: A<B<C<D>>>= y"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // A
                              "visit_variable_type_use",     // B
                              "visit_variable_type_use",     // C
                              "visit_variable_type_use",     // D
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"A", u8"B", u8"C", u8"D", u8"y"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, namespaced_type_reference) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"ns.Type"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_namespace_use",  // ns
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"ns.subns.subsubns.Type[ns2.K]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_namespace_use",  // ns
                              "visit_variable_namespace_use",  // ns2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns", u8"ns2"}));
  }

  for (String8 keyword : keywords) {
    Padded_String code(u8"mymodule." + keyword);
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        code.string_view(), no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_namespace_use",  // mymodule
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"mymodule"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, builtin_types) {
  for (String8 type : typescript_builtin_type_keywords) {
    SCOPED_TRACE(out_string8(type));
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        type, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "builtin type should not be treated as a variable";
  }
}

TEST_F(Test_Parse_TypeScript_Type, special_types) {
  for (String8 type : typescript_special_type_keywords) {
    SCOPED_TRACE(out_string8(type));
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        type, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "special type should not be treated as a variable";
  }
}

TEST_F(Test_Parse_TypeScript_Type, unique_symbol_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"unique symbol"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "'unique symbol' should not be treated as a variable";
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(unique symbol)"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "'unique symbol' should not be treated as a variable";
  }
}

TEST_F(Test_Parse_TypeScript_Type, this_type) {
  // TODO(#881): Only allow within class and interface method signatures.

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"this"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"this | OtherType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OtherType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, literal_type) {
  for (String8_View code : {
           u8"42"_sv,
           u8"-69"_sv,
           u8"'hello'"_sv,
           u8"null"_sv,
           u8"true"_sv,
           u8"false"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        code, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Type, template_literal_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"`hello`"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"`hello${other}`"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // other
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"`hello${other}${another}`"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // other
                              "visit_variable_type_use",  // another
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other", u8"another"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, parenthesized_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(Type)"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(((((Type)))))"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(number)"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Type, tuple_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, B, C]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, B, C, ]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, readonly_tuple_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly []"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly [A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly [A, B, C]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, tuple_type_optional_unnamed_element) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, B?]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?, B?]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?, B]"_sv,  //
        u8"      ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
        u8"  ^ .previous_optional_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    // Diagnostic should point to the last optional '?'.
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?, B?, C]"_sv,  //
        u8"          ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
        u8"      ^ .previous_optional_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?, B, C]"_sv,  //
        u8"         ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
        u8"  ^ .previous_optional_question"_diag,  //
        u8"      ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
        u8"  ^ .previous_optional_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, tuple_type_unnamed_spread_element) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, ...B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A, B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, ...B, C]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }
}

// NOTE(#867): TypeScript's rules here seem very strange. Is this possibly a
// TypeScript bug?
//
// Given:
//
//   type Ss = string[];
//   type Ns = number[];
//   type Os = object[];
//
// The following are legal:
//
//   type A = [...Ss, ...Ns];
//   type A = [...string[], ...Ns];
//   type A = [...Ss, ...Os | number[]];
//   type A = [...Ss, ...number[] | Os];
//   type A = [...Ss, ...Ns, ...Os];
//   type A = [...string[], ...Ns, ...Os];
//
// The following are illegal:
//
//   type A = [...string[], ...number[]];
//   type A = [...Ss, ...number[]];
//   type A = [...Ss, ...number[], ...Os];
//   type A = [...Ss, ...Ns, ...object[]];
//   type A = [...string[], ...[...string[]]];
//
// The rule seems to be this: If the spread is not the first, and if the spread
// type is syntactically an array type (not e.g. an alias to an array type) *or*
// if it's a tuple type with a spread, then report TypeScript diagnostic #1265.
//
// This rule is too complicated for me to implement right now, so let's just
// make sure we have no false positives.
TEST_F(Test_Parse_TypeScript_Type,
       tuple_type_can_only_have_one_array_spread_sorta) {
  {
    Test_Parser p(u8"[...A, ...B[]]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
    // TODO(#867): Assert a diagnostic.
  }

  {
    Test_Parser p(u8"[...A[], ...B[]]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
    // TODO(#867): Assert a diagnostic.
  }

  {
    // TypeScript's compiler only reports an error if the non-first spread is
    // syntactically an array type.
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A[], ...B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    // TypeScript's compiler only reports an error if the non-first spread is
    // syntactically an array type.
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A, ...B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Test_Parser p(u8"[...A[], ...B[], ...C[]]"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
    // TODO(#867): Assert a diagnostic.
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       tuple_type_unnamed_spread_element_with_optional_unnamed_element) {
  // Rest element can follow optional element.
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A?, ...B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  // Optional element cannot follow rest element.
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A, B?]"_sv,  //
        u8"        ^ Diag_TypeScript_Optional_Tuple_Element_Cannot_Follow_Spread_Element.optional_question\n"_diag
        u8" ^^^ .previous_spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...A?, B]"_sv,  //
        u8"     ^ Diag_TypeScript_Spread_Element_Cannot_Be_Optional.optional_question\n"_diag
        u8" ^^^ .spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, named_tuple_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, b: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, b: B, ]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  for (const String8& name :
       (keywords - disallowed_binding_identifier_keywords) | Dirty_Set<String8>{
                                                                 u8"false",
                                                                 u8"function",
                                                                 u8"import",
                                                                 u8"new",
                                                                 u8"null",
                                                                 u8"this",
                                                                 u8"true",
                                                                 u8"typeof",
                                                                 u8"void",
                                                             }) {
    Test_Parser p(concat(u8"["_sv, name, u8": A]"_sv), typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, named_tuple_type_with_missing_name) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, B]"_sv,  //
        u8"       ` Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type.expected_name_and_colon\n"_diag
        u8" ^^ .existing_name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, b: B, C]"_sv,  //
        u8"             ` Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type.expected_name_and_colon\n"_diag
        u8" ^^ .existing_name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[A, b: B]"_sv,  //
        u8" ` Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type.expected_name_and_colon\n"_diag
        u8"    ^^ .existing_name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[: A, b: B]"_sv,                                            //
        u8" ^ Diag_TypeScript_Missing_Name_In_Named_Tuple_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    // Should not also report a missing name for the second element, because
    // maybe the ':' was a mistake.
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[: A, B]"_sv,                                               //
        u8" ^ Diag_TypeScript_Missing_Name_In_Named_Tuple_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Test_Parser p(u8"[: A, b: B, C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"            ` Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type.expected_name_and_colon\n"_diag
            u8"      ^^ .existing_name"_diag,  //
            u8" ^ Diag_TypeScript_Missing_Name_In_Named_Tuple_Type"_diag,
        });
  }

  {
    Test_Parser p(u8"[: A, B, c: C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"      ` Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type.expected_name_and_colon\n"_diag
            u8"         ^^ .existing_name"_diag,  //
            u8" ^ Diag_TypeScript_Missing_Name_In_Named_Tuple_Type"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Type, tuple_type_optional_named_element) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a?: A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, b?: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a?: A, b?: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a?: A, b : B]"_sv,  //
        u8"         ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
        u8"  ^ .previous_optional_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Test_Parser p(u8"[a?: A, b?: B, c : C]"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
    // Diagnostic should point to the last optional '?'.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                ` Diag_TypeScript_Required_Tuple_Element_After_Optional_Element.expected_question\n"_diag
            u8"         ^ .previous_optional_question"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       tuple_type_optional_named_element_cannot_have_question_after_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A?]"_sv,  //
        u8"     ^ Diag_TypeScript_Named_Tuple_Element_Question_After_Type.question\n"_diag
        u8"  ` .expected_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a?: A?]"_sv,  //
        u8"      ^ Diag_TypeScript_Named_Tuple_Element_Question_After_Name_And_Type.type_question\n"_diag
        u8"  ^ .name_question"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, tuple_type_named_spread_element) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...a: A]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, ...b: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...a: A, b: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a: A, ...b: B, c: C]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B", u8"C"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       tuple_type_named_spread_element_with_optional_named_element) {
  // Rest element can follow optional element.
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[a?: A, ...b: B]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  // Optional element cannot follow rest element.
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...a: A, b?: B]"_sv,  //
        u8"           ^ Diag_TypeScript_Optional_Tuple_Element_Cannot_Follow_Spread_Element.optional_question\n"_diag
        u8" ^^^ .previous_spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...a?: A, b: B]"_sv,  //
        u8"     ^ Diag_TypeScript_Spread_Element_Cannot_Be_Optional.optional_question\n"_diag
        u8" ^^^ .spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       tuple_type_spread_named_element_cannot_have_dot_dot_dot_before_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[ a: ...A ]"_sv,  //
        u8"     ^^^ Diag_TypeScript_Named_Tuple_Element_Spread_Before_Type.spread\n"_diag
        u8"  ` .expected_spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"[...a: ...A]"_sv,  //
        u8"       ^^^ Diag_TypeScript_Named_Tuple_Element_Spread_Before_Name_And_Type.type_spread\n"_diag
        u8" ^^^ .name_spread"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, arrow_function) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"() => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(param) => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"param"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(a, b, c,) => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // a
                              "visit_variable_declaration",  // b
                              "visit_variable_declaration",  // c
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"a"_sv),
                                  func_type_param_decl(u8"b"_sv),
                                  func_type_param_decl(u8"c"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(param: ParamType) => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // ParamType
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ParamType", u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"param"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"([a, b, c]) => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // a
                              "visit_variable_declaration",  // b
                              "visit_variable_declaration",  // c
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"a"_sv),
                                  func_type_param_decl(u8"b"_sv),
                                  func_type_param_decl(u8"c"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"({key: param}: {key: ParamType}) => ReturnType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // ParamType
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ParamType", u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"param"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(...params: ParamsType) => ReturnType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // ParamsType
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // params
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ParamsType", u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"params"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, no_question_in_type_expression) {
  test_parse_and_visit_statement(
      u8"fs.promises.writeFile(outputPath, result).then((err: Error?) => {if (err) throw err;});"_sv,  //
      u8"                                                          ^ Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void"_diag,  //
      typescript_options);

  test_parse_and_visit_statement(
      u8"fs.promises.writeFile(outputPath, result).then((err: ?Error) => {if (err) throw err;});"_sv,  //
      u8"                                                     ^ Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void"_diag,  //
      typescript_options);

  test_parse_and_visit_typescript_type_expression(
      u8"Type?"_sv,  //
      u8"    ^ Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void"_diag,  //
      typescript_options);

  test_parse_and_visit_typescript_type_expression(
      u8"?Type"_sv,                                                           //
      u8"^ Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Type, generic_arrow_function) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"<T>() => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"new <T>() => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, constructor_function) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"new () => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"new (param1, param2) => ReturnType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // param1
                              "visit_variable_declaration",  // param2
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_type_param_decl(u8"param1"_sv),
                                  func_type_param_decl(u8"param2"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, abstract_constructor_function) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"abstract new () => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       abstract_constructor_function_requires_new_keyword) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"abstract () => ReturnType"_sv,  //
        u8"         ` Diag_Missing_New_In_Abstract_Constructor_Type.expected_new"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(
    Test_Parse_TypeScript_Type,
    newline_between_abstract_and_new_in_constructor_function_does_not_trigger_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"abstract\nnew () => ReturnType"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(
    Test_Parse_TypeScript_Type,
    newline_after_abstract_without_new_in_constructor_function_triggers_asi) {
  {
    Spy_Visitor p =
        test_parse_and_visit_module(u8"type T = abstract\n() => ReturnType;"_sv,
                                    no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // T
                              "visit_enter_type_scope",           //
                              "visit_variable_type_use",          // abstract
                              "visit_exit_type_scope",            //
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  // =>
                              "visit_variable_use",               // ReturnType
                              "visit_exit_function_scope",        //
                              "visit_end_of_module",              //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"abstract"_sv, u8"ReturnType"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, array) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"T[]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"T[][][][][][]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(((T)[])[])"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, readonly_array) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly T[]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly T[][][]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"(readonly ((T)[])[])"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly typeof v[]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // v
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, indexed) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type['key']"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type[Key]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                              "visit_variable_type_use",  // Key
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type", u8"Key"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, mixed_array_and_indexed) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type[][K1][][K2]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type", u8"K1", u8"K2"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, union_of_types) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 | Type2"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1", u8"Type2"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 | Type2 | Type3 | Type4"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Type1", u8"Type2", u8"Type3", u8"Type4"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"| Type1"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, union_disallows_consecutive_pipes) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"| | Type"_sv,  //
        u8"^ Diag_Missing_Type_Between_Intersection_Or_Union.left_operator\n"_diag
        u8"  ^ .right_operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 | | Type2"_sv,  //
        u8"      ^ Diag_Missing_Type_Between_Intersection_Or_Union.left_operator\n"_diag
        u8"        ^ .right_operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, intersection) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 & Type2"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1", u8"Type2"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 & Type2 & Type3 & Type4"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Type1", u8"Type2", u8"Type3", u8"Type4"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"& Type1"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       intersection_disallows_consecutive_ampersands) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"& & Type"_sv,  //
        u8"^ Diag_Missing_Type_Between_Intersection_Or_Union.left_operator\n"_diag
        u8"  ^ .right_operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Type1 & & Type2"_sv,  //
        u8"      ^ Diag_Missing_Type_Between_Intersection_Or_Union.left_operator\n"_diag
        u8"        ^ .right_operator"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, typeof) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof thing"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // thing
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"thing"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof Class.staticProperty"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // Class
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Class"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof ns.Class.staticProperty"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // ns
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
  }

  for (String8 keyword : keywords - typescript_special_type_keywords -
                             strict_only_reserved_keywords -
                             Dirty_Set<String8>{
                                 u8"import",
                                 u8"this",
                             }) {
    {
      Padded_String code(u8"typeof " + keyword);
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (keyword)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({keyword}));
    }
  }

  for (String8 keyword : keywords) {
    {
      Padded_String code(u8"typeof ns." + keyword);
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // ns
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns"}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Type, typeof_generic) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof Class<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // Class
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Class", u8"T"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof ns.Class<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // ns
                              "visit_variable_type_use",  // T
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns", u8"T"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       typeof_generic_with_arrow_type_requires_space) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof ns.Class<<T>() => ReturnType>"_sv,
        u8"                ` Diag_TypeScript_Generic_Less_Less_Not_Split.expected_space"_diag
        u8"{.context=Statement_Kind::typeof_type}"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // ns
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, typeof_import) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof import('some-module')"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof import('some-module').exportedThing"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  test_parse_and_visit_typescript_type_expression(
      u8"typeof import('mymod', {assert: {'resolution-mode': 'require'}}).MyClass"_sv,
      no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Type, typeof_this) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof this"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof this.myProperty"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

// As of 2022-06-29, this feature has been rolled back in TypeScript:
//
// https://github.com/microsoft/TypeScript/pull/48959
// https://github.com/microsoft/TypeScript/issues/47595
//
// We support it anyway.
TEST_F(Test_Parse_TypeScript_Type, typeof_allows_private_properties) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof Class.#myProperty"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // Class
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof this.#myProperty"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof import('mod').Class.#myProperty"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Type, typeof_generic_does_not_allow_dots_after) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof Class<T>.member"_sv,  //
        u8"               ^ Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type.dot\n"_diag
        u8"                ^^^^^^ .property_name"_diag,  //
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Class", u8"T"}));
  }

  for (String8 keyword : keywords) {
    test_parse_and_visit_typescript_type_expression(
        concat(u8"typeof Class<T>."_sv, keyword),
        u8"Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type"_diag,
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Type, typeof_allows_array_and_indexed) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof ns.subns.thingy[KeyType]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // ns
                              "visit_variable_type_use",  // KeyType
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns", u8"KeyType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"typeof somevar[]"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // somevar
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"somevar"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, imported_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"import('mymod')"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"import('mymod').MyClass"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"import('mymod').mynamespace.MyClass"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"import('mymod').MyClass<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // T
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"import('mymod').MyClass<<T>() => ReturnType>"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",   //
                          }));
  }

  test_parse_and_visit_typescript_type_expression(
      u8"import('mymod').MyClass<T>.prop"_sv,  //
      u8"                           ^^^^ Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type.property_name\n"_diag
      u8"                          ^ .dot"_diag,
      typescript_options);

  test_parse_and_visit_typescript_type_expression(
      u8"import('mymod', {assert: {'resolution-mode': 'require'}}).MyClass"_sv,
      no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Type, keyof) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"keyof Type"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, extends_condition) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"Derived extends Base ? TrueType : FalseType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // Derived
                              "visit_variable_type_use",             // Base
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_type_use",             // TrueType
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",  // FalseType
                          }));
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"Derived", u8"Base", u8"TrueType", u8"FalseType"}));
  }

  {
    Test_Parser p(
        u8"Derived[DK] extends Base[BK] ? TrueType[TK] : FalseType[FK]"_sv,
        typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"Derived", u8"DK", u8"Base", u8"BK", u8"TrueType",
                          u8"TK", u8"FalseType", u8"FK"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"T extends (param: ParamType) => ReturnType ? TrueType : FalseType"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",     // T
                              "visit_enter_function_scope",  //
                              "visit_enter_type_scope",      // :
                              "visit_variable_type_use",     // ParamType
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",   //
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_type_use",             // TrueType
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",  // FalseType
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, conditional_type_with_infer) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends infer T ? TrueType : FalseType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // MyType
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_declaration",          // T
                              "visit_variable_type_use",             // TrueType
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",  // FalseType
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"MyType", u8"TrueType", u8"FalseType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends infer T | U ? true : false"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyType", u8"U"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends (infer T)[] ? true : false"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyType"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }

  {
    Test_Parser p(
        u8"MyType extends [infer A, infer B, infer C] ? true : false"_sv,
        typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // MyType
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_declaration",          // A
                              "visit_variable_declaration",          // B
                              "visit_variable_declaration",          // C
                              "visit_exit_conditional_type_scope",   //
                          }));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({infer_type_decl(u8"A"), infer_type_decl(u8"B"),
                          infer_type_decl(u8"C")}));
  }

  {
    Test_Parser p(
        u8"MyType extends (OtherType extends infer T ? infer U : InnerFalse) ? OuterTrue : OuterFalse"_sv,
        typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // MyType
                              "visit_variable_type_use",  // OtherType
                              "visit_enter_conditional_type_scope",  // (inner)
                              "visit_variable_declaration",          // T
                              "visit_exit_conditional_type_scope",   // (inner)
                              "visit_variable_type_use",  // InnerFalse
                              "visit_enter_conditional_type_scope",  // (inner)
                              "visit_variable_declaration",          // U
                              "visit_variable_type_use",            // OuterTrue
                              "visit_exit_conditional_type_scope",  // (inner)
                              "visit_variable_type_use",  // OuterFalse
                          }));
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"MyType"_sv, u8"OtherType"_sv, u8"InnerFalse"_sv,
                          u8"OuterTrue"_sv, u8"OuterFalse"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {infer_type_decl(u8"T"_sv), infer_type_decl(u8"U"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, infer_constraint) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends infer T extends U ? true : false"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // MyType
                              "visit_variable_type_use",             // U
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_declaration",          // T
                              "visit_exit_conditional_type_scope",   //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyType"_sv, u8"U"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends (infer T extends U) ? true : false"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends (infer T extends U extends Q ? true : false) ? true : false"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       extends_after_infer_might_not_be_infer_constraint) {
  {
    // In this code, the inner 'extends' is a conditional type not an 'infer ...
    // extends ...'. In other words, this code should be parsed like the
    // following:
    //
    //   MyType extends ((infer T) extends U ? T1 : F1) ? T2 : F2
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"MyType extends (infer T extends U ? T1 : F1) ? T2 : F2"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // MyType
                              // (
                              "visit_variable_type_use",             // U
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_type_use",             // T1
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",             // F1
                              // )
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_declaration",          // T
                              "visit_variable_type_use",             // T2
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",             // F2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyType", u8"U", u8"T1",
                                                   u8"F1", u8"T2", u8"F2"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(u8"T")}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, keyof_in_extends_is_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"T extends keyof O ? TrueType : FalseType"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // T
                              "visit_variable_type_use",             // O
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_type_use",             // TrueType
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",  // FalseType
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray(
                    {u8"T"_sv, u8"O"_sv, u8"TrueType"_sv, u8"FalseType"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, infer_allows_certain_contextual_type_names) {
  for (String8_View keyword :
       (contextual_keywords - typescript_builtin_type_keywords -
        typescript_special_type_keywords -
        Dirty_Set<String8>{
            u8"let",
            u8"static",
            u8"yield",
        }) |
           Dirty_Set<String8>{
               // TODO(strager): Put 'async' in contextual_keywords.
               u8"async",
               // TypeScript allows 'infer undefined'.
               u8"undefined",
           }) {
    Padded_String code(concat(u8"MyType extends infer "_sv, keyword,
                              u8" ? TrueType : FalseType"_sv));
    SCOPED_TRACE(code);
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        code.string_view(), no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({infer_type_decl(keyword)}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, infer_outside_conditional_type) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"infer T"_sv,                                                 //
        u8"^^^^^ Diag_TypeScript_Infer_Outside_Conditional_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty())
        << "'infer T' should not declare or use 'T'";
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"infer T extends U"_sv,                                       //
        u8"^^^^^ Diag_TypeScript_Infer_Outside_Conditional_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // U
                          }))
        << "'infer T' should not declare or use 'T'";
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Type, conditional_type_with_invalid_infer) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"A extends infer T[] ? B : C"_sv,  //
        u8"          ^^^^^^^ Diag_TypeScript_Infer_Requires_Parentheses.infer_and_type\n"_diag
        u8"                ^ .type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",             // A
                              "visit_enter_conditional_type_scope",  //
                              "visit_variable_declaration",          // T
                              "visit_variable_type_use",             // B
                              "visit_exit_conditional_type_scope",   //
                              "visit_variable_type_use",             // C
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, missing) {
  // TODO(strager): Point to the ':' if there was one.

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8" "_sv,                                  //
        u8" ` Diag_Missing_TypeScript_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Example: const f = (param: ) => {};
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8" )"_sv,                                 //
        u8" ` Diag_Missing_TypeScript_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Example: interface I { myMethod(): }
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8" }"_sv,                                 //
        u8" ` Diag_Missing_TypeScript_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    // Example: function f(param1: , param2: T2) {}
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8" ,"_sv,                                 //
        u8" ` Diag_Missing_TypeScript_Type"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Type, doesnt_warn_in_javascript_code) {
  // When parsing a type in JavaScript code, we already reported elsewhere that
  // types are not supported. Therefore, we should not complain about things
  // like type annotations inside a type.

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ prop: MyType }"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // MyType
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, readonly_requires_tuple_or_array_type) {
  // In these cases, we recommend adding '[]' to the end of the type.
  for (String8_View code : {
           u8"readonly Type"_sv,
           u8"readonly typeof Type"_sv,
           u8"readonly ns.Type<T>"_sv,
           u8"readonly 42"_sv,
           u8"readonly 'hello'"_sv,
           u8"readonly ('hello' | 'world')"_sv,
           u8"readonly `hello${world}`"_sv,
           u8"readonly {key: Value}"_sv,
           u8"readonly Type[Key]"_sv,
           u8"readonly typeof ns.varname[Key]"_sv
           u8"readonly () => ReturnType[]"_sv,
           u8"readonly new () => ReturnType[]"_sv,
           u8"readonly <T>() => ReturnType[]"_sv,
           u8"readonly unique symbol"_sv,
           u8"readonly keyof T"_sv,
           u8"readonly keyof T[]"_sv,

           // TODO(strager): We should recommend removing the parentheses
           // instead.
           u8"readonly (T[])"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^^^^^^^ Diag_TypeScript_Readonly_In_Type_Needs_Array_Or_Tuple_Type"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Type, mixed) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"readonly A[] | readonly B[]"_sv, no_diags, javascript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // A
                              "visit_variable_type_use",  // B
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }
}

TEST_F(Test_Parse_TypeScript_Type,
       newline_is_not_allowed_before_index_or_array_operator_in_type) {
  {
    test_parse_and_visit_module(
        u8"interface I {\n"_sv
        u8"f(): C\n"_sv  // ASI
        u8"[v](): C;\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '[':
    Spy_Visitor v = test_parse_and_visit_module(
        u8"type A = C\n[].forEach(f);"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // C
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // f
                              "visit_end_of_module",         //
                          }));
  }

  {
    // ASI should insert a semicolon between 'C' and '[':
    Spy_Visitor v = test_parse_and_visit_module(u8"type A = C\n[T];"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // C
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // T
                              "visit_end_of_module",         //
                          }));
  }

  {
    // ASI should insert a semicolon between 'C' and '[':
    Spy_Visitor v = test_parse_and_visit_module(u8"type A = typeof C\n[T];"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_variable_use",          // C
                              "visit_exit_type_scope",       //
                              "visit_variable_use",          // T
                              "visit_end_of_module",         //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, newline_is_allowed_before_tuple_type) {
  {
    Spy_Visitor v = test_parse_and_visit_module(u8"type A =\n[T];"_sv, no_diags,
                                                typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // T
                              "visit_exit_type_scope",       //
                              "visit_end_of_module",         //
                          }));
  }

  {
    Spy_Visitor v = test_parse_and_visit_module(u8"type A = readonly\n[T];"_sv,
                                                no_diags, typescript_options);
    EXPECT_THAT(v.visits, ElementsAreArray({
                              "visit_variable_declaration",  // A
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // T
                              "visit_exit_type_scope",       //
                              "visit_end_of_module",         //
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Type, newline_is_not_allowed_before_extends) {
  {
    // ASI should insert a semicolon between 'number' and 'extends':
    test_parse_and_visit_module(
        u8"interface I {\n"_sv
        u8"  f(): number\n"_sv  // ASI
        u8"  extends: string;\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
  }

  // TypeScript triggers ASI, but we shouldn't because statements cannot start
  // with 'extends'.
  test_parse_and_visit_module(
      u8"type A = 42\n extends number ? T : F;"_sv,
      u8"              ^^^^^^^ Diag_Newline_Not_Allowed_Before_Extends_In_Type.extends_keyword"_diag,
      typescript_options);
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
