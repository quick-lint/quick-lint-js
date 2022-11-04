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

namespace quick_lint_js {
namespace {
class test_parse_typescript_type : public test_parse_expression {};

TEST_F(test_parse_typescript_type, direct_type_reference) {
  {
    test_parser p(u8"Type"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }
}

TEST_F(test_parse_typescript_type, direct_type_reference_with_keyword_name) {
  for (string8 keyword :
       contextual_keywords - typescript_builtin_type_keywords -
           typescript_special_type_keywords -
           dirty_set<string8>{
               // NOTE(strager): keyof is omitted on purpose because of
               // ambiguities in the grammar:
               // https://github.com/microsoft/TypeScript/issues/49724
               u8"keyof",
               u8"let",
               // NOTE(strager): readonly is omitted on purpose because
               // TypeScript complains about it, even though there is no
               // ambiguity in this case.
               u8"readonly",
               u8"static",
               // NOTE(strager): unique is omitted on purpose because of
               // ambiguities in the grammar.
               u8"unique",
           }) {
    {
      padded_string code(keyword);
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_type_use"));  // (keyword)
      EXPECT_THAT(p.variable_uses, ElementsAre(keyword));
    }

    {
      padded_string code(u8"[" + keyword + u8"]");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits,
                  ElementsAre("visit_variable_type_use"));  // (keyword)
      EXPECT_THAT(p.variable_uses, ElementsAre(keyword));
    }
  }
}

TEST_F(test_parse_typescript_type, direct_generic_type_reference) {
  {
    test_parser p(u8"Type<T>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type
                                      "visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type", u8"T"));
  }

  {
    test_parser p(u8"C<'hello', number, Banana>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // C
                                      "visit_variable_type_use"));  // Banana
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C", u8"Banana"));
  }

  {
    test_parser p(u8"ns.C<T>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_namespace_use",  // ns
                                      "visit_variable_type_use"));     // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns", u8"T"));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    test_parser p(u8"C<<T>() => ReturnType>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",     // C
                                      "visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"C", u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T")));
  }

  {
    SCOPED_TRACE("'>>' should be split into two tokens");
    test_parser p(u8"A<B<C>>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use",    // B
                                      "visit_variable_type_use"));  // C
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
  }

  {
    SCOPED_TRACE("'>>>' should be split into three tokens");
    test_parser p(u8"A<B<C<D>>>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use",    // B
                                      "visit_variable_type_use",    // C
                                      "visit_variable_type_use"));  // D
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C", u8"D"));
  }
}

TEST_F(test_parse_typescript_type, namespaced_type_reference) {
  {
    test_parser p(u8"ns.Type"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_namespace_use"));  // ns
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns"));
  }

  {
    test_parser p(u8"ns.subns.subsubns.Type[ns2.K]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_namespace_use",    // ns
                                      "visit_variable_namespace_use"));  // ns2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns", u8"ns2"));
  }

  for (string8 keyword : keywords) {
    padded_string code(u8"mymodule." + keyword);
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_variable_namespace_use"));  // mymodule
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"mymodule"));
  }
}

TEST_F(test_parse_typescript_type, builtin_types) {
  for (string8 type : typescript_builtin_type_keywords) {
    SCOPED_TRACE(out_string8(type));
    test_parser p(type, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "builtin type should not be treated as a variable";
  }
}

TEST_F(test_parse_typescript_type, special_types) {
  for (string8 type : typescript_special_type_keywords) {
    SCOPED_TRACE(out_string8(type));
    test_parser p(type, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "special type should not be treated as a variable";
  }
}

TEST_F(test_parse_typescript_type, unique_symbol_type) {
  {
    test_parser p(u8"unique symbol", typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "'unique symbol' should not be treated as a variable";
  }

  {
    test_parser p(u8"(unique symbol)", typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty())
        << "'unique symbol' should not be treated as a variable";
  }
}

TEST_F(test_parse_typescript_type, this_type) {
  {
    test_parser p(u8"this"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }
}

TEST_F(test_parse_typescript_type, literal_type) {
  for (string8_view code : {
           u8"42"_sv,
           u8"'hello'"_sv,
           u8"null"_sv,
           u8"true"_sv,
           u8"false"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }
}

TEST_F(test_parse_typescript_type, template_literal_type) {
  {
    test_parser p(u8"`hello`"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"`hello${other}`"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // other
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"other"));
  }

  {
    test_parser p(u8"`hello${other}${another}`"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // other
                                      "visit_variable_type_use"));  // another
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"other", u8"another"));
  }
}

TEST_F(test_parse_typescript_type, parenthesized_type) {
  {
    test_parser p(u8"(Type)"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }

  {
    test_parser p(u8"(((((Type)))))"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }

  {
    test_parser p(u8"(number)"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(test_parse_typescript_type, tuple_type) {
  {
    test_parser p(u8"[]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    test_parser p(u8"[A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[A, B, C]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
  }

  {
    test_parser p(u8"[A, B, C, ]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
  }
}

TEST_F(test_parse_typescript_type, readonly_tuple_type) {
  {
    test_parser p(u8"readonly []"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.variable_uses, IsEmpty());
  }

  {
    test_parser p(u8"readonly [A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"readonly [A, B, C]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
  }
}

TEST_F(test_parse_typescript_type, tuple_type_optional_unnamed_element) {
  {
    test_parser p(u8"[A?]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[A, B?]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[A?, B?]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[A?, B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_required_tuple_element_after_optional_element,
            expected_question, strlen(u8"[A?, B"), u8"",  //
            previous_optional_question, strlen(u8"[A"), u8"?")));
  }

  {
    test_parser p(u8"[A?, B?, C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_required_tuple_element_after_optional_element,
            expected_question, strlen(u8"[A?, B?, C"), u8"",  //
            previous_optional_question, strlen(u8"[A?, B"), u8"?")))
        << "diagnostic should point to the last optional '?'";
  }

  {
    test_parser p(u8"[A?, B, C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_required_tuple_element_after_optional_element,
                expected_question, strlen(u8"[A?, B"), u8"",  //
                previous_optional_question, strlen(u8"[A"), u8"?"),
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_required_tuple_element_after_optional_element,
                expected_question, strlen(u8"[A?, B, C"), u8"",  //
                previous_optional_question, strlen(u8"[A"), u8"?")));
  }
}

TEST_F(test_parse_typescript_type, tuple_type_unnamed_spread_element) {
  {
    test_parser p(u8"[...A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[A, ...B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[...A, B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[A, ...B, C]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
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
TEST_F(test_parse_typescript_type,
       tuple_type_can_only_have_one_array_spread_sorta) {
  {
    test_parser p(u8"[...A, ...B[]]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    // TODO(#867): Assert a diagnostic.
  }

  {
    test_parser p(u8"[...A[], ...B[]]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    // TODO(#867): Assert a diagnostic.
  }

  {
    test_parser p(u8"[...A[], ...B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors, IsEmpty())
        << "TypeScript's compiler only reports an error if the non-first "
           "spread is syntactically an array type";
  }

  {
    test_parser p(u8"[...A, ...B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors, IsEmpty())
        << "TypeScript's compiler only reports an error if the non-first "
           "spread is syntactically an array type";
  }

  {
    test_parser p(u8"[...A[], ...B[], ...C[]]"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    // TODO(#867): Assert a diagnostic.
  }
}

TEST_F(test_parse_typescript_type,
       tuple_type_unnamed_spread_element_with_optional_unnamed_element) {
  // Rest element can follow optional element.
  {
    test_parser p(u8"[A?, ...B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  // Optional element cannot follow rest element.
  {
    test_parser p(u8"[...A, B?]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_optional_tuple_element_cannot_follow_spread_element,
            optional_question, strlen(u8"[...A, B"), u8"?",  //
            previous_spread, strlen(u8"["), u8"...")));
  }

  {
    test_parser p(u8"[...A?, B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_typescript_spread_element_cannot_be_optional,
                    optional_question, strlen(u8"[...A"), u8"?",  //
                    spread, strlen(u8"["), u8"...")));
  }
}

TEST_F(test_parse_typescript_type, named_tuple_type) {
  {
    test_parser p(u8"[a: A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[a: A, b: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[a: A, b: B, ]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  for (const string8& name :
       (keywords - disallowed_binding_identifier_keywords) | dirty_set<string8>{
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
    test_parser p(concat(u8"[", name, u8": A]"), typescript_options);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }
}

TEST_F(test_parse_typescript_type, named_tuple_type_with_missing_name) {
  {
    test_parser p(u8"[a: A, B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_typescript_missing_name_and_colon_in_named_tuple_type,
            expected_name_and_colon, strlen(u8"[a: A, "), u8"",  //
            existing_name, strlen(u8"["), u8"a:")));
  }

  {
    test_parser p(u8"[a: A, b: B, C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_typescript_missing_name_and_colon_in_named_tuple_type,
            expected_name_and_colon, strlen(u8"[a: A, b: B, "), u8"",  //
            existing_name, strlen(u8"["), u8"a:")));
  }

  {
    test_parser p(u8"[A, b: B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_typescript_missing_name_and_colon_in_named_tuple_type,
            expected_name_and_colon, strlen(u8"["), u8"",  //
            existing_name, strlen(u8"[A, "), u8"b:")));
  }

  {
    test_parser p(u8"[: A, b: B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_typescript_missing_name_in_named_tuple_type,
                    colon, strlen(u8"["), u8":")));
  }

  {
    test_parser p(u8"[: A, B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_typescript_missing_name_in_named_tuple_type,
                    colon, strlen(u8"["), u8":")))
        << "should not also report a missing name for the second element, "
           "because maybe the ':' was a mistake";
  }

  {
    test_parser p(u8"[: A, b: B, C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_typescript_missing_name_in_named_tuple_type,
                              colon, strlen(u8"["), u8":"),
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_missing_name_and_colon_in_named_tuple_type,
                expected_name_and_colon, strlen(u8"[: A, b: B, "), u8"",  //
                existing_name, strlen(u8"[: A, "), u8"b:")));
  }

  {
    test_parser p(u8"[: A, B, c: C]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_typescript_missing_name_in_named_tuple_type,
                              colon, strlen(u8"["), u8":"),
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_typescript_missing_name_and_colon_in_named_tuple_type,
                expected_name_and_colon, strlen(u8"[: A, "), u8"",  //
                existing_name, strlen(u8"[: A, B, "), u8"c:")));
  }
}

TEST_F(test_parse_typescript_type, tuple_type_optional_named_element) {
  {
    test_parser p(u8"[a?: A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[a: A, b?: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[a?: A, b?: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[a?: A, b : B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_required_tuple_element_after_optional_element,
            expected_question, strlen(u8"[a?: A, b"), u8"",  //
            previous_optional_question, strlen(u8"[a"), u8"?")));
  }

  {
    test_parser p(u8"[a?: A, b?: B, c : C]"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_required_tuple_element_after_optional_element,
            expected_question, strlen(u8"[a?: A, b?: B, c"), u8"",  //
            previous_optional_question, strlen(u8"[a?: A, b"), u8"?")))
        << "diagnostic should point to the last optional '?'";
  }
}

TEST_F(test_parse_typescript_type,
       tuple_type_optional_named_element_cannot_have_question_after_type) {
  {
    test_parser p(u8"[a: A?]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_typescript_named_tuple_element_question_after_type,
            question, strlen(u8"[a: A"), u8"?",  //
            expected_question, strlen(u8"[a"), u8"")));
  }

  {
    test_parser p(u8"[a?: A?]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_named_tuple_element_question_after_name_and_type,
            type_question, strlen(u8"[a?: A"), u8"?",  //
            name_question, strlen(u8"[a"), u8"?")));
  }
}

TEST_F(test_parse_typescript_type, tuple_type_named_spread_element) {
  {
    test_parser p(u8"[...a: A]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
  }

  {
    test_parser p(u8"[a: A, ...b: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[...a: A, b: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  {
    test_parser p(u8"[a: A, ...b: B, c: C]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B", u8"C"));
  }
}

TEST_F(test_parse_typescript_type,
       tuple_type_named_spread_element_with_optional_named_element) {
  // Rest element can follow optional element.
  {
    test_parser p(u8"[a?: A, ...b: B]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
  }

  // Optional element cannot follow rest element.
  {
    test_parser p(u8"[...a: A, b?: B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_optional_tuple_element_cannot_follow_spread_element,
            optional_question, strlen(u8"[...a: A, b"), u8"?",  //
            previous_spread, strlen(u8"["), u8"...")));
  }

  {
    test_parser p(u8"[...a?: A, b: B]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_typescript_spread_element_cannot_be_optional,
                    optional_question, strlen(u8"[...a"), u8"?",  //
                    spread, strlen(u8"["), u8"...")));
  }
}

TEST_F(test_parse_typescript_type,
       tuple_type_spread_named_element_cannot_have_dot_dot_dot_before_type) {
  {
    test_parser p(u8"[ a: ...A ]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_typescript_named_tuple_element_spread_before_type,
            spread, strlen(u8"[ a: "), u8"...",  //
            expected_spread, strlen(u8"[ "), u8"")));
  }

  {
    test_parser p(u8"[...a: ...A]"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // A
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code,
            diag_typescript_named_tuple_element_spread_before_name_and_type,
            type_spread, strlen(u8"[...a: "), u8"...",  //
            name_spread, strlen(u8"["), u8"...")));
  }
}

TEST_F(test_parse_typescript_type, empty_object_type) {
  test_parser p(u8"{}"_sv, typescript_options);
  p.parse_and_visit_typescript_type_expression();
  EXPECT_THAT(p.visits, IsEmpty());
  EXPECT_THAT(p.variable_uses, IsEmpty());
}

TEST_F(test_parse_typescript_type, object_type_with_basic_properties) {
  {
    test_parser p(u8"{ untypedProperty }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"{ property: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }

  {
    test_parser p(u8"{ property: Type, }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
  }

  {
    test_parser p(u8"{ property: Type; }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
  }

  {
    test_parser p(u8"{ p1: Type1, p2: Type2 }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1", u8"Type2"));
  }

  {
    test_parser p(u8"{ p1: Type1; p2: Type2 }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1", u8"Type2"));
  }
}

TEST_F(test_parse_typescript_type, object_type_allows_asi_between_properties) {
  {
    test_parser p(u8"{\n  p1: Type1\n  p2: Type2\n}"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1", u8"Type2"));
  }
}

TEST_F(test_parse_typescript_type,
       object_type_requires_separator_between_properties) {
  {
    test_parser p(u8"{ p1: Type1 p2: Type2 }"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    p.code, diag_missing_separator_between_object_type_entries,
                    expected_separator, strlen(u8"{ p1: Type1"), u8"")));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_readonly_properties) {
  {
    test_parser p(u8"{ readonly untypedProperty }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"{ readonly property: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_optional_properties) {
  {
    test_parser p(u8"{ untypedProperty? }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"{ property?: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }

  {
    test_parser p(u8"{ method?(): Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   // method
                                      "visit_variable_type_use",      // Type
                                      "visit_exit_function_scope"));  // method
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_method) {
  {
    test_parser p(u8"{ method() }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   // method
                                      "visit_exit_function_scope"));  // method
  }

  {
    test_parser p(u8"{ method(param: Type) }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   // method
                                      "visit_variable_type_use",      // Type
                                      "visit_variable_declaration",   // param
                                      "visit_exit_function_scope"));  // method
  }

  {
    test_parser p(u8"{ method(): ReturnType }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  // method
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));  // method
  }
}

TEST_F(test_parse_typescript_type, object_type_with_generic_method) {
  {
    test_parser p(u8"{ method<T>() }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   // method
                                      "visit_variable_declaration",   // T
                                      "visit_exit_function_scope"));  // method
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T")));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_getter) {
  {
    test_parser p(u8"{ get prop() }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",   // get prop
                            "visit_exit_function_scope"));  // get prop
  }

  {
    test_parser p(u8"{ get prop(): ReturnType }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",   // get prop
                            "visit_variable_type_use",      // ReturnType
                            "visit_exit_function_scope"));  // get prop
  }
}

TEST_F(test_parse_typescript_type, object_type_with_setter) {
  {
    test_parser p(u8"{ set prop(v) }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",   // set prop
                            "visit_variable_declaration",   // v
                            "visit_exit_function_scope"));  // set prop
  }

  {
    test_parser p(u8"{ set prop(value: Type) }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits,
                ElementsAre("visit_enter_function_scope",   // set prop
                            "visit_variable_type_use",      // Type
                            "visit_variable_declaration",   // value
                            "visit_exit_function_scope"));  // set prop
  }
}

TEST_F(test_parse_typescript_type, object_type_with_computed_property) {
  {
    test_parser p(u8"{ ['prop'] }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"{ ['prop']: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
  }

  {
    test_parser p(u8"{ ['method']() }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",   // method
                                      "visit_exit_function_scope"));  // method
  }

  {
    test_parser p(u8"{ [varName]: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // varName
                                      "visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"varName", u8"Type"));
  }

  {
    test_parser p(u8"{ [ns.varName]: Type }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // ns
                                      "visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns", u8"Type"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_index_signature) {
  {
    test_parser p(u8"{ [key: KeyType]: PropType }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                      "visit_variable_type_use",     // KeyType
                                      "visit_variable_declaration",  // key
                                      "visit_variable_type_use",     // PropType
                                      "visit_exit_index_signature_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(index_signature_param_decl(u8"key")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"KeyType", u8"PropType"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_mapped_types) {
  {
    test_parser p(u8"{ [Key in Keys]: PropType }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                      "visit_variable_type_use",     // Keys
                                      "visit_variable_declaration",  // Key
                                      "visit_variable_type_use",     // PropType
                                      "visit_exit_index_signature_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"Key")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Keys", u8"PropType"));
  }

  {
    test_parser p(u8"{ [Key in Keys as KeyType]: PropType }"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                      "visit_variable_type_use",     // Keys
                                      "visit_variable_declaration",  // Key
                                      "visit_variable_type_use",     // KeyType
                                      "visit_variable_type_use",     // PropType
                                      "visit_exit_index_signature_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"Key")));
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"Keys", u8"KeyType", u8"PropType"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_modified_optional) {
  for (string8 modifier : {u8"-?", u8"+?", u8"?"}) {
    {
      padded_string code(u8"{ [key: KeyType]" + modifier + u8": PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",  // KeyType
                                        "visit_variable_declaration",  // key
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }

    {
      padded_string code(u8"{ [Key in Keys]" + modifier + u8": PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",     // Keys
                                        "visit_variable_declaration",  // Key
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }

    {
      padded_string code(u8"{ [Key in Keys as KeyType]" + modifier +
                         u8": PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",     // Keys
                                        "visit_variable_declaration",  // Key
                                        "visit_variable_type_use",  // KeyType
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }
  }
}

TEST_F(test_parse_typescript_type, object_type_with_modified_readonly) {
  for (string8 modifier : {u8"-readonly", u8"+readonly", u8"readonly"}) {
    {
      padded_string code(u8"{ " + modifier + u8" [key: KeyType]: PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",  // KeyType
                                        "visit_variable_declaration",  // key
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }

    {
      padded_string code(u8"{ " + modifier + u8" [Key in Keys]: PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",     // Keys
                                        "visit_variable_declaration",  // Key
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }

    {
      padded_string code(u8"{ " + modifier +
                         u8" [Key in Keys as KeyType]: PropType }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_index_signature_scope",  //
                                        "visit_variable_type_use",     // Keys
                                        "visit_variable_declaration",  // Key
                                        "visit_variable_type_use",  // KeyType
                                        "visit_variable_type_use",  // PropType
                                        "visit_exit_index_signature_scope"));
    }
  }
}

TEST_F(test_parse_typescript_type, object_type_with_call_signature) {
  {
    test_parser p(u8"{ () }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_exit_function_scope"));
  }

  {
    test_parser p(u8"{ (param: ParamType): ReturnType }"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_param_decl(u8"param")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ParamType", u8"ReturnType"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_generic_call_signature) {
  {
    test_parser p(u8"{ <T>(param): ReturnType }"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(generic_param_decl(u8"T"), func_param_decl(u8"param")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
  }
}

TEST_F(test_parse_typescript_type, object_type_with_keyword_named_properties) {
  for (string8 keyword : keywords) {
    {
      padded_string code(u8"{ " + keyword + u8" }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, IsEmpty());
    }

    {
      padded_string code(u8"{ " + keyword + u8"() }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                        "visit_exit_function_scope"));
    }

    {
      padded_string code(u8"{ " + keyword + u8": Type }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    }

    {
      padded_string code(u8"{ readonly " + keyword + u8": Type }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    }

    {
      padded_string code(u8"{ " + keyword + u8"?: Type }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    }
  }
}

TEST_F(test_parse_typescript_type,
       object_type_with_contextual_keyword_named_index_key) {
  for (string8 keyword :
       contextual_keywords - dirty_set<string8>{u8"let", u8"static"}) {
    {
      padded_string code(u8"{ [" + keyword + u8": T]: T }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(index_signature_param_decl(keyword)));
    }

    {
      padded_string code(u8"{ [" + keyword + u8" in T]: T }");
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAre(generic_param_decl(keyword)));
    }
  }
}

TEST_F(test_parse_typescript_type, arrow_function) {
  {
    test_parser p(u8"() => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
  }

  {
    test_parser p(u8"(param) => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_type_param_decl(u8"param")));
  }

  {
    test_parser p(u8"(a, b, c,) => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // a
                                      "visit_variable_declaration",  // b
                                      "visit_variable_declaration",  // c
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(func_type_param_decl(u8"a"), func_type_param_decl(u8"b"),
                    func_type_param_decl(u8"c")));
  }

  {
    test_parser p(u8"(param: ParamType) => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ParamType", u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_type_param_decl(u8"param")));
  }

  {
    test_parser p(u8"([a, b, c]) => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // a
                                      "visit_variable_declaration",  // b
                                      "visit_variable_declaration",  // c
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAre(func_type_param_decl(u8"a"), func_type_param_decl(u8"b"),
                    func_type_param_decl(u8"c")));
  }

  {
    test_parser p(u8"({key: param}: {key: ParamType}) => ReturnType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamType
                                      "visit_variable_declaration",  // param
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ParamType", u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_type_param_decl(u8"param")));
  }

  {
    test_parser p(u8"(...params: ParamsType) => ReturnType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ParamsType
                                      "visit_variable_declaration",  // params
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ParamsType", u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_type_param_decl(u8"params")));
  }
}

TEST_F(test_parse_typescript_type, generic_arrow_function) {
  {
    test_parser p(u8"<T>() => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
  }

  {
    test_parser p(u8"new <T>() => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // T
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(generic_param_decl(u8"T")));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
  }
}

TEST_F(test_parse_typescript_type, constructor_function) {
  {
    test_parser p(u8"new () => ReturnType"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
  }

  {
    test_parser p(u8"new (param1, param2) => ReturnType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_function_scope",  //
                                      "visit_variable_declaration",  // param1
                                      "visit_variable_declaration",  // param2
                                      "visit_variable_type_use",  // ReturnType
                                      "visit_exit_function_scope"));
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ReturnType"));
    EXPECT_THAT(p.variable_declarations,
                ElementsAre(func_type_param_decl(u8"param1"),
                            func_type_param_decl(u8"param2")));
  }
}

TEST_F(test_parse_typescript_type, array) {
  {
    test_parser p(u8"T[]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser p(u8"T[][][][][][]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
  }

  {
    test_parser p(u8"(((T)[])[])"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
  }
}

TEST_F(test_parse_typescript_type, readonly_array) {
  {
    test_parser p(u8"readonly T[]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser p(u8"readonly T[][][]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"T"));
  }

  {
    test_parser p(u8"(readonly ((T)[])[])"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // T
  }

  {
    test_parser p(u8"readonly typeof v[]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // v
  }
}

TEST_F(test_parse_typescript_type, indexed) {
  {
    test_parser p(u8"Type['key']"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
  }

  {
    test_parser p(u8"Type[Key]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type
                                      "visit_variable_type_use"));  // Key
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type", u8"Key"));
  }
}

TEST_F(test_parse_typescript_type, mixed_array_and_indexed) {
  {
    test_parser p(u8"Type[][K1][][K2]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type", u8"K1", u8"K2"));
  }
}

TEST_F(test_parse_typescript_type, union_of_types) {
  {
    test_parser p(u8"Type1 | Type2"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1", u8"Type2"));
  }

  {
    test_parser p(u8"Type1 | Type2 | Type3 | Type4"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"Type1", u8"Type2", u8"Type3", u8"Type4"));
  }

  {
    test_parser p(u8"| Type1"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1"));
  }
}

TEST_F(test_parse_typescript_type, union_disallows_consecutive_pipes) {
  {
    test_parser p(u8"| | Type"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_type_between_intersection_or_union,
                    left_operator, strlen(u8""), u8"|",  //
                    right_operator, strlen(u8"| "), u8"|")));
  }

  {
    test_parser p(u8"Type1 | | Type2"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_type_between_intersection_or_union,
                    left_operator, strlen(u8"Type1 "), u8"|",  //
                    right_operator, strlen(u8"Type1 | "), u8"|")));
  }
}

TEST_F(test_parse_typescript_type, intersection) {
  {
    test_parser p(u8"Type1 & Type2"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1", u8"Type2"));
  }

  {
    test_parser p(u8"Type1 & Type2 & Type3 & Type4"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"Type1", u8"Type2", u8"Type3", u8"Type4"));
  }

  {
    test_parser p(u8"& Type1"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type1"));
  }
}

TEST_F(test_parse_typescript_type,
       intersection_disallows_consecutive_ampersands) {
  {
    test_parser p(u8"& & Type"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_type_between_intersection_or_union,
                    left_operator, strlen(u8""), u8"&",  //
                    right_operator, strlen(u8"& "), u8"&")));
  }

  {
    test_parser p(u8"Type1 & & Type2"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Type1
                                      "visit_variable_type_use"));  // Type2
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE_2_OFFSETS(
                    p.code, diag_missing_type_between_intersection_or_union,
                    left_operator, strlen(u8"Type1 "), u8"&",  //
                    right_operator, strlen(u8"Type1 & "), u8"&")));
  }
}

TEST_F(test_parse_typescript_type, typeof) {
  {
    test_parser p(u8"typeof thing"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // thing
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"thing"));
  }

  {
    test_parser p(u8"typeof Class.staticProperty"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // Class
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Class"));
  }

  {
    test_parser p(u8"typeof ns.Class.staticProperty"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // ns
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns"));
  }

  for (string8 keyword :
       keywords - typescript_special_type_keywords -
           strict_only_reserved_keywords -
           dirty_set<string8>{
               u8"this",
               // This list is derived experimentally from TypeScript version
               // 4.7.4. Some of these seem arbitrary. *shrug*
               u8"boolean",
               u8"import",
               u8"let",
               u8"number",
               u8"static",
               u8"string",
               u8"yield",
           }) {
    {
      padded_string code(u8"typeof " + keyword);
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // (keyword)
      EXPECT_THAT(p.variable_uses, ElementsAre(keyword));
    }
  }

  for (string8 keyword : keywords) {
    {
      padded_string code(u8"typeof ns." + keyword);
      SCOPED_TRACE(code);
      test_parser p(code.string_view(), typescript_options);
      p.parse_and_visit_typescript_type_expression();
      EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // ns
      EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns"));
    }
  }
}

TEST_F(test_parse_typescript_type, typeof_generic) {
  {
    test_parser p(u8"typeof Class<T>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // Class
                                      "visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Class", u8"T"));
  }

  {
    test_parser p(u8"typeof ns.Class<T>"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // ns
                                      "visit_variable_type_use"));  // T
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns", u8"T"));
  }
}

TEST_F(test_parse_typescript_type, typeof_import) {
  {
    test_parser p(u8"typeof import('some-module')"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"typeof import('some-module').exportedThing"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(test_parse_typescript_type, typeof_this) {
  {
    test_parser p(u8"typeof this"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"typeof this.myProperty"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

// As of 2022-06-29, this feature has been rolled back in TypeScript:
//
// https://github.com/microsoft/TypeScript/pull/48959
// https://github.com/microsoft/TypeScript/issues/47595
//
// We support it anyway.
TEST_F(test_parse_typescript_type, typeof_allows_private_properties) {
  {
    test_parser p(u8"typeof Class.#myProperty"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // Class
  }

  {
    test_parser p(u8"typeof this.#myProperty"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    test_parser p(u8"typeof import('mod').Class.#myProperty"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(test_parse_typescript_type, typeof_generic_does_not_allow_dots_after) {
  {
    test_parser p(u8"typeof Class<T>.member"_sv, typescript_options,
                  capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Class", u8"T"));
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_2_OFFSETS(
            p.code, diag_dot_not_allowed_after_generic_arguments_in_type, dot,
            strlen(u8"typeof Class<T>"), u8".",  //
            property_name, strlen(u8"typeof Class<T>."), u8"member")));
  }

  for (string8 keyword : keywords) {
    test_parser p(concat(u8"typeof Class<T>.", keyword), typescript_options,
                  capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.errors,
                ElementsAre(DIAG_TYPE(
                    diag_dot_not_allowed_after_generic_arguments_in_type)));
  }
}

TEST_F(test_parse_typescript_type, typeof_allows_array_and_indexed) {
  {
    test_parser p(u8"typeof ns.subns.thingy[KeyType]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use",         // ns
                                      "visit_variable_type_use"));  // KeyType
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"ns", u8"KeyType"));
  }

  {
    test_parser p(u8"typeof somevar[]"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_use"));  // somevar
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"somevar"));
  }
}

TEST_F(test_parse_typescript_type, keyof) {
  {
    test_parser p(u8"keyof Type"_sv, typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // Type
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Type"));
  }
}

TEST_F(test_parse_typescript_type, extends_condition) {
  {
    test_parser p(u8"Derived extends Base ? TrueType : FalseType"_sv,
                  typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // Derived
                                      "visit_variable_type_use",    // Base
                                      "visit_variable_type_use",    // TrueType
                                      "visit_variable_type_use"));  // FalseType
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"Derived", u8"Base",
                                             u8"TrueType", u8"FalseType"));
  }

  {
    test_parser p(
        u8"Derived[DK] extends Base[BK] ? TrueType[TK] : FalseType[FK]"_sv,
        typescript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.variable_uses,
                ElementsAre(u8"Derived", u8"DK", u8"Base", u8"BK", u8"TrueType",
                            u8"TK", u8"FalseType", u8"FK"));
  }
}

TEST_F(test_parse_typescript_type, missing) {
  // TODO(strager): Point to the ':' if there was one.

  {
    test_parser p(u8" "_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_typescript_type,  //
                              expected_type, strlen(u8" "), u8"")));
  }

  {
    // Example: const f = (param: ) => {};
    test_parser p(u8" )"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_typescript_type,  //
                              expected_type, strlen(u8" "), u8"")));
  }

  {
    // Example: interface I { myMethod(): }
    test_parser p(u8" }"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_typescript_type,  //
                              expected_type, strlen(u8" "), u8"")));
  }

  {
    // Example: function f(param1: , param2: T2) {}
    test_parser p(u8" ,"_sv, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, IsEmpty());
    EXPECT_THAT(p.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              p.code, diag_missing_typescript_type,  //
                              expected_type, strlen(u8" "), u8"")));
  }
}

TEST_F(test_parse_typescript_type, doesnt_warn_in_javascript_code) {
  // When parsing a type in JavaScript code, we already reported elsewhere that
  // types are not supported. Therefore, we should not complain about things
  // like type annotations inside a type.

  {
    test_parser p(u8"{ prop: MyType }"_sv, javascript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use"));  // MyType
  }
}

TEST_F(test_parse_typescript_type, readonly_requires_tuple_or_array_type) {
  // In these cases, we recommend adding '[]' to the end of the type.
  for (string8_view code : {
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
    test_parser p(code, typescript_options, capture_diags);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            p.code, diag_typescript_readonly_in_type_needs_array_or_tuple_type,
            readonly_keyword, 0, u8"readonly")));
  }
}

TEST_F(test_parse_typescript_type, mixed) {
  {
    test_parser p(u8"readonly A[] | readonly B[]"_sv, javascript_options);
    p.parse_and_visit_typescript_type_expression();
    EXPECT_THAT(p.visits, ElementsAre("visit_variable_type_use",    // A
                                      "visit_variable_type_use"));  // B
    EXPECT_THAT(p.variable_uses, ElementsAre(u8"A", u8"B"));
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
