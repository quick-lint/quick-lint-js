// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/gtest.h>

namespace quick_lint_js {
namespace {
Diagnostic_Assertion parse_or_fail(
    const Char8* specification,
    Source_Location caller = Source_Location::current()) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(specification);
  if (!da.ok()) {
    EXPECT_FALSE(da.error().empty())
        << "if parsing failed, there should be at least one error message";
    for (const std::string& s : da.error()) {
      ADD_FAILURE_AT_CALLER("") << "diagnostic_assertion::parse failed: " << s;
    }
  }
  return *da;
}

TEST(Test_Diagnostic_Assertion, parse_one_character_span) {
  Diagnostic_Assertion da = parse_or_fail(u8"^ Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 0);
  EXPECT_EQ(da.members[0].span_end_offset, 1);
}

TEST(Test_Diagnostic_Assertion, parse_one_character_span_at_nonzero) {
  Diagnostic_Assertion da = parse_or_fail(u8"     ^ Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 5);
  EXPECT_EQ(da.members[0].span_end_offset, 6);
}

TEST(Test_Diagnostic_Assertion, parse_multiple_character_span) {
  Diagnostic_Assertion da = parse_or_fail(u8"^^^^ Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 0);
  EXPECT_EQ(da.members[0].span_end_offset, 4);
}

TEST(Test_Diagnostic_Assertion, parse_unit_character_span) {
  Diagnostic_Assertion da = parse_or_fail(u8"` Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 0);
  EXPECT_EQ(da.members[0].span_end_offset, 0);
}

TEST(Test_Diagnostic_Assertion, parse_unit_character_span_at_nonzero) {
  Diagnostic_Assertion da = parse_or_fail(u8"    ` Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  EXPECT_EQ(da.members[0].span_begin_offset, 4);
  EXPECT_EQ(da.members[0].span_end_offset, 4);
}

TEST(Test_Diagnostic_Assertion, parse_spaces_between_caret_and_diag_type) {
  Diagnostic_Assertion da = parse_or_fail(u8"^     Diag_Unexpected_Token");
  EXPECT_EQ(da.type, Diag_Type::Diag_Unexpected_Token);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 0);
  EXPECT_EQ(da.members[0].span_end_offset, 1);
}

TEST(Test_Diagnostic_Assertion, invalid_diag_type_fails) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(u8"^ Diag_Does_Not_Exist");
  ASSERT_FALSE(da.ok());
  EXPECT_THAT(da.error(), ::testing::ElementsAreArray({
                              "invalid diagnostic type: 'Diag_Does_Not_Exist'",
                          }));
}

TEST(Test_Diagnostic_Assertion, trailing_whitespace_fails) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(u8"^ Diag_Unexpected_Token   ");
  ASSERT_FALSE(da.ok());
  EXPECT_THAT(da.error(), ::testing::ElementsAreArray({
                              "trailing whitespace is not allowed in _diag",
                          }));
}

TEST(Test_Diagnostic_Assertion, stray_invalid_character_fails) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(u8"^~ Diag_Unexpected_Token");
  ASSERT_FALSE(da.ok());
  EXPECT_THAT(da.error(), ::testing::ElementsAreArray({
                              "unexpected '~' in _diag",
                          }));
}

TEST(Test_Diagnostic_Assertion, diag_type_with_only_one_member_implicit) {
  Diagnostic_Assertion da = parse_or_fail(u8"^ Diag_Unexpected_Token");
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_STREQ(da.members[0].name, "token");
  EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
  EXPECT_EQ(da.members[0].offset, offsetof(Diag_Unexpected_Token, token));
}

TEST(Test_Diagnostic_Assertion, diag_type_with_only_one_member_explicit) {
  Diagnostic_Assertion da = parse_or_fail(u8"^ Diag_Unexpected_Token.token");
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_STREQ(da.members[0].name, "token");
  EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
  EXPECT_EQ(da.members[0].offset, offsetof(Diag_Unexpected_Token, token));
}

TEST(Test_Diagnostic_Assertion,
     diag_type_with_multiple_members_implicit_is_not_allowed) {
  {
    Result<Diagnostic_Assertion, std::vector<std::string>> da =
        Diagnostic_Assertion::parse(
            u8"^ Diag_Assignment_Before_Variable_Declaration");
    ASSERT_FALSE(da.ok());
    EXPECT_THAT(
        da.error(),
        ::testing::ElementsAreArray({
            "member required for Diag_Assignment_Before_Variable_Declaration; "
            "try .assignment or .declaration",
        }));
  }

  {
    Result<Diagnostic_Assertion, std::vector<std::string>> da =
        Diagnostic_Assertion::parse(u8"^ Diag_Assignment_To_Const_Variable");
    ASSERT_FALSE(da.ok());
    EXPECT_THAT(da.error(),
                ::testing::ElementsAreArray({
                    "member required for Diag_Assignment_To_Const_Variable; "
                    "try .declaration or .assignment",
                }))
        << ".var_kind should not be listed because it is not a "
           "Source_Code_Span member variable";
  }

  {
    Result<Diagnostic_Assertion, std::vector<std::string>> da =
        Diagnostic_Assertion::parse(
            u8"^ Diag_Expected_Parenthesis_Around_Do_While_Condition");
    ASSERT_FALSE(da.ok());
    EXPECT_THAT(
        da.error(),
        ::testing::ElementsAreArray({
            "member required for "
            "Diag_Expected_Parenthesis_Around_Do_While_Condition; try .where",
        }))
        << "error should occur even if there is only one Source_Code_Span "
           "member variable\n"
        << ".token should not be listed because it is not a Source_Code_Span "
           "member variable";
  }
}

TEST(Test_Diagnostic_Assertion, diag_type_with_multiple_members_explicit) {
  {
    Diagnostic_Assertion da = parse_or_fail(
        u8"^ Diag_Assignment_Before_Variable_Declaration.declaration");
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_STREQ(da.members[0].name, "declaration");
    EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
    EXPECT_EQ(
        da.members[0].offset,
        offsetof(Diag_Assignment_Before_Variable_Declaration, declaration));
  }

  {
    Diagnostic_Assertion da = parse_or_fail(
        u8"^ Diag_Assignment_Before_Variable_Declaration.assignment");
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_STREQ(da.members[0].name, "assignment");
    EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
    EXPECT_EQ(
        da.members[0].offset,
        offsetof(Diag_Assignment_Before_Variable_Declaration, assignment));
  }
}

TEST(Test_Diagnostic_Assertion, diag_type_with_char8_member_explicit) {
  {
    Diagnostic_Assertion da = parse_or_fail(
        u8"^ "
        u8"Diag_Expected_Parenthesis_Around_Do_While_Condition.where{.token="
        u8"x}");
    ASSERT_EQ(da.member_count(), 2);

    EXPECT_STREQ(da.members[0].name, "where");
    EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
    EXPECT_EQ(
        da.members[0].offset,
        offsetof(Diag_Expected_Parenthesis_Around_Do_While_Condition, where));
    EXPECT_EQ(da.members[0].span_begin_offset, 0);
    EXPECT_EQ(da.members[0].span_end_offset, 1);

    EXPECT_STREQ(da.members[1].name, "token");
    EXPECT_EQ(da.members[1].type, Diagnostic_Arg_Type::char8);
    EXPECT_EQ(
        da.members[1].offset,
        offsetof(Diag_Expected_Parenthesis_Around_Do_While_Condition, token));
    EXPECT_EQ(da.members[1].character, u8'x');
  }
}

TEST(Test_Diagnostic_Assertion, diag_type_with_string8_view_member_explicit) {
  {
    Diagnostic_Assertion da = parse_or_fail(
        u8"^ "
        u8"Diag_Integer_Literal_Will_Lose_Precision.characters{.rounded_val="
        u8"hello}");
    ASSERT_EQ(da.member_count(), 2);

    EXPECT_STREQ(da.members[0].name, "characters");
    EXPECT_EQ(da.members[0].type, Diagnostic_Arg_Type::source_code_span);
    EXPECT_EQ(da.members[0].offset,
              offsetof(Diag_Integer_Literal_Will_Lose_Precision, characters));
    EXPECT_EQ(da.members[0].span_begin_offset, 0);
    EXPECT_EQ(da.members[0].span_end_offset, 1);

    EXPECT_STREQ(da.members[1].name, "rounded_val");
    EXPECT_EQ(da.members[1].type, Diagnostic_Arg_Type::string8_view);
    EXPECT_EQ(da.members[1].offset,
              offsetof(Diag_Integer_Literal_Will_Lose_Precision, rounded_val));
    EXPECT_EQ(da.members[1].string, u8"hello"_sv);
  }
}

TEST(Test_Diagnostic_Assertion, adjust_with_no_escaped_characters) {
  Diagnostic_Assertion da = parse_or_fail(u8"  ^^ Diag_Unexpected_Token");
  da = da.adjusted_for_escaped_characters(u8"abcdef"_sv);
  ASSERT_EQ(da.member_count(), 1);
  EXPECT_EQ(da.members[0].span_begin_offset, 2);
  EXPECT_EQ(da.members[0].span_end_offset, 4);
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_single_byte_escaped_characters_after_span_does_nothing) {
  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"abcde\n"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 4);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"abcd\ng"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 4);
  }
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_single_byte_escaped_characters_before_span) {
  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\nbcdef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 1);
    EXPECT_EQ(da.members[0].span_end_offset, 3);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"   ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\ncdef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 4);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"   ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"a\ndef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 4);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"     ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\nc\nfgh"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"    ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\tcdefg"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"    ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\"cdefg"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"    ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\\cdefg"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_single_byte_escaped_characters_inside_span) {
  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"ab\nef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 3);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^^^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"ab\ndefg"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 6);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^^^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"abc\nefg"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 6);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^^^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"abcde\ng"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 6);
  }

  {
    Diagnostic_Assertion da = parse_or_fail(u8"  ^^^^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"ab\ne\ng"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_single_byte_escaped_characters_before_and_inside_span) {
  {
    // This was an edge case.
    Diagnostic_Assertion da =
        parse_or_fail(u8"    ^^^ Diag_Invalid_Hex_Escape_Sequence");
    da = da.adjusted_for_escaped_characters(u8"a\\d\\gh"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_single_byte_escaped_characters_inside_and_after_span) {
  {
    // This was an edge case.
    Diagnostic_Assertion da =
        parse_or_fail(u8" ^^^ Diag_Invalid_Hex_Escape_Sequence");
    da = da.adjusted_for_escaped_characters(u8"a\\d\\g"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 1);
    EXPECT_EQ(da.members[0].span_end_offset, 3);
  }
}

TEST(Test_Diagnostic_Assertion,
     adjust_with_unicode_escaped_characters_before_span) {
  {
    static_assert(u8"\u0080"_sv.size() == 2);
    Diagnostic_Assertion da = parse_or_fail(u8"      ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\u0080bcdef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 2);
    EXPECT_EQ(da.members[0].span_end_offset, 4);
  }

  {
    static_assert(u8"\u2603"_sv.size() == 3);
    Diagnostic_Assertion da = parse_or_fail(u8"      ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\u2603bcdef"_sv);
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 3);
    EXPECT_EQ(da.members[0].span_end_offset, 5);
  }

  {
    static_assert(u8"\U0001f3b8"_sv.size() == 4);
    // clang-format off
    Diagnostic_Assertion da = parse_or_fail(u8"          ^^ Diag_Unexpected_Token");
    da = da.adjusted_for_escaped_characters(u8"\U0001f3b8bcdef"_sv);
    // clang-format on
    ASSERT_EQ(da.member_count(), 1);
    EXPECT_EQ(da.members[0].span_begin_offset, 4);
    EXPECT_EQ(da.members[0].span_end_offset, 6);
  }
}

TEST(Test_Diagnostic_Assertion, match_error_type_with_1_field) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher continue_matcher =
      diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Continue"_diag});
  EXPECT_TRUE(continue_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      }),
  }));
  EXPECT_FALSE(continue_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Break{
          .break_statement = Source_Code_Span(&code[0], &code[5]),
      }),
  }));

  ::testing::Matcher break_matcher =
      diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Break"_diag});
  EXPECT_FALSE(break_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      }),
  }));
  EXPECT_TRUE(break_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Break{
          .break_statement = Source_Code_Span(&code[0], &code[5]),
      }),
  }));
}

TEST(Test_Diagnostic_Assertion, match_error_type_with_1_field_message) {
  Padded_String code(u8"hello"_sv);
  ::testing::Matcher matcher =
      diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Continue"_diag});
  Diag_Collector::Diag value(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  });
  EXPECT_EQ(get_matcher_message(matcher, {value}),
            "whose element #0 doesn't match, whose type (Diag_Invalid_Break) "
            "isn't Diag_Invalid_Continue");
}

TEST(Test_Diagnostic_Assertion, match_offsets_of_1_field_span) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher continue_matcher =
      diagnostics_matcher(&code, {u8" ^^^^ Diag_Invalid_Continue"_diag});
  EXPECT_TRUE(continue_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[1], &code[5]),
      }),
  }));
  EXPECT_FALSE(continue_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      }),
  }));
  EXPECT_FALSE(continue_matcher.Matches({
      Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[4]),
      }),
  }));
}

TEST(Test_Diagnostic_Assertion, match_offsets_of_1_field_message) {
  Padded_String code(u8"hello"_sv);

  {
    ::testing::Matcher matcher =
        diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Continue"_diag});
    Diag_Collector::Diag value(Diag_Invalid_Continue{
        .continue_statement = Source_Code_Span(&code[1], &code[4]),
    });
    EXPECT_EQ(get_matcher_message(matcher, {value}),
              "whose element #0 doesn't match, whose .continue_statement (1-4) "
              "doesn't equal 0-5");
  }

  {
    ::testing::Matcher matcher =
        diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Break"_diag});
    Diag_Collector::Diag value(Diag_Invalid_Break{
        .break_statement = Source_Code_Span(&code[1], &code[4]),
    });
    EXPECT_EQ(get_matcher_message(matcher, {value}),
              "whose element #0 doesn't match, whose .break_statement (1-4) "
              "doesn't equal 0-5");
  }
}

TEST(Test_Diagnostic_Assertion, match_span_and_char8) {
  Padded_String code(u8"(hello"_sv);

  ::testing::Matcher matcher = diagnostics_matcher(
      &code,
      {u8"^ Diag_Expected_Parenthesis_Around_Do_While_Condition.where{.token=)}"_diag});
  EXPECT_TRUE(matcher.Matches({
      Diag_Collector::Diag(Diag_Expected_Parenthesis_Around_Do_While_Condition{
          .where = Source_Code_Span(&code[0], &code[1]),
          .token = u8')',
      }),
  }));
  EXPECT_FALSE(matcher.Matches({
      Diag_Collector::Diag(Diag_Expected_Parenthesis_Around_Do_While_Condition{
          .where = Source_Code_Span(&code[0], &code[1]),
          .token = u8'(',
      }),
  }));
}

TEST(Test_Diagnostic_Assertion, char8_message) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher matcher = diagnostics_matcher(
      &code,
      {u8"^ Diag_Expected_Parenthesis_Around_Do_While_Condition.where{.token=)}"_diag});

  Diag_Collector::Diag value(
      Diag_Expected_Parenthesis_Around_Do_While_Condition{
          .where = Source_Code_Span(&code[0], &code[1]),
          .token = u8'(',
      });
  EXPECT_EQ(get_matcher_message(matcher, {value}),
            "whose element #0 doesn't match, whose .where (0-1) equals 0-1 and "
            "whose .token ('(') doesn't equal ')'");
}

TEST(Test_Diagnostic_Assertion, match_span_and_string8_view) {
  Padded_String code(u8"hi"_sv);

  ::testing::Matcher matcher = diagnostics_matcher(
      &code,
      {u8"^ Diag_Integer_Literal_Will_Lose_Precision.characters{.rounded_val=hello}"_diag});
  EXPECT_TRUE(matcher.Matches({
      Diag_Collector::Diag(Diag_Integer_Literal_Will_Lose_Precision{
          .characters = Source_Code_Span(&code[0], &code[1]),
          .rounded_val = u8"hello"_sv,
      }),
  }));
  EXPECT_FALSE(matcher.Matches({
      Diag_Collector::Diag(Diag_Integer_Literal_Will_Lose_Precision{
          .characters = Source_Code_Span(&code[0], &code[1]),
          .rounded_val = u8"HELLO"_sv,
      }),
  }));
}

TEST(Test_Diagnostic_Assertion, string8_view_message) {
  Padded_String code(u8"hi"_sv);

  ::testing::Matcher matcher = diagnostics_matcher(
      &code,
      {u8"^ Diag_Integer_Literal_Will_Lose_Precision.characters{.rounded_val=hello}"_diag});

  Diag_Collector::Diag value(Diag_Integer_Literal_Will_Lose_Precision{
      .characters = Source_Code_Span(&code[0], &code[1]),
      .rounded_val = u8"HELLO"_sv,
  });
  EXPECT_EQ(
      get_matcher_message(matcher, {value}),
      "whose element #0 doesn't match, whose .characters (0-1) equals 0-1 and "
      "whose .rounded_val (\"HELLO\") doesn't equal \"hello\"");
}

TEST(Test_Diagnostic_Assertion, multiple_diagnostics_are_matched_in_any_order) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher continue_break_matcher =
      diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Continue"_diag,
                                  u8"^^^^^ Diag_Invalid_Break"_diag});
  ::testing::Matcher break_continue_matcher =
      diagnostics_matcher(&code, {u8"^^^^^ Diag_Invalid_Break"_diag,
                                  u8"^^^^^ Diag_Invalid_Continue"_diag});

  Diag_Collector::Diag continue_diag(Diag_Invalid_Continue{
      .continue_statement = Source_Code_Span(&code[0], &code[5]),
  });
  Diag_Collector::Diag break_diag(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  });

  EXPECT_TRUE(break_continue_matcher.Matches({break_diag, continue_diag}));
  EXPECT_TRUE(break_continue_matcher.Matches({continue_diag, break_diag}));
  EXPECT_TRUE(continue_break_matcher.Matches({break_diag, continue_diag}));
  EXPECT_TRUE(continue_break_matcher.Matches({continue_diag, break_diag}));
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
