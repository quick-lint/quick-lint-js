// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <deque>
#include <functional>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/source-location.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/utf-8.h>
#include <string_view>
#include <type_traits>
#include <vector>

using namespace std::literals::string_view_literals;

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;

// Like EXPECT_THAT, but using the 'caller' variable for source locations.
#define EXPECT_THAT_AT_CALLER(value, matcher)                                 \
  GTEST_PRED_FORMAT1_(                                                        \
      ::testing::internal::MakePredicateFormatterFromMatcher(matcher), value, \
      ADD_FAILURE_AT_CALLER)

// Like EXPECT_EQ, but using the 'caller' variable for source locations.
#define EXPECT_EQ_AT_CALLER(lhs, rhs)                                   \
  GTEST_PRED_FORMAT2_(::testing::internal::EqHelper::Compare, lhs, rhs, \
                      ADD_FAILURE_AT_CALLER)

#define ADD_FAILURE_AT_CALLER(message)                                    \
  GTEST_MESSAGE_AT_((caller.valid() ? caller.file_name() : __FILE__),     \
                    (caller.valid() ? caller.line() : __LINE__), message, \
                    ::testing::TestPartResult::kNonFatalFailure)

namespace quick_lint_js {
namespace {
class test_lex : public ::testing::Test {
 protected:
  void check_single_token(string8_view input,
                          string8_view expected_identifier_name,
                          source_location = source_location::current());
  void check_single_token_with_errors(
      string8_view input, string8_view expected_identifier_name,
      void (*check_errors)(padded_string_view input,
                           const std::vector<diag_collector::diag>&),
      source_location = source_location::current());
  void check_tokens(string8_view input,
                    std::initializer_list<token_type> expected_token_types,
                    source_location = source_location::current());
  void check_tokens(padded_string_view input,
                    std::initializer_list<token_type> expected_token_types,
                    source_location = source_location::current());
  void check_tokens_with_errors(
      string8_view input,
      std::initializer_list<token_type> expected_token_types,
      std::function<void(padded_string_view input,
                         const std::vector<diag_collector::diag>&)>
          check_errors,
      source_location = source_location::current());
  void check_tokens_with_errors(
      padded_string_view input,
      std::initializer_list<token_type> expected_token_types,
      std::function<void(padded_string_view input,
                         const std::vector<diag_collector::diag>&)>
          check_errors,
      source_location = source_location::current());
  std::vector<token> lex_to_eof(padded_string_view, diag_collector&);
  std::vector<token> lex_to_eof(padded_string_view,
                                source_location = source_location::current());
  std::vector<token> lex_to_eof(string8_view,
                                source_location = source_location::current());

  lexer& make_lexer(padded_string_view input, diag_collector* errors) {
    this->lexers_.emplace_back(input, errors);
    return this->lexers_.back();
  }

  // If true, tell check_tokens_with_errors, lex_to_eof, etc. to call
  // lexer::skip_in_jsx instead of lexer::skip. This has no effect on the first
  // token.
  bool lex_jsx_tokens = false;

 private:
  std::deque<lexer> lexers_;
};

TEST_F(test_lex, lex_block_comments) {
  this->check_single_token(u8"/* */ hello"_sv, u8"hello");
  this->check_single_token(u8"/*/ comment */ hi"_sv, u8"hi");
  this->check_single_token(u8"/* comment /*/ hi"_sv, u8"hi");
  this->check_single_token(u8"/* not /* nested */ ident"_sv, u8"ident");
  EXPECT_THAT(this->lex_to_eof(u8"/**/"_sv), IsEmpty());

  {
    diag_collector v;
    padded_string input(u8"hello /* unterminated comment "_sv);
    lexer l(&input, &v);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unclosed_block_comment,  //
                              comment_open, strlen(u8"hello "), u8"/*")));
  }
}

TEST_F(test_lex, lex_unopened_block_comment) {
  {
    diag_collector v;
    padded_string input(u8"hello */"_sv);
    lexer l(&input, &v);  // identifier
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();  // end of file
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unopened_block_comment,  //
                              comment_close, strlen(u8"hello "), u8"*/")));
  }
  {
    diag_collector v;
    padded_string input(u8"*-----*/"_sv);
    lexer l(&input, &v);

    while (l.peek().type != token_type::end_of_file) {
      l.skip();
    }
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unopened_block_comment,  //
                              comment_close, strlen(u8"*-----"), u8"*/")));
  }
  {
    diag_collector v;
    padded_string input(u8"*******/"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unopened_block_comment,  //
                              comment_close, strlen(u8"******"), u8"*/")));
  }
  {
    diag_collector v;
    padded_string input(u8"*/"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unopened_block_comment,  //
                              comment_close, 0, u8"*/")));
  }
  {
    diag_collector v;
    padded_string input(u8"**/"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unopened_block_comment,  //
                              comment_close, strlen(u8"*"), u8"*/")));
  }
}

TEST_F(test_lex, lex_regexp_literal_starting_with_star_slash) {
  {
    // '/*' is not an end of block comment because it precedes a regexp literal
    diag_collector v;
    padded_string input(u8"*/ hello/"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[input.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST_F(test_lex, lex_regexp_literal_starting_with_star_star_slash) {
  {
    padded_string input(u8"3 **/ banana/"_sv);
    diag_collector v;
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &input[4]);
    EXPECT_EQ(l.peek().end, &input[input.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST_F(test_lex, lex_line_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"// hello"_sv), IsEmpty());
  for (string8_view line_terminator : line_terminators) {
    this->check_single_token(
        u8"// hello" + string8(line_terminator) + u8"world", u8"world");
  }
  EXPECT_THAT(this->lex_to_eof(u8"// hello\n// world"_sv), IsEmpty());
  this->check_tokens(u8"hello//*/\n \n \nworld"_sv,
                     {token_type::identifier, token_type::identifier});
}

TEST_F(test_lex, lex_line_comments_with_control_characters) {
  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"// hello " + string8(control_character) +
                        u8" world\n42.0");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::number});
  }
}

TEST_F(test_lex, lex_html_open_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"<!-- --> hello"_sv), IsEmpty());
  for (string8_view line_terminator : line_terminators) {
    this->check_single_token(
        u8"<!-- hello" + string8(line_terminator) + u8"world", u8"world");
  }
  EXPECT_THAT(this->lex_to_eof(u8"<!-- hello\n<!-- world"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"<!--// hello"_sv), IsEmpty());
  this->check_tokens(u8"hello<!--->\n \n \nworld"_sv,
                     {token_type::identifier, token_type::identifier});
  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"<!-- hello " + string8(control_character) +
                        u8" world\n42.0");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::number});
  }

  this->check_tokens(u8"hello<!world"_sv,
                     {token_type::identifier, token_type::less,
                      token_type::bang, token_type::identifier});
  this->check_tokens(
      u8"hello<!-world"_sv,
      {token_type::identifier, token_type::less, token_type::bang,
       token_type::minus, token_type::identifier});
}

TEST_F(test_lex, lex_html_close_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"--> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"     --> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"/* */--> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"/**//**/--> comment"_sv), IsEmpty());

  for (string8_view line_terminator : line_terminators) {
    string8 eol(line_terminator);

    this->check_single_token(u8"-->" + eol + u8"hello", u8"hello");
    this->check_single_token(u8"--> comment" + eol + u8"hello", u8"hello");
    this->check_single_token(
        u8"--> comment1" + eol + u8"--> comment2" + eol + u8"hello", u8"hello");

    this->check_single_token(u8"/*" + eol + u8"*/--> comment" + eol + u8"hello",
                             u8"hello");
    this->check_single_token(
        u8"/* */ /*" + eol + u8"*/ --> comment" + eol + u8"hello", u8"hello");
    this->check_single_token(
        u8"/*" + eol + u8"*/ /* */ --> comment" + eol + u8"hello", u8"hello");
  }

  // Not an HTML close comment because non-whitespace non-comment preceeds.
  this->check_tokens(u8"3 --> 4", {token_type::number, token_type::minus_minus,
                                   token_type::greater, token_type::number});
}

TEST_F(test_lex, lex_numbers) {
  this->check_tokens(u8"0"_sv, {token_type::number});
  this->check_tokens(u8"2"_sv, {token_type::number});
  this->check_tokens(u8"42"_sv, {token_type::number});
  this->check_tokens(u8"12.34"_sv, {token_type::number});
  this->check_tokens(u8".34"_sv, {token_type::number});

  this->check_tokens(u8"1e3"_sv, {token_type::number});
  this->check_tokens(u8".1e3"_sv, {token_type::number});
  this->check_tokens(u8"1.e3"_sv, {token_type::number});
  this->check_tokens(u8"1.0e3"_sv, {token_type::number});
  this->check_tokens(u8"1e-3"_sv, {token_type::number});
  this->check_tokens(u8"1e+3"_sv, {token_type::number});
  this->check_tokens(u8"1E+3"_sv, {token_type::number});
  this->check_tokens(u8"1E123_233_22"_sv, {token_type::number});

  this->check_tokens(u8"0n"_sv, {token_type::number});
  this->check_tokens(u8"123456789n"_sv, {token_type::number});

  this->check_tokens(u8"123_123_123"_sv, {token_type::number});
  this->check_tokens(u8"123.123_123"_sv, {token_type::number});

  this->check_tokens(u8"123. 456"_sv, {token_type::number, token_type::number});

  this->check_tokens(u8"1.2.3"_sv, {token_type::number, token_type::number});
  this->check_tokens(u8".2.3"_sv, {token_type::number, token_type::number});
  this->check_tokens(u8"0.3"_sv, {token_type::number});
}

TEST_F(test_lex, lex_binary_numbers) {
  this->check_tokens(u8"0b0"_sv, {token_type::number});
  this->check_tokens(u8"0b1"_sv, {token_type::number});
  this->check_tokens(u8"0b010101010101010"_sv, {token_type::number});
  this->check_tokens(u8"0B010101010101010"_sv, {token_type::number});
  this->check_tokens(u8"0b01_11_00_10"_sv, {token_type::number});
  this->check_tokens(u8"0b01n"_sv, {token_type::number});
}

TEST_F(test_lex, fail_lex_integer_loses_precision) {
  this->check_tokens_with_errors(
      u8"9007199254740993"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_2_FIELDS(
                diag_integer_literal_will_lose_precision,                     //
                characters, offsets_matcher(input, 0, u8"9007199254740993"),  //
                rounded_val, u8"9007199254740992"sv)));
      });
  this->check_tokens(u8"999999999999999"_sv, {token_type::number});
  this->check_tokens(
      u8"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"_sv,
      {token_type::number});
  this->check_tokens_with_errors(
      u8"1" + string8(309, u8'0'), {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_2_FIELDS(
                        diag_integer_literal_will_lose_precision,  //
                        characters,
                        offsets_matcher(input, 0,
                                        310),  //
                        rounded_val, u8"inf"sv)));
      });
  this->check_tokens_with_errors(
      u8"179769313486231580000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"_sv,
      {token_type::number}, [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_2_FIELDS(
                diag_integer_literal_will_lose_precision,  //
                characters,
                offsets_matcher(input, 0,
                                309),  //
                rounded_val,
                u8"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"sv)));
      });
  this->check_tokens_with_errors(
      u8"179769313486231589999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"_sv,
      {token_type::number}, [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_2_FIELDS(
                        diag_integer_literal_will_lose_precision,  //
                        characters,
                        offsets_matcher(input, 0,
                                        309),  //
                        rounded_val, u8"inf"sv)));
      });
  this->check_tokens_with_errors(
      u8"18014398509481986"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_2_FIELDS(
                        diag_integer_literal_will_lose_precision,  //
                        characters,
                        offsets_matcher(input, 0, u8"18014398509481986"),  //
                        rounded_val, u8"18014398509481984"sv)));
      });
}

TEST_F(test_lex, fail_lex_binary_number_no_digits) {
  this->check_tokens_with_errors(
      u8"0b"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_binary_number,  //
                                characters, 0, u8"0b")));
      });
  this->check_tokens_with_errors(
      u8"0bn"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_binary_number,  //
                                characters, 0, u8"0bn")));
      });
  this->check_tokens_with_errors(
      u8"0b;"_sv, {token_type::number, token_type::semicolon},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_binary_number,  //
                                characters, 0, u8"0b")));
      });
  this->check_tokens_with_errors(
      u8"[0b]"_sv,
      {token_type::left_square, token_type::number, token_type::right_square},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_binary_number,  //
                                characters, strlen(u8"["), u8"0b")));
      });
}

TEST_F(test_lex, fail_lex_binary_number) {
  this->check_tokens_with_errors(
      u8"0b1.1"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_binary_number,  //
                        characters, strlen(u8"0b1"), u8".1")));
      });
}

TEST_F(test_lex, lex_modern_octal_numbers) {
  this->check_tokens(u8"0o51"_sv, {token_type::number});
  this->check_tokens(u8"0o0"_sv, {token_type::number});
  this->check_tokens(u8"0O0"_sv, {token_type::number});
  this->check_tokens(u8"0O12345670"_sv, {token_type::number});
  this->check_tokens(u8"0o775_775"_sv, {token_type::number});
  this->check_tokens(u8"0o0n"_sv, {token_type::number});
  this->check_tokens(u8"0o01"_sv, {token_type::number});
  this->check_tokens(u8"0o123n"_sv, {token_type::number});
}

TEST_F(test_lex, fail_lex_modern_octal_number_no_digits) {
  this->check_tokens_with_errors(
      u8"0o"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_octal_number,  //
                                characters, 0, u8"0o")));
      });
  this->check_tokens_with_errors(
      u8"0o;"_sv, {token_type::number, token_type::semicolon},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_octal_number,  //
                                characters, 0, u8"0o")));
      });
  this->check_tokens_with_errors(
      u8"[0o]"_sv,
      {token_type::left_square, token_type::number, token_type::right_square},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_octal_number,  //
                                characters, strlen(u8"["), u8"0o")));
      });
}

TEST_F(test_lex, fail_lex_modern_octal_numbers) {
  this->check_tokens_with_errors(
      u8"0o58"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_octal_number,  //
                        characters, strlen(u8"0o5"), u8"8")));
      });

  this->check_tokens_with_errors(
      u8"0o58.2"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_octal_number,  //
                        characters, strlen(u8"0o5"), u8"8.2")));
      });
}

TEST_F(test_lex, lex_legacy_octal_numbers_strict) {
  this->check_tokens(u8"000"_sv, {token_type::number});
  this->check_tokens(u8"001"_sv, {token_type::number});
  this->check_tokens(u8"00010101010101010"_sv, {token_type::number});
  this->check_tokens(u8"051"_sv, {token_type::number});
}

TEST_F(test_lex, lex_legacy_octal_numbers_lax) {
  this->check_tokens(u8"058"_sv, {token_type::number});
  this->check_tokens(u8"058.9"_sv, {token_type::number});
  this->check_tokens(u8"08"_sv, {token_type::number});
}

TEST_F(test_lex, fail_lex_legacy_octal_numbers) {
  this->check_tokens_with_errors(
      u8"0123n"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_legacy_octal_literal_may_not_be_big_int,  //
                        characters, strlen(u8"0123"), u8"n")));
      });

  this->check_tokens_with_errors(
      u8"052.2"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_octal_literal_may_not_have_decimal,  //
                        characters, strlen(u8"052"), u8".")));
      });
}

// TODO (#73) (when strict mode implemented) legacy octal number
// literal tests to fail in strict mode

TEST_F(test_lex, legacy_octal_numbers_cannot_contain_underscores) {
  this->check_tokens_with_errors(
      u8"0775_775"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_legacy_octal_literal_may_not_contain_underscores,  //
                underscores, strlen(u8"0775"), u8"_")));
      });

  this->check_tokens_with_errors(
      u8"0775____775"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_legacy_octal_literal_may_not_contain_underscores,  //
                underscores, strlen(u8"0775"), u8"____")));
      });
}

TEST_F(test_lex, lex_hex_numbers) {
  this->check_tokens(u8"0x0"_sv, {token_type::number});
  this->check_tokens(u8"0x123456789abcdef"_sv, {token_type::number});
  this->check_tokens(u8"0X123456789ABCDEF"_sv, {token_type::number});
  this->check_tokens(u8"0X123_4567_89AB_CDEF"_sv, {token_type::number});
  this->check_tokens(u8"0x1n"_sv, {token_type::number});
  this->check_tokens(u8"0xfn"_sv, {token_type::number});
}

TEST_F(test_lex, fail_lex_hex_number_no_digits) {
  this->check_tokens_with_errors(
      u8"0x"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_hex_number,  //
                                characters, 0, u8"0x")));
      });
  this->check_tokens_with_errors(
      u8"0xn"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_hex_number,  //
                                characters, 0, u8"0xn")));
      });
  this->check_tokens_with_errors(
      u8"0x;"_sv, {token_type::number, token_type::semicolon},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_hex_number,  //
                                characters, 0, u8"0x")));
      });
  this->check_tokens_with_errors(
      u8"[0x]"_sv,
      {token_type::left_square, token_type::number, token_type::right_square},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_no_digits_in_hex_number,  //
                                characters, strlen(u8"["), u8"0x")));
      });
}

TEST_F(test_lex, fail_lex_hex_number) {
  this->check_tokens_with_errors(
      u8"0xf.f"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_hex_number,  //
                        characters, strlen(u8"0xf"), u8".f")));
      });
}

TEST_F(test_lex, lex_number_with_trailing_garbage) {
  this->check_tokens_with_errors(
      u8"123abcd"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unexpected_characters_in_number,  //
                                characters, strlen(u8"123"), u8"abcd")));
      });
  this->check_tokens_with_errors(
      u8"123e f"_sv, {token_type::number, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unexpected_characters_in_number,  //
                                characters, strlen(u8"123"), u8"e")));
      });
  this->check_tokens_with_errors(
      u8"123e-f"_sv,
      {token_type::number, token_type::minus, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unexpected_characters_in_number,  //
                                characters, strlen(u8"123"), u8"e")));
      });
  this->check_tokens_with_errors(
      u8"0b01234"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_binary_number,  //
                        characters, strlen(u8"0b01"), u8"234")));
      });
  this->check_tokens_with_errors(
      u8"0b0h0lla"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_binary_number,  //
                        characters, strlen(u8"0b0"), u8"h0lla")));
      });
  this->check_tokens_with_errors(
      u8"0xabjjw"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_hex_number,  //
                        characters, strlen(u8"0xab"), u8"jjw")));
      });
  this->check_tokens_with_errors(
      u8"0o69"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_characters_in_octal_number,  //
                        characters, strlen(u8"0o6"), u8"9")));
      });
}

TEST_F(test_lex, lex_invalid_big_int_number) {
  this->check_tokens_with_errors(
      u8"12.34n"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_big_int_literal_contains_decimal_point,  //
                        where, 0, u8"12.34n")));
      });
  this->check_tokens_with_errors(
      u8"1e3n"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_big_int_literal_contains_exponent,  //
                        where, 0, u8"1e3n")));
      });

  // Only complain about the decimal point, not the leading 0 digit.
  this->check_tokens_with_errors(
      u8"0.1n"_sv, {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE(
                                diag_big_int_literal_contains_decimal_point)));
      });

  // Complain about both the decimal point and the leading 0 digit.
  this->check_tokens_with_errors(
      u8"01.2n"_sv, {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(
                DIAG_TYPE(diag_octal_literal_may_not_have_decimal),
                DIAG_TYPE(diag_legacy_octal_literal_may_not_be_big_int)));
      });

  // Complain about everything. What a disaster.
  this->check_tokens_with_errors(
      u8"01.2e+3n"_sv, {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(
                DIAG_TYPE(diag_octal_literal_may_not_have_decimal),
                DIAG_TYPE(diag_octal_literal_may_not_have_exponent),
                DIAG_TYPE(diag_legacy_octal_literal_may_not_be_big_int)));
      });
}

TEST_F(test_lex, lex_number_with_double_underscore) {
  this->check_tokens_with_errors(
      u8"123__000"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"123"), u8"__")));
      });
}

TEST_F(test_lex, lex_number_with_many_underscores) {
  this->check_tokens_with_errors(
      u8"123_____000"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"123"), u8"_____")));
      });
  this->check_tokens_with_errors(
      u8"0xfee_____eed"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"0xfee"), u8"_____")));
      });
  this->check_tokens_with_errors(
      u8"0o777_____000"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"0o777"), u8"_____")));
      });
  this->check_tokens_with_errors(
      u8"0b111_____000"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"0b111"), u8"_____")));
      });
}

TEST_F(test_lex, lex_number_with_multiple_groups_of_consecutive_underscores) {
  {
    diag_collector v;
    padded_string input(u8"123__45___6"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::number);
    EXPECT_EQ(*l.peek().begin, '1');
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(
        v.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(
                &input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"123"), u8"__"),
            DIAG_TYPE_OFFSETS(
                &input,
                diag_number_literal_contains_consecutive_underscores,  //
                underscores, strlen(u8"123__45"), u8"___")));
  }
}

TEST_F(test_lex, lex_number_with_trailing_underscore) {
  this->check_tokens_with_errors(
      u8"123456_"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_number_literal_contains_trailing_underscores,  //
                underscores, strlen(u8"123456"), u8"_")));
      });
}

TEST_F(test_lex, lex_number_with_trailing_underscores) {
  this->check_tokens_with_errors(
      u8"123456___"_sv, {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_number_literal_contains_trailing_underscores,  //
                underscores, strlen(u8"123456"), u8"___")));
      });
}

TEST_F(test_lex, lex_strings) {
  this->check_tokens(u8R"('hello')", {token_type::string});
  this->check_tokens(u8R"("hello")", {token_type::string});
  this->check_tokens(u8R"("hello\"world")", {token_type::string});
  this->check_tokens(u8R"('hello\'world')", {token_type::string});
  this->check_tokens(u8R"('hello"world')", {token_type::string});
  this->check_tokens(u8R"("hello'world")", {token_type::string});
  this->check_tokens(u8"'hello\\\nworld'"_sv, {token_type::string});
  this->check_tokens(u8"\"hello\\\nworld\"", {token_type::string});
  this->check_tokens(u8"'hello\\x0aworld'", {token_type::string});
  this->check_tokens(u8R"('\x68\x65\x6c\x6C\x6f')", {token_type::string});
  this->check_tokens(u8R"('\uabcd')", {token_type::string});
  this->check_tokens(u8R"('\u{abcd}')", {token_type::string});

  this->check_tokens_with_errors(
      u8R"("unterminated)", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unclosed_string_literal,  //
                                string_literal, 0, u8R"("unterminated)")));
      });

  for (string8_view line_terminator : line_terminators) {
    for (const char8* quotation_mark : {u8"'", u8"\""}) {
      padded_string input(quotation_mark +
                          (u8"line1\\" + string8(line_terminator) + u8"line2") +
                          quotation_mark);
      this->check_tokens(&input, {token_type::string});
    }
  }

  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    diag_collector v;
    padded_string input(u8"'unterminated" + string8(line_terminator) +
                        u8"hello");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello");

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unclosed_string_literal,  //
                              string_literal, 0, u8"'unterminated")));
  }

  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    diag_collector v;
    padded_string input(u8"'separated" + string8(line_terminator) + u8"hello'");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input,
                              diag_unclosed_string_literal,  //
                              string_literal, 0, input.string_view())));
  }

  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    diag_collector v;
    padded_string input(u8"'separated" + string8(line_terminator) +
                        string8(line_terminator) + u8"hello'");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(
        v.errors,
        ElementsAre(
            DIAG_TYPE_OFFSETS(&input, diag_unclosed_string_literal,  //
                              string_literal, 0, u8"'separated"),
            DIAG_TYPE_OFFSETS(
                &input, diag_unclosed_string_literal, string_literal,  //
                strlen(u8"'separatedhello") + 2 * line_terminator.size(),
                u8"'")));
  }

  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    diag_collector v;
    padded_string input(u8"let x = 'hello" + string8(line_terminator) +
                        u8"let y = 'world'");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::kw_let);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::equal);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::kw_let);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::equal);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &input, diag_unclosed_string_literal,  //
                    string_literal, strlen(u8"let x = "), u8"'hello")));
  }

  this->check_tokens_with_errors(
      u8"'unterminated\\"_sv, {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unclosed_string_literal,  //
                                string_literal, 0, u8"'unterminated\\")));
      });

  this->check_tokens_with_errors(
      u8"'\\x", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'"), u8"\\x"),
                DIAG_TYPE_OFFSETS(input, diag_unclosed_string_literal,  //
                                  string_literal, 0, u8"'\\x")));
      });

  this->check_tokens_with_errors(
      u8"'\\x1", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'"), u8"\\x"),
                DIAG_TYPE_OFFSETS(input, diag_unclosed_string_literal,  //
                                  string_literal, 0, u8"'\\x1")));
      });

  this->check_tokens_with_errors(
      u8"'\\x'", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_invalid_hex_escape_sequence,  //
                                escape_sequence, strlen(u8"'"), u8"\\x")));
      });

  this->check_tokens_with_errors(
      u8"'\\x\\xyz'", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'"), u8"\\x"),
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'\\x"), u8"\\x")));
      });

  this->check_tokens_with_errors(
      u8"'\\x1 \\xff \\xg '", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'"), u8"\\x"),
                DIAG_TYPE_OFFSETS(input, diag_invalid_hex_escape_sequence,  //
                                  escape_sequence, strlen(u8"'\\x1 \\xff "),
                                  u8"\\x")));
      });

  this->check_tokens_with_errors(
      u8"'hello\\u'", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"'hello"), u8"\\u'")));
      });

  this->check_tokens_with_errors(
      u8"'hello\\u{110000}'", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_code_point_in_unicode_out_of_range,  //
                escape_sequence, strlen(u8"'hello"), u8"\\u{110000}")));
      });

  // TODO(#187): Report octal escape sequences in strict mode.
  // TODO(#187): Report invalid octal escape sequences in non-strict mode.
}

TEST_F(test_lex, lex_string_with_ascii_control_characters) {
  for (string8_view control_character :
       concat(control_characters_except_line_terminators, ls_and_ps)) {
    padded_string input(u8"'hello" + string8(control_character) + u8"world'");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::string});
  }

  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"'hello\\" + string8(control_character) + u8"world'");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::string});
  }
}

TEST_F(test_lex, string_with_curly_quotes) {
  // Curly single quotes:
  this->check_tokens_with_errors(
      u8"\u2018string here\u2019", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u2018"),  //
                                suggested_quote, u8'\'')));
      });
  this->check_tokens_with_errors(
      u8"\u2019string here\u2018", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u2019"),  //
                                suggested_quote, u8'\'')));
      });
  this->check_tokens_with_errors(
      u8"\u2018string \u201c \" \u201d here\u2019", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u2018"),  //
                                suggested_quote, u8'\'')));
      });

  // Curly double quotes:
  this->check_tokens_with_errors(
      u8"\u201cstring here\u201d", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u201d"),  //
                                suggested_quote, u8'"')));
      });
  this->check_tokens_with_errors(
      u8"\u201dstring here\u201c", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u201c"),  //
                                suggested_quote, u8'"')));
      });
  this->check_tokens_with_errors(
      u8"\u201cstring \u2018 ' \u2019 here\u201d", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u201d"),  //
                                suggested_quote, u8'"')));
      });

  // Start with curly quote, but end with matching straight quote:
  this->check_tokens_with_errors(
      u8"\u2018string here'", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u2018"),  //
                                suggested_quote, u8'\'')));
      });
  this->check_tokens_with_errors(
      u8"\u201cstring here\"", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_2_FIELDS(
                                diag_invalid_quotes_around_string_literal,  //
                                opening_quote,
                                offsets_matcher(input, 0, u8"\u201d"),  //
                                suggested_quote, u8'"')));
      });

  // Unclosed string:
  for (string8 opening_quote : {u8"\u2018", u8"\u201c"}) {
    this->check_tokens_with_errors(
        opening_quote + u8"string here", {token_type::string},
        [&](padded_string_view input, const auto& errors) {
          EXPECT_THAT(
              errors,
              UnorderedElementsAre(
                  DIAG_TYPE(diag_invalid_quotes_around_string_literal),
                  DIAG_TYPE_OFFSETS(input, diag_unclosed_string_literal,  //
                                    string_literal, 0,
                                    opening_quote + u8"string here")));
        });
    for (string8_view line_terminator : line_terminators) {
      this->check_tokens_with_errors(
          opening_quote + u8"string here" + string8(line_terminator) +
              u8"next_line",
          {token_type::string, token_type::identifier},
          [&](padded_string_view input, const auto& errors) {
            EXPECT_THAT(
                errors,
                UnorderedElementsAre(
                    DIAG_TYPE(diag_invalid_quotes_around_string_literal),
                    DIAG_TYPE_OFFSETS(input, diag_unclosed_string_literal,  //
                                      string_literal, 0,
                                      opening_quote + u8"string here")));
          });
    }
  }
}

TEST_F(test_lex, lex_templates) {
  this->check_tokens(u8"``"_sv, {token_type::complete_template});
  this->check_tokens(u8"`hello`"_sv, {token_type::complete_template});
  this->check_tokens(u8"`hello$world`"_sv, {token_type::complete_template});
  this->check_tokens(u8"`hello{world`"_sv, {token_type::complete_template});
  this->check_tokens(u8R"(`hello\`world`)", {token_type::complete_template});
  this->check_tokens(u8R"(`hello$\{world`)", {token_type::complete_template});
  this->check_tokens(u8R"(`hello\${world`)", {token_type::complete_template});
  this->check_tokens(
      u8R"(`hello
world`)",
      {token_type::complete_template});
  this->check_tokens(u8"`hello\\\nworld`"_sv, {token_type::complete_template});
  this->check_tokens(u8R"(`\uabcd`)", {token_type::complete_template});
  this->check_tokens(u8R"(`\u{abcd}`)", {token_type::complete_template});

  {
    padded_string code(u8"`hello${42}`"_sv);
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`hello${");
    const char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`");
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string code(u8"`${42}world`"_sv);
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`${");
    const char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"world`");
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string code(u8"`${left}${right}`"_sv);
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    const char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  this->check_tokens_with_errors(
      u8"`unterminated"_sv, {token_type::complete_template},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unclosed_template,  //
                                incomplete_template, 0, u8"`unterminated")));
      });

  {
    diag_collector v;
    padded_string input(u8"`${un}terminated"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    const char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unclosed_template,  //
                              incomplete_template, 0, u8"`${un}terminated")));
  }

  this->check_tokens_with_errors(
      u8"`unterminated\\"_sv, {token_type::complete_template},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unclosed_template,  //
                                incomplete_template, 0, u8"`unterminated\\")));
      });
}

TEST_F(test_lex, templates_buffer_unicode_escape_errors) {
  {
    padded_string input(u8"`hello\\u`"_sv);
    diag_collector errors;
    lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &input, diag_expected_hex_digits_in_unicode_escape,  //
                    escape_sequence, strlen(u8"`hello"), u8"\\u`")));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string input(u8"`hello\\u{110000}`"_sv);
    diag_collector errors;
    lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &input, diag_escaped_code_point_in_unicode_out_of_range,  //
                    escape_sequence, strlen(u8"`hello"), u8"\\u{110000}")));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string input(u8"`hello\\u${expr}`"_sv);
    diag_collector errors;
    lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &input, diag_expected_hex_digits_in_unicode_escape,  //
                    escape_sequence, strlen(u8"`hello"), u8"\\u`")));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
  }
}

TEST_F(test_lex, templates_do_not_buffer_valid_unicode_escapes) {
  {
    padded_string input(u8"`hell\\u{6f}`"_sv);
    diag_collector errors;
    lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string input(u8"`hell\\u{6f}${expr}`"_sv);
    diag_collector errors;
    lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
  }
}

TEST_F(test_lex, lex_template_literal_with_ascii_control_characters) {
  for (string8_view control_character :
       concat(control_characters_except_line_terminators, line_terminators)) {
    padded_string input(u8"`hello" + string8(control_character) + u8"world`");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::complete_template});
  }

  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"`hello\\" + string8(control_character) + u8"world`");
    SCOPED_TRACE(input);
    this->check_tokens(&input, {token_type::complete_template});
  }
}

TEST_F(test_lex, lex_regular_expression_literals) {
  auto check_regexp = [](string8_view raw_code) {
    padded_string code(raw_code);
    SCOPED_TRACE(code);
    diag_collector errors;
    lexer l(&code, &errors);

    EXPECT_THAT(l.peek().type,
                testing::AnyOf(token_type::slash, token_type::slash_equal));
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  };

  check_regexp(u8"/ /"_sv);
  check_regexp(u8R"(/hello\/world/)"_sv);
  check_regexp(u8"/re/g"_sv);
  check_regexp(u8R"(/[/]/)"_sv);
  check_regexp(u8R"(/[\]/]/)"_sv);
  check_regexp(u8R"(/[\\]/)"_sv);
  check_regexp(u8"/=/"_sv);

  for (string8_view raw_code : {u8"/end_of_file"_sv, u8R"(/eof\)"_sv}) {
    padded_string code(raw_code);
    SCOPED_TRACE(code);
    diag_collector v;
    lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_regexp_literal,  //
                              regexp_literal, 0, code.string_view())));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"/first_line" + string8(line_terminator) +
                       u8"second_line/");
    SCOPED_TRACE(code);
    diag_collector v;
    lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, code.data() + strlen(u8"/first_line"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_regexp_literal,  //
                              regexp_literal, 0, u8"/first_line")));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"second_line");
  }

  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"/first[line" + string8(line_terminator) +
                       u8"second]line/");
    SCOPED_TRACE(code);
    diag_collector v;
    lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, code.data() + strlen(u8"/first[line"));

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code, diag_unclosed_regexp_literal,  //
                              regexp_literal, 0, u8"/first[line")));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"second");
  }

  // TODO(#187): Report invalid escape sequences.

  // TODO(#203): Report invalid characters and mismatched brackets.
}

TEST_F(test_lex, lex_regular_expression_literal_with_digit_flag) {
  padded_string input(u8"/cellular/3g"_sv);

  lexer l(&input, &null_diag_reporter::instance);
  EXPECT_EQ(l.peek().type, token_type::slash);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, token_type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  // TODO(#47): Report an error, because '3' is an invalid flag.
}

TEST_F(test_lex, lex_unicode_escape_in_regular_expression_literal_flags) {
  diag_collector errors;
  padded_string input(u8"/hello/\\u{67}i"_sv);

  lexer l(&input, &errors);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, token_type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  EXPECT_THAT(
      errors.errors,
      ElementsAre(DIAG_TYPE_OFFSETS(
          &input, diag_regexp_literal_flags_cannot_contain_unicode_escapes,  //
          escape_sequence, strlen(u8"/hello/"), u8"\\u{67}")));
}

TEST_F(test_lex, lex_non_ascii_in_regular_expression_literal_flags) {
  diag_collector errors;
  padded_string input(u8"/hello/\u05d0"_sv);

  lexer l(&input, &errors);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, token_type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  // TODO(#47): Report an error, because '\u05d0' is an invalid flag.
}

TEST_F(test_lex,
       lex_regular_expression_literals_preserves_leading_newline_flag) {
  {
    padded_string code(u8"\n/ /"_sv);
    lexer l(&code, &null_diag_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_TRUE(l.peek().has_leading_newline);
  }

  {
    padded_string code(u8"/ /"_sv);
    lexer l(&code, &null_diag_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_FALSE(l.peek().has_leading_newline);
  }
}

TEST_F(test_lex, lex_regular_expression_literal_with_ascii_control_characters) {
  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"/hello" + string8(control_character) + u8"world/");
    SCOPED_TRACE(input);
    diag_collector errors;
    lexer l(&input, &errors);

    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"/hello\\" + string8(control_character) + u8"world/");
    SCOPED_TRACE(input);
    diag_collector errors;
    lexer l(&input, &errors);

    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(test_lex, lex_identifiers) {
  this->check_tokens(u8"i"_sv, {token_type::identifier});
  this->check_tokens(u8"_"_sv, {token_type::identifier});
  this->check_tokens(u8"$"_sv, {token_type::identifier});
  this->check_single_token(u8"id"_sv, u8"id");
  this->check_single_token(u8"id "_sv, u8"id");
  this->check_single_token(u8"this_is_an_identifier"_sv,
                           u8"this_is_an_identifier");
  this->check_single_token(u8"MixedCaseIsAllowed"_sv, u8"MixedCaseIsAllowed");
  this->check_single_token(u8"ident$with$dollars"_sv, u8"ident$with$dollars");
  this->check_single_token(u8"digits0123456789"_sv, u8"digits0123456789");
}

TEST_F(test_lex, ascii_identifier_with_escape_sequence) {
  this->check_single_token(u8"\\u0061"_sv, u8"a");
  this->check_single_token(u8"\\u0041"_sv, u8"A");
  this->check_single_token(u8"\\u004E"_sv, u8"N");
  this->check_single_token(u8"\\u004e"_sv, u8"N");

  this->check_single_token(u8"\\u{41}"_sv, u8"A");
  this->check_single_token(u8"\\u{0041}"_sv, u8"A");
  this->check_single_token(u8"\\u{00000000000000000000041}"_sv, u8"A");
  this->check_single_token(u8"\\u{004E}"_sv, u8"N");
  this->check_single_token(u8"\\u{004e}"_sv, u8"N");

  this->check_single_token(u8"hell\\u006f"_sv, u8"hello");
  this->check_single_token(u8"\\u0068ello"_sv, u8"hello");
  this->check_single_token(u8"w\\u0061t"_sv, u8"wat");

  this->check_single_token(u8"hel\\u006c0"_sv, u8"hell0");

  this->check_single_token(u8"\\u0077\\u0061\\u0074"_sv, u8"wat");
  this->check_single_token(u8"\\u{77}\\u{61}\\u{74}"_sv, u8"wat");

  // _ and $ are in IdentifierStart, even though they aren't in UnicodeIDStart.
  this->check_single_token(u8"\\u{5f}wakka"_sv, u8"_wakka");
  this->check_single_token(u8"\\u{24}wakka"_sv, u8"$wakka");

  // $, ZWNJ, ZWJ in IdentifierPart, even though they aren't in
  // UnicodeIDContinue.
  this->check_single_token(u8"wakka\\u{24}"_sv, u8"wakka$");
  this->check_single_token(u8"wak\\u200cka"_sv, u8"wak\u200cka");
  this->check_single_token(u8"wak\\u200dka"_sv, u8"wak\u200dka");
}

TEST_F(test_lex, non_ascii_identifier) {
  this->check_single_token(u8"\U00013337"_sv, u8"\U00013337");

  this->check_single_token(u8"\u00b5"_sv, u8"\u00b5");          // 2 UTF-8 bytes
  this->check_single_token(u8"\u05d0"_sv, u8"\u05d0");          // 3 UTF-8 bytes
  this->check_single_token(u8"a\u0816"_sv, u8"a\u0816");        // 3 UTF-8 bytes
  this->check_single_token(u8"\U0001e93f"_sv, u8"\U0001e93f");  // 4 UTF-8 bytes
}

TEST_F(test_lex, non_ascii_identifier_with_escape_sequence) {
  this->check_single_token(u8"\\u{013337}"_sv, u8"\U00013337");

  this->check_single_token(u8"\\u{b5}"_sv, u8"\u00b5");         // 2 UTF-8 bytes
  this->check_single_token(u8"a\\u{816}"_sv, u8"a\u0816");      // 3 UTF-8 bytes
  this->check_single_token(u8"a\\u0816"_sv, u8"a\u0816");       // 3 UTF-8 bytes
  this->check_single_token(u8"\\u{1e93f}"_sv, u8"\U0001e93f");  // 4 UTF-8 bytes
}

TEST_F(test_lex,
       identifier_with_escape_sequences_source_code_span_is_in_place) {
  padded_string input(u8"\\u{77}a\\u{74}"_sv);
  lexer l(&input, &null_diag_reporter::instance);
  source_code_span span = l.peek().identifier_name().span();
  EXPECT_EQ(span.begin(), &input[0]);
  EXPECT_EQ(span.end(), &input[input.size()]);
}

TEST_F(test_lex, lex_identifier_with_malformed_escape_sequence) {
  this->check_single_token_with_errors(
      u8" are\\ufriendly ", u8"are\\ufriendly",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8" are"), u8"\\ufr")));
      });
  this->check_tokens_with_errors(
      u8"are\\uf riendly"_sv, {token_type::identifier, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"are"), u8"\\uf ")));
      });
  this->check_single_token_with_errors(
      u8"stray\\backslash", u8"stray\\backslash",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_backslash_in_identifier,  //
                        backslash, strlen(u8"stray"), u8"\\")));
      });
  this->check_single_token_with_errors(
      u8"stray\\", u8"stray\\",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unexpected_backslash_in_identifier,  //
                        backslash, strlen(u8"stray"), u8"\\")));
      });
  this->check_tokens_with_errors(
      u8"hello\\u}world"_sv,
      {token_type::identifier, token_type::right_curly, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"hello"), u8"\\u}")));
      });
  this->check_tokens_with_errors(
      u8"negative\\u-0041"_sv,
      {token_type::identifier, token_type::minus, token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"negative"), u8"\\u-")));
      });

  this->check_single_token_with_errors(
      u8"a\\u{}b", u8"a\\u{}b",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"a"), u8"\\u{}")));
      });
  this->check_single_token_with_errors(
      u8"a\\u{q}b", u8"a\\u{q}b",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"a"), u8"\\u{q}")));
      });

  this->check_single_token_with_errors(
      u8"unterminated\\u", u8"unterminated\\u",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_expected_hex_digits_in_unicode_escape,  //
                        escape_sequence, strlen(u8"unterminated"), u8"\\u")));
      });
  this->check_single_token_with_errors(
      u8"unterminated\\u012", u8"unterminated\\u012",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_expected_hex_digits_in_unicode_escape,  //
                escape_sequence, strlen(u8"unterminated"), u8"\\u012")));
      });
  this->check_single_token_with_errors(
      u8"unterminated\\u{", u8"unterminated\\u{",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unclosed_identifier_escape_sequence,  //
                        escape_sequence, strlen(u8"unterminated"), u8"\\u{")));
      });
  this->check_single_token_with_errors(
      u8"unterminated\\u{0123", u8"unterminated\\u{0123",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_unclosed_identifier_escape_sequence,  //
                escape_sequence, strlen(u8"unterminated"), u8"\\u{0123")));
      });

  this->check_tokens_with_errors(
      u8"unclosed\\u{0123 'string'",
      {token_type::identifier, token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unclosed_identifier_escape_sequence,  //
                        escape_sequence, strlen(u8"unclosed"), u8"\\u{0123")));
      });
  this->check_tokens_with_errors(
      u8"unclosed\\u{+=42",
      {token_type::identifier, token_type::plus_equal, token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_unclosed_identifier_escape_sequence,  //
                        escape_sequence, strlen(u8"unclosed"), u8"\\u{")));
      });
}

TEST_F(test_lex, lex_identifier_with_out_of_range_escaped_character) {
  this->check_single_token_with_errors(
      u8"too\\u{110000}big", u8"too\\u{110000}big",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_code_point_in_unicode_out_of_range,  //
                escape_sequence, strlen(u8"too"), u8"\\u{110000}")));
      });
  this->check_single_token_with_errors(
      u8"waytoo\\u{100000000000000}big", u8"waytoo\\u{100000000000000}big",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_code_point_in_unicode_out_of_range,  //
                escape_sequence, strlen(u8"waytoo"),
                u8"\\u{100000000000000}")));
      });
}

TEST_F(test_lex, lex_identifier_with_out_of_range_utf_8_sequence) {
  // f4 90 80 80 is U+110000
  this->check_single_token_with_errors(
      "too\xf4\x90\x80\x80\x62ig"_s8v, "too\xf4\x90\x80\x80\x62ig"_s8v,
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_FIELD(
                        diag_invalid_utf_8_sequence, sequence,
                        offsets_matcher(input, std::strlen("too"),
                                        std::strlen("too\xf4\x90\x80\x80")))));
      });
}

TEST_F(test_lex, lex_identifier_with_malformed_utf_8_sequence) {
  this->check_single_token_with_errors(
      "illegal\xc0\xc1\xc2\xc3\xc4utf8\xfe\xff"_s8v,
      "illegal\xc0\xc1\xc2\xc3\xc4utf8\xfe\xff"_s8v,
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(
                DIAG_TYPE_FIELD(
                    diag_invalid_utf_8_sequence, sequence,
                    offsets_matcher(
                        input, std::strlen("illegal"),
                        std::strlen("illegal\xc0\xc1\xc2\xc3\xc4"))),
                DIAG_TYPE_FIELD(
                    diag_invalid_utf_8_sequence, sequence,
                    offsets_matcher(
                        input, std::strlen("illegal\xc0\xc1\xc2\xc3\xc4utf8"),
                        std::strlen(
                            "illegal\xc0\xc1\xc2\xc3\xc4utf8\xfe\xff")))));
      });
}

TEST_F(test_lex, lex_identifier_with_disallowed_character_escape_sequence) {
  this->check_single_token_with_errors(
      u8"illegal\\u0020", u8"illegal\\u0020",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"illegal"), u8"\\u0020")));
      });
  this->check_single_token_with_errors(
      u8"illegal\\u{0020}", u8"illegal\\u{0020}",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"illegal"), u8"\\u{0020}")));
      });
  this->check_single_token_with_errors(
      u8"\\u{20}illegal", u8"\\u{20}illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, 0, u8"\\u{20}")));
      });
  this->check_single_token_with_errors(
      u8"illegal\\u{10ffff}", u8"illegal\\u{10ffff}",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"illegal"), u8"\\u{10ffff}")));
      });
  this->check_single_token_with_errors(
      u8"\\u{10ffff}illegal", u8"\\u{10ffff}illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, 0, u8"\\u{10ffff}")));
      });

  // U+005c is \ (backslash)
  this->check_single_token_with_errors(
      u8"\\u{5c}u0061illegal", u8"\\u{5c}u0061illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, 0, u8"\\u{5c}")));
      });
  this->check_single_token_with_errors(
      u8"illegal\\u{5c}u0061", u8"illegal\\u{5c}u0061",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"illegal"), u8"\\u{5c}")));
      });
}

TEST_F(test_lex, lex_identifier_with_disallowed_non_ascii_character) {
  this->check_single_token_with_errors(
      u8"illegal\U0010ffff", u8"illegal\U0010ffff",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_character_disallowed_in_identifiers,  //
                        character, strlen(u8"illegal"), u8"\U0010ffff")));
      });
  this->check_single_token_with_errors(
      u8"\U0010ffffillegal", u8"\U0010ffffillegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_character_disallowed_in_identifiers,  //
                        character, 0, u8"\U0010ffff")));
      });
}

TEST_F(test_lex, lex_identifier_with_disallowed_escaped_initial_character) {
  // Identifiers cannot start with a digit.
  this->check_single_token_with_errors(
      u8"\\u{30}illegal", u8"\\u{30}illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, 0, u8"\\u{30}")));
      });

  this->check_single_token_with_errors(
      u8"\\u0816illegal", u8"\\u0816illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, 0, u8"\\u0816")));
      });
}

TEST_F(test_lex, lex_identifier_with_disallowed_non_ascii_initial_character) {
  this->check_single_token_with_errors(
      u8"\u0816illegal", u8"\u0816illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_character_disallowed_in_identifiers,  //
                        character, 0, u8"\u0816")));
      });
}

TEST_F(
    test_lex,
    lex_identifier_with_disallowed_initial_character_as_subsequent_character) {
  // Identifiers can contain a digit.
  this->check_single_token(u8"legal0"_sv, u8"legal0");
  this->check_single_token(u8"legal\\u{30}"_sv, u8"legal0");

  this->check_single_token(u8"legal\\u0816"_sv, u8"legal\u0816");
  this->check_single_token(u8"legal\u0816"_sv, u8"legal\u0816");
}

TEST_F(test_lex, lex_identifiers_which_look_like_keywords) {
  this->check_tokens(u8"ifelse"_sv, {token_type::identifier});
  this->check_tokens(u8"IF"_sv, {token_type::identifier});
}

TEST_F(test_lex, private_identifier) {
  this->check_tokens(u8"#i"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#_"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#$"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#Mixed_Case_With_Underscores"_sv,
                     {token_type::private_identifier});
  this->check_tokens(u8"#digits0123456789"_sv,
                     {token_type::private_identifier});

  {
    padded_string code(u8" #id "_sv);
    std::vector<token> tokens = this->lex_to_eof(&code);
    ASSERT_EQ(tokens.size(), 1);
    identifier ident = tokens[0].identifier_name();
    EXPECT_EQ(ident.span().string_view(), u8"#id"_sv);
    EXPECT_EQ(ident.normalized_name(), u8"#id"_sv);
  }

  this->check_single_token(u8"#\u00b5"_sv, u8"#\u00b5");    // 2 UTF-8 bytes
  this->check_single_token(u8"#\u05d0"_sv, u8"#\u05d0");    // 2 UTF-8 bytes
  this->check_single_token(u8"#a\u0816"_sv, u8"#a\u0816");  // 3 UTF-8 bytes
  this->check_single_token(u8"#\U0001e93f"_sv,
                           u8"#\U0001e93f");  // 4 UTF-8 bytes

  this->check_single_token(u8"#\\u{b5}"_sv, u8"#\u00b5");
  this->check_single_token(u8"#a\\u0816"_sv, u8"#a\u0816");
  this->check_single_token(u8"#\\u{0001e93f}"_sv, u8"#\U0001e93f");

  {
    padded_string code(u8" #\\u{78} "_sv);
    std::vector<token> tokens = this->lex_to_eof(&code);
    ASSERT_EQ(tokens.size(), 1);
    identifier ident = tokens[0].identifier_name();
    EXPECT_EQ(ident.span().string_view(), u8"#\\u{78}"_sv);
    EXPECT_EQ(ident.normalized_name(), u8"#x"_sv);
  }

  // Keywords are allowed.
  this->check_tokens(u8"#async"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#for"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#function"_sv, {token_type::private_identifier});
  this->check_tokens(u8"#let"_sv, {token_type::private_identifier});
}

TEST_F(test_lex,
       private_identifier_with_disallowed_non_ascii_initial_character) {
  this->check_single_token_with_errors(
      u8"#\u0816illegal", u8"#\u0816illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_character_disallowed_in_identifiers,  //
                        character, strlen(u8"#"), u8"\u0816")));
      });

  this->check_tokens_with_errors(
      u8"#123", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                input, diag_unexpected_hash_character,  //
                                where, 0, u8"#")));
      });
}

TEST_F(test_lex, private_identifier_with_disallowed_escaped_initial_character) {
  // Private identifiers cannot start with a digit.
  this->check_single_token_with_errors(
      u8"#\\u{30}illegal", u8"#\\u{30}illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"#"), u8"\\u{30}")));
      });

  this->check_single_token_with_errors(
      u8"#\\u0816illegal", u8"#\\u0816illegal",
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(DIAG_TYPE_OFFSETS(
                input, diag_escaped_character_disallowed_in_identifiers,  //
                escape_sequence, strlen(u8"#"), u8"\\u0816")));
      });
}

TEST_F(test_lex, lex_reserved_keywords) {
  this->check_tokens(u8"await"_sv, {token_type::kw_await});
  this->check_tokens(u8"break"_sv, {token_type::kw_break});
  this->check_tokens(u8"case"_sv, {token_type::kw_case});
  this->check_tokens(u8"catch"_sv, {token_type::kw_catch});
  this->check_tokens(u8"class"_sv, {token_type::kw_class});
  this->check_tokens(u8"const"_sv, {token_type::kw_const});
  this->check_tokens(u8"continue"_sv, {token_type::kw_continue});
  this->check_tokens(u8"debugger"_sv, {token_type::kw_debugger});
  this->check_tokens(u8"default"_sv, {token_type::kw_default});
  this->check_tokens(u8"delete"_sv, {token_type::kw_delete});
  this->check_tokens(u8"do"_sv, {token_type::kw_do});
  this->check_tokens(u8"else"_sv, {token_type::kw_else});
  this->check_tokens(u8"enum"_sv, {token_type::kw_enum});
  this->check_tokens(u8"export"_sv, {token_type::kw_export});
  this->check_tokens(u8"extends"_sv, {token_type::kw_extends});
  this->check_tokens(u8"false"_sv, {token_type::kw_false});
  this->check_tokens(u8"finally"_sv, {token_type::kw_finally});
  this->check_tokens(u8"for"_sv, {token_type::kw_for});
  this->check_tokens(u8"function"_sv, {token_type::kw_function});
  this->check_tokens(u8"if"_sv, {token_type::kw_if});
  this->check_tokens(u8"implements"_sv, {token_type::kw_implements});
  this->check_tokens(u8"import"_sv, {token_type::kw_import});
  this->check_tokens(u8"in"_sv, {token_type::kw_in});
  this->check_tokens(u8"instanceof"_sv, {token_type::kw_instanceof});
  this->check_tokens(u8"interface"_sv, {token_type::kw_interface});
  this->check_tokens(u8"new"_sv, {token_type::kw_new});
  this->check_tokens(u8"null"_sv, {token_type::kw_null});
  this->check_tokens(u8"package"_sv, {token_type::kw_package});
  this->check_tokens(u8"private"_sv, {token_type::kw_private});
  this->check_tokens(u8"protected"_sv, {token_type::kw_protected});
  this->check_tokens(u8"public"_sv, {token_type::kw_public});
  this->check_tokens(u8"return"_sv, {token_type::kw_return});
  this->check_tokens(u8"super"_sv, {token_type::kw_super});
  this->check_tokens(u8"switch"_sv, {token_type::kw_switch});
  this->check_tokens(u8"this"_sv, {token_type::kw_this});
  this->check_tokens(u8"throw"_sv, {token_type::kw_throw});
  this->check_tokens(u8"true"_sv, {token_type::kw_true});
  this->check_tokens(u8"try"_sv, {token_type::kw_try});
  this->check_tokens(u8"typeof"_sv, {token_type::kw_typeof});
  this->check_tokens(u8"var"_sv, {token_type::kw_var});
  this->check_tokens(u8"void"_sv, {token_type::kw_void});
  this->check_tokens(u8"while"_sv, {token_type::kw_while});
  this->check_tokens(u8"with"_sv, {token_type::kw_with});
  this->check_tokens(u8"yield"_sv, {token_type::kw_yield});
}

TEST_F(test_lex, lex_contextual_keywords) {
  this->check_tokens(u8"as"_sv, {token_type::kw_as});
  this->check_tokens(u8"async"_sv, {token_type::kw_async});
  this->check_tokens(u8"from"_sv, {token_type::kw_from});
  this->check_tokens(u8"get"_sv, {token_type::kw_get});
  this->check_tokens(u8"let"_sv, {token_type::kw_let});
  this->check_tokens(u8"of"_sv, {token_type::kw_of});
  this->check_tokens(u8"set"_sv, {token_type::kw_set});
  this->check_tokens(u8"static"_sv, {token_type::kw_static});
}

TEST_F(
    test_lex,
    lex_reserved_keywords_except_await_and_yield_sometimes_cannot_contain_escape_sequences) {
  // TODO(#73): Also lex 'protected', 'implements', etc. as
  // reserved_keyword_with_escape_sequence in strict mode.
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    padded_string code(escape_first_character_in_keyword(keyword));
    SCOPED_TRACE(code);
    diag_collector errors;
    lexer& l = this->make_lexer(&code, &errors);

    EXPECT_THAT(l.peek().type,
                token_type::reserved_keyword_with_escape_sequence);
    EXPECT_THAT(l.peek().identifier_name().normalized_name(), keyword);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.peek().report_errors_for_escape_sequences_in_keyword(&errors);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_keywords_cannot_contain_escape_sequences,  //
                    escape_sequence, 0, u8"\\u{??}")));
  }
}

TEST_F(
    test_lex,
    lex_contextual_keywords_and_await_and_yield_can_contain_escape_sequences) {
  for (string8 keyword : contextual_keywords) {
    string8 code = escape_first_character_in_keyword(keyword);
    SCOPED_TRACE(out_string8(code));
    SCOPED_TRACE(out_string8(keyword));
    this->check_single_token(code, keyword);
  }
}

TEST_F(test_lex, lex_single_character_symbols) {
  this->check_tokens(u8"+"_sv, {token_type::plus});
  this->check_tokens(u8"-"_sv, {token_type::minus});
  this->check_tokens(u8"*"_sv, {token_type::star});
  this->check_tokens(u8"/"_sv, {token_type::slash});
  this->check_tokens(u8"<"_sv, {token_type::less});
  this->check_tokens(u8">"_sv, {token_type::greater});
  this->check_tokens(u8"="_sv, {token_type::equal});
  this->check_tokens(u8"&"_sv, {token_type::ampersand});
  this->check_tokens(u8"^"_sv, {token_type::circumflex});
  this->check_tokens(u8"!"_sv, {token_type::bang});
  this->check_tokens(u8"."_sv, {token_type::dot});
  this->check_tokens(u8","_sv, {token_type::comma});
  this->check_tokens(u8"~"_sv, {token_type::tilde});
  this->check_tokens(u8"%"_sv, {token_type::percent});
  this->check_tokens(u8"("_sv, {token_type::left_paren});
  this->check_tokens(u8")"_sv, {token_type::right_paren});
  this->check_tokens(u8"["_sv, {token_type::left_square});
  this->check_tokens(u8"]"_sv, {token_type::right_square});
  this->check_tokens(u8"{"_sv, {token_type::left_curly});
  this->check_tokens(u8"}"_sv, {token_type::right_curly});
  this->check_tokens(u8":"_sv, {token_type::colon});
  this->check_tokens(u8";"_sv, {token_type::semicolon});
  this->check_tokens(u8"?"_sv, {token_type::question});
  this->check_tokens(u8"|"_sv, {token_type::pipe});
}

TEST_F(test_lex, lex_multi_character_symbols) {
  this->check_tokens(u8"<="_sv, {token_type::less_equal});
  this->check_tokens(u8">="_sv, {token_type::greater_equal});
  this->check_tokens(u8"=="_sv, {token_type::equal_equal});
  this->check_tokens(u8"==="_sv, {token_type::equal_equal_equal});
  this->check_tokens(u8"!="_sv, {token_type::bang_equal});
  this->check_tokens(u8"!=="_sv, {token_type::bang_equal_equal});
  this->check_tokens(u8"**"_sv, {token_type::star_star});
  this->check_tokens(u8"++"_sv, {token_type::plus_plus});
  this->check_tokens(u8"--"_sv, {token_type::minus_minus});
  this->check_tokens(u8"<<"_sv, {token_type::less_less});
  this->check_tokens(u8">>"_sv, {token_type::greater_greater});
  this->check_tokens(u8">>>"_sv, {token_type::greater_greater_greater});
  this->check_tokens(u8"&&"_sv, {token_type::ampersand_ampersand});
  this->check_tokens(u8"||"_sv, {token_type::pipe_pipe});
  this->check_tokens(u8"+="_sv, {token_type::plus_equal});
  this->check_tokens(u8"-="_sv, {token_type::minus_equal});
  this->check_tokens(u8"*="_sv, {token_type::star_equal});
  this->check_tokens(u8"/="_sv, {token_type::slash_equal});
  this->check_tokens(u8"%="_sv, {token_type::percent_equal});
  this->check_tokens(u8"**="_sv, {token_type::star_star_equal});
  this->check_tokens(u8"&&="_sv, {token_type::ampersand_ampersand_equal});
  this->check_tokens(u8"&="_sv, {token_type::ampersand_equal});
  this->check_tokens(u8"?."_sv, {token_type::question_dot});
  this->check_tokens(u8"??"_sv, {token_type::question_question});
  this->check_tokens(u8"?\x3f="_sv, {token_type::question_question_equal});
  this->check_tokens(u8"^="_sv, {token_type::circumflex_equal});
  this->check_tokens(u8"|="_sv, {token_type::pipe_equal});
  this->check_tokens(u8"||="_sv, {token_type::pipe_pipe_equal});
  this->check_tokens(u8"<<="_sv, {token_type::less_less_equal});
  this->check_tokens(u8">>="_sv, {token_type::greater_greater_equal});
  this->check_tokens(u8">>>="_sv, {token_type::greater_greater_greater_equal});
  this->check_tokens(u8"=>"_sv, {token_type::equal_greater});
  this->check_tokens(u8"..."_sv, {token_type::dot_dot_dot});
}

TEST_F(test_lex, lex_adjacent_symbols) {
  this->check_tokens(u8"{}"_sv,
                     {token_type::left_curly, token_type::right_curly});
  this->check_tokens(u8"[]"_sv,
                     {token_type::left_square, token_type::right_square});
  this->check_tokens(u8"/!"_sv, {token_type::slash, token_type::bang});
  this->check_tokens(u8"*=="_sv, {token_type::star_equal, token_type::equal});
  this->check_tokens(u8"^>>"_sv,
                     {token_type::circumflex, token_type::greater_greater});
}

TEST_F(test_lex, lex_symbols_separated_by_whitespace) {
  this->check_tokens(u8"{ }"_sv,
                     {token_type::left_curly, token_type::right_curly});
  this->check_tokens(u8"< ="_sv, {token_type::less, token_type::equal});
  this->check_tokens(u8"? ."_sv, {token_type::question, token_type::dot});
  this->check_tokens(u8". . ."_sv,
                     {token_type::dot, token_type::dot, token_type::dot});
}

TEST_F(test_lex, question_followed_by_number_is_not_question_dot) {
  this->check_tokens(u8"?.3"_sv, {token_type::question, token_type::number});
}

TEST_F(test_lex, question_dot_followed_by_non_digit_is_question_dot) {
  this->check_tokens(u8"?.e"_sv,
                     {token_type::question_dot, token_type::identifier});
}

TEST_F(test_lex, lex_whitespace) {
  for (const char8* whitespace : {
           u8"\n",      //
           u8"\r",      //
           u8"\r\n",    //
           u8"\u2028",  // 0xe2 0x80 0xa8 Line Separator
           u8"\u2029",  // 0xe2 0x80 0xa9 Paragraph Separator
           u8" ",       //
           u8"\t",      //
           u8"\f",      //
           u8"\v",      //
           u8"\u00a0",  // 0xc2 0xa0      No-Break Space (NBSP)
           u8"\u1680",  // 0xe1 0x9a 0x80 Ogham Space Mark
           u8"\u2000",  // 0xe2 0x80 0x80 En Quad
           u8"\u2001",  // 0xe2 0x80 0x81 Em Quad
           u8"\u2002",  // 0xe2 0x80 0x82 En Space
           u8"\u2003",  // 0xe2 0x80 0x83 Em Space
           u8"\u2004",  // 0xe2 0x80 0x84 Three-Per-Em Space
           u8"\u2005",  // 0xe2 0x80 0x85 Four-Per-Em Space
           u8"\u2006",  // 0xe2 0x80 0x86 Six-Per-Em Space
           u8"\u2007",  // 0xe2 0x80 0x87 Figure Space
           u8"\u2008",  // 0xe2 0x80 0x88 Punctuation Space
           u8"\u2009",  // 0xe2 0x80 0x89 Thin Space
           u8"\u200a",  // 0xe2 0x80 0x8a Hair Space
           u8"\u202f",  // 0xe2 0x80 0xaf Narrow No-Break Space (NNBSP)
           u8"\u205f",  // 0xe2 0x81 0x9f Medium Mathematical Space (MMSP)
           u8"\u3000",  // 0xe3 0x80 0x80 Ideographic Space
           u8"\ufeff",  // 0xef 0xbb 0xbf Zero Width No-Break Space (BOM,
                        // ZWNBSP)
       }) {
    {
      string8 input = string8(u8"a") + whitespace + u8"b";
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input.c_str(),
                         {token_type::identifier, token_type::identifier});
    }

    {
      string8 input =
          string8(whitespace) + u8"10" + whitespace + u8"'hi'" + whitespace;
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input.c_str(),
                         {token_type::number, token_type::string});
    }

    {
      string8 input =
          string8(u8"async") + whitespace + u8"function" + whitespace;
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input.c_str(),
                         {token_type::kw_async, token_type::kw_function});
    }
  }
}

TEST_F(test_lex, lex_shebang) {
  this->check_tokens(u8"#!/usr/bin/env node\nhello"_sv,
                     {token_type::identifier});
  this->check_tokens(u8"#!ignored\n123"_sv, {token_type::number});
}

TEST_F(test_lex, lex_not_shebang) {
  // Whitespace must not appear between '#' and '!'.
  {
    diag_collector v;
    padded_string input(u8"# !notashebang"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_hash_character,  //
                              where, 0, u8"#")));
  }

  // '#!' must be on the first line.
  {
    diag_collector v;
    padded_string input(u8"\n#!notashebang\n"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_hash_character,  //
                              where, strlen(u8"\n"), u8"#")));
  }

  // Whitespace must not appear before '#!'.
  {
    diag_collector v;
    padded_string input(u8"  #!notashebang\n"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_hash_character,  //
                              where, strlen(u8"  "), u8"#")));
  }

  {
    diag_collector v;
    padded_string input(u8"#\\u{21}\n"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::private_identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"#\\u{21}");

    EXPECT_THAT(
        v.errors,
        ElementsAre(DIAG_TYPE_OFFSETS(
            &input, diag_escaped_character_disallowed_in_identifiers,  //
            escape_sequence, strlen(u8"#"), u8"\\u{21}")));
  }
}

TEST_F(test_lex, lex_unexpected_bom_before_shebang) {
  // BOM must not appear before '#!'.
  {
    diag_collector v;
    padded_string input(u8"\ufeff#!notashebang\n"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::end_of_file) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_bom_before_shebang,  //
                              bom, 0, u8"\ufeff")));
  }
}

TEST_F(test_lex, lex_invalid_common_characters_are_disallowed) {
  {
    diag_collector v;
    padded_string input(u8"hello @ world"_sv);
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier) << "@ should be skipped";
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_at_character,  //
                              character, strlen(u8"hello "), u8"@")));
  }
}

TEST_F(test_lex, ascii_control_characters_are_disallowed) {
  for (string8_view control_character : control_characters_except_whitespace) {
    padded_string input(string8(control_character) + u8"hello");
    SCOPED_TRACE(input);
    diag_collector v;

    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier)
        << "control character should be skipped";
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &input, diag_unexpected_control_character,  //
                              character, 0, control_character)));
  }
}

TEST_F(test_lex, ascii_control_characters_sorta_treated_like_whitespace) {
  for (string8_view control_character : control_characters_except_whitespace) {
    padded_string input(u8"  " + string8(control_character) + u8"  hello");
    SCOPED_TRACE(input);
    diag_collector v;
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier)
        << "control character should be skipped";
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }
}

TEST_F(test_lex, lex_token_notes_leading_newline) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"a b" + string8(line_terminator) + u8"c d");
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // b
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // c
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // d
  }
}

TEST_F(test_lex, lex_token_notes_leading_newline_after_single_line_comment) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"a // hello" + string8(line_terminator) + u8"b");
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // b
  }
}

TEST_F(test_lex, lex_token_notes_leading_newline_after_comment_with_newline) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"a /*" + string8(line_terminator) + u8"*/ b");
    lexer l(&code, &null_diag_reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // b
  }
}

TEST_F(test_lex, lex_token_notes_leading_newline_after_comment) {
  padded_string code(u8"a /* comment */\nb"_sv);
  lexer l(&code, &null_diag_reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // b
}

TEST_F(test_lex, inserting_semicolon_at_newline_remembers_next_token) {
  padded_string code(u8"hello\nworld"_sv);
  lexer l(&code, &null_diag_reporter::instance);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello");
  EXPECT_FALSE(l.peek().has_leading_newline);
  const char8* hello_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, hello_end);
  EXPECT_EQ(l.peek().end, hello_end);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

TEST_F(test_lex, insert_semicolon_at_beginning_of_input) {
  padded_string code(u8"hello world"_sv);
  lexer l(&code, &null_diag_reporter::instance);

  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, code.data());
  EXPECT_EQ(l.peek().end, code.data());

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello");

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world");

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

TEST_F(test_lex, inserting_semicolon_at_right_curly_remembers_next_token) {
  padded_string code(u8"{ x }"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::left_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"x");
  EXPECT_FALSE(l.peek().has_leading_newline);
  const char8* x_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::right_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, x_end);
  EXPECT_EQ(l.peek().end, x_end);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::right_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  EXPECT_THAT(errors.errors, IsEmpty());
}

TEST_F(test_lex, transaction_buffers_errors_until_commit) {
  padded_string code(u8"x 0b y"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(transaction));
  EXPECT_THAT(errors.errors,
              ElementsAre(DIAG_TYPE_FIELD(diag_no_digits_in_binary_number,
                                          characters, testing::_)));
}

TEST_F(test_lex, nested_transaction_buffers_errors_until_outer_commit) {
  padded_string code(u8"x y 0b z"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // y

  lexer_transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);  // 0b
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(inner_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "committing inner_transaction should not report 0b error";

  l.commit_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors,
              ElementsAre(DIAG_TYPE_FIELD(diag_no_digits_in_binary_number,
                                          characters, testing::_)))
      << "committing outer_transaction should report 0b error";
}

TEST_F(test_lex, rolled_back_inner_transaction_discards_errors) {
  padded_string code(u8"x y 0b z"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // y

  lexer_transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);  // 0b

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.roll_back_transaction(std::move(inner_transaction));
  l.commit_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";
}

TEST_F(test_lex, rolled_back_outer_transaction_discards_errors) {
  padded_string code(u8"x y 0b z"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // y

  lexer_transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);  // 0b

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(inner_transaction));
  l.roll_back_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";
}

TEST_F(test_lex, errors_after_transaction_commit_are_reported_unbuffered) {
  padded_string code(u8"x 'y' 0b"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  l.commit_transaction(std::move(transaction));
  EXPECT_EQ(l.peek().type, token_type::string);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);
  EXPECT_THAT(errors.errors,
              ElementsAre(DIAG_TYPE_FIELD(diag_no_digits_in_binary_number,
                                          characters, testing::_)));
}

TEST_F(test_lex, errors_after_transaction_rollback_are_reported_unbuffered) {
  padded_string code(u8"x 'y' 0b"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  l.roll_back_transaction(std::move(transaction));

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);
  EXPECT_THAT(errors.errors,
              ElementsAre(DIAG_TYPE_FIELD(diag_no_digits_in_binary_number,
                                          characters, testing::_)));
}

TEST_F(test_lex, rolling_back_transaction) {
  padded_string code(u8"x 'y' 3"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  lexer_transaction transaction = l.begin_transaction();
  EXPECT_EQ(l.peek().type, token_type::identifier);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);

  l.roll_back_transaction(std::move(transaction));
  EXPECT_EQ(l.peek().type, token_type::identifier);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);
}

TEST_F(test_lex, insert_semicolon_after_rolling_back_transaction) {
  padded_string code(u8"x 'y' 3"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  lexer_transaction transaction = l.begin_transaction();

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);

  l.roll_back_transaction(std::move(transaction));
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, token_type::number);
}

TEST_F(test_lex, unfinished_transaction_does_not_leak_memory) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  padded_string code(u8"a b c d e f g"_sv);
  diag_collector errors;
  lexer l(&code, &errors);

  [[maybe_unused]] lexer_transaction outer_transaction = l.begin_transaction();
  l.skip();

  [[maybe_unused]] lexer_transaction inner_transaction = l.begin_transaction();
  l.skip();

  // Don't end either transaction. The leak checker should report no leaks.
}

TEST_F(test_lex,
       is_initial_identifier_byte_agrees_with_is_initial_identifier_character) {
  constexpr char32_t min_code_point = U'\0';
  constexpr char32_t max_code_point = U'\U0010ffff';

  std::array<bool, 256> is_valid_byte = {};
  is_valid_byte[u8'\\'] = true;
  for (char32_t c = min_code_point; c <= max_code_point; ++c) {
    if (lexer::is_initial_identifier_character(c)) {
      char8 utf_8[10];
      encode_utf_8(c, utf_8);
      is_valid_byte[static_cast<std::uint8_t>(utf_8[0])] = true;
    }
  }

  for (std::size_t byte = 0; byte < is_valid_byte.size(); ++byte) {
    EXPECT_EQ(lexer::is_initial_identifier_byte(static_cast<char8>(byte)),
              is_valid_byte[byte])
        << "byte = 0x" << std::hex << byte;
  }
}

TEST_F(test_lex, is_identifier_byte_agrees_with_is_identifier_character) {
  constexpr char32_t min_code_point = U'\0';
  constexpr char32_t max_code_point = U'\U0010ffff';

  std::array<bool, 256> is_valid_byte = {};
  is_valid_byte[u8'\\'] = true;
  for (char32_t c = min_code_point; c <= max_code_point; ++c) {
    if (lexer::is_identifier_character(c, lexer::identifier_kind::javascript)) {
      char8 utf_8[10];
      encode_utf_8(c, utf_8);
      is_valid_byte[static_cast<std::uint8_t>(utf_8[0])] = true;
    }
  }

  for (std::size_t byte = 0; byte < is_valid_byte.size(); ++byte) {
    EXPECT_EQ(lexer::is_identifier_byte(static_cast<char8>(byte)),
              is_valid_byte[byte])
        << "byte = 0x" << std::hex << byte;
  }
}

TEST_F(test_lex, jsx_identifier) {
  auto check_identifier = [](string8_view tag_code,
                             string8_view expected_normalized) -> void {
    SCOPED_TRACE(out_string8(tag_code));

    padded_string code(u8"!" + string8(tag_code));
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(),
              expected_normalized);

    EXPECT_THAT(errors.errors, IsEmpty());
  };

  check_identifier(u8"div"_sv, u8"div"_sv);
  check_identifier(u8"MyComponent"_sv, u8"MyComponent"_sv);
  check_identifier(u8"my-web-component"_sv, u8"my-web-component"_sv);
  check_identifier(u8"MY-WEB-COMPONENT"_sv, u8"MY-WEB-COMPONENT"_sv);
  check_identifier(u8"test-0"_sv, u8"test-0"_sv);
  check_identifier(u8"_component_"_sv, u8"_component_"_sv);
  check_identifier(u8"$component$"_sv, u8"$component$"_sv);
  check_identifier(u8"test-"_sv, u8"test-"_sv);

  // NOTE(strager): Babel [1] and some other tools reject these. TypeScript
  // allows these. My reading of the spec is that these are allowed.
  //
  // [1] https://github.com/babel/babel/issues/14060
  check_identifier(u8"\\u{48}ello"_sv, u8"Hello"_sv);
  check_identifier(u8"\\u{68}ello-world"_sv, u8"hello-world"_sv);

  check_identifier(u8" div "_sv, u8"div"_sv);
  check_identifier(u8"/**/div/**/"_sv, u8"div"_sv);
  check_identifier(u8" banana-split "_sv, u8"banana-split"_sv);
  check_identifier(u8"/**/banana-split/**/"_sv, u8"banana-split"_sv);

  for (string8_view line_terminator : line_terminators) {
    check_identifier(
        string8(line_terminator) + u8"banana-split" + string8(line_terminator),
        u8"banana-split"_sv);
  }

  check_identifier(u8"<!-- line comment\nbanana-split"_sv, u8"banana-split"_sv);
  check_identifier(u8"\n--> line comment\nbanana-split"_sv,
                   u8"banana-split"_sv);

  check_identifier(u8"\u00c1gua"_sv, u8"\u00c1gua"_sv);
  check_identifier(u8"\u00c1gua-"_sv, u8"\u00c1gua-"_sv);
}

TEST_F(test_lex, invalid_jsx_identifier) {
  this->lex_jsx_tokens = true;

  this->check_tokens_with_errors(
      u8"<hello\\u{2d}world>",
      {token_type::less, token_type::identifier, token_type::greater},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(DIAG_TYPE_OFFSETS(
                        input, diag_escaped_hyphen_not_allowed_in_jsx_tag,  //
                        escape_sequence, strlen(u8"<hello"), u8"\\u{2d}")));
      });
}

TEST_F(test_lex, jsx_string) {
  auto check_string = [](string8_view string_code) -> void {
    SCOPED_TRACE(out_string8(string_code));

    padded_string code(u8"!" + string8(string_code));
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, token_type::string);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  };

  check_string(u8R"('hello')"sv);
  check_string(u8R"("hello")"sv);

  // Backslashes are ignored.
  check_string(u8R"("hello\nworld")"sv);
  check_string(u8R"("hello\")"sv);
  check_string(u8R"("hello\\")"sv);
  check_string(u8R"('hello\')"sv);
  check_string(u8R"('hello\\')"sv);
  check_string(u8R"('hello\u{}world')"sv);
  check_string(u8R"('hello\u00xyworld')"sv);
  check_string(u8R"('hello\u00')"sv);

  // Null bytes are allowed.
  check_string(u8"'hello\u0000world'"sv);
  check_string(u8"'\u0000world'"sv);
  check_string(u8"'hello\u0000'"sv);

  // Line terminators are allowed.
  for (string8_view line_terminator : line_terminators) {
    check_string(u8"'hello" + string8(line_terminator) + u8"world'");
  }
}

// Despite what the JSX specification says, comments are not interpreted in
// attribute strings.
// https://github.com/facebook/jsx/pull/133
TEST_F(test_lex, jsx_string_ignores_comments) {
  {
    padded_string code(u8"! 'hello // '\nworld'"sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, token_type::string);
    EXPECT_EQ(l.peek().begin, &code[strlen(u8"! ")]);
    EXPECT_EQ(l.peek().end, &code[strlen(u8"! 'hello // '")])
        << "string should end at ', treating // as part of the string";

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world");

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(! "hello/* not"comment */world")"sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, token_type::string);
    EXPECT_EQ(l.peek().begin, &code[strlen(u8"! ")]);
    EXPECT_EQ(l.peek().end, &code[strlen(u8R"(! "hello/* not")")])
        << "string should end at \", treating /* as part of the string";

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"comment");

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(test_lex, unterminated_jsx_string) {
  padded_string code(u8"! 'hello"sv);
  diag_collector errors;
  lexer l(&code, &errors);
  l.skip_in_jsx();  // Ignore '!'.

  EXPECT_EQ(l.peek().type, token_type::string);
  EXPECT_THAT(errors.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                                 &code,
                                 diag_unclosed_jsx_string_literal,  //
                                 string_literal_begin, strlen(u8"! "), u8"'")));

  l.skip_in_jsx();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

TEST_F(test_lex, jsx_tag) {
  {
    padded_string code(u8"<svg:rect>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"svg"sv);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::colon);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"rect"sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    padded_string code(u8"<myModule.MyComponent>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"myModule"sv);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::dot);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"MyComponent"sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(test_lex, jsx_text_children) {
  {
    padded_string code(u8"<>hello world"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.

    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    padded_string code(u8"<>hello</>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.

    EXPECT_EQ(l.peek().type, token_type::less);
    EXPECT_EQ(l.peek().begin, &code[strlen(u8"<>hello")]);
    EXPECT_EQ(l.peek().end, &code[strlen(u8"<>hello<")]);
    EXPECT_THAT(errors.errors, IsEmpty());
  }

  // '>=' might be interpreted as greater_equal by a buggy lexer, for example.
  for (string8 text_begin : {u8"=", u8">", u8">>", u8">=", u8">>="}) {
    padded_string code(u8"<>" + text_begin + u8"hello");
    SCOPED_TRACE(code);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, token_type::greater);
    EXPECT_EQ(l.peek().begin, &code[strlen(u8"<")]);
    EXPECT_EQ(l.peek().end, &code[strlen(u8"<>")]);
    l.skip_in_jsx_children();

    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    if (text_begin == u8"=") {
      EXPECT_THAT(errors.errors, IsEmpty());
    } else if (text_begin == u8">" || text_begin == u8">=") {
      EXPECT_THAT(errors.errors,
                  ElementsAre(DIAG_TYPE_OFFSETS(
                      &code, diag_unexpected_greater_in_jsx_text,  //
                      greater, strlen(u8"<>"), u8">")));
    } else if (text_begin == u8">>" || text_begin == u8">>=") {
      EXPECT_THAT(
          errors.errors,
          ElementsAre(
              DIAG_TYPE_OFFSETS(&code, diag_unexpected_greater_in_jsx_text,  //
                                greater, strlen(u8"<>"), u8">"),
              DIAG_TYPE_OFFSETS(&code, diag_unexpected_greater_in_jsx_text,  //
                                greater, strlen(u8"<>>"), u8">")));
    } else {
      QLJS_UNREACHABLE();
    }
  }
}

TEST_F(test_lex, jsx_illegal_text_children) {
  {
    padded_string code(u8"<>hello>world</>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.
    EXPECT_EQ(l.peek().type, token_type::less);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_greater_in_jsx_text,  //
                    greater, strlen(u8"<>hello"), u8">")));
  }

  {
    padded_string code(u8"<>hello}world</>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.
    EXPECT_EQ(l.peek().type, token_type::less);
    EXPECT_THAT(errors.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code, diag_unexpected_right_curly_in_jsx_text,  //
                    right_curly, strlen(u8"<>hello"), u8"}")));
  }
}

TEST_F(test_lex, jsx_expression_children) {
  this->lex_jsx_tokens = true;

  {
    padded_string code(u8"<>hello {name}!</>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);

    // <>hello
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);

    // {name}
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, token_type::left_curly);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);

    // !</>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(test_lex, jsx_nested_children) {
  this->lex_jsx_tokens = true;

  {
    padded_string code(u8"<>hello <span>world</span>!</>"_sv);
    diag_collector errors;
    lexer l(&code, &errors);
    // <>hello
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);

    // <span>world</span>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);

    // !</>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, token_type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, token_type::greater);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

void test_lex::check_single_token(string8_view input,
                                  string8_view expected_identifier_name,
                                  source_location local_caller) {
  static source_location caller;
  caller = local_caller;
  this->check_single_token_with_errors(
      input, expected_identifier_name,
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void test_lex::check_single_token_with_errors(
    string8_view input, string8_view expected_identifier_name,
    void (*check_errors)(padded_string_view input,
                         const std::vector<diag_collector::diag>&),
    source_location caller) {
  padded_string code(input);
  diag_collector errors;
  std::vector<token> lexed_tokens = this->lex_to_eof(&code, errors);

  EXPECT_THAT_AT_CALLER(lexed_tokens,
                        ElementsAre(::testing::Field(
                            "type", &token::type,
                            ::testing::AnyOf(token_type::identifier,
                                             token_type::private_identifier))));
  if (lexed_tokens.size() == 1 &&
      (lexed_tokens[0].type == token_type::identifier ||
       lexed_tokens[0].type == token_type::private_identifier)) {
    EXPECT_EQ_AT_CALLER(lexed_tokens[0].identifier_name().normalized_name(),
                        expected_identifier_name);
  }
  check_errors(&code, errors.errors);
}

void test_lex::check_tokens(
    string8_view input, std::initializer_list<token_type> expected_token_types,
    source_location local_caller) {
  static source_location caller;
  caller = local_caller;
  this->check_tokens_with_errors(
      input, expected_token_types,
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void test_lex::check_tokens(
    padded_string_view input,
    std::initializer_list<token_type> expected_token_types,
    source_location local_caller) {
  static source_location caller;
  caller = local_caller;
  this->check_tokens_with_errors(
      input, expected_token_types,
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void test_lex::check_tokens_with_errors(
    string8_view input, std::initializer_list<token_type> expected_token_types,
    std::function<void(padded_string_view input,
                       const std::vector<diag_collector::diag>&)>
        check_errors,
    source_location caller) {
  padded_string code(input);
  return this->check_tokens_with_errors(&code, expected_token_types,
                                        check_errors, caller);
}

void test_lex::check_tokens_with_errors(
    padded_string_view input,
    std::initializer_list<token_type> expected_token_types,
    std::function<void(padded_string_view input,
                       const std::vector<diag_collector::diag>&)>
        check_errors,
    source_location caller) {
  diag_collector errors;
  std::vector<token> lexed_tokens = this->lex_to_eof(input, errors);

  std::vector<token_type> lexed_token_types;
  for (const token& t : lexed_tokens) {
    lexed_token_types.push_back(t.type);
  }

  EXPECT_THAT_AT_CALLER(lexed_token_types,
                        ::testing::ElementsAreArray(expected_token_types));
  check_errors(input, errors.errors);
}

std::vector<token> test_lex::lex_to_eof(padded_string_view input,
                                        source_location caller) {
  diag_collector errors;
  std::vector<token> tokens = this->lex_to_eof(input, errors);
  EXPECT_THAT_AT_CALLER(errors.errors, IsEmpty());
  return tokens;
}

std::vector<token> test_lex::lex_to_eof(padded_string_view input,
                                        diag_collector& errors) {
  lexer& l = this->make_lexer(input, &errors);
  std::vector<token> tokens;
  while (l.peek().type != token_type::end_of_file) {
    tokens.push_back(l.peek());
    if (this->lex_jsx_tokens) {
      l.skip_in_jsx();
    } else {
      l.skip();
    }
  }
  return tokens;
}

std::vector<token> test_lex::lex_to_eof(string8_view input,
                                        source_location caller) {
  padded_string real_input(input);
  return this->lex_to_eof(&real_input, caller);
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
