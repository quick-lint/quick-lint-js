// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/source-location.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-8.h>
#include <string_view>
#include <type_traits>
#include <vector>

using namespace std::literals::string_view_literals;

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
class Test_Lex : public ::testing::Test {
 protected:
  // NOTE(strager): These functions take callbacks as function pointers to
  // reduce build times. Templates and std::function are slow to compile.
  void check_single_token(String8_View input,
                          String8_View expected_identifier_name,
                          Source_Location = Source_Location::current());
  void check_single_token_with_errors(
      String8_View input, String8_View expected_identifier_name,
      void (*check_errors)(Padded_String_View input,
                           const std::vector<Diag_Collector::Diag>&),
      Source_Location = Source_Location::current());
  void check_single_token_with_errors(
      String8_View input, Diagnostic_Assertion,
      String8_View expected_identifier_name,
      Source_Location = Source_Location::current());
  void check_tokens(String8_View input,
                    std::initializer_list<Token_Type> expected_token_types,
                    Source_Location = Source_Location::current());
  void check_tokens(Padded_String_View input,
                    std::initializer_list<Token_Type> expected_token_types,
                    Source_Location = Source_Location::current());
  void check_tokens_with_errors(
      String8_View input, Diagnostic_Assertion,
      std::initializer_list<Token_Type> expected_token_types,
      Source_Location = Source_Location::current());
  void check_tokens_with_errors(
      String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
      std::initializer_list<Token_Type> expected_token_types,
      Source_Location = Source_Location::current());
  void check_tokens_with_errors(
      String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
      Diagnostic_Assertion,
      std::initializer_list<Token_Type> expected_token_types,
      Source_Location = Source_Location::current());
  void check_tokens_with_errors(
      String8_View input, Span<const Diagnostic_Assertion>,
      std::initializer_list<Token_Type> expected_token_types, Source_Location);
  void check_tokens_with_errors(
      String8_View input,
      std::initializer_list<Token_Type> expected_token_types,
      void (*check_errors)(Padded_String_View input,
                           const std::vector<Diag_Collector::Diag>&),
      Source_Location = Source_Location::current());
  void check_tokens_with_errors(
      Padded_String_View input,
      std::initializer_list<Token_Type> expected_token_types,
      void (*check_errors)(Padded_String_View input,
                           const std::vector<Diag_Collector::Diag>&),
      Source_Location = Source_Location::current());
  std::vector<Token> lex_to_eof(Padded_String_View, Diag_Collector&);
  std::vector<Token> lex_to_eof(Padded_String_View,
                                Source_Location = Source_Location::current());
  std::vector<Token> lex_to_eof(String8_View,
                                Source_Location = Source_Location::current());

  Lexer& make_lexer(Padded_String_View input, Diag_Collector* errors) {
    return this->lexers_.emplace_back(input, errors);
  }

  // If true, tell check_tokens_with_errors, lex_to_eof, etc. to call
  // Lexer::skip_in_jsx instead of Lexer::skip. This has no effect on the first
  // token.
  bool lex_jsx_tokens = false;

 private:
  Linked_Vector<Lexer> lexers_ = Linked_Vector<Lexer>(new_delete_resource());
};

TEST_F(Test_Lex, lex_block_comments) {
  this->check_single_token(u8"/* */ hello"_sv, u8"hello"_sv);
  this->check_single_token(u8"/*/ comment */ hi"_sv, u8"hi"_sv);
  this->check_single_token(u8"/* comment /*/ hi"_sv, u8"hi"_sv);
  this->check_single_token(u8"/* not /* nested */ ident"_sv, u8"ident"_sv);
  EXPECT_THAT(this->lex_to_eof(u8"/**/"_sv), IsEmpty());

  {
    Diag_Collector v;
    Padded_String input(u8"hello /* unterminated comment "_sv);
    auto error = /*  */ u8"      ^^ Diag_Unclosed_Block_Comment"_diag;
    Lexer l(&input, &v);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error});
  }
}

TEST_F(Test_Lex, lex_unopened_block_comment) {
  {
    Diag_Collector v;
    Padded_String input(u8"hello */"_sv);
    auto error = /*  */ u8"      ^^ Diag_Unopened_Block_Comment"_diag;
    Lexer l(&input, &v);  // identifier
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();  // end of file
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    assert_diagnostics(&input, v.errors, {error});
  }
  {
    Diag_Collector v;
    Padded_String input(u8"*-----*/"_sv);
    auto error = /*  */ u8"      ^^ Diag_Unopened_Block_Comment"_diag;
    Lexer l(&input, &v);

    while (l.peek().type != Token_Type::end_of_file) {
      l.skip();
    }
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error});
  }
  {
    Diag_Collector v;
    Padded_String input(u8"*******/"_sv);
    auto error = /*  */ u8"      ^^ Diag_Unopened_Block_Comment"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error});
  }
  {
    Diag_Collector v;
    Padded_String input(u8"*/"_sv);
    auto error = /*  */ u8"^^ Diag_Unopened_Block_Comment"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error});
  }
  {
    Diag_Collector v;
    Padded_String input(u8"**/"_sv);
    auto error = /*  */ u8" ^^ Diag_Unopened_Block_Comment"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    assert_diagnostics(&input, v.errors, {error});
  }
}

TEST_F(Test_Lex, lex_regexp_literal_starting_with_star_slash) {
  {
    // '/*' is not an end of block comment because it precedes a regexp literal
    Diag_Collector v;
    Padded_String input(u8"*/ hello/"_sv);
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[input.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, lex_regexp_literal_starting_with_star_star_slash) {
  {
    Padded_String input(u8"3 **/ banana/"_sv);
    Diag_Collector v;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::star_star);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &input[4]);
    EXPECT_EQ(l.peek().end, &input[input.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, lex_line_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"// hello"_sv), IsEmpty());
  for (String8_View line_terminator : line_terminators) {
    this->check_single_token(
        concat(u8"// hello"_sv, line_terminator, u8"world"_sv), u8"world"_sv);
  }
  EXPECT_THAT(this->lex_to_eof(u8"// hello\n// world"_sv), IsEmpty());
  this->check_tokens(u8"hello//*/\n \n \nworld"_sv,
                     {Token_Type::identifier, Token_Type::identifier});

  /**
   * Also test for a unicode sign that starts with 0xe280, because the
   * skip_line_comment() will also look for U+2028 and U+2029
   *  > U+2028 Line Separator      (0xe280a8)
   *  > U+2029 Paragraph Separator (0xe280a9)
   *  > U+2030 Per Mille Sign      (0xe280b0)
   */
  EXPECT_THAT(this->lex_to_eof(u8"// 123‰"_sv), IsEmpty());
}

TEST_F(Test_Lex, lex_line_comments_with_control_characters) {
  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"// hello "_sv, control_character, u8" world\n42.0"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::number});
  }
}

TEST_F(Test_Lex, lex_html_open_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"<!-- --> hello"_sv), IsEmpty());
  for (String8_View line_terminator : line_terminators) {
    this->check_single_token(
        concat(u8"<!-- hello"_sv, line_terminator, u8"world"_sv), u8"world"_sv);
  }
  EXPECT_THAT(this->lex_to_eof(u8"<!-- hello\n<!-- world"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"<!--// hello"_sv), IsEmpty());
  this->check_tokens(u8"hello<!--->\n \n \nworld"_sv,
                     {Token_Type::identifier, Token_Type::identifier});
  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"<!-- hello "_sv, control_character, u8" world\n42.0"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::number});
  }

  this->check_tokens(u8"hello<!world"_sv,
                     {Token_Type::identifier, Token_Type::less,
                      Token_Type::bang, Token_Type::identifier});
  this->check_tokens(
      u8"hello<!-world"_sv,
      {Token_Type::identifier, Token_Type::less, Token_Type::bang,
       Token_Type::minus, Token_Type::identifier});
}

TEST_F(Test_Lex, lex_html_close_comments) {
  EXPECT_THAT(this->lex_to_eof(u8"--> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"     --> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"/* */--> comment"_sv), IsEmpty());
  EXPECT_THAT(this->lex_to_eof(u8"/**//**/--> comment"_sv), IsEmpty());

  for (String8_View line_terminator : line_terminators) {
    String8 eol(line_terminator);

    this->check_single_token(concat(u8"-->"_sv, eol, u8"hello"_sv),
                             u8"hello"_sv);
    this->check_single_token(concat(u8"--> comment"_sv, eol, u8"hello"_sv),
                             u8"hello"_sv);
    this->check_single_token(concat(u8"--> comment1"_sv, eol,
                                    u8"--> comment2"_sv, eol, u8"hello"_sv),
                             u8"hello"_sv);

    this->check_single_token(
        concat(u8"/*"_sv, eol, u8"*/--> comment"_sv, eol, u8"hello"_sv),
        u8"hello"_sv);
    this->check_single_token(
        concat(u8"/* */ /*"_sv, eol, u8"*/ --> comment"_sv, eol, u8"hello"_sv),
        u8"hello"_sv);
    this->check_single_token(
        concat(u8"/*"_sv, eol, u8"*/ /* */ --> comment"_sv, eol, u8"hello"_sv),
        u8"hello"_sv);
  }

  // Not an HTML close comment because non-whitespace non-comment preceeds.
  this->check_tokens(u8"3 --> 4"_sv,
                     {Token_Type::number, Token_Type::minus_minus,
                      Token_Type::greater, Token_Type::number});
}

TEST_F(Test_Lex, lex_numbers) {
  this->check_tokens(u8"0"_sv, {Token_Type::number});
  this->check_tokens(u8"2"_sv, {Token_Type::number});
  this->check_tokens(u8"42"_sv, {Token_Type::number});
  this->check_tokens(u8"12.34"_sv, {Token_Type::number});
  this->check_tokens(u8".34"_sv, {Token_Type::number});

  this->check_tokens(u8"1e3"_sv, {Token_Type::number});
  this->check_tokens(u8".1e3"_sv, {Token_Type::number});
  this->check_tokens(u8"1.e3"_sv, {Token_Type::number});
  this->check_tokens(u8"1.0e3"_sv, {Token_Type::number});
  this->check_tokens(u8"1e-3"_sv, {Token_Type::number});
  this->check_tokens(u8"1e+3"_sv, {Token_Type::number});
  this->check_tokens(u8"1E+3"_sv, {Token_Type::number});
  this->check_tokens(u8"1E123_233_22"_sv, {Token_Type::number});

  this->check_tokens(u8"0n"_sv, {Token_Type::number});
  this->check_tokens(u8"123456789n"_sv, {Token_Type::number});

  this->check_tokens(u8"123_123_123"_sv, {Token_Type::number});
  this->check_tokens(u8"123.123_123"_sv, {Token_Type::number});

  this->check_tokens(u8"123. 456"_sv, {Token_Type::number, Token_Type::number});

  this->check_tokens(u8"1.2.3"_sv, {Token_Type::number, Token_Type::number});
  this->check_tokens(u8".2.3"_sv, {Token_Type::number, Token_Type::number});
  this->check_tokens(u8"0.3"_sv, {Token_Type::number});
}

TEST_F(Test_Lex, lex_binary_numbers) {
  this->check_tokens(u8"0b0"_sv, {Token_Type::number});
  this->check_tokens(u8"0b1"_sv, {Token_Type::number});
  this->check_tokens(u8"0b010101010101010"_sv, {Token_Type::number});
  this->check_tokens(u8"0B010101010101010"_sv, {Token_Type::number});
  this->check_tokens(u8"0b01_11_00_10"_sv, {Token_Type::number});
  this->check_tokens(u8"0b01n"_sv, {Token_Type::number});

  this->check_tokens(u8"0b0.toString"_sv, {Token_Type::number, Token_Type::dot,
                                           Token_Type::identifier});
  this->check_tokens(
      u8"0b0101010101.toString"_sv,
      {Token_Type::number, Token_Type::dot, Token_Type::identifier});
}

TEST_F(Test_Lex, fail_lex_integer_loses_precision) {
  this->check_tokens_with_errors(
      u8"9007199254740993"_sv,
      u8"^^^^^^^^^^^^^^^^ Diag_Integer_Literal_Will_Lose_Precision.characters"_diag
      u8"{.rounded_val=9007199254740992}"_diag,
      {Token_Type::number});
  this->check_tokens(u8"999999999999999"_sv, {Token_Type::number});
  this->check_tokens(
      u8"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"_sv,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"_sv,
      u8"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Diag_Integer_Literal_Will_Lose_Precision.characters"_diag
      u8"{.rounded_val=inf}"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"179769313486231580000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"_sv,  //
      u8"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Diag_Integer_Literal_Will_Lose_Precision.characters"_diag
      u8"{.rounded_val=179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368}"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"179769313486231589999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"_sv,  //
      u8"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Diag_Integer_Literal_Will_Lose_Precision.characters"_diag
      u8"{.rounded_val=inf}"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"18014398509481986"_sv,  //
      u8"^^^^^^^^^^^^^^^^^ Diag_Integer_Literal_Will_Lose_Precision.characters"_diag
      u8"{.rounded_val=18014398509481984}"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, fail_lex_binary_number_no_digits) {
  this->check_tokens_with_errors(u8"0b"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Binary_Number"_diag,
                                 {Token_Type::number});
  this->check_tokens_with_errors(u8"0bn"_sv,  //
                                 u8"^^^ Diag_No_Digits_In_Binary_Number"_diag,
                                 {Token_Type::number});
  this->check_tokens_with_errors(u8"0b;"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Binary_Number"_diag,
                                 {Token_Type::number, Token_Type::semicolon});
  this->check_tokens_with_errors(
      u8"[0b]"_sv,  //
      u8" ^^ Diag_No_Digits_In_Binary_Number"_diag,
      {Token_Type::left_square, Token_Type::number, Token_Type::right_square});
}

TEST_F(Test_Lex, fail_lex_binary_number) {
  this->check_tokens_with_errors(
      u8"0b1ee"_sv,  //
      u8"   ^^ Diag_Unexpected_Characters_In_Binary_Number"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_modern_octal_numbers) {
  this->check_tokens(u8"0o51"_sv, {Token_Type::number});
  this->check_tokens(u8"0o0"_sv, {Token_Type::number});
  this->check_tokens(u8"0O0"_sv, {Token_Type::number});
  this->check_tokens(u8"0O12345670"_sv, {Token_Type::number});
  this->check_tokens(u8"0o775_775"_sv, {Token_Type::number});
  this->check_tokens(u8"0o0n"_sv, {Token_Type::number});
  this->check_tokens(u8"0o01"_sv, {Token_Type::number});
  this->check_tokens(u8"0o123n"_sv, {Token_Type::number});
}

TEST_F(Test_Lex, fail_lex_modern_octal_number_no_digits) {
  this->check_tokens_with_errors(u8"0o"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Octal_Number"_diag,
                                 {Token_Type::number});
  this->check_tokens_with_errors(u8"0o;"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Octal_Number"_diag,
                                 {Token_Type::number, Token_Type::semicolon});
  this->check_tokens_with_errors(
      u8"[0o]"_sv,  //
      u8" ^^ Diag_No_Digits_In_Octal_Number"_diag,
      {Token_Type::left_square, Token_Type::number, Token_Type::right_square});
}

TEST_F(Test_Lex, fail_lex_modern_octal_numbers) {
  this->check_tokens_with_errors(
      u8"0o58"_sv,  //
      u8"   ^ Diag_Unexpected_Characters_In_Octal_Number"_diag,
      {Token_Type::number});

  this->check_tokens_with_errors(
      u8"0o58.2"_sv,  //
      u8"   ^^^ Diag_Unexpected_Characters_In_Octal_Number"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_legacy_octal_numbers_strict) {
  this->check_tokens(u8"000"_sv, {Token_Type::number});
  this->check_tokens(u8"001"_sv, {Token_Type::number});
  this->check_tokens(u8"00010101010101010"_sv, {Token_Type::number});
  this->check_tokens(u8"051"_sv, {Token_Type::number});

  // Legacy octal number literals which ended up actually being octal support
  // method calls with '.'.
  this->check_tokens(u8"0123.toString"_sv, {Token_Type::number, Token_Type::dot,
                                            Token_Type::identifier});
  this->check_tokens(u8"00.toString"_sv, {Token_Type::number, Token_Type::dot,
                                          Token_Type::identifier});
}

TEST_F(Test_Lex, lex_legacy_octal_numbers_lax) {
  this->check_tokens(u8"058"_sv, {Token_Type::number});
  this->check_tokens(u8"058.9"_sv, {Token_Type::number});
  this->check_tokens(u8"08"_sv, {Token_Type::number});
}

TEST_F(Test_Lex, fail_lex_legacy_octal_numbers) {
  this->check_tokens_with_errors(
      u8"0123n"_sv,  //
      u8"    ^ Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int"_diag,
      {Token_Type::number});

  this->check_tokens_with_errors(
      u8"052.2"_sv,  //
      u8"   ^ Diag_Octal_Literal_May_Not_Have_Decimal"_diag,
      {Token_Type::number});
}

// TODO (#73) (when strict mode implemented) legacy octal number
// literal tests to fail in strict mode

TEST_F(Test_Lex, legacy_octal_numbers_cannot_contain_underscores) {
  this->check_tokens_with_errors(
      u8"0775_775"_sv,  //
      u8"    ^ Diag_Legacy_Octal_Literal_May_Not_Contain_Underscores"_diag,
      {Token_Type::number});

  this->check_tokens_with_errors(
      u8"0775____775"_sv,  //
      u8"    ^^^^ Diag_Legacy_Octal_Literal_May_Not_Contain_Underscores"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_hex_numbers) {
  this->check_tokens(u8"0x0"_sv, {Token_Type::number});
  this->check_tokens(u8"0x123456789abcdef"_sv, {Token_Type::number});
  this->check_tokens(u8"0X123456789ABCDEF"_sv, {Token_Type::number});
  this->check_tokens(u8"0X123_4567_89AB_CDEF"_sv, {Token_Type::number});
  this->check_tokens(u8"0x1n"_sv, {Token_Type::number});
  this->check_tokens(u8"0xfn"_sv, {Token_Type::number});

  this->check_tokens(u8"0x0.toString"_sv, {Token_Type::number, Token_Type::dot,
                                           Token_Type::identifier});
  this->check_tokens(u8"0xe.toString"_sv, {Token_Type::number, Token_Type::dot,
                                           Token_Type::identifier});
}

TEST_F(Test_Lex, fail_lex_hex_number_no_digits) {
  this->check_tokens_with_errors(u8"0x"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Hex_Number"_diag,
                                 {Token_Type::number});
  this->check_tokens_with_errors(u8"0xn"_sv,  //
                                 u8"^^^ Diag_No_Digits_In_Hex_Number"_diag,
                                 {Token_Type::number});
  this->check_tokens_with_errors(u8"0x;"_sv,  //
                                 u8"^^ Diag_No_Digits_In_Hex_Number"_diag,
                                 {Token_Type::number, Token_Type::semicolon});
  this->check_tokens_with_errors(
      u8"[0x]"_sv,  //
      u8" ^^ Diag_No_Digits_In_Hex_Number"_diag,
      {Token_Type::left_square, Token_Type::number, Token_Type::right_square});
}

TEST_F(Test_Lex, fail_lex_hex_number) {
  this->check_tokens_with_errors(
      u8"0xfqqn"_sv,  //
      u8"   ^^^ Diag_Unexpected_Characters_In_Hex_Number"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_number_with_trailing_garbage) {
  this->check_tokens_with_errors(
      u8"123abcd"_sv,  //
      u8"   ^^^^ Diag_Unexpected_Characters_In_Number"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"123e f"_sv,  //
      u8"   ^ Diag_Unexpected_Characters_In_Number"_diag,
      {Token_Type::number, Token_Type::identifier});
  this->check_tokens_with_errors(
      u8"123e-f"_sv,  //
      u8"   ^ Diag_Unexpected_Characters_In_Number"_diag,
      {Token_Type::number, Token_Type::minus, Token_Type::identifier});
  this->check_tokens_with_errors(
      u8"0b01234"_sv,  //
      u8"    ^^^ Diag_Unexpected_Characters_In_Binary_Number"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0b0h0lla"_sv,  //
      u8"   ^^^^^ Diag_Unexpected_Characters_In_Binary_Number"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0xabjjw"_sv,  //
      u8"    ^^^ Diag_Unexpected_Characters_In_Hex_Number"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0o69"_sv,  //
      u8"   ^ Diag_Unexpected_Characters_In_Octal_Number"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_decimal_number_with_dot_method_call_is_invalid) {
  // TODO(strager): Perhaps a better diagnostic would suggest adding parentheses
  // or another '.' to make a valid method call.
  this->check_tokens_with_errors(
      u8"0.toString()"_sv,  //
      u8"  ^^^^^^^^ Diag_Unexpected_Characters_In_Number"_diag,
      {Token_Type::number, Token_Type::left_paren, Token_Type::right_paren});
  this->check_tokens_with_errors(
      u8"09.toString"_sv,  //
      u8"   ^^^^^^^^ Diag_Unexpected_Characters_In_Number"_diag,
      {Token_Type::number});

  // NOTE(strager): Other numbers with leading zeroes, like '00' and '012345',
  // are legacy octal literals and *can* have a dot method call.
}

TEST_F(Test_Lex, lex_invalid_big_int_number) {
  this->check_tokens_with_errors(
      u8"12.34n"_sv,  //
      u8"^^^^^^ Diag_Big_Int_Literal_Contains_Decimal_Point"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"1e3n"_sv,  //
      u8"^^^^ Diag_Big_Int_Literal_Contains_Exponent"_diag,
      {Token_Type::number});

  // Only complain about the decimal point, not the leading 0 digit.
  this->check_tokens_with_errors(
      u8"0.1n"_sv,  //
      u8"^^^^ Diag_Big_Int_Literal_Contains_Decimal_Point"_diag,
      {Token_Type::number});

  // Complain about both the decimal point and the leading 0 digit.
  this->check_tokens_with_errors(
      u8"01.2n"_sv,  //
      u8"  ^   Diag_Octal_Literal_May_Not_Have_Decimal"_diag,
      u8"  ^^^ Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int"_diag,
      {Token_Type::number});

  // Complain about everything. What a disaster.
  this->check_tokens_with_errors(
      u8"01.2e+3n"_sv,  //
      u8"  ^      Diag_Octal_Literal_May_Not_Have_Decimal"_diag,
      u8"  ^^^^   Diag_Octal_Literal_May_Not_Have_Exponent"_diag,
      u8"  ^^^^^^ Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_number_with_double_underscore) {
  this->check_tokens_with_errors(
      u8"123__000"_sv,  //
      u8"   ^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_number_with_many_underscores) {
  this->check_tokens_with_errors(
      u8"123_____000"_sv,  //
      u8"   ^^^^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0xfee_____eed"_sv,  //
      u8"     ^^^^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0o777_____000"_sv,  //
      u8"     ^^^^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag,
      {Token_Type::number});
  this->check_tokens_with_errors(
      u8"0b111_____000"_sv,  //
      u8"     ^^^^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_number_with_multiple_groups_of_consecutive_underscores) {
  {
    Diag_Collector v;
    // clang-format off
    Padded_String input(u8"123__45___6"_sv);
    auto error1 = /* */ u8"       ^^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag;
    auto error0 = /* */ u8"   ^^ Diag_Number_Literal_Contains_Consecutive_Underscores"_diag;
    // clang-format on
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::number);
    EXPECT_EQ(*l.peek().begin, '1');
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error0, error1});
  }
}

TEST_F(Test_Lex, lex_number_with_trailing_underscore) {
  this->check_tokens_with_errors(
      u8"123456_"_sv,  //
      u8"      ^ Diag_Number_Literal_Contains_Trailing_Underscores"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_number_with_trailing_underscores) {
  this->check_tokens_with_errors(
      u8"123456___"_sv,  //
      u8"      ^^^ Diag_Number_Literal_Contains_Trailing_Underscores"_diag,
      {Token_Type::number});
}

TEST_F(Test_Lex, lex_strings) {
  this->check_tokens(u8R"('hello')"_sv, {Token_Type::string});
  this->check_tokens(u8R"("hello")"_sv, {Token_Type::string});
  this->check_tokens(u8R"("hello\"world")"_sv, {Token_Type::string});
  this->check_tokens(u8R"('hello\'world')"_sv, {Token_Type::string});
  this->check_tokens(u8R"('hello"world')"_sv, {Token_Type::string});
  this->check_tokens(u8R"("hello'world")"_sv, {Token_Type::string});
  this->check_tokens(u8"'hello\\\nworld'"_sv, {Token_Type::string});
  this->check_tokens(u8"\"hello\\\nworld\""_sv, {Token_Type::string});
  this->check_tokens(u8"'hello\\x0aworld'"_sv, {Token_Type::string});
  this->check_tokens(u8R"('\x68\x65\x6c\x6C\x6f')"_sv, {Token_Type::string});
  this->check_tokens(u8R"('\uabcd')"_sv, {Token_Type::string});
  this->check_tokens(u8R"('\u{abcd}')"_sv, {Token_Type::string});

  this->check_tokens_with_errors(
      u8"\"unterminated"_sv,  //
      u8"^^^^^^^^^^^^^^ Diag_Unclosed_String_Literal"_diag,
      {Token_Type::string});

  for (String8_View line_terminator : line_terminators) {
    for (String8_View quotation_mark : {u8"'"_sv, u8"\""_sv}) {
      Padded_String input(concat(quotation_mark, u8"line1\\"_sv,
                                 line_terminator, u8"line2"_sv,
                                 quotation_mark));
      this->check_tokens(&input, {Token_Type::string});
    }
  }

  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Diag_Collector v;
    Padded_String input(
        concat(u8"'unterminated"_sv, line_terminator, u8"hello"_sv));
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello"_sv);

    assert_diagnostics(&input, v.errors,
                       {
                           // 'unterminated\nhello
                           u8"^^^^^^^^^^^^^ Diag_Unclosed_String_Literal"_diag,
                       });
  }

  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Diag_Collector v;
    Padded_String input(
        concat(u8"'separated"_sv, line_terminator, u8"hello'"_sv));
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(&input,
                                      Diag_Unclosed_String_Literal,  //
                                      string_literal, 0, input.string_view()),
                }));
  }

  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Diag_Collector v;
    Padded_String input(concat(u8"'separated"_sv, line_terminator,
                               line_terminator, u8"hello'"_sv));
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(&input, Diag_Unclosed_String_Literal,  //
                              string_literal, 0, u8"'separated"_sv),
            DIAG_TYPE_OFFSETS(
                &input, Diag_Unclosed_String_Literal, string_literal,  //
                u8"'separatedhello"_sv.size() + 2 * line_terminator.size(),
                u8"'"_sv),
        }));
  }

  for (String8_View line_terminator : line_terminators_except_ls_ps) {
    Diag_Collector v;
    Padded_String input(
        concat(u8"let x = 'hello"_sv, line_terminator, u8"let y = 'world'"_sv));
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::kw_let);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::equal);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::kw_let);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::equal);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors,
                       {
                           // let x = 'hello\nlet y = 'world'
                           u8"        ^^^^^^ Diag_Unclosed_String_Literal"_diag,
                       });
  }

  this->check_tokens_with_errors(
      u8"'unterminated\\"_sv,  //
      u8"^^^^^^^^^^^^^^^ Diag_Unclosed_String_Literal"_diag,
      {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'\\x"_sv,                                     //
      u8" ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag,  //
      u8"^^^^ Diag_Unclosed_String_Literal"_diag, {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'\\x1"_sv,                                    //
      u8" ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag,  //
      u8"^^^^^ Diag_Unclosed_String_Literal"_diag, {Token_Type::string});

  this->check_tokens_with_errors(u8"'\\x'"_sv,  //
                                 u8" ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag,
                                 {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'\\x\\xyz'"_sv,                                  //
      u8"    ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag,  //
      u8" ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag, {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'\\x1 \\xff \\xg '"_sv,                                  //
      u8"            ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag,  //
      u8" ^^^ Diag_Invalid_Hex_Escape_Sequence"_diag, {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'hello\\u'"_sv,  //
      u8"      ^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      {Token_Type::string});

  this->check_tokens_with_errors(
      u8"'hello\\u{110000}'"_sv,  //
      u8"      ^^^^^^^^^^^ Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range"_diag,
      {Token_Type::string});

  // TODO(#187): Report octal escape sequences in strict mode.
  // TODO(#187): Report invalid octal escape sequences in non-strict mode.
}

TEST_F(Test_Lex, lex_string_with_ascii_control_characters) {
  for (String8_View control_character :
       concat(control_characters_except_line_terminators, ls_and_ps)) {
    Padded_String input(
        concat(u8"'hello"_sv, control_character, u8"world'"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::string});
  }

  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"'hello\\"_sv, control_character, u8"world'"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::string});
  }
}

TEST_F(Test_Lex, string_with_curly_quotes) {
  // Curly single quotes:
  this->check_tokens_with_errors(
      u8"\u2018string here\u2019"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote='}"_diag,
      {Token_Type::string});
  this->check_tokens_with_errors(
      u8"\u2019string here\u2018"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote='}"_diag,
      {Token_Type::string});
  this->check_tokens_with_errors(
      u8"\u2018string \u201c \" \u201d here\u2019"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote='}"_diag,
      {Token_Type::string});

  // Curly double quotes:
  this->check_tokens_with_errors(
      u8"\u201cstring here\u201d"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote=\"}"_diag,
      {Token_Type::string});
  this->check_tokens_with_errors(
      u8"\u201dstring here\u201c"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote=\"}"_diag,
      {Token_Type::string});
  this->check_tokens_with_errors(
      u8"\u201cstring \u2018 ' \u2019 here\u201d"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote=\"}"_diag,
      {Token_Type::string});

  // Start with curly quote, but end with matching straight quote:
  this->check_tokens_with_errors(
      u8"\u2018string here'"_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote='}"_diag,
      {Token_Type::string});
  this->check_tokens_with_errors(
      u8"\u201cstring here\""_sv,  //
      u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote{.suggested_quote=\"}"_diag,
      {Token_Type::string});

  // Unclosed string:
  for (String8 opening_quote : {u8"\u2018", u8"\u201c"}) {
    this->check_tokens_with_errors(
        opening_quote + u8"string here",  //
        // \u2018string here
        u8"^^^^^^^^^^^^^^^^^ Diag_Unclosed_String_Literal"_diag,
        u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote"_diag,
        {Token_Type::string});
    for (String8_View line_terminator : line_terminators) {
      this->check_tokens_with_errors(
          opening_quote + u8"string here" + String8(line_terminator) +
              u8"next_line",  //
          // \u2018string here\nnext_line
          u8"^^^^^^^^^^^^^^^^^ Diag_Unclosed_String_Literal"_diag,
          u8"^^^^^^ Diag_Invalid_Quotes_Around_String_Literal.opening_quote"_diag,
          {Token_Type::string, Token_Type::identifier});
    }
  }
}

TEST_F(Test_Lex, lex_templates) {
  this->check_tokens(u8"``"_sv, {Token_Type::complete_template});
  this->check_tokens(u8"`hello`"_sv, {Token_Type::complete_template});
  this->check_tokens(u8"`hello$world`"_sv, {Token_Type::complete_template});
  this->check_tokens(u8"`hello{world`"_sv, {Token_Type::complete_template});
  this->check_tokens(u8R"(`hello\`world`)"_sv, {Token_Type::complete_template});
  this->check_tokens(u8R"(`hello$\{world`)"_sv,
                     {Token_Type::complete_template});
  this->check_tokens(u8R"(`hello\${world`)"_sv,
                     {Token_Type::complete_template});
  this->check_tokens(
      u8R"(`hello
world`)"_sv,
      {Token_Type::complete_template});
  this->check_tokens(u8"`hello\\\nworld`"_sv, {Token_Type::complete_template});
  this->check_tokens(u8R"(`\uabcd`)"_sv, {Token_Type::complete_template});
  this->check_tokens(u8R"(`\u{abcd}`)"_sv, {Token_Type::complete_template});

  {
    Padded_String code(u8"`hello${42}`"_sv);
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`hello${"_sv);
    const Char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`"_sv);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  {
    Padded_String code(u8"`${42}world`"_sv);
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"`${"_sv);
    const Char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), u8"world`"_sv);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  {
    Padded_String code(u8"`${left}${right}`"_sv);
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    const Char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  this->check_tokens_with_errors(u8"`unterminated"_sv,  //
                                 u8"^^^^^^^^^^^^^ Diag_Unclosed_Template"_diag,
                                 {Token_Type::complete_template});

  {
    Diag_Collector v;
    Padded_String input(u8"`${un}terminated"_sv);
    auto error = /*  */ u8"^^^^^^^^^^^^^^^^ Diag_Unclosed_Template"_diag;

    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    const Char8* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    assert_diagnostics(&input, v.errors, {error});
  }

  this->check_tokens_with_errors(
      u8"`unterminated\\"_sv,  //
      u8"^^^^^^^^^^^^^^^ Diag_Unclosed_Template"_diag,
      {Token_Type::complete_template});
}

TEST_F(Test_Lex, templates_buffer_unicode_escape_errors) {
  {
    // clang-format off
    Padded_String input(u8"`hello\\u`"_sv);
    auto error = /*  */ u8"      ^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag;
    // clang-format on
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    assert_diagnostics(&input, errors.errors, {error});

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  {
    // clang-format off
    Padded_String input(u8"`hello\\u{110000}`"_sv);
    auto error = /*  */ u8"      ^^^^^^^^^^^ Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range"_diag;
    // clang-format on
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    assert_diagnostics(&input, errors.errors, {error});

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  {
    // clang-format off
    Padded_String input(u8"`hello\\u${expr}`"_sv);
    auto error = /*  */ u8"      ^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag;
    // clang-format on
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    assert_diagnostics(&input, errors.errors, {error});

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
  }
}

TEST_F(Test_Lex, templates_do_not_buffer_valid_unicode_escapes) {
  {
    Padded_String input(u8"`hell\\u{6f}`"_sv);
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, Token_Type::complete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  {
    Padded_String input(u8"`hell\\u{6f}${expr}`"_sv);
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&input, &errors);

    EXPECT_EQ(l.peek().type, Token_Type::incomplete_template);
    EXPECT_THAT(errors.errors, IsEmpty());
    l.peek().report_errors_for_escape_sequences_in_template(&errors);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
  }
}

TEST_F(Test_Lex, lex_template_literal_with_ascii_control_characters) {
  for (String8_View control_character :
       concat(control_characters_except_line_terminators, line_terminators)) {
    Padded_String input(
        concat(u8"`hello"_sv, control_character, u8"world`"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::complete_template});
  }

  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"`hello\\"_sv, control_character, u8"world`"_sv));
    SCOPED_TRACE(input);
    this->check_tokens(&input, {Token_Type::complete_template});
  }
}

TEST_F(Test_Lex, lex_regular_expression_literals) {
  auto check_regexp = [](String8_View raw_code) {
    Padded_String code(raw_code);
    SCOPED_TRACE(code);
    Diag_Collector errors;
    Lexer l(&code, &errors);

    EXPECT_THAT(l.peek().type,
                testing::AnyOf(Token_Type::slash, Token_Type::slash_equal));
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  };

  check_regexp(u8"/ /"_sv);
  check_regexp(u8R"(/hello\/world/)"_sv);
  check_regexp(u8"/re/g"_sv);
  check_regexp(u8R"(/[/]/)"_sv);
  check_regexp(u8R"(/[\]/]/)"_sv);
  check_regexp(u8R"(/[\\]/)"_sv);
  check_regexp(u8"/=/"_sv);

  for (String8_View raw_code : {u8"/end_of_file"_sv, u8R"(/eof\)"_sv}) {
    Padded_String code(raw_code);
    SCOPED_TRACE(code);
    Diag_Collector v;
    Lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(&code, Diag_Unclosed_Regexp_Literal,  //
                                      regexp_literal, 0, code.string_view()),
                }));

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }

  for (String8_View line_terminator : line_terminators) {
    Padded_String code(
        concat(u8"/first_line"_sv, line_terminator, u8"second_line/"_sv));
    SCOPED_TRACE(code);
    Diag_Collector v;
    Lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, code.data() + u8"/first_line"_sv.size());

    assert_diagnostics(&code, v.errors,
                       {
                           // /first_line\nsecond_line/
                           u8"^^^^^^^^^^^ Diag_Unclosed_Regexp_Literal"_diag,
                       });

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"second_line"_sv);
  }

  for (String8_View line_terminator : line_terminators) {
    Padded_String code(
        concat(u8"/first[line"_sv, line_terminator, u8"second]line/"_sv));
    SCOPED_TRACE(code);
    Diag_Collector v;
    Lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, code.data() + u8"/first[line"_sv.size());

    assert_diagnostics(&code, v.errors,
                       {
                           // /first[line\nsecond]line/
                           u8"^^^^^^^^^^^ Diag_Unclosed_Regexp_Literal"_diag,
                       });

    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"second"_sv);
  }

  // TODO(#187): Report invalid escape sequences.

  // TODO(#203): Report invalid characters and mismatched brackets.
}

TEST_F(Test_Lex, lex_regular_expression_literal_with_digit_flag) {
  Padded_String input(u8"/cellular/3g"_sv);

  Lexer l(&input, &Null_Diag_Reporter::instance);
  EXPECT_EQ(l.peek().type, Token_Type::slash);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, Token_Type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

  // TODO(#47): Report an error, because '3' is an invalid flag.
}

TEST_F(Test_Lex, lex_unicode_escape_in_regular_expression_literal_flags) {
  Diag_Collector errors;
  // clang-format off
  Padded_String input(u8"/hello/\\u{67}i"_sv);
  auto error = /*  */ u8"       ^^^^^^^Diag_Regexp_Literal_Flags_Cannot_Contain_Unicode_Escapes"_diag;
  // clang-format on

  Lexer l(&input, &errors);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, Token_Type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

  assert_diagnostics(&input, errors.errors, {error});
}

TEST_F(Test_Lex, lex_non_ascii_in_regular_expression_literal_flags) {
  Diag_Collector errors;
  Padded_String input(u8"/hello/\u05d0"_sv);

  Lexer l(&input, &errors);
  l.reparse_as_regexp();
  EXPECT_EQ(l.peek().type, Token_Type::regexp);
  EXPECT_EQ(l.peek().begin, &input[0]);
  EXPECT_EQ(l.peek().end, &input[input.size()]);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

  // TODO(#47): Report an error, because '\u05d0' is an invalid flag.
}

TEST_F(Test_Lex,
       lex_regular_expression_literals_preserves_leading_newline_flag) {
  {
    Padded_String code(u8"\n/ /"_sv);
    Lexer l(&code, &Null_Diag_Reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_TRUE(l.peek().has_leading_newline);
  }

  {
    Padded_String code(u8"/ /"_sv);
    Lexer l(&code, &Null_Diag_Reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    EXPECT_FALSE(l.peek().has_leading_newline);
  }
}

TEST_F(Test_Lex, lex_regular_expression_literal_with_ascii_control_characters) {
  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"/hello"_sv, control_character, u8"world/"_sv));
    SCOPED_TRACE(input);
    Diag_Collector errors;
    Lexer l(&input, &errors);

    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  for (String8_View control_character :
       control_characters_except_line_terminators) {
    Padded_String input(
        concat(u8"/hello\\"_sv, control_character, u8"world/"_sv));
    SCOPED_TRACE(input);
    Diag_Collector errors;
    Lexer l(&input, &errors);

    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, Token_Type::regexp);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, split_less_less_into_two_tokens) {
  Padded_String input(u8"<<T>() => T>"_sv);

  Lexer l(&input, &Null_Diag_Reporter::instance);
  EXPECT_EQ(l.peek().type, Token_Type::less_less);
  l.skip_less_less_as_less();
  EXPECT_EQ(l.peek().type, Token_Type::less);
  EXPECT_EQ(l.peek().begin, &input[1]);
  EXPECT_EQ(l.peek().end, &input[2]);
  EXPECT_EQ(l.end_of_previous_token(), &input[1]);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier) << "T";
}

TEST_F(Test_Lex, split_less_less_has_no_leading_newline) {
  Padded_String input(u8"\n<<"_sv);

  Lexer l(&input, &Null_Diag_Reporter::instance);
  EXPECT_EQ(l.peek().type, Token_Type::less_less);
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.skip_less_less_as_less();
  EXPECT_EQ(l.peek().type, Token_Type::less);
  EXPECT_FALSE(l.peek().has_leading_newline);
}

TEST_F(Test_Lex, split_greater_from_bigger_token) {
  {
    Padded_String input(u8">>;"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater);

    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::greater);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[2]);
    EXPECT_EQ(l.end_of_previous_token(), &input[1]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  }

  {
    Padded_String input(u8">>>;"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater_greater);

    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[3]);
    EXPECT_EQ(l.end_of_previous_token(), &input[1]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  }

  {
    Padded_String input(u8">=;"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_equal);

    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::equal);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[2]);
    EXPECT_EQ(l.end_of_previous_token(), &input[1]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  }

  {
    Padded_String input(u8">>=;"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater_equal);

    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::greater_equal);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[3]);
    EXPECT_EQ(l.end_of_previous_token(), &input[1]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  }

  {
    Padded_String input(u8">>>=;"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater_greater_equal);

    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater_equal);
    EXPECT_EQ(l.peek().begin, &input[1]);
    EXPECT_EQ(l.peek().end, &input[4]);
    EXPECT_EQ(l.end_of_previous_token(), &input[1]);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  }
}

TEST_F(Test_Lex, split_greater_from_bigger_token_has_no_leading_newline) {
  {
    Padded_String input(u8"\n>>"_sv);
    Lexer l(&input, &Null_Diag_Reporter::instance);
    EXPECT_EQ(l.peek().type, Token_Type::greater_greater);
    EXPECT_TRUE(l.peek().has_leading_newline);
    l.skip_as_greater();
    EXPECT_EQ(l.peek().type, Token_Type::greater);
    EXPECT_FALSE(l.peek().has_leading_newline);
  }
}

TEST_F(Test_Lex, lex_identifiers) {
  this->check_tokens(u8"i"_sv, {Token_Type::identifier});
  this->check_tokens(u8"_"_sv, {Token_Type::identifier});
  this->check_tokens(u8"$"_sv, {Token_Type::identifier});
  this->check_single_token(u8"id"_sv, u8"id"_sv);
  this->check_single_token(u8"id "_sv, u8"id"_sv);
  this->check_single_token(u8"this_is_an_identifier"_sv,
                           u8"this_is_an_identifier"_sv);
  this->check_single_token(u8"MixedCaseIsAllowed"_sv,
                           u8"MixedCaseIsAllowed"_sv);
  this->check_single_token(u8"ident$with$dollars"_sv,
                           u8"ident$with$dollars"_sv);
  this->check_single_token(u8"digits0123456789"_sv, u8"digits0123456789"_sv);

  // This identifier used to read the keyword table out of bounds.
  this->check_single_token(u8"kedhinkunnunnnunuwnunununnun"_sv,
                           u8"kedhinkunnunnnunuwnunununnun"_sv);
}

TEST_F(Test_Lex, ascii_identifier_with_escape_sequence) {
  this->check_single_token(u8"\\u0061"_sv, u8"a"_sv);
  this->check_single_token(u8"\\u0041"_sv, u8"A"_sv);
  this->check_single_token(u8"\\u004E"_sv, u8"N"_sv);
  this->check_single_token(u8"\\u004e"_sv, u8"N"_sv);

  this->check_single_token(u8"\\u{41}"_sv, u8"A"_sv);
  this->check_single_token(u8"\\u{0041}"_sv, u8"A"_sv);
  this->check_single_token(u8"\\u{00000000000000000000041}"_sv, u8"A"_sv);
  this->check_single_token(u8"\\u{004E}"_sv, u8"N"_sv);
  this->check_single_token(u8"\\u{004e}"_sv, u8"N"_sv);

  this->check_single_token(u8"hell\\u006f"_sv, u8"hello"_sv);
  this->check_single_token(u8"\\u0068ello"_sv, u8"hello"_sv);
  this->check_single_token(u8"w\\u0061t"_sv, u8"wat"_sv);

  this->check_single_token(u8"hel\\u006c0"_sv, u8"hell0"_sv);

  this->check_single_token(u8"\\u0077\\u0061\\u0074"_sv, u8"wat"_sv);
  this->check_single_token(u8"\\u{77}\\u{61}\\u{74}"_sv, u8"wat"_sv);

  // _ and $ are in IdentifierStart, even though they aren't in UnicodeIDStart.
  this->check_single_token(u8"\\u{5f}wakka"_sv, u8"_wakka"_sv);
  this->check_single_token(u8"\\u{24}wakka"_sv, u8"$wakka"_sv);

  // $, ZWNJ, ZWJ in IdentifierPart, even though they aren't in
  // UnicodeIDContinue.
  this->check_single_token(u8"wakka\\u{24}"_sv, u8"wakka$"_sv);
  this->check_single_token(u8"wak\\u200cka"_sv, u8"wak\u200cka"_sv);
  this->check_single_token(u8"wak\\u200dka"_sv, u8"wak\u200dka"_sv);
}

TEST_F(Test_Lex, non_ascii_identifier) {
  this->check_single_token(u8"\U00013337"_sv, u8"\U00013337"_sv);

  this->check_single_token(u8"\u00b5"_sv, u8"\u00b5"_sv);    // 2 UTF-8 bytes
  this->check_single_token(u8"\u05d0"_sv, u8"\u05d0"_sv);    // 3 UTF-8 bytes
  this->check_single_token(u8"a\u0816"_sv, u8"a\u0816"_sv);  // 3 UTF-8 bytes
  this->check_single_token(u8"\U0001e93f"_sv,
                           u8"\U0001e93f"_sv);  // 4 UTF-8 bytes

  // KHOJKI LETTER QA, introduced in Unicode 15.
  this->check_single_token(u8"\U0001123f"_sv, u8"\U0001123f"_sv);
}

TEST_F(Test_Lex, non_ascii_identifier_with_escape_sequence) {
  this->check_single_token(u8"\\u{013337}"_sv, u8"\U00013337"_sv);

  this->check_single_token(u8"\\u{b5}"_sv, u8"\u00b5"_sv);     // 2 UTF-8 bytes
  this->check_single_token(u8"a\\u{816}"_sv, u8"a\u0816"_sv);  // 3 UTF-8 bytes
  this->check_single_token(u8"a\\u0816"_sv, u8"a\u0816"_sv);   // 3 UTF-8 bytes
  this->check_single_token(u8"\\u{1e93f}"_sv,
                           u8"\U0001e93f"_sv);  // 4 UTF-8 bytes
}

TEST_F(Test_Lex,
       identifier_with_escape_sequences_source_code_span_is_in_place) {
  Padded_String input(u8"\\u{77}a\\u{74}"_sv);
  Lexer l(&input, &Null_Diag_Reporter::instance);
  Source_Code_Span span = l.peek().identifier_name().span();
  EXPECT_EQ(span.begin(), &input[0]);
  EXPECT_EQ(span.end(), &input[input.size()]);
}

TEST_F(Test_Lex, lex_identifier_with_malformed_escape_sequence) {
  this->check_single_token_with_errors(
      u8" are\\ufriendly "_sv,  //
      u8"    ^^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      u8"are\\ufriendly"_sv);
  this->check_tokens_with_errors(
      u8"are\\uf riendly"_sv,  //
      u8"   ^^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      {Token_Type::identifier, Token_Type::identifier});
  this->check_single_token_with_errors(
      u8"stray\\backslash"_sv,  //
      u8"     ^^ Diag_Unexpected_Backslash_In_Identifier"_diag,
      u8"stray\\backslash"_sv);
  this->check_single_token_with_errors(
      u8"stray\\"_sv,  //
      u8"     ^^ Diag_Unexpected_Backslash_In_Identifier"_diag, u8"stray\\"_sv);
  this->check_tokens_with_errors(
      u8"hello\\u}world"_sv,  //
      u8"     ^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      {Token_Type::identifier, Token_Type::right_curly,
       Token_Type::identifier});
  this->check_tokens_with_errors(
      u8"negative\\u-0041"_sv,  //
      u8"        ^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      {Token_Type::identifier, Token_Type::minus, Token_Type::number});

  this->check_single_token_with_errors(
      u8"a\\u{}b"_sv,  //
      u8" ^^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      u8"a\\u{}b"_sv);
  this->check_single_token_with_errors(
      u8"a\\u{q}b"_sv,  //
      u8" ^^^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      u8"a\\u{q}b"_sv);

  this->check_single_token_with_errors(
      u8"unterminated\\u"_sv,  //
      u8"            ^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      u8"unterminated\\u"_sv);
  this->check_single_token_with_errors(
      u8"unterminated\\u012"_sv,  //
      u8"            ^^^^^^ Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
      u8"unterminated\\u012"_sv);
  this->check_single_token_with_errors(
      u8"unterminated\\u{"_sv,  //
      u8"            ^^^^ Diag_Unclosed_Identifier_Escape_Sequence"_diag,
      u8"unterminated\\u{"_sv);
  this->check_single_token_with_errors(
      u8"unterminated\\u{0123"_sv,  //
      u8"            ^^^^^^^^ Diag_Unclosed_Identifier_Escape_Sequence"_diag,
      u8"unterminated\\u{0123"_sv);

  this->check_tokens_with_errors(
      u8"unclosed\\u{0123 'string'"_sv,  //
      u8"        ^^^^^^^^ Diag_Unclosed_Identifier_Escape_Sequence"_diag,
      {Token_Type::identifier, Token_Type::string});
  this->check_tokens_with_errors(
      u8"unclosed\\u{+=42"_sv,  //
      u8"        ^^^^ Diag_Unclosed_Identifier_Escape_Sequence"_diag,
      {Token_Type::identifier, Token_Type::plus_equal, Token_Type::number});
}

TEST_F(Test_Lex, lex_identifier_with_out_of_range_escaped_character) {
  this->check_single_token_with_errors(
      u8"too\\u{110000}big"_sv,  //
      u8"   ^^^^^^^^^^^ Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range"_diag,
      u8"too\\u{110000}big"_sv);
  this->check_single_token_with_errors(
      u8"waytoo\\u{100000000000000}big"_sv,  //
      u8"      ^^^^^^^^^^^^^^^^^^^^ Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range"_diag,
      u8"waytoo\\u{100000000000000}big"_sv);
}

TEST_F(Test_Lex, lex_identifier_with_out_of_range_utf_8_sequence) {
  // f4 90 80 80 is U+110000
  this->check_single_token_with_errors(
      "too\xf4\x90\x80\x80\x62ig"_s8v, "too\xf4\x90\x80\x80\x62ig"_s8v,
      [](Padded_String_View input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(input, Diag_Invalid_Utf_8_Sequence,  //
                                  sequence, std::strlen("too"),
                                  "\xf4\x90\x80\x80"_s8v),
            }));
      });
}

TEST_F(Test_Lex, lex_identifier_with_malformed_utf_8_sequence) {
  this->check_single_token_with_errors(
      "illegal\xc0\xc1\xc2\xc3\xc4utf8\xfe\xff"_s8v,
      "illegal\xc0\xc1\xc2\xc3\xc4utf8\xfe\xff"_s8v,
      [](Padded_String_View input, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(input, Diag_Invalid_Utf_8_Sequence,  //
                                  sequence, std::strlen("illegal"),
                                  "\xc0\xc1\xc2\xc3\xc4"_s8v),
                DIAG_TYPE_OFFSETS(
                    input, Diag_Invalid_Utf_8_Sequence,  //
                    sequence, std::strlen("illegal\xc0\xc1\xc2\xc3\xc4utf8"),
                    "\xfe\xff"_s8v),
            }));
      });
}

TEST_F(Test_Lex, lex_identifier_with_disallowed_character_escape_sequence) {
  this->check_single_token_with_errors(
      u8"illegal\\u0020"_sv,  //
      u8"       ^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"illegal\\u0020"_sv);
  this->check_single_token_with_errors(
      u8"illegal\\u{0020}"_sv,  //
      u8"       ^^^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"illegal\\u{0020}"_sv);
  this->check_single_token_with_errors(
      u8"\\u{20}illegal"_sv,  //
      u8"^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"\\u{20}illegal"_sv);
  this->check_single_token_with_errors(
      u8"illegal\\u{10ffff}"_sv,  //
      u8"       ^^^^^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"illegal\\u{10ffff}"_sv);
  this->check_single_token_with_errors(
      u8"\\u{10ffff}illegal"_sv,  //
      u8"^^^^^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"\\u{10ffff}illegal"_sv);

  // U+005c is \ (backslash)
  this->check_single_token_with_errors(
      u8"\\u{5c}u0061illegal"_sv,  //
      u8"^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"\\u{5c}u0061illegal"_sv);
  this->check_single_token_with_errors(
      u8"illegal\\u{5c}u0061"_sv,  //
      u8"       ^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"illegal\\u{5c}u0061"_sv);
}

TEST_F(Test_Lex, lex_identifier_with_disallowed_non_ascii_character) {
  this->check_single_token_with_errors(
      u8"illegal\U0010ffff"_sv,  //
      u8"       ^^^^^^^^^^ Diag_Character_Disallowed_In_Identifiers"_diag,
      u8"illegal\U0010ffff"_sv);
  this->check_single_token_with_errors(
      u8"\U0010ffffillegal"_sv,  //
      u8"^^^^^^^^^^ Diag_Character_Disallowed_In_Identifiers"_diag,
      u8"\U0010ffffillegal"_sv);
}

TEST_F(Test_Lex, lex_identifier_with_disallowed_escaped_initial_character) {
  // Identifiers cannot start with a digit.
  this->check_single_token_with_errors(
      u8"\\u{30}illegal"_sv,  //
      u8"^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"\\u{30}illegal"_sv);

  this->check_single_token_with_errors(
      u8"\\u0816illegal"_sv,  //
      u8"^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"\\u0816illegal"_sv);
}

TEST_F(Test_Lex, lex_identifier_with_disallowed_non_ascii_initial_character) {
  this->check_single_token_with_errors(
      u8"\u0816illegal"_sv,  //
      u8"^^^^^^ Diag_Character_Disallowed_In_Identifiers"_diag,
      u8"\u0816illegal"_sv);
}

TEST_F(
    Test_Lex,
    lex_identifier_with_disallowed_initial_character_as_subsequent_character) {
  // Identifiers can contain a digit.
  this->check_single_token(u8"legal0"_sv, u8"legal0"_sv);
  this->check_single_token(u8"legal\\u{30}"_sv, u8"legal0"_sv);

  this->check_single_token(u8"legal\\u0816"_sv, u8"legal\u0816"_sv);
  this->check_single_token(u8"legal\u0816"_sv, u8"legal\u0816"_sv);
}

TEST_F(Test_Lex, lex_identifiers_which_look_like_keywords) {
  this->check_tokens(u8"ifelse"_sv, {Token_Type::identifier});
  this->check_tokens(u8"IF"_sv, {Token_Type::identifier});
}

TEST_F(Test_Lex, private_identifier) {
  this->check_tokens(u8"#i"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#_"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#$"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#Mixed_Case_With_Underscores"_sv,
                     {Token_Type::private_identifier});
  this->check_tokens(u8"#digits0123456789"_sv,
                     {Token_Type::private_identifier});

  {
    Padded_String code(u8" #id "_sv);
    std::vector<Token> tokens = this->lex_to_eof(&code);
    ASSERT_EQ(tokens.size(), 1);
    Identifier ident = tokens[0].identifier_name();
    EXPECT_EQ(ident.span().string_view(), u8"#id"_sv);
    EXPECT_EQ(ident.normalized_name(), u8"#id"_sv);
  }

  this->check_single_token(u8"#\u00b5"_sv, u8"#\u00b5"_sv);    // 2 UTF-8 bytes
  this->check_single_token(u8"#\u05d0"_sv, u8"#\u05d0"_sv);    // 2 UTF-8 bytes
  this->check_single_token(u8"#a\u0816"_sv, u8"#a\u0816"_sv);  // 3 UTF-8 bytes
  this->check_single_token(u8"#\U0001e93f"_sv,
                           u8"#\U0001e93f"_sv);  // 4 UTF-8 bytes

  this->check_single_token(u8"#\\u{b5}"_sv, u8"#\u00b5"_sv);
  this->check_single_token(u8"#a\\u0816"_sv, u8"#a\u0816"_sv);
  this->check_single_token(u8"#\\u{0001e93f}"_sv, u8"#\U0001e93f"_sv);

  {
    Padded_String code(u8" #\\u{78} "_sv);
    std::vector<Token> tokens = this->lex_to_eof(&code);
    ASSERT_EQ(tokens.size(), 1);
    Identifier ident = tokens[0].identifier_name();
    EXPECT_EQ(ident.span().string_view(), u8"#\\u{78}"_sv);
    EXPECT_EQ(ident.normalized_name(), u8"#x"_sv);
  }

  // Keywords are allowed.
  this->check_tokens(u8"#async"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#for"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#function"_sv, {Token_Type::private_identifier});
  this->check_tokens(u8"#let"_sv, {Token_Type::private_identifier});
}

TEST_F(Test_Lex,
       private_identifier_with_disallowed_non_ascii_initial_character) {
  this->check_single_token_with_errors(
      u8"#\u0816illegal"_sv,  //
      u8" ^^^^^^ Diag_Character_Disallowed_In_Identifiers"_diag,
      u8"#\u0816illegal"_sv);

  this->check_tokens_with_errors(u8"#123"_sv,  //
                                 u8"^ Diag_Unexpected_Hash_Character"_diag,
                                 {Token_Type::number});
}

TEST_F(Test_Lex, private_identifier_with_disallowed_escaped_initial_character) {
  // Private identifiers cannot start with a digit.
  this->check_single_token_with_errors(
      u8"#\\u{30}illegal"_sv,  //
      u8" ^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"#\\u{30}illegal"_sv);

  this->check_single_token_with_errors(
      u8"#\\u0816illegal"_sv,  //
      u8" ^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag,
      u8"#\\u0816illegal"_sv);
}

TEST_F(Test_Lex, lex_reserved_keywords) {
  this->check_tokens(u8"await"_sv, {Token_Type::kw_await});
  this->check_tokens(u8"break"_sv, {Token_Type::kw_break});
  this->check_tokens(u8"case"_sv, {Token_Type::kw_case});
  this->check_tokens(u8"catch"_sv, {Token_Type::kw_catch});
  this->check_tokens(u8"class"_sv, {Token_Type::kw_class});
  this->check_tokens(u8"const"_sv, {Token_Type::kw_const});
  this->check_tokens(u8"continue"_sv, {Token_Type::kw_continue});
  this->check_tokens(u8"debugger"_sv, {Token_Type::kw_debugger});
  this->check_tokens(u8"default"_sv, {Token_Type::kw_default});
  this->check_tokens(u8"delete"_sv, {Token_Type::kw_delete});
  this->check_tokens(u8"do"_sv, {Token_Type::kw_do});
  this->check_tokens(u8"else"_sv, {Token_Type::kw_else});
  this->check_tokens(u8"enum"_sv, {Token_Type::kw_enum});
  this->check_tokens(u8"export"_sv, {Token_Type::kw_export});
  this->check_tokens(u8"extends"_sv, {Token_Type::kw_extends});
  this->check_tokens(u8"false"_sv, {Token_Type::kw_false});
  this->check_tokens(u8"finally"_sv, {Token_Type::kw_finally});
  this->check_tokens(u8"for"_sv, {Token_Type::kw_for});
  this->check_tokens(u8"function"_sv, {Token_Type::kw_function});
  this->check_tokens(u8"if"_sv, {Token_Type::kw_if});
  this->check_tokens(u8"implements"_sv, {Token_Type::kw_implements});
  this->check_tokens(u8"import"_sv, {Token_Type::kw_import});
  this->check_tokens(u8"in"_sv, {Token_Type::kw_in});
  this->check_tokens(u8"instanceof"_sv, {Token_Type::kw_instanceof});
  this->check_tokens(u8"interface"_sv, {Token_Type::kw_interface});
  this->check_tokens(u8"new"_sv, {Token_Type::kw_new});
  this->check_tokens(u8"null"_sv, {Token_Type::kw_null});
  this->check_tokens(u8"package"_sv, {Token_Type::kw_package});
  this->check_tokens(u8"private"_sv, {Token_Type::kw_private});
  this->check_tokens(u8"protected"_sv, {Token_Type::kw_protected});
  this->check_tokens(u8"public"_sv, {Token_Type::kw_public});
  this->check_tokens(u8"return"_sv, {Token_Type::kw_return});
  this->check_tokens(u8"super"_sv, {Token_Type::kw_super});
  this->check_tokens(u8"switch"_sv, {Token_Type::kw_switch});
  this->check_tokens(u8"this"_sv, {Token_Type::kw_this});
  this->check_tokens(u8"throw"_sv, {Token_Type::kw_throw});
  this->check_tokens(u8"true"_sv, {Token_Type::kw_true});
  this->check_tokens(u8"try"_sv, {Token_Type::kw_try});
  this->check_tokens(u8"typeof"_sv, {Token_Type::kw_typeof});
  this->check_tokens(u8"var"_sv, {Token_Type::kw_var});
  this->check_tokens(u8"void"_sv, {Token_Type::kw_void});
  this->check_tokens(u8"while"_sv, {Token_Type::kw_while});
  this->check_tokens(u8"with"_sv, {Token_Type::kw_with});
  this->check_tokens(u8"yield"_sv, {Token_Type::kw_yield});
}

TEST_F(Test_Lex, lex_contextual_keywords) {
  this->check_tokens(u8"as"_sv, {Token_Type::kw_as});
  this->check_tokens(u8"async"_sv, {Token_Type::kw_async});
  this->check_tokens(u8"from"_sv, {Token_Type::kw_from});
  this->check_tokens(u8"get"_sv, {Token_Type::kw_get});
  this->check_tokens(u8"let"_sv, {Token_Type::kw_let});
  this->check_tokens(u8"of"_sv, {Token_Type::kw_of});
  this->check_tokens(u8"set"_sv, {Token_Type::kw_set});
  this->check_tokens(u8"static"_sv, {Token_Type::kw_static});
}

TEST_F(Test_Lex, lex_typescript_contextual_keywords) {
  this->check_tokens(u8"abstract"_sv, {Token_Type::kw_abstract});
  this->check_tokens(u8"any"_sv, {Token_Type::kw_any});
  this->check_tokens(u8"assert"_sv, {Token_Type::kw_assert});
  this->check_tokens(u8"asserts"_sv, {Token_Type::kw_asserts});
  this->check_tokens(u8"bigint"_sv, {Token_Type::kw_bigint});
  this->check_tokens(u8"boolean"_sv, {Token_Type::kw_boolean});
  this->check_tokens(u8"constructor"_sv, {Token_Type::kw_constructor});
  this->check_tokens(u8"declare"_sv, {Token_Type::kw_declare});
  this->check_tokens(u8"global"_sv, {Token_Type::kw_global});
  this->check_tokens(u8"infer"_sv, {Token_Type::kw_infer});
  this->check_tokens(u8"intrinsic"_sv, {Token_Type::kw_intrinsic});
  this->check_tokens(u8"is"_sv, {Token_Type::kw_is});
  this->check_tokens(u8"keyof"_sv, {Token_Type::kw_keyof});
  this->check_tokens(u8"module"_sv, {Token_Type::kw_module});
  this->check_tokens(u8"namespace"_sv, {Token_Type::kw_namespace});
  this->check_tokens(u8"never"_sv, {Token_Type::kw_never});
  this->check_tokens(u8"number"_sv, {Token_Type::kw_number});
  this->check_tokens(u8"object"_sv, {Token_Type::kw_object});
  this->check_tokens(u8"out"_sv, {Token_Type::kw_out});
  this->check_tokens(u8"override"_sv, {Token_Type::kw_override});
  this->check_tokens(u8"readonly"_sv, {Token_Type::kw_readonly});
  this->check_tokens(u8"require"_sv, {Token_Type::kw_require});
  this->check_tokens(u8"string"_sv, {Token_Type::kw_string});
  this->check_tokens(u8"symbol"_sv, {Token_Type::kw_symbol});
  this->check_tokens(u8"type"_sv, {Token_Type::kw_type});
  this->check_tokens(u8"undefined"_sv, {Token_Type::kw_undefined});
  this->check_tokens(u8"unique"_sv, {Token_Type::kw_unique});
  this->check_tokens(u8"unknown"_sv, {Token_Type::kw_unknown});
}

TEST_F(
    Test_Lex,
    lex_reserved_keywords_except_await_and_yield_sometimes_cannot_contain_escape_sequences) {
  // TODO(#73): Also lex 'protected', 'implements', etc. as
  // reserved_keyword_with_escape_sequence in strict mode.
  for (String8 keyword : disallowed_binding_identifier_keywords) {
    Padded_String code(escape_first_character_in_keyword(keyword));
    SCOPED_TRACE(code);
    Diag_Collector errors;
    Lexer& l = this->make_lexer(&code, &errors);

    EXPECT_THAT(l.peek().type,
                Token_Type::reserved_keyword_with_escape_sequence);
    EXPECT_THAT(l.peek().identifier_name().normalized_name(), keyword);
    EXPECT_THAT(errors.errors, IsEmpty());

    l.peek().report_errors_for_escape_sequences_in_keyword(&errors);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &code, Diag_Keywords_Cannot_Contain_Escape_Sequences,  //
                    escape_sequence, 0, u8"\\u{??}"_sv)}));
  }
}

TEST_F(
    Test_Lex,
    lex_contextual_keywords_and_await_and_yield_can_contain_escape_sequences) {
  for (String8 keyword : contextual_keywords) {
    String8 code = escape_first_character_in_keyword(keyword);
    SCOPED_TRACE(out_string8(code));
    SCOPED_TRACE(out_string8(keyword));
    this->check_single_token(code, keyword);
  }
}

TEST_F(Test_Lex, lex_single_character_symbols) {
  this->check_tokens(u8"!"_sv, {Token_Type::bang});
  this->check_tokens(u8"%"_sv, {Token_Type::percent});
  this->check_tokens(u8"&"_sv, {Token_Type::ampersand});
  this->check_tokens(u8"("_sv, {Token_Type::left_paren});
  this->check_tokens(u8")"_sv, {Token_Type::right_paren});
  this->check_tokens(u8"*"_sv, {Token_Type::star});
  this->check_tokens(u8"+"_sv, {Token_Type::plus});
  this->check_tokens(u8","_sv, {Token_Type::comma});
  this->check_tokens(u8"-"_sv, {Token_Type::minus});
  this->check_tokens(u8"."_sv, {Token_Type::dot});
  this->check_tokens(u8"/"_sv, {Token_Type::slash});
  this->check_tokens(u8":"_sv, {Token_Type::colon});
  this->check_tokens(u8";"_sv, {Token_Type::semicolon});
  this->check_tokens(u8"<"_sv, {Token_Type::less});
  this->check_tokens(u8"="_sv, {Token_Type::equal});
  this->check_tokens(u8">"_sv, {Token_Type::greater});
  this->check_tokens(u8"?"_sv, {Token_Type::question});
  this->check_tokens(u8"@"_sv, {Token_Type::at});
  this->check_tokens(u8"["_sv, {Token_Type::left_square});
  this->check_tokens(u8"]"_sv, {Token_Type::right_square});
  this->check_tokens(u8"^"_sv, {Token_Type::circumflex});
  this->check_tokens(u8"{"_sv, {Token_Type::left_curly});
  this->check_tokens(u8"|"_sv, {Token_Type::pipe});
  this->check_tokens(u8"}"_sv, {Token_Type::right_curly});
  this->check_tokens(u8"~"_sv, {Token_Type::tilde});
}

TEST_F(Test_Lex, lex_multi_character_symbols) {
  this->check_tokens(u8"<="_sv, {Token_Type::less_equal});
  this->check_tokens(u8">="_sv, {Token_Type::greater_equal});
  this->check_tokens(u8"=="_sv, {Token_Type::equal_equal});
  this->check_tokens(u8"==="_sv, {Token_Type::equal_equal_equal});
  this->check_tokens(u8"!="_sv, {Token_Type::bang_equal});
  this->check_tokens(u8"!=="_sv, {Token_Type::bang_equal_equal});
  this->check_tokens(u8"**"_sv, {Token_Type::star_star});
  this->check_tokens(u8"++"_sv, {Token_Type::plus_plus});
  this->check_tokens(u8"--"_sv, {Token_Type::minus_minus});
  this->check_tokens(u8"<<"_sv, {Token_Type::less_less});
  this->check_tokens(u8">>"_sv, {Token_Type::greater_greater});
  this->check_tokens(u8">>>"_sv, {Token_Type::greater_greater_greater});
  this->check_tokens(u8"&&"_sv, {Token_Type::ampersand_ampersand});
  this->check_tokens(u8"||"_sv, {Token_Type::pipe_pipe});
  this->check_tokens(u8"+="_sv, {Token_Type::plus_equal});
  this->check_tokens(u8"-="_sv, {Token_Type::minus_equal});
  this->check_tokens(u8"*="_sv, {Token_Type::star_equal});
  this->check_tokens(u8"/="_sv, {Token_Type::slash_equal});
  this->check_tokens(u8"%="_sv, {Token_Type::percent_equal});
  this->check_tokens(u8"**="_sv, {Token_Type::star_star_equal});
  this->check_tokens(u8"&&="_sv, {Token_Type::ampersand_ampersand_equal});
  this->check_tokens(u8"&="_sv, {Token_Type::ampersand_equal});
  this->check_tokens(u8"?."_sv, {Token_Type::question_dot});
  this->check_tokens(u8"??"_sv, {Token_Type::question_question});
  this->check_tokens(u8"?\x3f="_sv, {Token_Type::question_question_equal});
  this->check_tokens(u8"^="_sv, {Token_Type::circumflex_equal});
  this->check_tokens(u8"|="_sv, {Token_Type::pipe_equal});
  this->check_tokens(u8"||="_sv, {Token_Type::pipe_pipe_equal});
  this->check_tokens(u8"<<="_sv, {Token_Type::less_less_equal});
  this->check_tokens(u8">>="_sv, {Token_Type::greater_greater_equal});
  this->check_tokens(u8">>>="_sv, {Token_Type::greater_greater_greater_equal});
  this->check_tokens(u8"=>"_sv, {Token_Type::equal_greater});
  this->check_tokens(u8"..."_sv, {Token_Type::dot_dot_dot});
}

TEST_F(Test_Lex, lex_adjacent_symbols) {
  this->check_tokens(u8"{}"_sv,
                     {Token_Type::left_curly, Token_Type::right_curly});
  this->check_tokens(u8"[]"_sv,
                     {Token_Type::left_square, Token_Type::right_square});
  this->check_tokens(u8"/!"_sv, {Token_Type::slash, Token_Type::bang});
  this->check_tokens(u8"*=="_sv, {Token_Type::star_equal, Token_Type::equal});
  this->check_tokens(u8"^>>"_sv,
                     {Token_Type::circumflex, Token_Type::greater_greater});
}

TEST_F(Test_Lex, lex_symbols_separated_by_whitespace) {
  this->check_tokens(u8"{ }"_sv,
                     {Token_Type::left_curly, Token_Type::right_curly});
  this->check_tokens(u8"< ="_sv, {Token_Type::less, Token_Type::equal});
  this->check_tokens(u8"? ."_sv, {Token_Type::question, Token_Type::dot});
  this->check_tokens(u8". . ."_sv,
                     {Token_Type::dot, Token_Type::dot, Token_Type::dot});
}

TEST_F(Test_Lex, question_followed_by_number_is_not_question_dot) {
  this->check_tokens(u8"?.3"_sv, {Token_Type::question, Token_Type::number});
}

TEST_F(Test_Lex, question_dot_followed_by_non_digit_is_question_dot) {
  this->check_tokens(u8"?.e"_sv,
                     {Token_Type::question_dot, Token_Type::identifier});
}

TEST_F(Test_Lex, lex_whitespace) {
  for (const Char8* whitespace : {
           // Introduced in ECMAScript 3 (Unicode 2.1):
           u8"\n",      // 0x0a           Line Feed
           u8"\r",      // 0x0d           Carriage Return
           u8"\r\n",    // 0x0d 0x0a      Carriage Return & Line Feed
           u8"\u2028",  // 0xe2 0x80 0xa8 Line Separator
           u8"\u2029",  // 0xe2 0x80 0xa9 Paragraph Separator
           u8" ",       // 0x20           Space
           u8"\t",      // 0x09           Horizontal Tabulation
           u8"\f",      // 0x0c           Form Feed
           u8"\v",      // 0x0b           Vertical Tabulation
           u8"\u00a0",  // 0xc2 0xa0      No-Break Space (NBSP)
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
           u8"\u3000",  // 0xe3 0x80 0x80 Ideographic Space

           // Introduced in ECMAScript 5.1 (Unicode 3.0):
           u8"\u1680",  // 0xe1 0x9a 0x80 Ogham Space Mark
           u8"\u202f",  // 0xe2 0x80 0xaf Narrow No-Break Space (NNBSP)
           u8"\ufeff",  // 0xef 0xbb 0xbf Zero Width No-Break Space (BOM,
                        // ZWNBSP)

           // Introduced in ECMAScript 6.0 (Unicode 5.1.0):
           u8"\u205f",  // 0xe2 0x81 0x9f Medium Mathematical Space (MMSP)

           // Introduced in ECMAScript 3 (Unicode 2.1) but removed in ECMAScript
           // 6.0 (Unicode 5.1.0):
           // u8"\u200b",  // 0xe2 0x80 0x8b Zero Width Space

           // Introduced in ECMAScript 6.0 (Unicode 5.1.0) but removed in
           // ECMAScript 2016 (Unicode 8.0.0):
           // u8"\u180e",  // 0xe1 0xa0 0x8e Mongolian Vowel Separator
       }) {
    {
      String8 input = String8(u8"a") + whitespace + u8"b";
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input,
                         {Token_Type::identifier, Token_Type::identifier});
    }

    {
      String8 input =
          String8(whitespace) + u8"10" + whitespace + u8"'hi'" + whitespace;
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input, {Token_Type::number, Token_Type::string});
    }

    {
      String8 input =
          String8(u8"async") + whitespace + u8"function" + whitespace;
      SCOPED_TRACE(out_string8(input));
      this->check_tokens(input,
                         {Token_Type::kw_async, Token_Type::kw_function});
    }
  }
}

TEST_F(Test_Lex, unicode_next_line_is_invalid_in_javascript) {
  // TODO(strager): Treat U+0085 as whitespace but report a nice diagnostic.
  Diag_Collector v;
  Padded_String code(u8"a\u0085false"_sv);
  auto error = /* */ u8" ^^^^^^ Diag_Character_Disallowed_In_Identifiers"_diag;
  Lexer l(&code, &v, Lexer_Options{.typescript = false});

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file)
      << "U+0085 should be interpreted as part of the identifier";

  assert_diagnostics(&code, v.errors, {error});
}

TEST_F(Test_Lex, unicode_next_line_is_horizontal_whitespace_in_typescript) {
  Diag_Collector v;
  Padded_String code(u8"a\u0085false"_sv);
  Lexer l(&code, &v, Lexer_Options{.typescript = true});

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::kw_false);
  EXPECT_FALSE(l.peek().has_leading_newline);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST_F(Test_Lex, lex_shebang) {
  this->check_tokens(u8"#!/usr/bin/env node\nhello"_sv,
                     {Token_Type::identifier});
  this->check_tokens(u8"#!ignored\n123"_sv, {Token_Type::number});
}

TEST_F(Test_Lex, lex_not_shebang) {
  // Whitespace must not appear between '#' and '!'.
  {
    Diag_Collector v;
    Padded_String input(u8"# !notashebang"_sv);
    auto error = /*  */ u8"^ Diag_Unexpected_Hash_Character"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::bang) << "# should be skipped";

    assert_diagnostics(&input, v.errors, {error});
  }

  // '#!' must be on the first line.
  {
    Diag_Collector v;
    Padded_String input(u8"\n#!notashebang\n"_sv);
    auto error = /*  */ u8"  ^ Diag_Unexpected_Hash_Character"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::bang) << "# should be skipped";

    assert_diagnostics(&input, v.errors, {error});
  }

  // Whitespace must not appear before '#!'.
  {
    Diag_Collector v;
    Padded_String input(u8"  #!notashebang\n"_sv);
    auto error = /*  */ u8"  ^ Diag_Unexpected_Hash_Character"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::bang) << "# should be skipped";

    assert_diagnostics(&input, v.errors, {error});
  }

  {
    Diag_Collector v;
    // clang-format off
    Padded_String input(u8"#\\u{21}\n"_sv);
    auto error = /*  */ u8" ^^^^^^^ Diag_Escaped_Character_Disallowed_In_Identifiers"_diag;
    // clang-format on
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::private_identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"#\\u{21}"_sv);

    assert_diagnostics(&input, v.errors, {error});
  }
}

TEST_F(Test_Lex, lex_unexpected_bom_before_shebang) {
  // BOM must not appear before '#!'.
  {
    Diag_Collector v;
    Padded_String input(u8"\ufeff#!notashebang\n"_sv);
    auto error = /*  */ u8"^^^^^^ Diag_Unexpected_Bom_Before_Shebang"_diag;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file) << "# should be skipped";

    assert_diagnostics(&input, v.errors, {error});
  }
}

TEST_F(Test_Lex, ascii_control_characters_are_disallowed) {
  for (String8_View control_character : control_characters_except_whitespace) {
    Padded_String input(String8(control_character) + u8"hello");
    SCOPED_TRACE(input);
    Diag_Collector v;

    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::identifier)
        << "control character should be skipped";
    EXPECT_THAT(
        v.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(&input, Diag_Unexpected_Control_Character,  //
                              character, 0, control_character),
        }));
  }
}

TEST_F(Test_Lex, ascii_control_characters_sorta_treated_like_whitespace) {
  for (String8_View control_character : control_characters_except_whitespace) {
    Padded_String input(concat(u8"  "_sv, control_character, u8"  hello"_sv));
    SCOPED_TRACE(input);
    Diag_Collector v;
    Lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, Token_Type::identifier)
        << "control character should be skipped";
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
  }
}

TEST_F(Test_Lex, lex_token_notes_leading_newline) {
  for (String8_View line_terminator : line_terminators) {
    Padded_String code(concat(u8"a b"_sv, line_terminator, u8"c d"_sv));
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // b
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // c
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // d
  }
}

TEST_F(Test_Lex, lex_token_notes_leading_newline_after_single_line_comment) {
  for (String8_View line_terminator : line_terminators) {
    Padded_String code(concat(u8"a // hello"_sv, line_terminator, u8"b"_sv));
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // b
  }
}

TEST_F(Test_Lex, lex_token_notes_leading_newline_after_comment_with_newline) {
  for (String8_View line_terminator : line_terminators) {
    Padded_String code(concat(u8"a /*"_sv, line_terminator, u8"*/ b"_sv));
    Lexer l(&code, &Null_Diag_Reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // b
  }
}

TEST_F(Test_Lex, lex_token_notes_leading_newline_after_comment) {
  Padded_String code(u8"a /* comment */\nb"_sv);
  Lexer l(&code, &Null_Diag_Reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // b
}

TEST_F(Test_Lex, inserting_semicolon_at_newline_remembers_next_token) {
  Padded_String code(u8"hello\nworld"_sv);
  Lexer l(&code, &Null_Diag_Reporter::instance);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello"_sv);
  EXPECT_FALSE(l.peek().has_leading_newline);
  const Char8* hello_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world"_sv);
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, hello_end);
  EXPECT_EQ(l.peek().end, hello_end);
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world"_sv);
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
}

TEST_F(Test_Lex, insert_semicolon_at_beginning_of_input) {
  Padded_String code(u8"hello world"_sv);
  Lexer l(&code, &Null_Diag_Reporter::instance);

  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, code.data());
  EXPECT_EQ(l.peek().end, code.data());

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"hello"_sv);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world"_sv);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
}

TEST_F(Test_Lex, inserting_semicolon_at_right_curly_remembers_next_token) {
  Padded_String code(u8"{ x }"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::left_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"x"_sv);
  EXPECT_FALSE(l.peek().has_leading_newline);
  const Char8* x_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::right_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, Token_Type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, x_end);
  EXPECT_EQ(l.peek().end, x_end);
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::right_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

  EXPECT_THAT(errors.errors, IsEmpty());
}

TEST_F(Test_Lex, transaction_buffers_errors_until_commit) {
  Padded_String code(u8"x 0b y"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(transaction));
  EXPECT_THAT(errors.errors,
              ElementsAreArray({DIAG_TYPE(Diag_No_Digits_In_Binary_Number)}));
}

TEST_F(Test_Lex, nested_transaction_buffers_errors_until_outer_commit) {
  Padded_String code(u8"x y 0b z"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // y

  Lexer_Transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);  // 0b
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(inner_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "committing inner_transaction should not report 0b error";

  l.commit_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors, ElementsAreArray({
                                 DIAG_TYPE(Diag_No_Digits_In_Binary_Number),
                             }))
      << "committing outer_transaction should report 0b error";
}

TEST_F(Test_Lex, rolled_back_inner_transaction_discards_errors) {
  Padded_String code(u8"x y 0b z"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // y

  Lexer_Transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);  // 0b

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.roll_back_transaction(std::move(inner_transaction));
  l.commit_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";
}

TEST_F(Test_Lex, rolled_back_outer_transaction_discards_errors) {
  Padded_String code(u8"x y 0b z"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // x
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction outer_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // y

  Lexer_Transaction inner_transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);  // 0b

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);  // z
  EXPECT_THAT(errors.errors, IsEmpty());

  l.commit_transaction(std::move(inner_transaction));
  l.roll_back_transaction(std::move(outer_transaction));
  EXPECT_THAT(errors.errors, IsEmpty())
      << "0b error shouldn't be written to error reporter";
}

TEST_F(Test_Lex, errors_after_transaction_commit_are_reported_unbuffered) {
  Padded_String code(u8"x 'y' 0b"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  l.commit_transaction(std::move(transaction));
  EXPECT_EQ(l.peek().type, Token_Type::string);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);
  EXPECT_THAT(errors.errors,
              ElementsAreArray({DIAG_TYPE(Diag_No_Digits_In_Binary_Number)}));
}

TEST_F(Test_Lex, errors_after_transaction_rollback_are_reported_unbuffered) {
  Padded_String code(u8"x 'y' 0b"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction transaction = l.begin_transaction();
  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  l.roll_back_transaction(std::move(transaction));

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);
  EXPECT_THAT(errors.errors,
              ElementsAreArray({DIAG_TYPE(Diag_No_Digits_In_Binary_Number)}));
}

TEST_F(Test_Lex, rolling_back_transaction) {
  Padded_String code(u8"x 'y' 3"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  Lexer_Transaction transaction = l.begin_transaction();
  EXPECT_EQ(l.peek().type, Token_Type::identifier);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);

  l.roll_back_transaction(std::move(transaction));
  EXPECT_EQ(l.peek().type, Token_Type::identifier);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);
}

TEST_F(Test_Lex, insert_semicolon_after_rolling_back_transaction) {
  Padded_String code(u8"x 'y' 3"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, Token_Type::identifier);
  EXPECT_THAT(errors.errors, IsEmpty());

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  Lexer_Transaction transaction = l.begin_transaction();

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);

  l.roll_back_transaction(std::move(transaction));
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, Token_Type::semicolon);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::string);

  l.skip();
  EXPECT_EQ(l.peek().type, Token_Type::number);
}

TEST_F(Test_Lex, unfinished_transaction_does_not_leak_memory) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  Padded_String code(u8"a b c d e f g"_sv);
  Diag_Collector errors;
  Lexer l(&code, &errors);

  [[maybe_unused]] Lexer_Transaction outer_transaction = l.begin_transaction();
  l.skip();

  [[maybe_unused]] Lexer_Transaction inner_transaction = l.begin_transaction();
  l.skip();

  // Don't end either transaction. The leak checker should report no leaks.
}

TEST_F(Test_Lex,
       is_initial_identifier_byte_agrees_with_is_initial_identifier_character) {
  constexpr char32_t min_code_point = U'\0';
  constexpr char32_t max_code_point = U'\U0010ffff';

  std::array<bool, 256> is_valid_byte = {};
  is_valid_byte[u8'\\'] = true;
  for (char32_t c = min_code_point; c <= max_code_point; ++c) {
    if (Lexer::is_initial_identifier_character(c)) {
      Char8 utf_8[10];
      encode_utf_8(c, utf_8);
      is_valid_byte[static_cast<std::uint8_t>(utf_8[0])] = true;
    }
  }

  for (std::size_t byte = 0; byte < is_valid_byte.size(); ++byte) {
    EXPECT_EQ(Lexer::is_initial_identifier_byte(static_cast<Char8>(byte)),
              is_valid_byte[byte])
        << "byte = 0x" << std::hex << byte;
  }
}

TEST_F(Test_Lex, is_identifier_byte_agrees_with_is_identifier_character) {
  constexpr char32_t min_code_point = U'\0';
  constexpr char32_t max_code_point = U'\U0010ffff';

  std::array<bool, 256> is_valid_byte = {};
  is_valid_byte[u8'\\'] = true;
  for (char32_t c = min_code_point; c <= max_code_point; ++c) {
    if (Lexer::is_identifier_character(c, Lexer::Identifier_Kind::javascript)) {
      Char8 utf_8[10];
      encode_utf_8(c, utf_8);
      is_valid_byte[static_cast<std::uint8_t>(utf_8[0])] = true;
    }
  }

  for (std::size_t byte = 0; byte < is_valid_byte.size(); ++byte) {
    EXPECT_EQ(Lexer::is_identifier_byte(static_cast<Char8>(byte)),
              is_valid_byte[byte])
        << "byte = 0x" << std::hex << byte;
  }
}

TEST_F(Test_Lex, jsx_identifier) {
  auto check_identifier = [](String8_View tag_code,
                             String8_View expected_normalized) -> void {
    SCOPED_TRACE(out_string8(tag_code));

    Padded_String code(u8"!" + String8(tag_code));
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, Token_Type::identifier);
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

  for (String8_View line_terminator : line_terminators) {
    check_identifier(
        String8(line_terminator) + u8"banana-split" + String8(line_terminator),
        u8"banana-split"_sv);
  }

  check_identifier(u8"<!-- line comment\nbanana-split"_sv, u8"banana-split"_sv);
  check_identifier(u8"\n--> line comment\nbanana-split"_sv,
                   u8"banana-split"_sv);

  check_identifier(u8"\u00c1gua"_sv, u8"\u00c1gua"_sv);
  check_identifier(u8"\u00c1gua-"_sv, u8"\u00c1gua-"_sv);
}

TEST_F(Test_Lex, invalid_jsx_identifier) {
  this->lex_jsx_tokens = true;

  this->check_tokens_with_errors(
      u8"<hello\\u{2d}world>"_sv,  //
      u8"      ^^^^^^^ Diag_Escaped_Hyphen_Not_Allowed_In_JSX_Tag"_diag,
      {Token_Type::less, Token_Type::identifier, Token_Type::greater});
}

TEST_F(Test_Lex, jsx_string) {
  auto check_string = [](String8_View string_code) -> void {
    SCOPED_TRACE(out_string8(string_code));

    Padded_String code(u8"!" + String8(string_code));
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, Token_Type::string);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  };

  check_string(u8R"('hello')"_sv);
  check_string(u8R"("hello")"_sv);

  // Backslashes are ignored.
  check_string(u8R"("hello\nworld")"_sv);
  check_string(u8R"("hello\")"_sv);
  check_string(u8R"("hello\\")"_sv);
  check_string(u8R"('hello\')"_sv);
  check_string(u8R"('hello\\')"_sv);
  check_string(u8R"('hello\u{}world')"_sv);
  check_string(u8R"('hello\u00xyworld')"_sv);
  check_string(u8R"('hello\u00')"_sv);

  // Null bytes are allowed.
  check_string(u8"'hello\u0000world'"_sv);
  check_string(u8"'\u0000world'"_sv);
  check_string(u8"'hello\u0000'"_sv);

  // Line terminators are allowed.
  for (String8_View line_terminator : line_terminators) {
    check_string(u8"'hello" + String8(line_terminator) + u8"world'");
  }
}

// Despite what the JSX specification says, comments are not interpreted in
// attribute strings.
// https://github.com/facebook/jsx/pull/133
TEST_F(Test_Lex, jsx_string_ignores_comments) {
  {
    Padded_String code(u8"! 'hello // '\nworld'"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, Token_Type::string);
    EXPECT_EQ(l.peek().begin, &code[u8"! "_sv.size()]);
    EXPECT_EQ(l.peek().end, &code[u8"! 'hello // '"_sv.size()])
        << "string should end at ', treating // as part of the string";

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"world"_sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    Padded_String code(u8R"(! "hello/* not"comment */world")"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '!'.

    EXPECT_EQ(l.peek().type, Token_Type::string);
    EXPECT_EQ(l.peek().begin, &code[u8"! "_sv.size()]);
    EXPECT_EQ(l.peek().end, &code[u8R"(! "hello/* not")"_sv.size()])
        << "string should end at \", treating /* as part of the string";

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"comment"_sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, unterminated_jsx_string) {
  Padded_String code(u8"! 'hello"_sv);
  auto error = /* */ u8"  ^ Diag_Unclosed_JSX_String_Literal"_diag;
  Diag_Collector errors;
  Lexer l(&code, &errors);
  l.skip_in_jsx();  // Ignore '!'.

  EXPECT_EQ(l.peek().type, Token_Type::string);
  assert_diagnostics(&code, errors.errors, {error});

  l.skip_in_jsx();
  EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
}

TEST_F(Test_Lex, jsx_tag) {
  {
    Padded_String code(u8"<svg:rect>"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"svg"_sv);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::colon);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"rect"_sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    Padded_String code(u8"<myModule.MyComponent>"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"myModule"_sv);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::dot);

    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    EXPECT_EQ(l.peek().identifier_name().normalized_name(), u8"MyComponent"_sv);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, jsx_text_children) {
  {
    Padded_String code(u8"<>hello world"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.

    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    EXPECT_THAT(errors.errors, IsEmpty());
  }

  {
    Padded_String code(u8"<>hello</>"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.

    EXPECT_EQ(l.peek().type, Token_Type::less);
    EXPECT_EQ(l.peek().begin, &code[u8"<>hello"_sv.size()]);
    EXPECT_EQ(l.peek().end, &code[u8"<>hello<"_sv.size()]);
    EXPECT_THAT(errors.errors, IsEmpty());
  }

  // '>=' might be interpreted as greater_equal by a buggy lexer, for example.
  for (String8 text_begin : {u8"=", u8">", u8">>", u8">=", u8">>="}) {
    Padded_String code(u8"<>" + text_begin + u8"hello");
    SCOPED_TRACE(code);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    EXPECT_EQ(l.peek().type, Token_Type::greater);
    EXPECT_EQ(l.peek().begin, &code[u8"<"_sv.size()]);
    EXPECT_EQ(l.peek().end, &code[u8"<>"_sv.size()]);
    l.skip_in_jsx_children();

    EXPECT_EQ(l.peek().type, Token_Type::end_of_file);
    if (text_begin == u8"=") {
      EXPECT_THAT(errors.errors, IsEmpty());
    } else if (text_begin == u8">" || text_begin == u8">=") {
      assert_diagnostics(&code, errors.errors,
                         {
                             // <>>hello
                             // <>>=hello
                             u8"  ^ Diag_Unexpected_Greater_In_JSX_Text"_diag,
                         });
    } else if (text_begin == u8">>" || text_begin == u8">>=") {
      assert_diagnostics(&code, errors.errors,
                         {
                             // <>>>hello
                             // <>>>=hello
                             u8"   ^ Diag_Unexpected_Greater_In_JSX_Text"_diag,
                             u8"  ^ Diag_Unexpected_Greater_In_JSX_Text"_diag,
                         });
    } else {
      QLJS_UNREACHABLE();
    }
  }
}

TEST_F(Test_Lex, jsx_illegal_text_children) {
  {
    Padded_String code(u8"<>hello>world</>"_sv);
    auto error = /* */ u8"       ^ Diag_Unexpected_Greater_In_JSX_Text"_diag;
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.
    EXPECT_EQ(l.peek().type, Token_Type::less);
    assert_diagnostics(&code, errors.errors, {error});
  }

  {
    // clang-format off
    Padded_String code(u8"<>hello}world</>"_sv);
    auto error = /* */ u8"       ^ Diag_Unexpected_Right_Curly_In_JSX_Text"_diag;
    // clang-format on
    Diag_Collector errors;
    Lexer l(&code, &errors);
    l.skip_in_jsx();  // Ignore '<'.

    l.skip_in_jsx_children();  // Skip '>'.
    EXPECT_EQ(l.peek().type, Token_Type::less);
    assert_diagnostics(&code, errors.errors, {error});
  }
}

TEST_F(Test_Lex, jsx_expression_children) {
  this->lex_jsx_tokens = true;

  {
    Padded_String code(u8"<>hello {name}!</>"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);

    // <>hello
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);

    // {name}
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, Token_Type::left_curly);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, Token_Type::right_curly);

    // !</>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST_F(Test_Lex, jsx_nested_children) {
  this->lex_jsx_tokens = true;

  {
    Padded_String code(u8"<>hello <span>world</span>!</>"_sv);
    Diag_Collector errors;
    Lexer l(&code, &errors);
    // <>hello
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);

    // <span>world</span>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::identifier);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);

    // !</>
    l.skip_in_jsx_children();
    EXPECT_EQ(l.peek().type, Token_Type::less);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::slash);
    l.skip_in_jsx();
    EXPECT_EQ(l.peek().type, Token_Type::greater);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

void Test_Lex::check_single_token(String8_View input,
                                  String8_View expected_identifier_name,
                                  Source_Location local_caller) {
  static Source_Location caller;
  caller = local_caller;
  this->check_single_token_with_errors(
      input, expected_identifier_name,
      [](Padded_String_View, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void Test_Lex::check_single_token_with_errors(
    String8_View input, String8_View expected_identifier_name,
    void (*check_errors)(Padded_String_View input,
                         const std::vector<Diag_Collector::Diag>&),
    Source_Location caller) {
  Padded_String code(input);
  Diag_Collector errors;
  std::vector<Token> lexed_tokens = this->lex_to_eof(&code, errors);

  EXPECT_THAT_AT_CALLER(
      lexed_tokens,
      ElementsAreArray({
          ::testing::Field("type", &Token::type,
                           ::testing::AnyOf(Token_Type::identifier,
                                            Token_Type::private_identifier)),
      }));
  if (lexed_tokens.size() == 1 &&
      (lexed_tokens[0].type == Token_Type::identifier ||
       lexed_tokens[0].type == Token_Type::private_identifier)) {
    EXPECT_EQ_AT_CALLER(lexed_tokens[0].identifier_name().normalized_name(),
                        expected_identifier_name);
  }
  check_errors(&code, errors.errors);
}

void Test_Lex::check_single_token_with_errors(
    String8_View input, Diagnostic_Assertion diag0,
    String8_View expected_identifier_name, Source_Location caller) {
  Padded_String code(input);
  Diag_Collector errors;
  std::vector<Token> lexed_tokens = this->lex_to_eof(&code, errors);

  EXPECT_THAT_AT_CALLER(
      lexed_tokens,
      ElementsAreArray({
          ::testing::Field("type", &Token::type,
                           ::testing::AnyOf(Token_Type::identifier,
                                            Token_Type::private_identifier)),
      }));
  if (lexed_tokens.size() == 1 &&
      (lexed_tokens[0].type == Token_Type::identifier ||
       lexed_tokens[0].type == Token_Type::private_identifier)) {
    EXPECT_EQ_AT_CALLER(lexed_tokens[0].identifier_name().normalized_name(),
                        expected_identifier_name);
  }

  assert_diagnostics(&code, errors.errors, {diag0}, caller);
}

void Test_Lex::check_tokens(
    String8_View input, std::initializer_list<Token_Type> expected_token_types,
    Source_Location local_caller) {
  static Source_Location caller;
  caller = local_caller;
  this->check_tokens_with_errors(
      input, expected_token_types,
      [](Padded_String_View, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void Test_Lex::check_tokens(
    Padded_String_View input,
    std::initializer_list<Token_Type> expected_token_types,
    Source_Location local_caller) {
  static Source_Location caller;
  caller = local_caller;
  this->check_tokens_with_errors(
      input, expected_token_types,
      [](Padded_String_View, const auto& errors) {
        EXPECT_THAT_AT_CALLER(errors, IsEmpty());
      },
      caller);
}

void Test_Lex::check_tokens_with_errors(
    String8_View input, Diagnostic_Assertion diag0,
    std::initializer_list<Token_Type> expected_token_types,
    Source_Location caller) {
  Diagnostic_Assertion diag_assertions[] = {diag0};
  this->check_tokens_with_errors(
      input, Span<const Diagnostic_Assertion>(diag_assertions),
      expected_token_types, caller);
}

void Test_Lex::check_tokens_with_errors(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    std::initializer_list<Token_Type> expected_token_types,
    Source_Location caller) {
  Diagnostic_Assertion diag_assertions[] = {diag0, diag1};
  this->check_tokens_with_errors(
      input, Span<const Diagnostic_Assertion>(diag_assertions),
      expected_token_types, caller);
}

void Test_Lex::check_tokens_with_errors(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Diagnostic_Assertion diag2,
    std::initializer_list<Token_Type> expected_token_types,
    Source_Location caller) {
  Diagnostic_Assertion diag_assertions[] = {diag0, diag1, diag2};
  this->check_tokens_with_errors(
      input, Span<const Diagnostic_Assertion>(diag_assertions),
      expected_token_types, caller);
}

void Test_Lex::check_tokens_with_errors(
    String8_View input, Span<const Diagnostic_Assertion> diag_assertions,
    std::initializer_list<Token_Type> expected_token_types,
    Source_Location caller) {
  Padded_String code(input);
  Diag_Collector errors;
  std::vector<Token> lexed_tokens = this->lex_to_eof(&code, errors);

  std::vector<Token_Type> lexed_token_types;
  for (const Token& t : lexed_tokens) {
    lexed_token_types.push_back(t.type);
  }

  EXPECT_THAT_AT_CALLER(lexed_token_types,
                        ::testing::ElementsAreArray(expected_token_types));

  assert_diagnostics(&code, errors.errors, diag_assertions, caller);
}

void Test_Lex::check_tokens_with_errors(
    String8_View input, std::initializer_list<Token_Type> expected_token_types,
    void (*check_errors)(Padded_String_View input,
                         const std::vector<Diag_Collector::Diag>&),
    Source_Location caller) {
  Padded_String code(input);
  return this->check_tokens_with_errors(&code, expected_token_types,
                                        check_errors, caller);
}

void Test_Lex::check_tokens_with_errors(
    Padded_String_View input,
    std::initializer_list<Token_Type> expected_token_types,
    void (*check_errors)(Padded_String_View input,
                         const std::vector<Diag_Collector::Diag>&),
    Source_Location caller) {
  Diag_Collector errors;
  std::vector<Token> lexed_tokens = this->lex_to_eof(input, errors);

  std::vector<Token_Type> lexed_token_types;
  for (const Token& t : lexed_tokens) {
    lexed_token_types.push_back(t.type);
  }

  EXPECT_THAT_AT_CALLER(lexed_token_types,
                        ::testing::ElementsAreArray(expected_token_types));
  check_errors(input, errors.errors);
}

std::vector<Token> Test_Lex::lex_to_eof(Padded_String_View input,
                                        Source_Location caller) {
  Diag_Collector errors;
  std::vector<Token> tokens = this->lex_to_eof(input, errors);
  EXPECT_THAT_AT_CALLER(errors.errors, IsEmpty());
  return tokens;
}

std::vector<Token> Test_Lex::lex_to_eof(Padded_String_View input,
                                        Diag_Collector& errors) {
  Lexer& l = this->make_lexer(input, &errors);
  std::vector<Token> tokens;
  while (l.peek().type != Token_Type::end_of_file) {
    tokens.push_back(l.peek());
    if (this->lex_jsx_tokens) {
      l.skip_in_jsx();
    } else {
      l.skip();
    }
  }
  return tokens;
}

std::vector<Token> Test_Lex::lex_to_eof(String8_View input,
                                        Source_Location caller) {
  Padded_String real_input(input);
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
