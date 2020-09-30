// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <array>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/characters.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <string_view>
#include <type_traits>

using namespace std::literals::string_view_literals;

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
void check_single_token(const char8* input, token_type expected_token_type);
void check_single_token(padded_string_view input,
                        token_type expected_token_type);
void check_single_token(const char8* input,
                        string8_view expected_identifier_name);
void check_single_token(const string8& input,
                        string8_view expected_identifier_name);
void check_tokens(const char8* input,
                  std::initializer_list<token_type> expected_token_types);
void check_tokens_with_errors(
    const char8* input, std::initializer_list<token_type> expected_token_types,
    void (*check_errors)(padded_string_view input,
                         const std::vector<error_collector::error>&));

TEST(test_lex, lex_block_comments) {
  check_single_token(u8"/* */ hello", u8"hello");
  check_single_token(u8"/*/ comment */ hi", u8"hi");
  check_single_token(u8"/* comment /*/ hi", u8"hi");
  check_single_token(u8"/* not /* nested */ ident", u8"ident");
  check_single_token(u8"/**/", token_type::end_of_file);

  {
    error_collector v;
    padded_string input(u8"hello /* unterminated comment ");
    lexer l(&input, &v);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unclosed_block_comment, comment_open,
                              offsets_matcher(&input, 6, 8))));
  }
}

TEST(test_lex, lex_line_comments) {
  check_single_token(u8"// hello", token_type::end_of_file);
  for (string8_view line_terminator : line_terminators) {
    check_single_token(u8"// hello" + string8(line_terminator) + u8"world",
                       u8"world");
  }
  check_single_token(u8"// hello\n// world", token_type::end_of_file);
  check_tokens(u8"hello//*/\n \n \nworld",
               {token_type::identifier, token_type::identifier});
}

TEST(test_lex, lex_line_comments_with_control_characters) {
  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"// hello " + string8(control_character) +
                        u8" world\n42.0");
    SCOPED_TRACE(input);
    check_single_token(&input, token_type::number);
  }
}

TEST(test_lex, lex_numbers) {
  check_single_token(u8"0", token_type::number);
  check_single_token(u8"2", token_type::number);
  check_single_token(u8"42", token_type::number);
  check_single_token(u8"12.34", token_type::number);
  check_single_token(u8".34", token_type::number);

  check_single_token(u8"1e3", token_type::number);
  check_single_token(u8".1e3", token_type::number);
  check_single_token(u8"1.e3", token_type::number);
  check_single_token(u8"1.0e3", token_type::number);
  check_single_token(u8"1e-3", token_type::number);
  check_single_token(u8"1e+3", token_type::number);
  check_single_token(u8"1E+3", token_type::number);
  check_single_token(u8"1E123_233_22", token_type::number);

  check_single_token(u8"0n", token_type::number);
  check_single_token(u8"123456789n", token_type::number);

  check_single_token(u8"123_123_123", token_type::number);
  check_single_token(u8"123.123_123", token_type::number);

  check_tokens(u8"123. 456", {token_type::number, token_type::number});

  check_tokens(u8"1.2.3", {token_type::number, token_type::number});
  check_tokens(u8".2.3", {token_type::number, token_type::number});
}

TEST(test_lex, lex_binary_numbers) {
  check_single_token(u8"0b0", token_type::number);
  check_single_token(u8"0b1", token_type::number);
  check_single_token(u8"0b010101010101010", token_type::number);
  check_single_token(u8"0B010101010101010", token_type::number);
}

TEST(test_lex, lex_hex_numbers) {
  check_single_token(u8"0x0", token_type::number);
  check_single_token(u8"0x123456789abcdef", token_type::number);
  check_single_token(u8"0X123456789ABCDEF", token_type::number);
}

TEST(test_lex, lex_number_with_trailing_garbage) {
  check_tokens_with_errors(
      u8"123abcd", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 3, 7))));
      });
  check_tokens_with_errors(
      u8"123e f", {token_type::number, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 3, 4))));
      });
  check_tokens_with_errors(
      u8"123e-f",
      {token_type::number, token_type::minus, token_type::identifier},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 3, 4))));
      });
  check_tokens_with_errors(
      u8"0b01234", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 4, 7))));
      });
  check_tokens_with_errors(
      u8"0b0h0lla", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 3, 8))));
      });
  check_tokens_with_errors(
      u8"0xabjjw", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_characters_in_number,
                                characters, offsets_matcher(input, 4, 7))));
      });
}

TEST(test_lex, lex_invalid_big_int_number) {
  check_tokens_with_errors(
      u8"12.34n", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_big_int_literal_contains_decimal_point,
                                where, offsets_matcher(input, 0, 6))));
      });
  check_tokens_with_errors(
      u8"0123n", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_big_int_literal_contains_leading_zero,
                                where, offsets_matcher(input, 0, 5))));
      });
  check_tokens_with_errors(
      u8"1e3n", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_big_int_literal_contains_exponent, where,
                                offsets_matcher(input, 0, 4))));
      });

  // Only complain about the decimal point, not the leading 0 digit.
  check_tokens_with_errors(
      u8"0.1n", {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(
            errors,
            ElementsAre(
                VariantWith<error_big_int_literal_contains_decimal_point>(_)));
      });

  // Complain about both the decimal point and the leading 0 digit.
  check_tokens_with_errors(
      u8"01.2n", {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                VariantWith<error_big_int_literal_contains_decimal_point>(_),
                VariantWith<error_big_int_literal_contains_leading_zero>(_)));
      });

  // Complain about everything. What a disaster.
  check_tokens_with_errors(
      u8"01.2e+3n", {token_type::number},
      [](padded_string_view, const auto& errors) {
        EXPECT_THAT(
            errors,
            UnorderedElementsAre(
                VariantWith<error_big_int_literal_contains_decimal_point>(_),
                VariantWith<error_big_int_literal_contains_exponent>(_),
                VariantWith<error_big_int_literal_contains_leading_zero>(_)));
      });
}

TEST(test_lex, lex_number_with_double_underscore) {
  check_tokens_with_errors(
      u8"123__000", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(ERROR_TYPE_FIELD(
                        error_number_literal_contains_consecutive_underscores,
                        underscores, offsets_matcher(input, 3, 5))));
      });
}

TEST(test_lex, lex_number_with_many_underscores) {
  check_tokens_with_errors(
      u8"123_____000", {token_type::number},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors,
                    ElementsAre(ERROR_TYPE_FIELD(
                        error_number_literal_contains_consecutive_underscores,
                        underscores, offsets_matcher(input, 3, 8))));
      });
}

TEST(test_lex, lex_number_with_multiple_groups_of_consecutive_underscores) {
  {
    error_collector v;
    padded_string input(u8"123__45___6");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::number);
    EXPECT_EQ(*l.peek().begin, '1');
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_FIELD(
                        error_number_literal_contains_consecutive_underscores,
                        underscores, offsets_matcher(&input, 3, 5)),
                    ERROR_TYPE_FIELD(
                        error_number_literal_contains_consecutive_underscores,
                        underscores, offsets_matcher(&input, 7, 10))));
  }
}

TEST(test_lex, lex_strings) {
  check_single_token(u8R"('hello')", token_type::string);
  check_single_token(u8R"("hello")", token_type::string);
  check_single_token(u8R"("hello\"world")", token_type::string);
  check_single_token(u8R"('hello\'world')", token_type::string);
  check_single_token(u8R"('hello"world')", token_type::string);
  check_single_token(u8R"("hello'world")", token_type::string);

  check_tokens_with_errors(
      u8R"("unterminated)", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unclosed_string_literal, string_literal,
                                offsets_matcher(input, 0, 13))));
      });

  for (string8_view line_terminator : line_terminators_except_ls_ps) {
    error_collector v;
    padded_string input(u8"'unterminated" + string8(line_terminator) +
                        u8"hello");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().string_view(), u8"hello");

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unclosed_string_literal, string_literal,
                              offsets_matcher(&input, 0, 13))));
  }

  check_tokens_with_errors(
      u8"'unterminated\\", {token_type::string},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unclosed_string_literal, string_literal,
                                offsets_matcher(input, 0, 14))));
      });

  // TODO(strager): Lex line continuations in string literals. For example:
  //
  // "hello\   (backslash followed by end of line)
  // world"

  // TODO(strager): Report invalid hex escape sequences. For example:
  //
  // "hello\x1qworld"
  // '\x'

  // TODO(strager): Report invalid unicode escape sequences. For example:
  //
  // "hello\u"
  // "hello\u{110000}"

  // TODO(strager): Report octal escape sequences in strict mode.

  // TODO(strager): Report invalid octal escape sequences in non-strict mode.
}

TEST(test_lex, lex_string_with_ascii_control_characters) {
  for (string8_view control_character :
       concat(control_characters_except_line_terminators, ls_and_ps)) {
    padded_string input(u8"'hello" + string8(control_character) + u8"world'");
    SCOPED_TRACE(input);
    check_single_token(&input, token_type::string);
  }

  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"'hello\\" + string8(control_character) + u8"world'");
    SCOPED_TRACE(input);
    check_single_token(&input, token_type::string);
  }
}

TEST(test_lex, lex_templates) {
  check_tokens(u8"``", {token_type::complete_template});
  check_tokens(u8"`hello`", {token_type::complete_template});
  check_tokens(u8"`hello$world`", {token_type::complete_template});
  check_tokens(u8"`hello{world`", {token_type::complete_template});
  check_tokens(u8R"(`hello\`world`)", {token_type::complete_template});
  check_tokens(u8R"(`hello$\{world`)", {token_type::complete_template});
  check_tokens(u8R"(`hello\${world`)", {token_type::complete_template});
  check_tokens(
      u8R"(`hello
world`)",
      {token_type::complete_template});

  {
    padded_string code(u8"`hello${42}`");
    lexer l(&code, &null_error_reporter::instance);
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
    padded_string code(u8"`${42}world`");
    lexer l(&code, &null_error_reporter::instance);
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
    padded_string code(u8"`${left}${right}`");
    lexer l(&code, &null_error_reporter::instance);
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

  // TODO(strager): Lex line continuations in templates. For example:
  //
  // `hello\   (backslash followed by end of line)
  // world`

  check_tokens_with_errors(
      u8"`unterminated", {token_type::complete_template},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unclosed_template, incomplete_template,
                                offsets_matcher(input, 0, 13))));
      });

  {
    error_collector v;
    padded_string input(u8"`${un}terminated");
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

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unclosed_template, incomplete_template,
                              offsets_matcher(&input, 0, 16))));
  }

  check_tokens_with_errors(
      u8"`unterminated\\", {token_type::complete_template},
      [](padded_string_view input, const auto& errors) {
        EXPECT_THAT(errors, ElementsAre(ERROR_TYPE_FIELD(
                                error_unclosed_template, incomplete_template,
                                offsets_matcher(input, 0, 14))));
      });

  // TODO(strager): Report invalid escape sequences, like with plain string
  // literals.
}

TEST(test_lex, lex_template_literal_with_ascii_control_characters) {
  for (string8_view control_character :
       concat(control_characters_except_line_terminators, line_terminators)) {
    padded_string input(u8"`hello" + string8(control_character) + u8"world`");
    SCOPED_TRACE(input);
    check_single_token(&input, token_type::complete_template);
  }

  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"`hello\\" + string8(control_character) + u8"world`");
    SCOPED_TRACE(input);
    check_single_token(&input, token_type::complete_template);
  }
}

TEST(test_lex, lex_regular_expression_literals) {
  auto check_regexp = [](const char8* raw_code) {
    padded_string code(raw_code);
    SCOPED_TRACE(code);
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  };

  check_regexp(u8"/ /");
  check_regexp(u8R"(/hello\/world/)");
  check_regexp(u8"/re/g");
  check_regexp(u8R"(/[/]/)");
  check_regexp(u8R"(/[\]/]/)");

  for (const char8* raw_code : {u8"/end_of_file", u8R"(/eof\)"}) {
    padded_string code(raw_code);
    SCOPED_TRACE(code);
    error_collector v;
    lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);

    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unclosed_regexp_literal, regexp_literal,
                    offsets_matcher(&code, 0,
                                    narrow_cast<source_position::offset_type>(
                                        code.size())))));

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  // TODO(strager): Report invalid escape sequences.

  // TODO(strager): Report invalid characters and mismatched brackets.
}

TEST(test_lex, lex_regular_expression_literals_preserves_leading_newline_flag) {
  {
    padded_string code(u8"\n/ /");
    lexer l(&code, &null_error_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_TRUE(l.peek().has_leading_newline);
  }

  {
    padded_string code(u8"/ /");
    lexer l(&code, &null_error_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_FALSE(l.peek().has_leading_newline);
  }
}

TEST(test_lex, lex_regular_expression_literal_with_ascii_control_characters) {
  for (string8_view control_character :
       control_characters_except_line_terminators) {
    padded_string input(u8"/hello" + string8(control_character) + u8"world/");
    SCOPED_TRACE(input);
    error_collector errors;
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
    error_collector errors;
    lexer l(&input, &errors);

    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(errors.errors, IsEmpty());
  }
}

TEST(test_lex, lex_identifiers) {
  check_single_token(u8"i", token_type::identifier);
  check_single_token(u8"_", token_type::identifier);
  check_single_token(u8"$", token_type::identifier);
  check_single_token(u8"id", u8"id");
  check_single_token(u8"id ", u8"id");
  check_single_token(u8"this_is_an_identifier", u8"this_is_an_identifier");
  check_single_token(u8"MixedCaseIsAllowed", u8"MixedCaseIsAllowed");
  check_single_token(u8"ident$with$dollars", u8"ident$with$dollars");
  check_single_token(u8"digits0123456789", u8"digits0123456789");
  // TODO(strager): Lex identifiers containing \u1234 or \u{1234}.
}

TEST(test_lex, lex_identifiers_which_look_like_keywords) {
  check_single_token(u8"ifelse", token_type::identifier);
  check_single_token(u8"IF", token_type::identifier);
}

TEST(test_lex, lex_keywords) {
  check_single_token(u8"as", token_type::kw_as);
  check_single_token(u8"async", token_type::kw_async);
  check_single_token(u8"await", token_type::kw_await);
  check_single_token(u8"break", token_type::kw_break);
  check_single_token(u8"case", token_type::kw_case);
  check_single_token(u8"catch", token_type::kw_catch);
  check_single_token(u8"class", token_type::kw_class);
  check_single_token(u8"const", token_type::kw_const);
  check_single_token(u8"continue", token_type::kw_continue);
  check_single_token(u8"debugger", token_type::kw_debugger);
  check_single_token(u8"default", token_type::kw_default);
  check_single_token(u8"delete", token_type::kw_delete);
  check_single_token(u8"do", token_type::kw_do);
  check_single_token(u8"else", token_type::kw_else);
  check_single_token(u8"export", token_type::kw_export);
  check_single_token(u8"extends", token_type::kw_extends);
  check_single_token(u8"false", token_type::kw_false);
  check_single_token(u8"finally", token_type::kw_finally);
  check_single_token(u8"for", token_type::kw_for);
  check_single_token(u8"from", token_type::kw_from);
  check_single_token(u8"function", token_type::kw_function);
  check_single_token(u8"if", token_type::kw_if);
  check_single_token(u8"import", token_type::kw_import);
  check_single_token(u8"in", token_type::kw_in);
  check_single_token(u8"instanceof", token_type::kw_instanceof);
  check_single_token(u8"let", token_type::kw_let);
  check_single_token(u8"new", token_type::kw_new);
  check_single_token(u8"null", token_type::kw_null);
  check_single_token(u8"of", token_type::kw_of);
  check_single_token(u8"return", token_type::kw_return);
  check_single_token(u8"static", token_type::kw_static);
  check_single_token(u8"super", token_type::kw_super);
  check_single_token(u8"switch", token_type::kw_switch);
  check_single_token(u8"this", token_type::kw_this);
  check_single_token(u8"throw", token_type::kw_throw);
  check_single_token(u8"true", token_type::kw_true);
  check_single_token(u8"try", token_type::kw_try);
  check_single_token(u8"typeof", token_type::kw_typeof);
  check_single_token(u8"var", token_type::kw_var);
  check_single_token(u8"void", token_type::kw_void);
  check_single_token(u8"while", token_type::kw_while);
  check_single_token(u8"with", token_type::kw_with);
  check_single_token(u8"yield", token_type::kw_yield);
}

TEST(test_lex, lex_contextual_keywords) {
  // TODO(strager): Move some assertions from lex_keywords into here.
  check_single_token(u8"get", token_type::kw_get);
}

TEST(test_lex, lex_single_character_symbols) {
  check_single_token(u8"+", token_type::plus);
  check_single_token(u8"-", token_type::minus);
  check_single_token(u8"*", token_type::star);
  check_single_token(u8"/", token_type::slash);
  check_single_token(u8"<", token_type::less);
  check_single_token(u8">", token_type::greater);
  check_single_token(u8"=", token_type::equal);
  check_single_token(u8"&", token_type::ampersand);
  check_single_token(u8"^", token_type::circumflex);
  check_single_token(u8"!", token_type::bang);
  check_single_token(u8".", token_type::dot);
  check_single_token(u8",", token_type::comma);
  check_single_token(u8"~", token_type::tilde);
  check_single_token(u8"%", token_type::percent);
  check_single_token(u8"(", token_type::left_paren);
  check_single_token(u8")", token_type::right_paren);
  check_single_token(u8"[", token_type::left_square);
  check_single_token(u8"]", token_type::right_square);
  check_single_token(u8"{", token_type::left_curly);
  check_single_token(u8"}", token_type::right_curly);
  check_single_token(u8":", token_type::colon);
  check_single_token(u8";", token_type::semicolon);
  check_single_token(u8"?", token_type::question);
  check_single_token(u8"|", token_type::pipe);
}

TEST(test_lex, lex_multi_character_symbols) {
  check_single_token(u8"<=", token_type::less_equal);
  check_single_token(u8">=", token_type::greater_equal);
  check_single_token(u8"==", token_type::equal_equal);
  check_single_token(u8"===", token_type::equal_equal_equal);
  check_single_token(u8"!=", token_type::bang_equal);
  check_single_token(u8"!==", token_type::bang_equal_equal);
  check_single_token(u8"**", token_type::star_star);
  check_single_token(u8"++", token_type::plus_plus);
  check_single_token(u8"--", token_type::minus_minus);
  check_single_token(u8"<<", token_type::less_less);
  check_single_token(u8">>", token_type::greater_greater);
  check_single_token(u8">>>", token_type::greater_greater_greater);
  check_single_token(u8"&&", token_type::ampersand_ampersand);
  check_single_token(u8"||", token_type::pipe_pipe);
  check_single_token(u8"+=", token_type::plus_equal);
  check_single_token(u8"-=", token_type::minus_equal);
  check_single_token(u8"*=", token_type::star_equal);
  check_single_token(u8"/=", token_type::slash_equal);
  check_single_token(u8"%=", token_type::percent_equal);
  check_single_token(u8"**=", token_type::star_star_equal);
  check_single_token(u8"&=", token_type::ampersand_equal);
  check_single_token(u8"^=", token_type::circumflex_equal);
  check_single_token(u8"|=", token_type::pipe_equal);
  check_single_token(u8"<<=", token_type::less_less_equal);
  check_single_token(u8">>=", token_type::greater_greater_equal);
  check_single_token(u8">>>=", token_type::greater_greater_greater_equal);
  check_single_token(u8"=>", token_type::equal_greater);
  check_single_token(u8"...", token_type::dot_dot_dot);
}

TEST(test_lex, lex_adjacent_symbols) {
  check_tokens(u8"{}", {token_type::left_curly, token_type::right_curly});
  check_tokens(u8"[]", {token_type::left_square, token_type::right_square});
  check_tokens(u8"/!", {token_type::slash, token_type::bang});
  check_tokens(u8"*==", {token_type::star_equal, token_type::equal});
  check_tokens(u8"||=", {token_type::pipe_pipe, token_type::equal});
  check_tokens(u8"^>>", {token_type::circumflex, token_type::greater_greater});
}

TEST(test_lex, lex_symbols_separated_by_whitespace) {
  check_tokens(u8"{ }", {token_type::left_curly, token_type::right_curly});
  check_tokens(u8"< =", {token_type::less, token_type::equal});
  check_tokens(u8". . .", {token_type::dot, token_type::dot, token_type::dot});
}

TEST(test_lex, lex_whitespace) {
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
    string8 input = string8(u8"a") + whitespace + u8"b";
    SCOPED_TRACE(out_string8(input));
    check_tokens(input.c_str(),
                 {token_type::identifier, token_type::identifier});
  }
}

TEST(test_lex, lex_shebang) {
  check_single_token(u8"#!/usr/bin/env node\nhello", token_type::identifier);
  check_single_token(u8"#!ignored\n123", token_type::number);
}

TEST(test_lex, lex_not_shebang) {
  // Whitespace must not appear between '#' and '!'.
  {
    error_collector v;
    padded_string input(u8"# !notashebang");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_hash_character, where,
                              offsets_matcher(&input, 0, 1))));
  }

  // '#!' must be on the first line.
  {
    error_collector v;
    padded_string input(u8"\n#!notashebang\n");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_hash_character, where,
                              offsets_matcher(&input, 1, 2))));
  }

  // Whitespace must not appear before '#!'.
  {
    error_collector v;
    padded_string input(u8"  #!notashebang\n");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::bang) << "# should be skipped";

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_hash_character, where,
                              offsets_matcher(&input, 2, 3))));
  }
}

TEST(test_lex, lex_invalid_common_characters_are_disallowed) {
  {
    error_collector v;
    padded_string input(u8"hello @ world");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier) << "@ should be skipped";
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_at_character, character,
                              offsets_matcher(&input, 6, 7))));
  }
}

TEST(test_lex, ascii_control_characters_are_disallowed) {
  for (string8_view control_character : control_characters_except_whitespace) {
    padded_string input(string8(control_character) + u8"hello");
    SCOPED_TRACE(input);
    error_collector v;

    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier)
        << "control character should be skipped";
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              error_unexpected_control_character, character,
                              offsets_matcher(&input, 0, 1))));
  }
}

TEST(test_lex, ascii_control_characters_sorta_treated_like_whitespace) {
  for (string8_view control_character : control_characters_except_whitespace) {
    padded_string input(u8"  " + string8(control_character) + u8"  hello");
    SCOPED_TRACE(input);
    error_collector v;
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::identifier)
        << "control character should be skipped";
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }
}

TEST(test_lex, lex_token_notes_leading_newline) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"a b" + string8(line_terminator) + u8"c d");
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // b
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // c
    l.skip();
    EXPECT_FALSE(l.peek().has_leading_newline);  // d
  }
}

TEST(test_lex, lex_token_notes_leading_newline_after_comment_with_newline) {
  for (string8_view line_terminator : line_terminators) {
    padded_string code(u8"a /*" + string8(line_terminator) + u8"*/ b");
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_FALSE(l.peek().has_leading_newline);  // a
    l.skip();
    EXPECT_TRUE(l.peek().has_leading_newline);  // b
  }
}

TEST(test_lex, lex_token_notes_leading_newline_after_comment) {
  padded_string code(u8"a /* comment */\nb");
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // b
}

TEST(test_lex, inserting_semicolon_at_newline_remembers_next_token) {
  padded_string code(u8"hello\nworld");
  lexer l(&code, &null_error_reporter::instance);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), u8"hello");
  EXPECT_FALSE(l.peek().has_leading_newline);
  const char8* hello_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), u8"world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, hello_end);
  EXPECT_EQ(l.peek().end, hello_end);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), u8"world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

TEST(test_lex, inserting_semicolon_at_right_curly_remembers_next_token) {
  padded_string code(u8"{ x }");
  error_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::left_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), u8"x");
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

void check_single_token(const char8* input, token_type expected_token_type) {
  padded_string code(input);
  check_single_token(&code, expected_token_type);
}

void check_single_token(padded_string_view input,
                        token_type expected_token_type) {
  error_collector errors;
  lexer l(input, &errors);

  EXPECT_EQ(l.peek().type, expected_token_type);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  EXPECT_THAT(errors.errors, IsEmpty());
}

void check_single_token(const char8* input,
                        string8_view expected_identifier_name) {
  padded_string code(input);
  error_collector errors;
  lexer l(&code, &errors);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), expected_identifier_name);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  EXPECT_THAT(errors.errors, IsEmpty());
}

void check_single_token(const string8& input,
                        string8_view expected_identifier_name) {
  return check_single_token(input.c_str(), expected_identifier_name);
}

void check_tokens(const char8* input,
                  std::initializer_list<token_type> expected_token_types) {
  check_tokens_with_errors(input, expected_token_types,
                           [](padded_string_view, const auto& errors) {
                             EXPECT_THAT(errors, IsEmpty());
                           });
}

void check_tokens_with_errors(
    const char8* input, std::initializer_list<token_type> expected_token_types,
    void (*check_errors)(padded_string_view input,
                         const std::vector<error_collector::error>&)) {
  padded_string code(input);
  error_collector errors;
  lexer l(&code, &errors);

  for (token_type expected_token_type : expected_token_types) {
    EXPECT_EQ(l.peek().type, expected_token_type);
    l.skip();
  }
  EXPECT_EQ(l.peek().type, token_type::end_of_file);

  check_errors(&code, errors.errors);
}
}
}
