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

#include <cstring>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <string_view>

namespace quick_lint_js {
namespace {
void check_single_token(const char* input, token_type expected_token_type);
void check_single_token(const char* input,
                        std::string_view expected_identifier_name);
void check_tokens(const char* input,
                  std::initializer_list<token_type> expected_token_types);

TEST(test_lex, lex_block_comments) {
  check_single_token("/* */ hello", "hello");
  check_single_token("/*/ comment */ hi", "hi");
  check_single_token("/* comment /*/ hi", "hi");
  check_single_token("/* not /* nested */ ident", "ident");
  check_single_token("/**/", token_type::end_of_file);

  {
    error_collector v;
    padded_string input("hello /* unterminated comment ");
    lexer l(&input, &v);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
    EXPECT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_block_comment);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 6);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 8);
  }
}

TEST(test_lex, lex_line_comments) {
  check_single_token("// hello", token_type::end_of_file);
  check_single_token("// hello\nworld", "world");
  check_single_token("// hello\n// world", token_type::end_of_file);
  check_tokens("hello//*/\n \n \nworld",
               {token_type::identifier, token_type::identifier});
}

TEST(test_lex, lex_numbers) {
  check_single_token("0", token_type::number);
  check_single_token("2", token_type::number);
  check_single_token("42", token_type::number);
  check_single_token("12.34", token_type::number);
  check_single_token(".34", token_type::number);
  check_tokens("123. 456", {token_type::number, token_type::number});
}

TEST(test_lex, lex_hex_numbers) {
  check_single_token("0x0", token_type::number);
  check_single_token("0x123456789abcdef", token_type::number);
  check_single_token("0X123456789ABCDEF", token_type::number);
}

TEST(test_lex, lex_strings) {
  check_single_token(R"('hello')", token_type::string);
  check_single_token(R"("hello")", token_type::string);
  check_single_token(R"("hello\"world")", token_type::string);
  check_single_token(R"('hello\'world')", token_type::string);
  check_single_token(R"('hello"world')", token_type::string);
  check_single_token(R"("hello'world")", token_type::string);

  {
    error_collector v;
    padded_string input(R"("unterminated)");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_string_literal);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 13);
  }

  {
    error_collector v;
    padded_string input("'unterminated\nhello");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    EXPECT_EQ(l.peek().identifier_name().string_view(), "hello");

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_string_literal);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 13);
  }

  {
    error_collector v;
    padded_string input("'unterminated\\");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::string);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_string_literal);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 14);
  }

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

TEST(test_lex, lex_templates) {
  check_tokens("``", {token_type::complete_template});
  check_tokens("`hello`", {token_type::complete_template});
  check_tokens("`hello$world`", {token_type::complete_template});
  check_tokens("`hello{world`", {token_type::complete_template});
  check_tokens(R"(`hello\`world`)", {token_type::complete_template});
  check_tokens(R"(`hello$\{world`)", {token_type::complete_template});
  check_tokens(R"(`hello\${world`)", {token_type::complete_template});
  check_tokens(R"(`hello
world`)",
               {token_type::complete_template});

  {
    padded_string code("`hello${42}`");
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), "`hello${");
    const char* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), "`");
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string code("`${42}world`");
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    EXPECT_EQ(l.peek().span().string_view(), "`${");
    const char* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::number);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    EXPECT_EQ(l.peek().span().string_view(), "world`");
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  {
    padded_string code("`${left}${right}`");
    lexer l(&code, &null_error_reporter::instance);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    const char* template_begin = l.peek().begin;
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

  {
    error_collector v;
    padded_string input("`unterminated");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_template);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 13);
  }

  {
    error_collector v;
    padded_string input("`${un}terminated");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::incomplete_template);
    const char* template_begin = l.peek().begin;
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::identifier);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::right_curly);
    l.skip_in_template(template_begin);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_template);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 16);
  }

  {
    error_collector v;
    padded_string input("`unterminated\\");
    lexer l(&input, &v);
    EXPECT_EQ(l.peek().type, token_type::complete_template);
    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);

    ASSERT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_template);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&input).range(v.errors[0].where).end_offset(), 14);
  }

  // TODO(strager): Report invalid escape sequences, like with plain string
  // literals.
}

TEST(test_lex, lex_regular_expression_literals) {
  for (const char* raw_code : {"/ /", R"(/hello\/world/)", "/re/g"}) {
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
  }

  for (const char* raw_code : {"/end_of_file", R"(/eof\)"}) {
    padded_string code(raw_code);
    SCOPED_TRACE(code);
    error_collector v;
    lexer l(&code, &v);
    EXPECT_EQ(l.peek().type, token_type::slash);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_EQ(l.peek().begin, &code[0]);
    EXPECT_EQ(l.peek().end, &code[code.size()]);

    EXPECT_EQ(v.errors.size(), 1);
    EXPECT_EQ(v.errors[0].kind, error_collector::error_unclosed_regexp_literal);
    EXPECT_EQ(locator(&code).range(v.errors[0].where).begin_offset(), 0);
    EXPECT_EQ(locator(&code).range(v.errors[0].where).end_offset(),
              code.size());

    l.skip();
    EXPECT_EQ(l.peek().type, token_type::end_of_file);
  }

  // TODO(strager): Parse trailing flags.

  // TODO(strager): Parse '/' inside character classes.

  // TODO(strager): Report invalid escape sequences.

  // TODO(strager): Report invalid characters and mismatched brackets.
}

TEST(test_lex, lex_regular_expression_literals_preserves_leading_newline_flag) {
  {
    padded_string code("\n/ /");
    lexer l(&code, &null_error_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_TRUE(l.peek().has_leading_newline);
  }

  {
    padded_string code("/ /");
    lexer l(&code, &null_error_reporter::instance);
    l.reparse_as_regexp();
    EXPECT_EQ(l.peek().type, token_type::regexp);
    EXPECT_FALSE(l.peek().has_leading_newline);
  }
}

TEST(test_lex, lex_identifiers) {
  check_single_token("i", token_type::identifier);
  check_single_token("_", token_type::identifier);
  check_single_token("$", token_type::identifier);
  check_single_token("id", "id");
  check_single_token("id ", "id");
  check_single_token("this_is_an_identifier", "this_is_an_identifier");
  check_single_token("MixedCaseIsAllowed", "MixedCaseIsAllowed");
  check_single_token("ident$with$dollars", "ident$with$dollars");
  check_single_token("digits0123456789", "digits0123456789");
  // TODO(strager): Lex identifiers containing \u1234 or \u{1234}.
}

TEST(test_lex, lex_identifiers_which_look_like_keywords) {
  check_single_token("ifelse", token_type::identifier);
  check_single_token("IF", token_type::identifier);
}

TEST(test_lex, lex_keywords) {
  check_single_token("as", token_type::kw_as);
  check_single_token("async", token_type::kw_async);
  check_single_token("await", token_type::kw_await);
  check_single_token("break", token_type::kw_break);
  check_single_token("case", token_type::kw_case);
  check_single_token("catch", token_type::kw_catch);
  check_single_token("class", token_type::kw_class);
  check_single_token("const", token_type::kw_const);
  check_single_token("continue", token_type::kw_continue);
  check_single_token("debugger", token_type::kw_debugger);
  check_single_token("default", token_type::kw_default);
  check_single_token("delete", token_type::kw_delete);
  check_single_token("do", token_type::kw_do);
  check_single_token("else", token_type::kw_else);
  check_single_token("export", token_type::kw_export);
  check_single_token("extends", token_type::kw_extends);
  check_single_token("false", token_type::kw_false);
  check_single_token("finally", token_type::kw_finally);
  check_single_token("for", token_type::kw_for);
  check_single_token("from", token_type::kw_from);
  check_single_token("function", token_type::kw_function);
  check_single_token("if", token_type::kw_if);
  check_single_token("import", token_type::kw_import);
  check_single_token("in", token_type::kw_in);
  check_single_token("instanceof", token_type::kw_instanceof);
  check_single_token("let", token_type::kw_let);
  check_single_token("new", token_type::kw_new);
  check_single_token("null", token_type::kw_null);
  check_single_token("of", token_type::kw_of);
  check_single_token("return", token_type::kw_return);
  check_single_token("static", token_type::kw_static);
  check_single_token("super", token_type::kw_super);
  check_single_token("switch", token_type::kw_switch);
  check_single_token("this", token_type::kw_this);
  check_single_token("throw", token_type::kw_throw);
  check_single_token("true", token_type::kw_true);
  check_single_token("try", token_type::kw_try);
  check_single_token("typeof", token_type::kw_typeof);
  check_single_token("var", token_type::kw_var);
  check_single_token("void", token_type::kw_void);
  check_single_token("while", token_type::kw_while);
  check_single_token("with", token_type::kw_with);
  check_single_token("yield", token_type::kw_yield);
}

TEST(test_lex, lex_contextual_keywords) {
  // TODO(strager): Move some assertions from lex_keywords into here.
  check_single_token("get", token_type::kw_get);
}

TEST(test_lex, lex_single_character_symbols) {
  check_single_token("+", token_type::plus);
  check_single_token("-", token_type::minus);
  check_single_token("*", token_type::star);
  check_single_token("/", token_type::slash);
  check_single_token("<", token_type::less);
  check_single_token(">", token_type::greater);
  check_single_token("=", token_type::equal);
  check_single_token("&", token_type::ampersand);
  check_single_token("^", token_type::circumflex);
  check_single_token("!", token_type::bang);
  check_single_token(".", token_type::dot);
  check_single_token(",", token_type::comma);
  check_single_token("~", token_type::tilde);
  check_single_token("%", token_type::percent);
  check_single_token("(", token_type::left_paren);
  check_single_token(")", token_type::right_paren);
  check_single_token("[", token_type::left_square);
  check_single_token("]", token_type::right_square);
  check_single_token("{", token_type::left_curly);
  check_single_token("}", token_type::right_curly);
  check_single_token(":", token_type::colon);
  check_single_token(";", token_type::semicolon);
  check_single_token("?", token_type::question);
  check_single_token("|", token_type::pipe);
}

TEST(test_lex, lex_multi_character_symbols) {
  check_single_token("<=", token_type::less_equal);
  check_single_token(">=", token_type::greater_equal);
  check_single_token("==", token_type::equal_equal);
  check_single_token("===", token_type::equal_equal_equal);
  check_single_token("!=", token_type::bang_equal);
  check_single_token("!==", token_type::bang_equal_equal);
  check_single_token("**", token_type::star_star);
  check_single_token("++", token_type::plus_plus);
  check_single_token("--", token_type::minus_minus);
  check_single_token("<<", token_type::less_less);
  check_single_token(">>", token_type::greater_greater);
  check_single_token(">>>", token_type::greater_greater_greater);
  check_single_token("&&", token_type::ampersand_ampersand);
  check_single_token("||", token_type::pipe_pipe);
  check_single_token("+=", token_type::plus_equal);
  check_single_token("-=", token_type::minus_equal);
  check_single_token("*=", token_type::star_equal);
  check_single_token("/=", token_type::slash_equal);
  check_single_token("%=", token_type::percent_equal);
  check_single_token("**=", token_type::star_star_equal);
  check_single_token("&=", token_type::ampersand_equal);
  check_single_token("^=", token_type::circumflex_equal);
  check_single_token("|=", token_type::pipe_equal);
  check_single_token("<<=", token_type::less_less_equal);
  check_single_token(">>=", token_type::greater_greater_equal);
  check_single_token(">>>=", token_type::greater_greater_greater_equal);
  check_single_token("=>", token_type::equal_greater);
  check_single_token("...", token_type::dot_dot_dot);
}

TEST(test_lex, lex_adjacent_symbols) {
  check_tokens("{}", {token_type::left_curly, token_type::right_curly});
  check_tokens("[]", {token_type::left_square, token_type::right_square});
  check_tokens("/!", {token_type::slash, token_type::bang});
  check_tokens("*==", {token_type::star_equal, token_type::equal});
  check_tokens("||=", {token_type::pipe_pipe, token_type::equal});
  check_tokens("^>>", {token_type::circumflex, token_type::greater_greater});
}

TEST(test_lex, lex_symbols_separated_by_whitespace) {
  check_tokens("{ }", {token_type::left_curly, token_type::right_curly});
  check_tokens("< =", {token_type::less, token_type::equal});
  check_tokens(". . .", {token_type::dot, token_type::dot, token_type::dot});
}

TEST(test_lex, lex_whitespace) {
  for (const char* whitespace : {
           " ",
           "\n",
           "\t",
           "\f",
           "\xc2\xa0",      // U+00A0 No-Break Space (NBSP)
           "\xe1\x9a\x80",  // U+1680 Ogham Space Mark
           "\xe2\x80\x80",  // U+2000 En Quad
           "\xe2\x80\x81",  // U+2001 Em Quad
           "\xe2\x80\x82",  // U+2002 En Space
           "\xe2\x80\x83",  // U+2003 Em Space
           "\xe2\x80\x84",  // U+2004 Three-Per-Em Space
           "\xe2\x80\x85",  // U+2005 Four-Per-Em Space
           "\xe2\x80\x86",  // U+2006 Six-Per-Em Space
           "\xe2\x80\x87",  // U+2007 Figure Space
           "\xe2\x80\x88",  // U+2008 Punctuation Space
           "\xe2\x80\x89",  // U+2009 Thin Space
           "\xe2\x80\x8a",  // U+200A Hair Space
           "\xe2\x80\xaf",  // U+202F Narrow No-Break Space (NNBSP)
           "\xe2\x81\x9f",  // U+205F Medium Mathematical Space (MMSP)
           "\xe3\x80\x80",  // U+3000 Ideographic Space
           "\xef\xbb\xbf",  // U+FEFF Zero Width No-Break Space (BOM, ZWNBSP)
       }) {
    std::string input = std::string("a") + whitespace + "b";
    SCOPED_TRACE(input);
    check_tokens(input.c_str(),
                 {token_type::identifier, token_type::identifier});
  }
}

TEST(test_lex, lex_token_notes_leading_newline) {
  padded_string code("a b\nc d");
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_FALSE(l.peek().has_leading_newline);  // b
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // c
  l.skip();
  EXPECT_FALSE(l.peek().has_leading_newline);  // d
}

TEST(test_lex, lex_token_notes_leading_newline_after_comment_with_newline) {
  padded_string code("a /*\n*/ b");
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // b
}

TEST(test_lex, lex_token_notes_leading_newline_after_comment) {
  padded_string code("a /* comment */\nb");
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_FALSE(l.peek().has_leading_newline);  // a
  l.skip();
  EXPECT_TRUE(l.peek().has_leading_newline);  // b
}

TEST(test_lex, inserting_semicolon_at_newline_remembers_next_token) {
  padded_string code("hello\nworld");
  lexer l(&code, &null_error_reporter::instance);

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), "hello");
  EXPECT_FALSE(l.peek().has_leading_newline);
  const char* hello_end = l.peek().end;
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), "world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.insert_semicolon();
  EXPECT_EQ(l.peek().type, token_type::semicolon);
  EXPECT_FALSE(l.peek().has_leading_newline);
  EXPECT_EQ(l.peek().begin, hello_end);
  EXPECT_EQ(l.peek().end, hello_end);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), "world");
  EXPECT_TRUE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

TEST(test_lex, inserting_semicolon_at_right_curly_remembers_next_token) {
  padded_string code("{ x }");
  lexer l(&code, &null_error_reporter::instance);

  EXPECT_EQ(l.peek().type, token_type::left_curly);
  EXPECT_FALSE(l.peek().has_leading_newline);
  l.skip();

  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), "x");
  EXPECT_FALSE(l.peek().has_leading_newline);
  const char* x_end = l.peek().end;
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
}

void check_single_token(const char* input, token_type expected_token_type) {
  padded_string code(input);
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_EQ(l.peek().type, expected_token_type);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

void check_single_token(const char* input,
                        std::string_view expected_identifier_name) {
  padded_string code(input);
  lexer l(&code, &null_error_reporter::instance);
  EXPECT_EQ(l.peek().type, token_type::identifier);
  EXPECT_EQ(l.peek().identifier_name().string_view(), expected_identifier_name);
  l.skip();
  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}

void check_tokens(const char* input,
                  std::initializer_list<token_type> expected_token_types) {
  padded_string code(input);
  lexer l(&code, &null_error_reporter::instance);
  for (token_type expected_token_type : expected_token_types) {
    EXPECT_EQ(l.peek().type, expected_token_type);
    l.skip();
  }
  EXPECT_EQ(l.peek().type, token_type::end_of_file);
}
}  // namespace
}  // namespace quick_lint_js
