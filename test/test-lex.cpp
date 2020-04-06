#include <doctest/doctest.h>
#include <initializer_list>
#include <iostream>
#include <quicklint-js/error-collector.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <string_view>

namespace quicklint_js {
namespace {
void check_single_token(const char* input, token_type expected_token_type);
void check_single_token(const char* input,
                        std::string_view expected_identifier_name);
void check_tokens(const char* input,
                  std::initializer_list<token_type> expected_token_types);

TEST_CASE("lex block comments") {
  check_single_token("/* */ hello", "hello");
  check_single_token("/*/ comment */ hi", "hi");
  check_single_token("/* comment /*/ hi", "hi");
  check_single_token("/* not /* nested */ ident", "ident");
  check_single_token("/**/", token_type::end_of_file);

  {
    error_collector v;
    const char* input = "hello /* unterminated comment ";
    lexer l(input, &v);
    l.skip();
    CHECK(l.peek().type == token_type::end_of_file);
    CHECK(v.errors.size() == 1);
    CHECK(v.errors[0].kind == error_collector::error_unclosed_block_comment);
    CHECK(locator(input).range(v.errors[0].where).begin_offset() == 6);
    CHECK(locator(input).range(v.errors[0].where).end_offset() == 8);
  }
}

TEST_CASE("lex numbers") {
  check_single_token("0", token_type::number);
  check_single_token("2", token_type::number);
  check_single_token("42", token_type::number);
}

TEST_CASE("lex strings") {
  check_single_token(R"('hello')", token_type::string);
  check_single_token(R"("hello")", token_type::string);
  check_single_token(R"("hello\"world")", token_type::string);
  check_single_token(R"('hello\'world')", token_type::string);

  {
    error_collector v;
    const char* input = R"("unterminated)";
    lexer l(input, &v);
    CHECK(l.peek().type == token_type::string);
    l.skip();
    CHECK(l.peek().type == token_type::end_of_file);

    REQUIRE(v.errors.size() == 1);
    CHECK(v.errors[0].kind == error_collector::error_unclosed_string_literal);
    CHECK(locator(input).range(v.errors[0].where).begin_offset() == 0);
    CHECK(locator(input).range(v.errors[0].where).end_offset() == 13);
  }

  {
    error_collector v;
    const char* input = "'unterminated\nhello";
    lexer l(input, &v);
    CHECK(l.peek().type == token_type::string);
    l.skip();
    CHECK(l.peek().type == token_type::identifier);
    CHECK(l.peek().identifier_name().string_view() == "hello");

    REQUIRE(v.errors.size() == 1);
    CHECK(v.errors[0].kind == error_collector::error_unclosed_string_literal);
    CHECK(locator(input).range(v.errors[0].where).begin_offset() == 0);
    CHECK(locator(input).range(v.errors[0].where).end_offset() == 13);
  }

  {
    error_collector v;
    const char* input = "'unterminated\\";
    lexer l(input, &v);
    CHECK(l.peek().type == token_type::string);
    l.skip();
    CHECK(l.peek().type == token_type::end_of_file);

    REQUIRE(v.errors.size() == 1);
    CHECK(v.errors[0].kind == error_collector::error_unclosed_string_literal);
    CHECK(locator(input).range(v.errors[0].where).begin_offset() == 0);
    CHECK(locator(input).range(v.errors[0].where).end_offset() == 14);
  }

  // TODO(strager): Lex line continuations in string literals. For example:
  //
  // "hello\   (backslash followed by end of line)
  // world"

  // TODO(strager): Report invalid hex escape squences. For example:
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

TEST_CASE("lex identifiers") {
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

TEST_CASE("lex identifiers which look like keywords") {
  check_single_token("ifelse", token_type::identifier);
  check_single_token("IF", token_type::identifier);
}

TEST_CASE("lex keywords") {
  check_single_token("as", token_type::_as);
  check_single_token("await", token_type::_await);
  check_single_token("break", token_type::_break);
  check_single_token("case", token_type::_case);
  check_single_token("catch", token_type::_catch);
  check_single_token("class", token_type::_class);
  check_single_token("const", token_type::_const);
  check_single_token("continue", token_type::_continue);
  check_single_token("debugger", token_type::_debugger);
  check_single_token("default", token_type::_default);
  check_single_token("delete", token_type::_delete);
  check_single_token("do", token_type::_do);
  check_single_token("else", token_type::_else);
  check_single_token("export", token_type::_export);
  check_single_token("extends", token_type::_extends);
  check_single_token("false", token_type::_false);
  check_single_token("finally", token_type::_finally);
  check_single_token("for", token_type::_for);
  check_single_token("from", token_type::_from);
  check_single_token("function", token_type::_function);
  check_single_token("if", token_type::_if);
  check_single_token("import", token_type::_import);
  check_single_token("in", token_type::_in);
  check_single_token("instanceof", token_type::_instanceof);
  check_single_token("let", token_type::_let);
  check_single_token("new", token_type::_new);
  check_single_token("null", token_type::_null);
  check_single_token("return", token_type::_return);
  check_single_token("static", token_type::_static);
  check_single_token("super", token_type::_super);
  check_single_token("switch", token_type::_switch);
  check_single_token("this", token_type::_this);
  check_single_token("throw", token_type::_throw);
  check_single_token("true", token_type::_true);
  check_single_token("try", token_type::_try);
  check_single_token("typeof", token_type::_typeof);
  check_single_token("var", token_type::_var);
  check_single_token("void", token_type::_void);
  check_single_token("while", token_type::_while);
  check_single_token("with", token_type::_with);
  check_single_token("yield", token_type::_yield);
}

TEST_CASE("lex single-character symbols") {
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

TEST_CASE("lex multi-character symbols") {
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

TEST_CASE("lex adjacent symbols") {
  check_tokens("{}", {token_type::left_curly, token_type::right_curly});
  check_tokens("[]", {token_type::left_square, token_type::right_square});
  check_tokens("/!", {token_type::slash, token_type::bang});
  check_tokens("*==", {token_type::star_equal, token_type::equal});
  check_tokens("||=", {token_type::pipe_pipe, token_type::equal});
  check_tokens("^>>", {token_type::circumflex, token_type::greater_greater});
}

TEST_CASE("lex symbols separated by whitespace") {
  check_tokens("{ }", {token_type::left_curly, token_type::right_curly});
  check_tokens("< =", {token_type::less, token_type::equal});
  check_tokens(". . .", {token_type::dot, token_type::dot, token_type::dot});
}

void check_single_token(const char* input, token_type expected_token_type) {
  lexer l(input, &null_error_reporter::instance);
  CHECK(l.peek().type == expected_token_type);
  l.skip();
  CHECK(l.peek().type == token_type::end_of_file);
}

void check_single_token(const char* input,
                        std::string_view expected_identifier_name) {
  lexer l(input, &null_error_reporter::instance);
  CHECK(l.peek().type == token_type::identifier);
  CHECK(l.peek().identifier_name().string_view() == expected_identifier_name);
  l.skip();
  CHECK(l.peek().type == token_type::end_of_file);
}

void check_tokens(const char* input,
                  std::initializer_list<token_type> expected_token_types) {
  lexer l(input, &null_error_reporter::instance);
  for (token_type expected_token_type : expected_token_types) {
    CHECK(l.peek().type == expected_token_type);
    l.skip();
  }
  CHECK(l.peek().type == token_type::end_of_file);
}
}  // namespace
}  // namespace quicklint_js
