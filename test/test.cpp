#include <doctest/doctest.h>
#include <quicklint-js/lex.h>

namespace quicklint_js {
namespace {
void check_single_token(const char* input, token_type expected_token_type);

TEST_CASE("lex numbers") { check_single_token("2", token_type::number); }

TEST_CASE("lex identifiers") {
  check_single_token("i", token_type::identifier);
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
  lexer l("{}");
  CHECK(l.peek().type == token_type::left_curly);
  l.skip();
  CHECK(l.peek().type == token_type::right_curly);
  l.skip();
  CHECK(l.peek().type == token_type::end_of_file);
}

void check_single_token(const char* input, token_type expected_token_type) {
  lexer l(input);
  CHECK(l.peek().type == expected_token_type);
  l.skip();
  CHECK(l.peek().type == token_type::end_of_file);
}
}  // namespace
}  // namespace quicklint_js
