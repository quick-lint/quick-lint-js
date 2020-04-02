#ifndef QUICKLINT_JS_LEX_H
#define QUICKLINT_JS_LEX_H

#include <cassert>

namespace quicklint_js {
enum class token_type {
  // Single-character symbols:
  ampersand = '&',
  bang = '!',
  circumflex = '^',
  colon = ':',
  comma = ',',
  slash = '/',
  dot = '.',
  equal = '=',
  greater = '>',
  left_curly = '{',
  left_paren = '(',
  left_square = '[',
  less = '<',
  minus = '-',
  percent = '%',
  pipe = '|',
  plus = '+',
  question = '?',
  right_curly = '}',
  right_paren = ')',
  right_square = ']',
  semicolon = ';',
  star = '*',
  tilde = '~',

  end_of_file,
  identifier,
  number,

  // Symbols:
  ampersand_ampersand,
  ampersand_equal,
  bang_equal,
  bang_equal_equal,
  circumflex_equal,
  dot_dot_dot,
  equal_equal,
  equal_equal_equal,
  equal_greater,
  greater_equal,
  greater_greater,
  greater_greater_equal,
  greater_greater_greater,
  greater_greater_greater_equal,
  less_equal,
  less_less,
  less_less_equal,
  minus_equal,
  minus_minus,
  percent_equal,
  pipe_equal,
  pipe_pipe,
  plus_equal,
  plus_plus,
  slash_equal,
  star_equal,
  star_star,
  star_star_equal,
};

struct token {
  token_type type;
};

class lexer {
 public:
  lexer(const char* input) : input_(input) { this->parse_current_token(); }

  void parse_current_token();

  const token& peek() { return this->last_token_; }

  void skip() { this->parse_current_token(); }

 private:
  void skip_whitespace();

  static bool is_identifier_character(char);

  token last_token_;
  const char* input_;
};
}  // namespace quicklint_js

#endif
