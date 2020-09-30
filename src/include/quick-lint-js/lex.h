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

#ifndef QUICK_LINT_JS_LEX_H
#define QUICK_LINT_JS_LEX_H

#include <cassert>
#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>

#define QLJS_CASE_KEYWORD                          \
  case ::quick_lint_js::token_type::kw_as:         \
  case ::quick_lint_js::token_type::kw_async:      \
  case ::quick_lint_js::token_type::kw_await:      \
  case ::quick_lint_js::token_type::kw_break:      \
  case ::quick_lint_js::token_type::kw_case:       \
  case ::quick_lint_js::token_type::kw_catch:      \
  case ::quick_lint_js::token_type::kw_class:      \
  case ::quick_lint_js::token_type::kw_const:      \
  case ::quick_lint_js::token_type::kw_continue:   \
  case ::quick_lint_js::token_type::kw_debugger:   \
  case ::quick_lint_js::token_type::kw_default:    \
  case ::quick_lint_js::token_type::kw_delete:     \
  case ::quick_lint_js::token_type::kw_do:         \
  case ::quick_lint_js::token_type::kw_else:       \
  case ::quick_lint_js::token_type::kw_export:     \
  case ::quick_lint_js::token_type::kw_extends:    \
  case ::quick_lint_js::token_type::kw_false:      \
  case ::quick_lint_js::token_type::kw_finally:    \
  case ::quick_lint_js::token_type::kw_for:        \
  case ::quick_lint_js::token_type::kw_from:       \
  case ::quick_lint_js::token_type::kw_function:   \
  case ::quick_lint_js::token_type::kw_get:        \
  case ::quick_lint_js::token_type::kw_if:         \
  case ::quick_lint_js::token_type::kw_import:     \
  case ::quick_lint_js::token_type::kw_in:         \
  case ::quick_lint_js::token_type::kw_instanceof: \
  case ::quick_lint_js::token_type::kw_let:        \
  case ::quick_lint_js::token_type::kw_new:        \
  case ::quick_lint_js::token_type::kw_null:       \
  case ::quick_lint_js::token_type::kw_of:         \
  case ::quick_lint_js::token_type::kw_return:     \
  case ::quick_lint_js::token_type::kw_static:     \
  case ::quick_lint_js::token_type::kw_super:      \
  case ::quick_lint_js::token_type::kw_switch:     \
  case ::quick_lint_js::token_type::kw_this:       \
  case ::quick_lint_js::token_type::kw_throw:      \
  case ::quick_lint_js::token_type::kw_true:       \
  case ::quick_lint_js::token_type::kw_try:        \
  case ::quick_lint_js::token_type::kw_typeof:     \
  case ::quick_lint_js::token_type::kw_var:        \
  case ::quick_lint_js::token_type::kw_void:       \
  case ::quick_lint_js::token_type::kw_while:      \
  case ::quick_lint_js::token_type::kw_with:       \
  case ::quick_lint_js::token_type::kw_yield

namespace quick_lint_js {
class error_reporter;

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

  complete_template,
  end_of_file,
  identifier,
  incomplete_template,
  number,
  regexp,
  string,

  // Keywords ('kw' stands for 'KeyWord'):
  kw_as,
  kw_async,
  kw_await,
  kw_break,
  kw_case,
  kw_catch,
  kw_class,
  kw_const,
  kw_continue,
  kw_debugger,
  kw_default,
  kw_delete,
  kw_do,
  kw_else,
  kw_export,
  kw_extends,
  kw_false,
  kw_finally,
  kw_for,
  kw_from,
  kw_function,
  kw_get,
  kw_if,
  kw_import,
  kw_in,
  kw_instanceof,
  kw_let,
  kw_new,
  kw_null,
  kw_of,
  kw_return,
  kw_static,
  kw_super,
  kw_switch,
  kw_this,
  kw_throw,
  kw_true,
  kw_try,
  kw_typeof,
  kw_var,
  kw_void,
  kw_while,
  kw_with,
  kw_yield,

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

std::ostream& operator<<(std::ostream&, token_type);

class identifier {
 public:
  explicit identifier(source_code_span span) noexcept : span_(span) {}

  source_code_span span() const noexcept { return this->span_; }

  string8_view string_view() const noexcept {
    return this->span_.string_view();
  }

 private:
  source_code_span span_;
};

struct token {
  identifier identifier_name() const noexcept;
  source_code_span span() const noexcept;

  token_type type;

  const char8* begin;
  const char8* end;

  bool has_leading_newline;
};

// A lexer reads JavaScript source code one token at a time.
//
// A token is (roughly) either a keyword (if, function, let, etc.), an operator
// (+, !==, *=, etc.), an identifier (variable name), or a literal (number,
// string, boolean, etc.).
//
// Whitespace and comments are not interpreted as tokens.
class lexer {
 public:
  explicit lexer(padded_string_view input, error_reporter*) noexcept;

  void parse_current_token();

  // Return information about the current token.
  const token& peek() const noexcept { return this->last_token_; }

  // Advance to the next token. Use this->peek() after to observe the next
  // token.
  //
  // This function ignores leading and trailing whitespace and comments.
  //
  // Precondition: this->peek().type != token_type::end_of_file.
  void skip() { this->parse_current_token(); }

  // Like this->skip(), but interpret '}' as ending an expression inside a
  // template literal.
  //
  // For example:
  //
  //   `senior ${language} engineer`
  //
  // After seeing the 'language' token, the caller should use
  // this->skip_in_template() so '} engineer`' is interpreted as part of the
  // template literal (instead of a '}' token, an 'engineer' token, and the
  // beginning of another template literal).
  //
  // The given template_begin is used for error reporting.
  void skip_in_template(const char8* template_begin);

  // Reparse a '/' token as a regular expression literal.
  //
  // Precondition: this->peek().type == token_type::slash.
  // Postcondition: this->peek().type == token_type::regexp.
  void reparse_as_regexp();

  void insert_semicolon();

  // Do not call this after calling insert_semicolon, unless skip has been
  // called after.
  const char8* end_of_previous_token() const noexcept;

 private:
  struct parsed_template_body {
    token_type type;
    char8* end;
  };

  parsed_template_body parse_template_body(char8* input,
                                           const char8* template_begin,
                                           error_reporter*);

  void parse_binary_number();
  void parse_hexadecimal_number();
  char8* check_garbage_in_number_literal(char8* input);
  void parse_number();
  char8* parse_decimal_digits_and_underscores(char8* input) noexcept;

  static char8* parse_identifier(char8*);

  void skip_whitespace();
  void skip_block_comment();
  void skip_line_comment();

  bool is_eof(const char8*) noexcept;

  static bool is_binary_digit(char8);
  static bool is_digit(char8);
  static bool is_hex_digit(char8);
  static bool is_identifier_character(char8);

  static int newline_character_size(const char8*);

  static token_type identifier_token_type(string8_view) noexcept;

  token last_token_;
  char8* last_last_token_end_;
  char8* input_;
  error_reporter* error_reporter_;
  padded_string_view original_input_;
};
}

#endif
