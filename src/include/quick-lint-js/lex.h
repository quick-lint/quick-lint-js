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
#include <quick-lint-js/location.h>
#include <string_view>

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
  string,

  // Keywords:
  first_keyword,
  _as = first_keyword,
  _async,
  _await,
  _break,
  _case,
  _catch,
  _class,
  _const,
  _continue,
  _debugger,
  _default,
  _delete,
  _do,
  _else,
  _export,
  _extends,
  _false,
  _finally,
  _for,
  _from,
  _function,
  _if,
  _import,
  _in,
  _instanceof,
  _let,
  _new,
  _null,
  _return,
  _static,
  _super,
  _switch,
  _this,
  _throw,
  _true,
  _try,
  _typeof,
  _var,
  _void,
  _while,
  _with,
  _yield,

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

  std::string_view string_view() const noexcept {
    return this->span_.string_view();
  }

 private:
  source_code_span span_;
};

struct token {
  identifier identifier_name() const noexcept;
  source_code_span span() const noexcept;

  token_type type;

  const char* begin;
  const char* end;

  bool has_leading_newline;
};

class lexer {
 public:
  explicit lexer(const char* input, error_reporter* error_reporter) noexcept
      : input_(input), error_reporter_(error_reporter) {
    this->last_token_.begin = nullptr;
    this->last_last_token_end_ = nullptr;
    this->parse_current_token();
  }

  void parse_current_token();

  const token& peek() const noexcept { return this->last_token_; }

  void skip() { this->parse_current_token(); }
  void skip_in_template(const char* template_begin);

  void insert_semicolon();

 private:
  struct parsed_template_body {
    token_type type;
    const char* end;
  };

  static parsed_template_body parse_template_body(const char* input,
                                                  const char* template_begin,
                                                  error_reporter*);

  void skip_whitespace();
  void skip_comment();

  static bool is_digit(char);
  static bool is_identifier_character(char);
  static token_type keyword_from_index(std::ptrdiff_t);

  token last_token_;
  const char* last_last_token_end_;
  const char* input_;
  error_reporter* error_reporter_;
};
}  // namespace quick_lint_js

#endif
