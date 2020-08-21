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

#include <algorithm>
#include <cstring>
#include <iterator>
#include <ostream>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <string_view>
#include <type_traits>

#define QLJS_CASE_IDENTIFIER_START \
  case '$':                        \
  case '_':                        \
  case 'A':                        \
  case 'B':                        \
  case 'C':                        \
  case 'D':                        \
  case 'E':                        \
  case 'F':                        \
  case 'G':                        \
  case 'H':                        \
  case 'I':                        \
  case 'J':                        \
  case 'K':                        \
  case 'L':                        \
  case 'M':                        \
  case 'N':                        \
  case 'O':                        \
  case 'P':                        \
  case 'Q':                        \
  case 'R':                        \
  case 'S':                        \
  case 'T':                        \
  case 'U':                        \
  case 'V':                        \
  case 'W':                        \
  case 'X':                        \
  case 'Y':                        \
  case 'Z':                        \
  case 'a':                        \
  case 'b':                        \
  case 'c':                        \
  case 'd':                        \
  case 'e':                        \
  case 'f':                        \
  case 'g':                        \
  case 'h':                        \
  case 'i':                        \
  case 'j':                        \
  case 'k':                        \
  case 'l':                        \
  case 'm':                        \
  case 'n':                        \
  case 'o':                        \
  case 'p':                        \
  case 'q':                        \
  case 'r':                        \
  case 's':                        \
  case 't':                        \
  case 'u':                        \
  case 'v':                        \
  case 'w':                        \
  case 'x':                        \
  case 'y':                        \
  case 'z'

#define QLJS_CASE_DECIMAL_DIGIT \
  case '0':                     \
  case '1':                     \
  case '2':                     \
  case '3':                     \
  case '4':                     \
  case '5':                     \
  case '6':                     \
  case '7':                     \
  case '8':                     \
  case '9'

namespace quick_lint_js {
identifier token::identifier_name() const noexcept {
  switch (this->type) {
  QLJS_CASE_KEYWORD:
  case token_type::identifier:
    break;
    default:
      assert(false);
      break;
  }
  return identifier(this->span());
}

source_code_span token::span() const noexcept {
  return source_code_span(this->begin, this->end);
}

void lexer::parse_current_token() {
  this->last_last_token_end_ = this->last_token_.end;
  this->last_token_.has_leading_newline = false;
  this->skip_whitespace();

retry:
  this->last_token_.begin = this->input_;
  switch (this->input_[0]) {
    case '\0':
      this->last_token_.type = token_type::end_of_file;
      break;

    QLJS_CASE_DECIMAL_DIGIT:
      this->last_token_.type = token_type::number;
      if (this->input_[0] == '0') {
        switch (this->input_[1]) {
          case 'x':
          case 'X':
            this->input_ += 2;
            this->parse_hexadecimal_number();
            break;
          default:
            this->parse_number();
            break;
        }
      } else {
        this->parse_number();
      }
      break;

    QLJS_CASE_IDENTIFIER_START : {
      this->input_ += 1;
      while (this->is_identifier_character(this->input_[0])) {
        this->input_ += 1;
      }
      this->last_token_.end = this->input_;
      this->last_token_.type = this->identifier_token_type(
          std::string_view(this->last_token_.begin,
                           narrow_cast<std::size_t>(this->last_token_.end -
                                                    this->last_token_.begin)));
      break;
    }

    case '(':
    case ')':
    case ',':
    case ':':
    case ';':
    case '?':
    case '[':
    case ']':
    case '{':
    case '}':
    case '~':
      this->last_token_.type = static_cast<token_type>(*this->input_);
      this->input_ += 1;
      break;

    case '.':
      if (this->input_[1] == '.' && this->input_[2] == '.') {
        this->last_token_.type = token_type::dot_dot_dot;
        this->input_ += 3;
      } else if (this->is_digit(this->input_[1])) {
        this->last_token_.type = token_type::number;
        this->parse_number();
      } else {
        this->last_token_.type = token_type::dot;
        this->input_ += 1;
      }
      break;

    case '=':
      if (this->input_[1] == '=') {
        if (this->input_[2] == '=') {
          this->last_token_.type = token_type::equal_equal_equal;
          this->input_ += 3;
        } else {
          this->last_token_.type = token_type::equal_equal;
          this->input_ += 2;
        }
      } else if (this->input_[1] == '>') {
        this->last_token_.type = token_type::equal_greater;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::equal;
        this->input_ += 1;
      }
      break;

    case '!':
      if (this->input_[1] == '=') {
        if (this->input_[2] == '=') {
          this->last_token_.type = token_type::bang_equal_equal;
          this->input_ += 3;
        } else {
          this->last_token_.type = token_type::bang_equal;
          this->input_ += 2;
        }
      } else {
        this->last_token_.type = token_type::bang;
        this->input_ += 1;
      }
      break;

    case '<':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::less_equal;
        this->input_ += 2;
      } else if (this->input_[1] == '<') {
        if (this->input_[2] == '=') {
          this->last_token_.type = token_type::less_less_equal;
          this->input_ += 3;
        } else {
          this->last_token_.type = token_type::less_less;
          this->input_ += 2;
        }
      } else {
        this->last_token_.type = token_type::less;
        this->input_ += 1;
      }
      break;

    case '>':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::greater_equal;
        this->input_ += 2;
      } else if (this->input_[1] == '>') {
        if (this->input_[2] == '>') {
          if (this->input_[3] == '=') {
            this->last_token_.type = token_type::greater_greater_greater_equal;
            this->input_ += 4;
          } else {
            this->last_token_.type = token_type::greater_greater_greater;
            this->input_ += 3;
          }
        } else if (this->input_[2] == '=') {
          this->last_token_.type = token_type::greater_greater_equal;
          this->input_ += 3;
        } else {
          this->last_token_.type = token_type::greater_greater;
          this->input_ += 2;
        }
      } else {
        this->last_token_.type = token_type::greater;
        this->input_ += 1;
      }
      break;

    case '+':
      if (this->input_[1] == '+') {
        this->last_token_.type = token_type::plus_plus;
        this->input_ += 2;
      } else if (this->input_[1] == '=') {
        this->last_token_.type = token_type::plus_equal;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::plus;
        this->input_ += 1;
      }
      break;

    case '-':
      if (this->input_[1] == '-') {
        this->last_token_.type = token_type::minus_minus;
        this->input_ += 2;
      } else if (this->input_[1] == '=') {
        this->last_token_.type = token_type::minus_equal;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::minus;
        this->input_ += 1;
      }
      break;

    case '*':
      if (this->input_[1] == '*') {
        if (this->input_[2] == '=') {
          this->last_token_.type = token_type::star_star_equal;
          this->input_ += 3;
        } else {
          this->last_token_.type = token_type::star_star;
          this->input_ += 2;
        }
      } else if (this->input_[1] == '=') {
        this->last_token_.type = token_type::star_equal;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::star;
        this->input_ += 1;
      }
      break;

    case '/':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::slash_equal;
        this->input_ += 2;
      } else if (this->input_[1] == '*') {
        this->skip_block_comment();
        goto retry;
      } else if (this->input_[1] == '/') {
        this->skip_line_comment();
        goto retry;
      } else {
        this->last_token_.type = token_type::slash;
        this->input_ += 1;
      }
      break;

    case '^':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::circumflex_equal;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::circumflex;
        this->input_ += 1;
      }
      break;

    case '%':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::percent_equal;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::percent;
        this->input_ += 1;
      }
      break;

    case '&':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::ampersand_equal;
        this->input_ += 2;
      } else if (this->input_[1] == '&') {
        this->last_token_.type = token_type::ampersand_ampersand;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::ampersand;
        this->input_ += 1;
      }
      break;

    case '|':
      if (this->input_[1] == '=') {
        this->last_token_.type = token_type::pipe_equal;
        this->input_ += 2;
      } else if (this->input_[1] == '|') {
        this->last_token_.type = token_type::pipe_pipe;
        this->input_ += 2;
      } else {
        this->last_token_.type = token_type::pipe;
        this->input_ += 1;
      }
      break;

    case '"':
    case '\'': {
      char opening_quote = this->input_[0];

      const char* c = &this->input_[1];
      for (;;) {
        switch (*c) {
          case '\0':
            // TODO(strager): Support other kinds of line terminators.
          case '\n':
            this->error_reporter_->report_error_unclosed_string_literal(
                source_code_span(&this->input_[0], c));
            goto done;

          case '\\':
            ++c;
            switch (*c) {
              case '\0':
                this->error_reporter_->report_error_unclosed_string_literal(
                    source_code_span(&this->input_[0], c));
                goto done;
              default:
                ++c;
                break;
            }
            break;

          case '"':
          case '\'':
            if (*c == opening_quote) {
              ++c;
              goto done;
            }
            ++c;
            break;

          default:
            ++c;
            break;
        }
      }
    done:
      this->last_token_.type = token_type::string;
      this->input_ = c;
      break;
    }

    case '`': {
      this->input_ += 1;
      parsed_template_body body = this->parse_template_body(
          this->input_, this->last_token_.begin, this->error_reporter_);
      this->last_token_.type = body.type;
      this->input_ = body.end;
      break;
    }

    default:
      assert(false);
      break;
  }
  this->last_token_.end = this->input_;
}

void lexer::skip_in_template(const char* template_begin) {
  this->last_token_.begin = this->input_;
  parsed_template_body body = this->parse_template_body(
      this->input_, template_begin, this->error_reporter_);
  this->last_token_.type = body.type;
  this->input_ = body.end;
  this->last_token_.end = body.end;
}

lexer::parsed_template_body lexer::parse_template_body(
    const char* input, const char* template_begin,
    error_reporter* error_reporter) {
  const char* c = input;
  for (;;) {
    switch (*c) {
      case '\0':
        error_reporter->report_error_unclosed_template(
            source_code_span(template_begin, c));
        return parsed_template_body{token_type::complete_template, c};

      case '`':
        ++c;
        return parsed_template_body{token_type::complete_template, c};

      case '\\':
        ++c;
        switch (*c) {
          case '\0':
            error_reporter->report_error_unclosed_template(
                source_code_span(template_begin, c));
            return parsed_template_body{token_type::complete_template, c};
          default:
            ++c;
            break;
        }
        break;

      case '$':
        if (c[1] == '{') {
          c += 2;
          return parsed_template_body{token_type::incomplete_template, c};
        }
        ++c;
        break;

      default:
        ++c;
        break;
    }
  }
}

void lexer::reparse_as_regexp() {
  assert(this->last_token_.type == token_type::slash);

  this->input_ = this->last_token_.begin;
  assert(this->input_[0] == '/');
  this->last_token_.type = token_type::regexp;

  const char* c = &this->input_[1];
next:
  switch (*c) {
    case '\0':
      this->error_reporter_->report_error_unclosed_regexp_literal(
          source_code_span(this->last_token_.begin, c));
      break;

    case '\\':
      ++c;
      switch (*c) {
        case '\0':
          this->error_reporter_->report_error_unclosed_regexp_literal(
              source_code_span(this->last_token_.begin, c));
          break;
        default:
          ++c;
          goto next;
      }
      break;

    case '/':
      ++c;
      while (this->is_identifier_character(*c)) {
        ++c;
      }
      break;

    default:
      ++c;
      goto next;
  }

  this->input_ = c;
  this->last_token_.end = this->input_;
}

void lexer::insert_semicolon() {
  this->input_ = this->last_last_token_end_;

  this->last_token_.type = token_type::semicolon;
  this->last_token_.has_leading_newline = false;
  this->last_token_.begin = this->input_;
  this->last_token_.end = this->input_;
}

const char* lexer::end_of_previous_token() const noexcept {
  bool semicolon_was_inserted =
      this->last_token_.type == token_type::semicolon &&
      this->last_token_.begin == this->last_token_.end;
  assert(!semicolon_was_inserted);

  return this->last_last_token_end_;
}

void lexer::parse_hexadecimal_number() {
  assert(this->is_hex_digit(this->input_[0]) || this->input_[0] == '.');
  while (this->is_hex_digit(this->input_[0])) {
    this->input_ += 1;
  }
}

void lexer::parse_number() {
  assert(this->is_digit(this->input_[0]) || this->input_[0] == '.');
  while (this->is_digit(this->input_[0])) {
    this->input_ += 1;
  }
  if (this->input_[0] == '.') {
    this->input_ += 1;
    while (this->is_digit(this->input_[0])) {
      this->input_ += 1;
    }
  }
}

void lexer::skip_whitespace() {
next:
  switch (this->input_[0]) {
    case ' ':
    case '\f':
    case '\t':
      this->input_ += 1;
      goto next;

    case '\n':
      this->last_token_.has_leading_newline = true;
      this->input_ += 1;
      goto next;

    case static_cast<char>(0xe1):
      if (this->input_[1] == static_cast<char>(0x9a) &&
          this->input_[2] == static_cast<char>(0x80)) {
        // U+1680 Ogham Space Mark
        this->input_ += 3;
        goto next;
      } else {
        return;
      }

    case static_cast<char>(0xe2):
      if (this->input_[1] == static_cast<char>(0x80)) {
        switch (this->input_[2]) {
          case static_cast<char>(0x80):  // U+2000 En Quad
          case static_cast<char>(0x81):  // U+2001 Em Quad
          case static_cast<char>(0x82):  // U+2002 En Space
          case static_cast<char>(0x83):  // U+2003 Em Space
          case static_cast<char>(0x84):  // U+2004 Three-Per-Em Space
          case static_cast<char>(0x85):  // U+2005 Four-Per-Em Space
          case static_cast<char>(0x86):  // U+2006 Six-Per-Em Space
          case static_cast<char>(0x87):  // U+2007 Figure Space
          case static_cast<char>(0x88):  // U+2008 Punctuation Space
          case static_cast<char>(0x89):  // U+2009 Thin Space
          case static_cast<char>(0x8a):  // U+200A Hair Space
          case static_cast<char>(0xaf):  // U+202F Narrow No-Break Space (NNBSP)
            this->input_ += 3;
            goto next;

          default:
            return;
        }
      } else if (this->input_[1] == static_cast<char>(0x81)) {
        if (this->input_[2] == static_cast<char>(0x9f)) {
          // U+205F Medium Mathematical Space (MMSP)
          this->input_ += 3;
          goto next;
        } else {
          return;
        }
      } else {
        return;
      }

    case static_cast<char>(0xe3):
      if (this->input_[1] == static_cast<char>(0x80) &&
          this->input_[2] == static_cast<char>(0x80)) {
        // U+3000 Ideographic Space
        this->input_ += 3;
        goto next;
      } else {
        return;
      }

    case static_cast<char>(0xef):
      if (this->input_[1] == static_cast<char>(0xbb) &&
          this->input_[2] == static_cast<char>(0xbf)) {
        // U+FEFF Zero Width No-Break Space (BOM, ZWNBSP)
        this->input_ += 3;
        goto next;
      } else {
        return;
      }

    case static_cast<char>(0xc2):
      if (this->input_[1] == static_cast<char>(0xa0)) {
        // U+00A0 No-Break Space (NBSP)
        this->input_ += 2;
        goto next;
      } else {
        return;
      }

    default:
      return;
  }
}

void lexer::skip_block_comment() {
  assert(this->input_[0] == '/' && this->input_[1] == '*');

  const char* comment_end = std::strstr(this->input_ + 2, "*/");
  if (comment_end == nullptr) {
    this->error_reporter_->report_error_unclosed_block_comment(
        source_code_span(&this->input_[0], &this->input_[2]));
    this->input_ += std::strlen(this->input_);
    return;
  }

  const char* newline = std::find(this->input_ + 2, comment_end, '\n');
  if (newline != comment_end) {
    this->last_token_.has_leading_newline = true;
  }

  this->input_ = comment_end + 2;
  this->skip_whitespace();
}

void lexer::skip_line_comment() {
  assert(this->input_[0] == '/' && this->input_[1] == '/');
  const char* comment_end = std::strchr(this->input_ + 2, '\n');
  if (comment_end == nullptr) {
    this->input_ += std::strlen(this->input_);
  } else {
    this->input_ = comment_end + 1;
    this->skip_whitespace();
  }
}

bool lexer::is_digit(char c) {
  switch (c) {
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
    default:
      return false;
  }
}

bool lexer::is_hex_digit(char c) {
  switch (c) {
  QLJS_CASE_DECIMAL_DIGIT:
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
    return true;
    default:
      return false;
  }
}

bool lexer::is_identifier_character(char c) {
  switch (c) {
  QLJS_CASE_IDENTIFIER_START:
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
    default:
      return false;
  }
}

token_type lexer::keyword_from_index(std::ptrdiff_t index) noexcept {
  using token_type_int = std::underlying_type_t<token_type>;
  return static_cast<token_type>(
      static_cast<token_type_int>(token_type::first_keyword) + index);
}

namespace {
const char* to_string(token_type type) {
#define QLJS_CASE(t)  \
  case token_type::t: \
    return #t;
  switch (type) {
    QLJS_CASE(kw_as)
    QLJS_CASE(kw_async)
    QLJS_CASE(kw_await)
    QLJS_CASE(kw_break)
    QLJS_CASE(kw_case)
    QLJS_CASE(kw_catch)
    QLJS_CASE(kw_class)
    QLJS_CASE(kw_const)
    QLJS_CASE(kw_continue)
    QLJS_CASE(kw_debugger)
    QLJS_CASE(kw_default)
    QLJS_CASE(kw_delete)
    QLJS_CASE(kw_do)
    QLJS_CASE(kw_else)
    QLJS_CASE(kw_export)
    QLJS_CASE(kw_extends)
    QLJS_CASE(kw_false)
    QLJS_CASE(kw_finally)
    QLJS_CASE(kw_for)
    QLJS_CASE(kw_from)
    QLJS_CASE(kw_function)
    QLJS_CASE(kw_get)
    QLJS_CASE(kw_if)
    QLJS_CASE(kw_import)
    QLJS_CASE(kw_in)
    QLJS_CASE(kw_instanceof)
    QLJS_CASE(kw_let)
    QLJS_CASE(kw_new)
    QLJS_CASE(kw_null)
    QLJS_CASE(kw_of)
    QLJS_CASE(kw_return)
    QLJS_CASE(kw_static)
    QLJS_CASE(kw_super)
    QLJS_CASE(kw_switch)
    QLJS_CASE(kw_this)
    QLJS_CASE(kw_throw)
    QLJS_CASE(kw_true)
    QLJS_CASE(kw_try)
    QLJS_CASE(kw_typeof)
    QLJS_CASE(kw_var)
    QLJS_CASE(kw_void)
    QLJS_CASE(kw_while)
    QLJS_CASE(kw_with)
    QLJS_CASE(kw_yield)
    QLJS_CASE(ampersand)
    QLJS_CASE(ampersand_ampersand)
    QLJS_CASE(ampersand_equal)
    QLJS_CASE(bang)
    QLJS_CASE(bang_equal)
    QLJS_CASE(bang_equal_equal)
    QLJS_CASE(circumflex)
    QLJS_CASE(circumflex_equal)
    QLJS_CASE(colon)
    QLJS_CASE(comma)
    QLJS_CASE(complete_template)
    QLJS_CASE(dot)
    QLJS_CASE(dot_dot_dot)
    QLJS_CASE(end_of_file)
    QLJS_CASE(equal)
    QLJS_CASE(equal_equal)
    QLJS_CASE(equal_equal_equal)
    QLJS_CASE(equal_greater)
    QLJS_CASE(greater)
    QLJS_CASE(greater_equal)
    QLJS_CASE(greater_greater)
    QLJS_CASE(greater_greater_equal)
    QLJS_CASE(greater_greater_greater)
    QLJS_CASE(greater_greater_greater_equal)
    QLJS_CASE(identifier)
    QLJS_CASE(incomplete_template)
    QLJS_CASE(left_curly)
    QLJS_CASE(left_paren)
    QLJS_CASE(left_square)
    QLJS_CASE(less)
    QLJS_CASE(less_equal)
    QLJS_CASE(less_less)
    QLJS_CASE(less_less_equal)
    QLJS_CASE(minus)
    QLJS_CASE(minus_equal)
    QLJS_CASE(minus_minus)
    QLJS_CASE(number)
    QLJS_CASE(percent)
    QLJS_CASE(percent_equal)
    QLJS_CASE(pipe)
    QLJS_CASE(pipe_equal)
    QLJS_CASE(pipe_pipe)
    QLJS_CASE(plus)
    QLJS_CASE(plus_equal)
    QLJS_CASE(plus_plus)
    QLJS_CASE(question)
    QLJS_CASE(regexp)
    QLJS_CASE(right_curly)
    QLJS_CASE(right_paren)
    QLJS_CASE(right_square)
    QLJS_CASE(semicolon)
    QLJS_CASE(slash)
    QLJS_CASE(slash_equal)
    QLJS_CASE(star)
    QLJS_CASE(star_equal)
    QLJS_CASE(star_star)
    QLJS_CASE(star_star_equal)
    QLJS_CASE(string)
    QLJS_CASE(tilde)
  }
  return "???";
}
}  // namespace

std::ostream& operator<<(std::ostream& out, token_type type) {
  out << to_string(type);
  return out;
}
}  // namespace quick_lint_js
