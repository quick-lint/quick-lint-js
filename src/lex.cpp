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
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
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
namespace {
constexpr const char keywords[][11] = {
    // clang-format off
  "as",
  "async",
  "await",
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "from",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "null",
  "return",
  "static",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "yield",
    // clang-format on
};
}

bool operator==(source_code_span x, std::string_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(source_code_span x, std::string_view y) noexcept {
  return !(x == y);
}

identifier token::identifier_name() const noexcept {
  assert(this->type == token_type::identifier);
  return identifier(this->span());
}

source_code_span token::span() const noexcept {
  return source_code_span(this->begin, this->end);
}

void lexer::parse_current_token() {
retry:
  this->last_token_.begin = this->input_;
  switch (this->input_[0]) {
    case '\0':
      this->last_token_.type = token_type::end_of_file;
      break;

    QLJS_CASE_DECIMAL_DIGIT:
      this->last_token_.type = token_type::number;
      this->input_ += 1;
      while (this->is_digit(this->input_[0])) {
        this->input_ += 1;
      }
      break;

    QLJS_CASE_IDENTIFIER_START : {
      this->input_ += 1;
      while (this->is_identifier_character(this->input_[0])) {
        this->input_ += 1;
      }
      this->last_token_.end = this->input_;
      this->last_token_.type = token_type::identifier;

      identifier identifier_name = this->last_token_.identifier_name();
      auto found_keyword = std::find_if(
          std::begin(keywords), std::end(keywords), [&](const char* keyword) {
            return identifier_name.string_view() == keyword;
          });
      if (found_keyword != std::end(keywords)) {
        this->last_token_.type =
            this->keyword_from_index(found_keyword - std::begin(keywords));
      }
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
        const char* comment_end = std::strstr(this->input_ + 2, "*/");
        if (comment_end == nullptr) {
          this->error_reporter_->report_error_unclosed_block_comment(
              source_code_span(&this->input_[0], &this->input_[2]));
          this->input_ += std::strlen(this->input_);
        } else {
          this->input_ = comment_end + 2;
          this->skip_whitespace();
        }
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

  this->skip_whitespace();
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

void lexer::skip_whitespace() {
  while (this->input_[0] == ' ' || this->input_[0] == '\n') {
    this->input_ += 1;
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

bool lexer::is_identifier_character(char c) {
  switch (c) {
  QLJS_CASE_IDENTIFIER_START:
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
    default:
      return false;
  }
}

token_type lexer::keyword_from_index(std::ptrdiff_t index) {
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
    QLJS_CASE(_as)
    QLJS_CASE(_async)
    QLJS_CASE(_await)
    QLJS_CASE(_break)
    QLJS_CASE(_case)
    QLJS_CASE(_catch)
    QLJS_CASE(_class)
    QLJS_CASE(_const)
    QLJS_CASE(_continue)
    QLJS_CASE(_debugger)
    QLJS_CASE(_default)
    QLJS_CASE(_delete)
    QLJS_CASE(_do)
    QLJS_CASE(_else)
    QLJS_CASE(_export)
    QLJS_CASE(_extends)
    QLJS_CASE(_false)
    QLJS_CASE(_finally)
    QLJS_CASE(_for)
    QLJS_CASE(_from)
    QLJS_CASE(_function)
    QLJS_CASE(_if)
    QLJS_CASE(_import)
    QLJS_CASE(_in)
    QLJS_CASE(_instanceof)
    QLJS_CASE(_let)
    QLJS_CASE(_new)
    QLJS_CASE(_null)
    QLJS_CASE(_return)
    QLJS_CASE(_static)
    QLJS_CASE(_super)
    QLJS_CASE(_switch)
    QLJS_CASE(_this)
    QLJS_CASE(_throw)
    QLJS_CASE(_true)
    QLJS_CASE(_try)
    QLJS_CASE(_typeof)
    QLJS_CASE(_var)
    QLJS_CASE(_void)
    QLJS_CASE(_while)
    QLJS_CASE(_with)
    QLJS_CASE(_yield)
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
