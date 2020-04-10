// quicklint-js finds bugs in JavaScript programs.
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
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
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

namespace quicklint_js {
namespace {
constexpr const char keywords[][11] = {
    // clang-format off
  "as",
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

    default:
      assert(false);
      break;
  }
  this->last_token_.end = this->input_;

  this->skip_whitespace();
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
}  // namespace quicklint_js
