#include <quicklint-js/lex.h>

namespace quicklint_js {
void lexer::parse_current_token() {
  switch (this->input_[0]) {
    case '\0':
      this->last_token_.type = token_type::end_of_file;
      break;

    case '2':
      this->last_token_.type = token_type::number;
      this->input_ += 1;
      break;

    case 'i':
      this->last_token_.type = token_type::identifier;
      this->input_ += 1;
      break;

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

    default:
      assert(false);
      break;
  }

  this->skip_whitespace();
}

void lexer::skip_whitespace() {
  while (this->input_[0] == ' ') {
    this->input_ += 1;
  }
}
}  // namespace quicklint_js
