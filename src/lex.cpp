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
#include <cstdint>
#include <cstring>
#include <iterator>
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/bit.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/simd.h>
#include <quick-lint-js/warning.h>
#include <type_traits>

#if QLJS_HAVE_X86_SSE4_2
#include <nmmintrin.h>
#endif

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
      QLJS_ASSERT(false);
      break;
  }
  return identifier(this->span());
}

source_code_span token::span() const noexcept {
  return source_code_span(this->begin, this->end);
}

lexer::lexer(padded_string_view input, error_reporter* error_reporter) noexcept
    : input_(input.data()),
      error_reporter_(error_reporter),
      original_input_(input) {
  this->last_token_.end = nullptr;
  this->parse_current_token();
}

void lexer::parse_current_token() {
  this->last_last_token_end_ = const_cast<char8*>(this->last_token_.end);
  this->last_token_.has_leading_newline = false;
  this->skip_whitespace();

retry:
  this->last_token_.begin = this->input_;
  switch (this->input_[0]) {
  QLJS_CASE_DECIMAL_DIGIT:
    this->last_token_.type = token_type::number;
    if (this->input_[0] == '0') {
      switch (this->input_[1]) {
        case 'x':
        case 'X':
          this->input_ += 2;
          this->parse_hexadecimal_number();
          break;
        case 'b':
        case 'B':
          this->input_ += 2;
          this->parse_binary_number();
          break;
        default:
          this->parse_number();
          break;
      }
    } else {
      this->parse_number();
    }
    break;

  QLJS_CASE_IDENTIFIER_START:
    this->input_ = this->parse_identifier(this->input_);
    this->last_token_.end = this->input_;
    this->last_token_.type = this->identifier_token_type(
        string8_view(this->last_token_.begin,
                     narrow_cast<std::size_t>(this->last_token_.end -
                                              this->last_token_.begin)));
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
      char8 opening_quote = this->input_[0];

      char8* c = &this->input_[1];
      for (;;) {
        switch (static_cast<unsigned char>(*c)) {
          case '\0':
            if (this->is_eof(c)) {
              this->error_reporter_->report(error_unclosed_string_literal{
                  source_code_span(&this->input_[0], c)});
              goto done;
            } else {
              ++c;
              break;
            }

          case '\n':
          case '\r':
            this->error_reporter_->report(error_unclosed_string_literal{
                source_code_span(&this->input_[0], c)});
            goto done;

          case '\\':
            ++c;
            switch (*c) {
              case '\0':
                if (this->is_eof(c)) {
                  this->error_reporter_->report(error_unclosed_string_literal{
                      source_code_span(&this->input_[0], c)});
                  goto done;
                } else {
                  ++c;
                  break;
                }
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

    case '#':
      if (this->input_[1] == '!' &&
          this->input_ == this->original_input_.data()) {
        this->skip_line_comment();
        goto retry;
      } else {
        this->error_reporter_->report(error_unexpected_hash_character{
            source_code_span(&this->input_[0], &this->input_[1])});
        this->input_ += 1;
        this->skip_whitespace();
        goto retry;
      }
      break;

    case '\0':
      if (this->is_eof(this->input_)) {
        this->last_token_.type = token_type::end_of_file;
        break;
      } else {
        [[fallthrough]];
      }
    case u8'\x01':    // SOH Start of Heading
    case u8'\x02':    // STX Start of Text
    case u8'\x03':    // ETX End-of-text character
    case u8'\x04':    // EOT End-of-transmission character
    case u8'\x05':    // ENQ Enquiry character
    case u8'\x06':    // ACK Acknowledge character
    case u8'\x07':    // BEL Bell character
    case u8'\x08':    // BS Backspace
    case u8'\x0e':    // SO Shift Out
    case u8'\x0f':    // SI Shift In
    case u8'\x10':    // DLE Data Link Escape
    case u8'\x11':    // DC1 Device Control 1
    case u8'\x12':    // DC2 Device Control 2
    case u8'\x13':    // DC3 Device Control 3
    case u8'\x14':    // DC4 Device Control 4
    case u8'\x15':    // NAK Negative-acknowledge character
    case u8'\x16':    // SYN Synchronous Idle
    case u8'\x17':    // ETB End of Transmission Block
    case u8'\x18':    // CAN Cancel character
    case u8'\x19':    // EM End of Medium
    case u8'\x1a':    // SUB Substitute character
    case u8'\x1b':    // ESC Escape character
    case u8'\x1c':    // FS File Separator
    case u8'\x1d':    // GS Group Separator
    case u8'\x1e':    // RS Record Separator
    case u8'\x1f':    // US Unit Separator
    case u8'\x7f': {  // DEL Delete
      char8* end = this->input_ + 1;
      this->error_reporter_->report(error_unexpected_control_character{
          .character = source_code_span(this->input_, end)});
      this->input_ = end;
      this->skip_whitespace();
      goto retry;
    }

    case u8'@': {
      char8* end = this->input_ + 1;
      this->error_reporter_->report(error_unexpected_at_character{
          .character = source_code_span(this->input_, end)});
      this->input_ = end;
      this->skip_whitespace();
      goto retry;
    }

    default:
      this->error_reporter_->report_fatal_error_unimplemented_character(
          /*qljs_file_name=*/__FILE__,
          /*qljs_line=*/__LINE__,
          /*qljs_function_name=*/__func__,
          /*character=*/this->input_);
      QLJS_CRASH_DISALLOWING_CORE_DUMP();
      break;
  }
  this->last_token_.end = this->input_;
}

void lexer::skip_in_template(const char8* template_begin) {
  this->last_token_.begin = this->input_;
  parsed_template_body body = this->parse_template_body(
      this->input_, template_begin, this->error_reporter_);
  this->last_token_.type = body.type;
  this->input_ = body.end;
  this->last_token_.end = body.end;
}

lexer::parsed_template_body lexer::parse_template_body(
    char8* input, const char8* template_begin, error_reporter* error_reporter) {
  char8* c = input;
  for (;;) {
    switch (*c) {
      case '\0':
        if (this->is_eof(c)) {
          error_reporter->report(
              error_unclosed_template{source_code_span(template_begin, c)});
          return parsed_template_body{token_type::complete_template, c};
        } else {
          ++c;
          break;
        }

      case '`':
        ++c;
        return parsed_template_body{token_type::complete_template, c};

      case '\\':
        ++c;
        switch (*c) {
          case '\0':
            if (this->is_eof(c)) {
              error_reporter->report(
                  error_unclosed_template{source_code_span(template_begin, c)});
              return parsed_template_body{token_type::complete_template, c};
            } else {
              ++c;
              break;
            }
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
  QLJS_ASSERT(this->last_token_.type == token_type::slash);

  this->input_ = const_cast<char8*>(this->last_token_.begin);
  QLJS_ASSERT(this->input_[0] == '/');
  this->last_token_.type = token_type::regexp;

  char8* c = &this->input_[1];
next:
  switch (*c) {
    case '\0':
      if (this->is_eof(c)) {
        this->error_reporter_->report(error_unclosed_regexp_literal{
            source_code_span(this->last_token_.begin, c)});
        break;
      } else {
        ++c;
        goto next;
      }

    case '\\':
      ++c;
      switch (*c) {
        case '\0':
          if (this->is_eof(c)) {
            this->error_reporter_->report(error_unclosed_regexp_literal{
                source_code_span(this->last_token_.begin, c)});
            break;
          } else {
            ++c;
            goto next;
          }

        default:
          ++c;
          goto next;
      }
      break;

    case '[':
      ++c;
      for (;;) {
        switch (*c) {
          case u8']':
          case u8'\0':
            goto next;

          case u8'\\':
            if (c[1] == u8']') {
              c += 2;
            } else {
              c += 1;
            }
            break;

          default:
            ++c;
            break;
        }
      }
      QLJS_UNREACHABLE();

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

const char8* lexer::end_of_previous_token() const noexcept {
  bool semicolon_was_inserted =
      this->last_token_.type == token_type::semicolon &&
      this->last_token_.begin == this->last_token_.end;
  QLJS_ASSERT(!semicolon_was_inserted);

  return this->last_last_token_end_;
}

void lexer::parse_hexadecimal_number() {
  QLJS_ASSERT(this->is_hex_digit(this->input_[0]) || this->input_[0] == '.');
  char8* input = this->input_;

  while (this->is_hex_digit(*input)) {
    input += 1;
  }

  this->input_ = check_garbage_in_number_literal(input);
}

void lexer::parse_binary_number() {
  QLJS_ASSERT(this->is_binary_digit(this->input_[0]));
  char8* input = this->input_;

  while (this->is_binary_digit(*input)) {
    input += 1;
  }

  this->input_ = check_garbage_in_number_literal(input);
}

char8* lexer::check_garbage_in_number_literal(char8* input) {
  char8* garbage_begin = input;
  for (;;) {
    switch (*input) {
    QLJS_CASE_DECIMAL_DIGIT:
    QLJS_CASE_IDENTIFIER_START:
      input += 1;
      break;
      default:
        goto done_parsing_garbage;
    }
  }
done_parsing_garbage:
  char8* garbage_end = input;
  if (garbage_end != garbage_begin) {
    this->error_reporter_->report(error_unexpected_characters_in_number{
        source_code_span(garbage_begin, garbage_end)});
    input = garbage_end;
  }

  return input;
}

void lexer::parse_number() {
  QLJS_ASSERT(this->is_digit(this->input_[0]) || this->input_[0] == '.');
  char8* input = this->input_;
  const char8* number_begin = input;

  auto consume_garbage = [this, &input]() {
    char8* garbage_begin = input;
    char8* garbage_end = this->parse_identifier(garbage_begin);
    this->error_reporter_->report(error_unexpected_characters_in_number{
        source_code_span(garbage_begin, garbage_end)});
    input = garbage_end;
  };

  input = this->parse_decimal_digits_and_underscores(input);
  bool has_decimal_point = *input == '.';
  if (has_decimal_point) {
    input += 1;
    input = this->parse_decimal_digits_and_underscores(input);
    has_decimal_point = true;
  }
  bool has_exponent = *input == 'e' || *input == 'E';
  if (has_exponent) {
    char8* e = input;
    input += 1;
    if (*input == '-' || *input == '+') {
      input += 1;
    }
    if (this->is_digit(*input)) {
      input = this->parse_decimal_digits_and_underscores(input);
    } else {
      input = e;
      consume_garbage();
    }
  }
  if (*input == 'n') {
    input += 1;
    if (has_decimal_point) {
      this->error_reporter_->report(
          error_big_int_literal_contains_decimal_point{
              source_code_span(number_begin, input)});
    }
    if (has_exponent) {
      this->error_reporter_->report(error_big_int_literal_contains_exponent{
          source_code_span(number_begin, input)});
    }
    if (number_begin[0] == u8'0' && this->is_digit(number_begin[1])) {
      this->error_reporter_->report(error_big_int_literal_contains_leading_zero{
          source_code_span(number_begin, input)});
    }
  }

  switch (*input) {
  QLJS_CASE_IDENTIFIER_START:
    consume_garbage();
    break;
  }
  this->input_ = input;
}

char8* lexer::parse_decimal_digits_and_underscores(char8* input) noexcept {
  bool has_trailing_underscore = false;
  while (is_digit(*input)) {
    has_trailing_underscore = false;
    input += 1;
    if (*input == '_') {
      const char8* garbage_begin = input;
      has_trailing_underscore = true;
      input += 1;
      if (*input == '_') {
        has_trailing_underscore = false;

        while (*input == '_') {
          input += 1;
        }

        this->error_reporter_->report(
            error_number_literal_contains_consecutive_underscores{
                source_code_span(garbage_begin, input)});
      }
    }
  }
  if (has_trailing_underscore == true) {
    // TODO error_reporter: SyntaxError: Numeric separators are not allowed at
    // the end of numeric literals
  }
  return input;
}

char8* lexer::parse_identifier(char8* input) {
  switch (*input) {
  QLJS_CASE_IDENTIFIER_START:
    break;
    default:
      QLJS_ASSERT(false);
      break;
  }
  input += 1;

#if QLJS_HAVE_X86_SSE2
  using char_vector = char_vector_16_sse2;
#else
  using char_vector = char_vector_1;
#endif

  auto count_identifier_characters = [](char_vector chars) -> int {
#if QLJS_HAVE_X86_SSE4_2
    __m128i ranges =
        _mm_setr_epi8('$', '$',  //
                      '_', '_',  //
                      '0', '9',  //
                      'a', 'z',  //
                      'A', 'Z',  //
                      // For unused table entries, duplicate a previous entry.
                      // (If we zero-filled, we would match null bytes!)
                      '$', '$',  //
                      '$', '$',  //
                      '$', '$');
    return _mm_cmpistri(ranges, chars.m128i(),
                        _SIDD_CMP_RANGES | _SIDD_LEAST_SIGNIFICANT |
                            _SIDD_NEGATIVE_POLARITY | _SIDD_UBYTE_OPS);
#else
#if QLJS_HAVE_X86_SSE2
    using bool_vector = bool_vector_16_sse2;
#else
    using bool_vector = bool_vector_1;
#endif

    constexpr std::uint8_t upper_to_lower_mask = u8'a' - u8'A';
    static_assert((u8'A' | upper_to_lower_mask) == u8'a');

    char_vector lower_cased_characters =
        chars | char_vector::repeated(upper_to_lower_mask);
    bool_vector is_alpha =
        (lower_cased_characters > char_vector::repeated(u8'a' - 1)) &
        (lower_cased_characters < char_vector::repeated(u8'z' + 1));
    bool_vector is_digit = (chars > char_vector::repeated(u8'0' - 1)) &
                           (chars < char_vector::repeated(u8'9' + 1));
    bool_vector is_identifier = is_alpha | is_digit |  //
                                (chars == char_vector::repeated(u8'$')) |
                                (chars == char_vector::repeated(u8'_'));
    return is_identifier.find_first_false();
#endif
  };

  bool is_all_identifier_characters;
  do {
    char_vector chars = char_vector::load(input);
    int identifier_character_count = count_identifier_characters(chars);

    for (int i = 0; i < identifier_character_count; ++i) {
      QLJS_ASSERT(is_identifier_character(input[i]));
    }
    input += identifier_character_count;

    is_all_identifier_characters = identifier_character_count == chars.size;
  } while (is_all_identifier_characters);

  QLJS_ASSERT(!is_identifier_character(input[0]));
  return input;
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wunknown-attributes")
QLJS_WARNING_IGNORE_GCC("-Wattributes")
void lexer::skip_whitespace() {
  char8* input = this->input_;

next:
  char8 c = input[0];
  if (c == ' ' || c == '\t' || c == '\f' || c == '\v') {
    input += 1;
    goto next;
  } else if (c == '\n' || c == '\r') {
    this->last_token_.has_leading_newline = true;
    input += 1;
    goto next;
  } else if (static_cast<unsigned char>(c) >= 0xc2) {
    [[unlikely]] switch (static_cast<unsigned char>(c)) {
      case 0xe1:
        if (static_cast<unsigned char>(input[1]) == 0x9a &&
            static_cast<unsigned char>(input[2]) == 0x80) {
          // U+1680 Ogham Space Mark
          input += 3;
          goto next;
        } else {
          goto done;
        }

      case 0xe2:
        if (static_cast<unsigned char>(input[1]) == 0x80) {
          switch (static_cast<unsigned char>(input[2])) {
            case 0x80:  // U+2000 En Quad
            case 0x81:  // U+2001 Em Quad
            case 0x82:  // U+2002 En Space
            case 0x83:  // U+2003 Em Space
            case 0x84:  // U+2004 Three-Per-Em Space
            case 0x85:  // U+2005 Four-Per-Em Space
            case 0x86:  // U+2006 Six-Per-Em Space
            case 0x87:  // U+2007 Figure Space
            case 0x88:  // U+2008 Punctuation Space
            case 0x89:  // U+2009 Thin Space
            case 0x8a:  // U+200A Hair Space
            case 0xaf:  // U+202F Narrow No-Break Space (NNBSP)
              input += 3;
              goto next;

            case 0xa8:  // U+2028 Line Separator
            case 0xa9:  // U+2029 Paragraph Separator
              QLJS_ASSERT(this->newline_character_size(input) == 3);
              this->last_token_.has_leading_newline = true;
              input += 3;
              goto next;

            default:
              goto done;
          }
        } else if (static_cast<unsigned char>(input[1]) == 0x81) {
          if (static_cast<unsigned char>(input[2]) == 0x9f) {
            // U+205F Medium Mathematical Space (MMSP)
            input += 3;
            goto next;
          } else {
            goto done;
          }
        } else {
          goto done;
        }

      case 0xe3:
        if (static_cast<unsigned char>(input[1]) == 0x80 &&
            static_cast<unsigned char>(input[2]) == 0x80) {
          // U+3000 Ideographic Space
          input += 3;
          goto next;
        } else {
          goto done;
        }

      case 0xef:
        if (static_cast<unsigned char>(input[1]) == 0xbb &&
            static_cast<unsigned char>(input[2]) == 0xbf) {
          // U+FEFF Zero Width No-Break Space (BOM, ZWNBSP)
          input += 3;
          goto next;
        } else {
          goto done;
        }

      case 0xc2:
        if (static_cast<unsigned char>(input[1]) == 0xa0) {
          // U+00A0 No-Break Space (NBSP)
          input += 2;
          goto next;
        } else {
          goto done;
        }

      default:
        goto done;
    }
  } else {
    goto done;
  }

done:
  this->input_ = input;
}
QLJS_WARNING_POP

void lexer::skip_block_comment() {
  QLJS_ASSERT(this->input_[0] == '/' && this->input_[1] == '*');
  char8* c = this->input_ + 2;

#if QLJS_HAVE_X86_SSE2
  using bool_vector = bool_vector_16_sse2;
  using char_vector = char_vector_16_sse2;
#else
  using bool_vector = bool_vector_1;
  using char_vector = char_vector_1;
#endif

  auto is_comment_end = [](const char8* string) -> bool {
    return string[0] == '*' && string[1] == '/';
  };

  for (;;) {
    char_vector chars = char_vector::load(c);
    bool_vector matches = (chars == char_vector::repeated(u8'*')) |
                          (chars == char_vector::repeated(u8'\0')) |
                          (chars == char_vector::repeated(u8'\n')) |
                          (chars == char_vector::repeated(u8'\r')) |
                          (chars == char_vector::repeated(0xe2));
    std::uint32_t mask = matches.mask();
    if (mask != 0) {
      for (int i = countr_zero(mask); i < chars.size; ++i) {
        if (mask & (1U << i)) {
          if (is_comment_end(&c[i])) {
            c += i;
            goto found_comment_end;
          }
          int newline_size = this->newline_character_size(&c[i]);
          if (newline_size > 0) {
            c += narrow_cast<std::ptrdiff_t>(i) + newline_size;
            goto found_newline_in_comment;
          }
          if (c[i] == '\0') {
            c += i;
            goto found_end_of_file;
          }
        }
      }
    }
    c += chars.size;
  }
  QLJS_UNREACHABLE();

found_newline_in_comment:
  this->last_token_.has_leading_newline = true;
  for (;;) {
    char_vector chars = char_vector::load(c);
    bool_vector matches = (chars == char_vector::repeated(u8'\0')) |
                          (chars == char_vector::repeated(u8'*'));
    std::uint32_t mask = matches.mask();
    if (mask != 0) {
      for (int i = countr_zero(mask); i < chars.size; ++i) {
        if (mask & (1U << i)) {
          if (is_comment_end(&c[i])) {
            c += i;
            goto found_comment_end;
          }
          if (c[i] == '\0') {
            c += i;
            goto found_end_of_file;
          }
        }
      }
    }
    c += chars.size;
  }
  QLJS_UNREACHABLE();

found_comment_end:
  this->input_ = c + 2;
  this->skip_whitespace();
  return;

  QLJS_UNREACHABLE();
found_end_of_file:
  this->error_reporter_->report(error_unclosed_block_comment{
      .comment_open = source_code_span(&this->input_[0], &this->input_[2])});
  this->input_ += strlen(this->input_);
}

void lexer::skip_line_comment() {
  QLJS_ASSERT((this->input_[0] == '/' && this->input_[1] == '/') ||
              (this->input_[0] == '#' && this->input_[1] == '!'));
  for (char8* c = this->input_ + 2;; ++c) {
    int newline_size = this->newline_character_size(c);
    if (newline_size > 0) {
      this->input_ = c + newline_size;
      this->skip_whitespace();
      break;
    }
    // TODO(strager): Should we handle null bytes differently for #! comments?
    if (*c == u8'\0' && this->is_eof(c)) {
      this->input_ = c;
      break;
    }
  }
}

bool lexer::is_eof(const char8* input) noexcept {
  QLJS_ASSERT(*input == u8'\0');
  return input == this->original_input_.null_terminator();
}

bool lexer::is_binary_digit(char8 c) { return c == u8'0' || c == u8'1'; }

bool lexer::is_digit(char8 c) {
  switch (c) {
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
    default:
      return false;
  }
}

bool lexer::is_hex_digit(char8 c) {
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

bool lexer::is_identifier_character(char8 c) {
  switch (c) {
  QLJS_CASE_IDENTIFIER_START:
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
    default:
      return false;
  }
}

int lexer::newline_character_size(const char8* input) {
  if (input[0] == u8'\n' || input[0] == u8'\r') {
    return 1;
  }
  if (static_cast<unsigned char>(input[0]) == 0xe2 &&
      static_cast<unsigned char>(input[1]) == 0x80) {
    switch (static_cast<unsigned char>(input[2])) {
      case 0xa8:  // U+2028 Line Separator
      case 0xa9:  // U+2029 Paragraph Separator
        return 3;
    }
  }
  return 0;
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
}

std::ostream& operator<<(std::ostream& out, token_type type) {
  out << to_string(type);
  return out;
}
}
