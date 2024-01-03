// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/bit.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/simd.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/utf-8.h>
#include <string>
#include <type_traits>
#include <utility>

#if QLJS_HAVE_X86_SSE4_2
#include <nmmintrin.h>
#endif

// clang-format off
#define QLJS_CASE_IDENTIFIER_START                                       \
  case '\\': case '$': case '_':                                         \
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':  \
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':  \
  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':  \
  case 'V': case 'W': case 'X': case 'Y': case 'Z':                      \
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':  \
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':  \
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':  \
  case 'v': case 'w': case 'x': case 'y': case 'z'
// clang-format on

// clang-format off
#define QLJS_CASE_OCTAL_DIGIT \
  case '0': case '1': case '2': case '3': \
  case '4': case '5': case '6': case '7'
// clang-format on

#define QLJS_CASE_DECIMAL_DIGIT \
  QLJS_CASE_OCTAL_DIGIT:        \
  case '8':                     \
  case '9'

#define QLJS_CASE_CHAR8(byte_code) case static_cast<Char8>(byte_code)

#define QLJS_CASE_NON_ASCII QLJS_CASE_NON_ASCII_(QLJS_CASE_CHAR8)
// clang-format off
#define QLJS_CASE_NON_ASCII_(c) \
  c(0x80):c(0x81):c(0x82):c(0x83):c(0x84):c(0x85):c(0x86):c(0x87):c(0x88):c(0x89):c(0x8a):c(0x8b):c(0x8c):c(0x8d):c(0x8e):c(0x8f): \
  c(0x90):c(0x91):c(0x92):c(0x93):c(0x94):c(0x95):c(0x96):c(0x97):c(0x98):c(0x99):c(0x9a):c(0x9b):c(0x9c):c(0x9d):c(0x9e):c(0x9f): \
  c(0xa0):c(0xa1):c(0xa2):c(0xa3):c(0xa4):c(0xa5):c(0xa6):c(0xa7):c(0xa8):c(0xa9):c(0xaa):c(0xab):c(0xac):c(0xad):c(0xae):c(0xaf): \
  c(0xb0):c(0xb1):c(0xb2):c(0xb3):c(0xb4):c(0xb5):c(0xb6):c(0xb7):c(0xb8):c(0xb9):c(0xba):c(0xbb):c(0xbc):c(0xbd):c(0xbe):c(0xbf): \
  c(0xc0):c(0xc1):c(0xc2):c(0xc3):c(0xc4):c(0xc5):c(0xc6):c(0xc7):c(0xc8):c(0xc9):c(0xca):c(0xcb):c(0xcc):c(0xcd):c(0xce):c(0xcf): \
  c(0xd0):c(0xd1):c(0xd2):c(0xd3):c(0xd4):c(0xd5):c(0xd6):c(0xd7):c(0xd8):c(0xd9):c(0xda):c(0xdb):c(0xdc):c(0xdd):c(0xde):c(0xdf): \
  c(0xe0):c(0xe1):c(0xe2):c(0xe3):c(0xe4):c(0xe5):c(0xe6):c(0xe7):c(0xe8):c(0xe9):c(0xea):c(0xeb):c(0xec):c(0xed):c(0xee):c(0xef): \
  c(0xf0):c(0xf1):c(0xf2):c(0xf3):c(0xf4):c(0xf5):c(0xf6):c(0xf7):c(0xf8):c(0xf9):c(0xfa):c(0xfb):c(0xfc):c(0xfd):c(0xfe):c(0xff)
// clang-format on

#define QLJS_CASE_NEWLINE_START \
  case u8'\n':                  \
  case u8'\r':                  \
  case line_separator_paragraph_separator_first_byte

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
namespace {
constexpr Char8 line_separator_paragraph_separator_first_byte =
    static_cast<Char8>(0xe2);

constexpr char32_t left_single_quote = U'\u2018';
constexpr char32_t left_double_quote = U'\u201c';
constexpr char32_t right_single_quote = U'\u2019';
constexpr char32_t right_double_quote = U'\u201d';

bool look_up_in_unicode_table(const std::uint8_t* table, std::size_t table_size,
                              char32_t code_point) {
  constexpr int bits_per_byte = 8;
  constexpr char32_t max_code_point = U'\U0010ffff';
  constexpr std::size_t bits_per_chunk = Lexer::unicode_table_chunk_size;
  constexpr std::size_t bytes_per_chunk = bits_per_chunk / bits_per_byte;
  using Chunk_Index_Type = Lexer::Unicode_Table_Chunk_Index_Type;

  QLJS_ASSERT(code_point <= max_code_point);
  std::size_t chunk_index_index = code_point / bits_per_chunk;
  if (chunk_index_index >= table_size / sizeof(Chunk_Index_Type)) {
    return false;
  }
  Chunk_Index_Type chunk_index = table[chunk_index_index];

  const std::uint8_t* chunk =
      &Lexer::unicode_tables_chunks[chunk_index * bytes_per_chunk];
  std::size_t bit_in_chunk = code_point % bits_per_chunk;
  return chunk[bit_in_chunk / bits_per_byte] &
         (1 << (bit_in_chunk % bits_per_byte));
}
}

Lexer::Lexer(Padded_String_View input, Diag_Reporter* diag_reporter)
    : Lexer(input, diag_reporter, Lexer_Options()) {}

Lexer::Lexer(Padded_String_View input, Diag_Reporter* diag_reporter,
             Lexer_Options options)
    : input_(input.data()),
      diag_reporter_(diag_reporter),
      original_input_(input),
      options_(options) {
  this->last_token_.end = this->input_;
  this->parse_bom_before_shebang();
  this->parse_current_token();
}

void Lexer::parse_bom_before_shebang() {
  const Char8* input = this->input_;
  if (static_cast<unsigned char>(input[0]) == 0xef &&
      static_cast<unsigned char>(input[1]) == 0xbb &&
      static_cast<unsigned char>(input[2]) == 0xbf) {
    input += 3;
    if (input[0] == u8'#' && input[1] == u8'!') {
      this->diag_reporter_->report(Diag_Unexpected_Bom_Before_Shebang{
          Source_Code_Span(&this->input_[0], &this->input_[3])});
      input += 2;
      this->input_ = input;
      this->skip_line_comment_body();
    } else {
      this->input_ = input;
    }
  }
}

[[gnu::noinline]] void Lexer::parse_current_token() {
  this->last_last_token_end_ = this->last_token_.end;
  this->last_token_.has_leading_newline = false;
  this->last_token_.has_leading_comment = false;
  this->skip_whitespace();

  while (!this->try_parse_current_token()) {
    // Loop.
  }
}

bool Lexer::try_parse_current_token() {
  this->last_token_.begin = this->input_;

  switch (this->input_[0]) {
  QLJS_CASE_DECIMAL_DIGIT:
    this->last_token_.type = Token_Type::number;
    if (this->input_[0] == '0') {
      switch (this->input_[1]) {
      case 'b':
      case 'B':
        this->input_ += 2;
        this->parse_binary_number();
        break;
      case 'o':
      case 'O':
        this->input_ += 2;
        this->parse_modern_octal_number();
        break;
      QLJS_CASE_DECIMAL_DIGIT:
        this->input_ += 1;
        this->parse_legacy_octal_number();
        break;
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
    this->last_token_.end = this->input_;
    break;

  QLJS_CASE_IDENTIFIER_START : {
    Parsed_Identifier ident =
        this->parse_identifier(this->input_, Identifier_Kind::javascript);
    this->input_ = ident.after;
    this->last_token_.normalized_identifier = ident.normalized;
    this->last_token_.end = ident.after;
    this->last_token_.type = this->identifier_token_type(ident.normalized);
    if (ident.escape_sequences && !ident.escape_sequences->empty()) {
      switch (this->last_token_.type) {
      case Token_Type::identifier:
        this->last_token_.type = Token_Type::identifier;
        break;

      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::kw_await:
      case Token_Type::kw_yield:
        // Escape sequences in identifiers prevent it from becoming a
        // contextual keyword.
        this->last_token_.type = Token_Type::identifier;
        break;

      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
        // TODO(#73): Treat 'protected', 'implements', etc. in strict mode as
        // reserved words.
        this->last_token_.type = Token_Type::identifier;
        break;

      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD:
        // Escape sequences in identifiers prevent it from becoming a reserved
        // keyword.
        this->last_token_.type =
            Token_Type::reserved_keyword_with_escape_sequence;
        this->last_token_.identifier_escape_sequences = ident.escape_sequences;
        break;

      default:
        QLJS_UNREACHABLE();
      }
    }
    break;
  }

  // Non-ASCII or control character.
  default:
    this->parse_non_ascii();
    break;

  // NOTE[one-byte-symbols]:
  case '(':
  case ')':
  case ',':
  case ':':
  case ';':
  case '@':
  case '[':
  case ']':
  case '{':
  case '}':
  case '~':
    this->last_token_.type = int_to_enum_cast<Token_Type>(*this->input_);
    this->input_ += 1;
    this->last_token_.end = this->input_;
    break;

  case '?':
    if (this->input_[1] == '?') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::question_question_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::question_question;
        this->input_ += 2;
      }
    } else if (this->input_[1] == '.') {
      if (this->is_digit(this->input_[2])) {
        // '?.3' is '?' followed by '.3'.
        this->last_token_.type = Token_Type::question;
        this->input_ += 1;
      } else {
        this->last_token_.type = Token_Type::question_dot;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::question;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '.':
    if (this->input_[1] == '.' && this->input_[2] == '.') {
      this->last_token_.type = Token_Type::dot_dot_dot;
      this->input_ += 3;
    } else if (this->is_digit(this->input_[1])) {
      this->last_token_.type = Token_Type::number;
      this->parse_number();
    } else {
      this->last_token_.type = Token_Type::dot;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '=':
    if (this->input_[1] == '=') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::equal_equal_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::equal_equal;
        this->input_ += 2;
      }
    } else if (this->input_[1] == '>') {
      this->last_token_.type = Token_Type::equal_greater;
      this->input_ += 2;
    } else {
      this->last_token_.type = Token_Type::equal;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '!':
    if (this->input_[1] == '=') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::bang_equal_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::bang_equal;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::bang;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '>':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::greater_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '>') {
      if (this->input_[2] == '>') {
        if (this->input_[3] == '=') {
          this->last_token_.type = Token_Type::greater_greater_greater_equal;
          this->input_ += 4;
        } else {
          this->last_token_.type = Token_Type::greater_greater_greater;
          this->input_ += 3;
        }
      } else if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::greater_greater_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::greater_greater;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::greater;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '+':
    if (this->input_[1] == '+') {
      this->last_token_.type = Token_Type::plus_plus;
      this->input_ += 2;
    } else if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::plus_equal;
      this->input_ += 2;
    } else {
      this->last_token_.type = Token_Type::plus;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '^':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::circumflex_equal;
      this->input_ += 2;
    } else {
      this->last_token_.type = Token_Type::circumflex;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '%':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::percent_equal;
      this->input_ += 2;
    } else {
      this->last_token_.type = Token_Type::percent;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '&':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::ampersand_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '&') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::ampersand_ampersand_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::ampersand_ampersand;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::ampersand;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '|':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::pipe_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '|') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::pipe_pipe_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::pipe_pipe;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::pipe;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '<':
    if (this->input_[1] == '!' && this->input_[2] == '-' &&
        this->input_[3] == '-') {
      this->input_ += 4;
      this->skip_line_comment_body();
      return false;
    } else if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::less_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '<') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::less_less_equal;
        this->input_ += 3;
      } else {
        this->last_token_.type = Token_Type::less_less;
        this->input_ += 2;
      }
    } else {
      this->last_token_.type = Token_Type::less;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '-':
    if (this->input_[1] == '-') {
      if (this->input_[2] == '>' && this->is_first_token_on_line()) {
        this->input_ += 3;
        this->skip_line_comment_body();
        return false;
      } else {
        this->last_token_.type = Token_Type::minus_minus;
        this->input_ += 2;
      }
    } else if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::minus_equal;
      this->input_ += 2;
    } else {
      this->last_token_.type = Token_Type::minus;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '*':
    if (this->input_[1] == '*') {
      if (this->input_[2] == '=') {
        this->last_token_.type = Token_Type::star_star_equal;
        this->input_ += 3;
      } else if (this->input_[2] == '/') {
        bool parsed_ok = this->test_for_regexp(&this->input_[2]);
        if (!parsed_ok) {
          // We saw '**/'. Emit a '*' token now. Later, we will interpret the
          // following '*/' as a comment.
          this->last_token_.type = Token_Type::star;
          this->input_ += 1;
        } else {
          this->last_token_.type = Token_Type::star_star;
          this->input_ += 2;
        }
      } else {
        this->last_token_.type = Token_Type::star_star;
        this->input_ += 2;
      }
    } else if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::star_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '/') {
      const Char8* starpos = &this->input_[0];
      bool parsed_ok = this->test_for_regexp(&this->input_[1]);

      if (!parsed_ok) {
        this->diag_reporter_->report(Diag_Unopened_Block_Comment{
            Source_Code_Span(starpos, &this->input_[2])});
        this->input_ += 2;
        this->skip_whitespace();
        return false;
      } else {
        this->last_token_.type = Token_Type::star;
        this->input_ += 1;
      }
    } else {
      this->last_token_.type = Token_Type::star;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '/':
    if (this->input_[1] == '=') {
      this->last_token_.type = Token_Type::slash_equal;
      this->input_ += 2;
    } else if (this->input_[1] == '*') {
      this->skip_block_comment();
      return false;
    } else if (this->input_[1] == '/') {
      this->input_ += 2;
      this->skip_line_comment_body();
      return false;
    } else {
      this->last_token_.type = Token_Type::slash;
      this->input_ += 1;
    }
    this->last_token_.end = this->input_;
    break;

  case '"':
  case '\'':
    this->input_ = this->parse_string_literal();
    this->last_token_.type = Token_Type::string;
    this->last_token_.end = this->input_;
    break;

  case '`': {
    this->input_ += 1;
    Parsed_Template_Body body = this->parse_template_body(
        this->input_, this->last_token_.begin, this->diag_reporter_);
    this->last_token_.template_escape_sequence_diagnostics =
        body.escape_sequence_diagnostics;
    this->last_token_.type = body.type;
    this->input_ = body.end;
    this->last_token_.end = this->input_;
    break;
  }

  case '#':
    if (this->input_[1] == '!' &&
        this->input_ == this->original_input_.data()) {
      this->input_ += 2;
      this->skip_line_comment_body();
      return false;
    } else if (this->is_initial_identifier_byte(this->input_[1])) {
      // Private identifier: #alphaNumeric
      Parsed_Identifier ident =
          this->parse_identifier(this->input_ + 1, Identifier_Kind::javascript);
      if (ident.normalized.data() == this->input_ + 1) {
        // Include the '#'.
        ident.normalized =
            String8_View(this->input_, ident.normalized.size() + 1);
      } else {
        // parse_identifier called parse_identifier_slow, and it included the
        // '#' already in normalized_name.
        QLJS_ASSERT(ident.normalized[0] == u8'#');
      }
      this->input_ = ident.after;
      this->last_token_.normalized_identifier = ident.normalized;
      this->last_token_.end = ident.after;
      this->last_token_.type = Token_Type::private_identifier;
    } else {
      this->diag_reporter_->report(Diag_Unexpected_Hash_Character{
          Source_Code_Span(&this->input_[0], &this->input_[1])});
      this->input_ += 1;
      this->skip_whitespace();
      return false;
    }
    break;

  case '\0':
    if (this->is_eof(this->input_)) {
      this->last_token_.type = Token_Type::end_of_file;
      this->last_token_.end = this->input_;
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
    const Char8* end = this->input_ + 1;
    this->diag_reporter_->report(Diag_Unexpected_Control_Character{
        .character = Source_Code_Span(this->input_, end)});
    this->input_ = end;
    this->skip_whitespace();
    return false;
  }
  }

  return true;
}

bool Lexer::test_for_regexp(const Char8* regexp_begin) {
  Lexer_Transaction transaction = this->begin_transaction();

  this->last_token_.type = Token_Type::slash;
  this->input_ = regexp_begin;
  this->last_token_.begin = this->input_;
  this->reparse_as_regexp();

  bool parsed_ok = !this->transaction_has_lex_diagnostics(transaction);
  this->roll_back_transaction(std::move(transaction));
  return parsed_ok;
}

const Char8* Lexer::parse_string_literal() {
  Char8 opening_quote = this->input_[0];

  const Char8* c = &this->input_[1];
  for (;;) {
    switch (static_cast<unsigned char>(*c)) {
    case '\0':
      if (this->is_eof(c)) {
        this->diag_reporter_->report(Diag_Unclosed_String_Literal{
            Source_Code_Span(&this->input_[0], c)});
        return c;
      } else {
        ++c;
        break;
      }

    case '\n':
    case '\r': {
      const Char8* matching_quote = nullptr;
      const Char8* current_c = c;
      if (current_c[0] == '\r' && current_c[1] == '\n') {
        current_c += 2;
      } else {
        current_c += 1;
      }
      for (;;) {
        if (*current_c == opening_quote) {
          if (matching_quote) {
            break;
          }
          matching_quote = current_c;
          ++current_c;
        } else if (*current_c == '\r' || *current_c == '\n' ||
                   (*current_c == '\0' && this->is_eof(current_c))) {
          if (matching_quote) {
            c = matching_quote + 1;
          }
          break;
        } else {
          ++current_c;
        }
      }
      this->diag_reporter_->report(
          Diag_Unclosed_String_Literal{Source_Code_Span(&this->input_[0], c)});
      return c;
    }

    case '\\': {
      const Char8* escape_sequence_start = c;
      ++c;
      switch (*c) {
      case '\0':
        if (this->is_eof(c)) {
          this->diag_reporter_->report(Diag_Unclosed_String_Literal{
              Source_Code_Span(&this->input_[0], c)});
          return c;
        } else {
          ++c;
          break;
        }
        break;
      case '\r':
        ++c;
        if (*c == '\n') {
          ++c;
        }
        break;
      case 'x':
        ++c;
        for (int i = 0; i < 2; ++i) {
          if (!is_hex_digit(*(c + i))) {
            this->diag_reporter_->report(Diag_Invalid_Hex_Escape_Sequence{
                Source_Code_Span(escape_sequence_start, c)});
            break;
          }
        }
        break;
      case 'u': {
        // TODO(#1154): Remove this temporary Diag_List.
        Diag_List diags(&this->allocator_);
        c = this->parse_unicode_escape(escape_sequence_start, &diags).end;
        this->diag_reporter_->report(diags);
        break;
      }
      default:
        ++c;
        break;
      }
      break;
    }

    case '"':
    case '\'':
      if (*c == opening_quote) {
        ++c;
        return c;
      }
      ++c;
      break;

    default:
      ++c;
      break;
    }
  }
}

const Char8* Lexer::parse_jsx_string_literal() {
  Char8 opening_quote = this->input_[0];
  const Char8* c = &this->input_[1];
  for (; *c != opening_quote; ++c) {
    if (*c == '\0' && this->is_eof(c)) {
      this->diag_reporter_->report(Diag_Unclosed_JSX_String_Literal{
          .string_literal_begin =
              Source_Code_Span(&this->input_[0], &this->input_[1]),
      });
      return c;
    }
    // Loop.
  }
  ++c;
  return c;
}

const Char8* Lexer::parse_smart_quote_string_literal(
    const Decode_UTF8_Result& opening_quote) {
  QLJS_ASSERT(opening_quote.ok);
  QLJS_ASSERT(opening_quote.code_point == left_single_quote ||
              opening_quote.code_point == right_single_quote ||
              opening_quote.code_point == left_double_quote ||
              opening_quote.code_point == right_double_quote);
  const Char8* opening_quote_begin = this->input_;
  const Char8* opening_quote_end = opening_quote_begin + opening_quote.size;

  bool is_double_quote = opening_quote.code_point == left_double_quote ||
                         opening_quote.code_point == right_double_quote;
  this->diag_reporter_->report(Diag_Invalid_Quotes_Around_String_Literal{
      .opening_quote = Source_Code_Span(opening_quote_begin, opening_quote_end),
      .suggested_quote = is_double_quote ? u8'"' : u8'\'',
  });

  static constexpr char32_t double_ending_quotes[] = {U'"', left_double_quote,
                                                      right_double_quote};
  static constexpr char32_t single_ending_quotes[] = {U'\'', left_single_quote,
                                                      right_single_quote};
  const char32_t* ending_quotes =
      is_double_quote ? double_ending_quotes : single_ending_quotes;
  auto is_ending_quote = [&](char32_t code_point) -> bool {
    static_assert(std::size(double_ending_quotes) ==
                  std::size(single_ending_quotes));
    const char32_t* end = ending_quotes + std::size(double_ending_quotes);
    return contains(ending_quotes, end, code_point);
  };

  const Char8* c = opening_quote_end;
  for (;;) {
    Decode_UTF8_Result decoded = decode_utf_8(
        Padded_String_View(c, this->original_input_.null_terminator()));
    if (decoded.ok) {
      if (is_ending_quote(decoded.code_point)) {
        return c + decoded.size;
      }
      if (this->is_newline_character(decoded.code_point)) {
        this->diag_reporter_->report(Diag_Unclosed_String_Literal{
            .string_literal = Source_Code_Span(opening_quote_begin, c),
        });
        return c;
      }
    }
    if (*c == '\0' && this->is_eof(c)) {
      this->diag_reporter_->report(Diag_Unclosed_String_Literal{
          .string_literal = Source_Code_Span(opening_quote_begin, c),
      });
      return c;
    }
    c += decoded.size;
    // Loop.
  }
}

void Lexer::skip_in_template(const Char8* template_begin) {
  QLJS_ASSERT(this->peek().type == Token_Type::right_curly);
  this->last_token_.begin = this->input_;
  Parsed_Template_Body body = this->parse_template_body(
      this->input_, template_begin, this->diag_reporter_);
  this->last_token_.type = body.type;
  this->last_token_.template_escape_sequence_diagnostics =
      body.escape_sequence_diagnostics;
  this->input_ = body.end;
  this->last_token_.end = body.end;
}

Lexer::Parsed_Template_Body Lexer::parse_template_body(
    const Char8* input, const Char8* template_begin,
    Diag_Reporter* diag_reporter) {
  Diag_List* escape_sequence_diagnostics = nullptr;
  const Char8* c = input;
  for (;;) {
    switch (*c) {
    case '\0':
      if (this->is_eof(c)) {
        diag_reporter->report(
            Diag_Unclosed_Template{Source_Code_Span(template_begin, c)});
        return Parsed_Template_Body{Token_Type::complete_template, c,
                                    escape_sequence_diagnostics};
      } else {
        ++c;
        break;
      }

    case '`':
      ++c;
      return Parsed_Template_Body{Token_Type::complete_template, c,
                                  escape_sequence_diagnostics};

    case '\\': {
      const Char8* escape_sequence_start = c;
      ++c;
      switch (*c) {
      case '\0':
        if (this->is_eof(c)) {
          diag_reporter->report(
              Diag_Unclosed_Template{Source_Code_Span(template_begin, c)});
          return Parsed_Template_Body{Token_Type::complete_template, c,
                                      escape_sequence_diagnostics};
        } else {
          ++c;
          break;
        }
      case 'u': {
        if (!escape_sequence_diagnostics) {
          escape_sequence_diagnostics =
              this->allocator_.new_object<Diag_List>(&this->allocator_);
        }
        c = this->parse_unicode_escape(escape_sequence_start,
                                       escape_sequence_diagnostics)
                .end;
        break;
      }
      default:
        ++c;
        break;
      }
      break;
    }

    case '$':
      if (c[1] == '{') {
        c += 2;
        return Parsed_Template_Body{Token_Type::incomplete_template, c,
                                    escape_sequence_diagnostics};
      }
      ++c;
      break;

    default:
      ++c;
      break;
    }
  }
}

void Lexer::skip_in_jsx() {
  this->last_last_token_end_ = this->last_token_.end;
  this->last_token_.has_leading_newline = false;
  this->last_token_.has_leading_comment = false;
  this->skip_whitespace();

retry:
  this->last_token_.begin = this->input_;
  switch (this->input_[0]) {
  QLJS_CASE_IDENTIFIER_START:
  QLJS_CASE_NON_ASCII : {
    Parsed_Identifier ident =
        this->parse_identifier(this->input_, Identifier_Kind::jsx);
    this->input_ = ident.after;
    this->last_token_.normalized_identifier = ident.normalized;
    this->last_token_.end = ident.after;
    this->last_token_.type = Token_Type::identifier;
    break;
  }

  case '>':
    this->last_token_.type = Token_Type::greater;
    this->input_ += 1;
    this->last_token_.end = this->input_;
    break;

  case '"':
  case '\'':
    this->input_ = this->parse_jsx_string_literal();
    this->last_token_.type = Token_Type::string;
    this->last_token_.end = this->input_;
    break;

  default:
    if (!this->try_parse_current_token()) {
      goto retry;
    }
    break;
  }
}

void Lexer::skip_in_jsx_children() {
  QLJS_ASSERT(this->peek().type == Token_Type::right_curly ||
              this->peek().type == Token_Type::greater);
  this->skip_jsx_text();
  this->skip_in_jsx();
}

const Char8* Lexer::find_equal_greater_in_jsx_children() const {
  const Char8* c;
  for (c = this->input_;; ++c) {
    switch (*c) {
    case u8'<':
    case u8'{':
    case u8'}':
      return nullptr;

    case u8'>':
      if (c[-1] == u8'=') {
        return &c[-1];
      }
      return nullptr;

    case u8'\0':
      if (this->is_eof(c)) {
        return nullptr;
      }
      break;

    default:
      break;
    }
  }
}

void Lexer::skip_less_less_as_less() {
  QLJS_ASSERT(this->peek().type == Token_Type::less_less);
  this->last_token_.has_leading_newline = false;
  this->last_token_.has_leading_comment = false;
  this->last_token_.type = Token_Type::less;
  this->last_token_.begin += 1;
  this->last_last_token_end_ = this->last_token_.begin;
}

void Lexer::skip_as_greater() {
  switch (this->last_token_.type) {
  case Token_Type::greater_equal:
    this->last_token_.type = Token_Type::equal;
    break;
  case Token_Type::greater_greater_equal:
    this->last_token_.type = Token_Type::greater_equal;
    break;
  case Token_Type::greater_greater_greater_equal:
    this->last_token_.type = Token_Type::greater_greater_equal;
    break;
  case Token_Type::greater_greater:
    this->last_token_.type = Token_Type::greater;
    break;
  case Token_Type::greater_greater_greater:
    this->last_token_.type = Token_Type::greater_greater;
    break;
  default:
    QLJS_UNREACHABLE();
    break;
  }
  this->last_token_.has_leading_newline = false;
  this->last_token_.begin += 1;
  this->last_last_token_end_ = this->last_token_.begin;
}

void Lexer::reparse_as_regexp() {
  QLJS_ASSERT(this->last_token_.type == Token_Type::slash ||
              this->last_token_.type == Token_Type::slash_equal);

  this->input_ = this->last_token_.begin;
  QLJS_ASSERT(this->input_[0] == '/');
  this->last_token_.type = Token_Type::regexp;

  const Char8* c = &this->input_[1];
next:
  switch (*c) {
  case '\0':
    if (this->is_eof(c)) {
      this->diag_reporter_->report(Diag_Unclosed_Regexp_Literal{
          Source_Code_Span(this->last_token_.begin, c)});
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
        this->diag_reporter_->report(Diag_Unclosed_Regexp_Literal{
            Source_Code_Span(this->last_token_.begin, c)});
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
        if (c[1] == u8']' || c[1] == u8'\\') {
          c += 2;
        } else {
          c += 1;
        }
        break;

      QLJS_CASE_NEWLINE_START:
        if (this->newline_character_size(c) != 0) {
          goto next;
        }
        [[fallthrough]];
      default:
        ++c;
        break;
      }
    }
    QLJS_UNREACHABLE();

  case '/': {
    ++c;
    // TODO(strager): Is the check for '\\' correct?
    if (this->is_identifier_byte(*c) || *c == u8'\\') {
      Parsed_Identifier ident =
          this->parse_identifier(c, Identifier_Kind::javascript);
      c = ident.after;
      if (ident.escape_sequences) {
        for (const Source_Code_Span& escape_sequence :
             *ident.escape_sequences) {
          this->diag_reporter_->report(
              Diag_Regexp_Literal_Flags_Cannot_Contain_Unicode_Escapes{
                  .escape_sequence = escape_sequence});
        }
      }
    }
    break;
  }

  QLJS_CASE_NEWLINE_START:
    if (this->newline_character_size(c) != 0) {
      this->diag_reporter_->report(Diag_Unclosed_Regexp_Literal{
          Source_Code_Span(this->last_token_.begin, c)});
      break;
    }
    [[fallthrough]];
  default:
    ++c;
    goto next;
  }

  this->input_ = c;
  this->last_token_.end = this->input_;
}

Lexer_Transaction Lexer::begin_transaction() {
  return Lexer_Transaction(
      /*old_last_token=*/this->last_token_,
      /*old_last_last_token_end=*/this->last_last_token_end_,
      /*old_input=*/this->input_,
      /*diag_reporter_pointer=*/
      &this->diag_reporter_,
      /*memory=*/&this->transaction_allocator_);
}

void Lexer::commit_transaction(Lexer_Transaction&& transaction) {
  Buffering_Diag_Reporter* buffered_diagnostics =
      derived_cast<Buffering_Diag_Reporter*>(this->diag_reporter_);
  buffered_diagnostics->move_into(transaction.old_diag_reporter);

  this->diag_reporter_ = transaction.old_diag_reporter;

  transaction.reporter.reset();
}

void Lexer::roll_back_transaction(Lexer_Transaction&& transaction) {
  this->last_token_ = transaction.old_last_token;
  this->last_last_token_end_ = transaction.old_last_last_token_end;
  this->input_ = transaction.old_input;
  this->diag_reporter_ = transaction.old_diag_reporter;

  transaction.reporter.reset();
  this->transaction_allocator_.rewind(std::move(transaction.allocator_rewind));
}

bool Lexer::transaction_has_lex_diagnostics(const Lexer_Transaction&) const {
  Buffering_Diag_Reporter* buffered_diagnostics =
      derived_cast<Buffering_Diag_Reporter*>(this->diag_reporter_);
  return !buffered_diagnostics->empty();
}

void Lexer::insert_semicolon() {
  QLJS_ASSERT(this->last_last_token_end_ != nullptr);
  this->input_ = this->last_last_token_end_;

  this->last_token_.type = Token_Type::semicolon;
  this->last_token_.has_leading_newline = false;
  this->last_token_.has_leading_comment = false;
  this->last_token_.begin = this->input_;
  this->last_token_.end = this->input_;
}

const Char8* Lexer::end_of_previous_token() const {
  bool semicolon_was_inserted =
      this->last_token_.type == Token_Type::semicolon &&
      this->last_token_.begin == this->last_token_.end;
  QLJS_ASSERT(!semicolon_was_inserted);

  return this->last_last_token_end_;
}

Padded_String_View Lexer::original_input() const {
  return this->original_input_;
}

void Lexer::debug_dump_location() const {
  this->debug_dump_location(this->input_);
}

void Lexer::debug_dump_location(const Char8* c) const {
  CLI_Locator locator(this->original_input_);
  CLI_Source_Position token_position = locator.position(this->peek().begin);
  std::fprintf(stderr, "%p: file offset %zd, line %d, column %d\n",
               reinterpret_cast<const void*>(c),
               c - this->original_input_.data(), token_position.line_number,
               token_position.column_number);
}

template <class Error>
const Char8* Lexer::check_garbage_in_number_literal(const Char8* input) {
  const Char8* garbage_begin = input;
  for (;;) {
    switch (*input) {
    // 0xffffq  // Invalid.
    // 0b0123   // Invalid.
    QLJS_CASE_DECIMAL_DIGIT:
    QLJS_CASE_IDENTIFIER_START:
      input += 1;
      break;

    // 0b0000.toString()
    // 0b0000.2  // Invalid.
    case u8'.':
      if (this->is_digit(input[1])) {
        // 0b0000.2  // Invalid.
        input += 2;
        break;
      }

      // 0b0000.toString()
      // 0b0000. 2          // Invalid.
      goto done_parsing_garbage;

    default:
      goto done_parsing_garbage;
    }
  }

done_parsing_garbage:
  const Char8* garbage_end = input;
  if (garbage_end != garbage_begin) {
    this->diag_reporter_->report(
        Error{Source_Code_Span(garbage_begin, garbage_end)});
    input = garbage_end;
  }

  return input;
}

void Lexer::parse_binary_number() {
  const Char8* input = this->input_;

  input = this->parse_digits_and_underscores(
      [](Char8 character) -> bool { return is_binary_digit(character); },
      input);
  bool found_digits = input != this->input_;
  if (*input == u8'n') {
    ++input;
  }

  if (found_digits) {
    this->input_ = check_garbage_in_number_literal<
        Diag_Unexpected_Characters_In_Binary_Number>(input);
  } else {
    this->diag_reporter_->report(Diag_No_Digits_In_Binary_Number{
        Source_Code_Span(this->last_token_.begin, input)});
    this->input_ = input;
  }
}

void Lexer::parse_legacy_octal_number() {
  const Char8* input = this->input_;

parse_digits_again:
  input = this->parse_octal_digits(input);
  if (*input == u8'_') {
    const Char8* underscores_start = input;
    while (*input == '_') {
      input += 1;
    }
    this->diag_reporter_->report(
        Diag_Legacy_Octal_Literal_May_Not_Contain_Underscores{
            .underscores = Source_Code_Span(underscores_start, input),
        });
    goto parse_digits_again;
  }

  if (is_digit(*input)) {
    this->input_ = input;
    this->parse_number();
    return;
  }

  const Char8* garbage_begin = input;
  bool has_decimal_point = *input == '.' && this->is_digit(input[1]);
  if (has_decimal_point) {
    input += 1;
    this->diag_reporter_->report(Diag_Octal_Literal_May_Not_Have_Decimal{
        Source_Code_Span(garbage_begin, input)});
    input = this->parse_octal_digits(input);
  }
  bool has_exponent = *input == 'e' || *input == 'E';
  if (has_exponent) {
    input += 1;
    if (*input == '-' || *input == '+') {
      input += 1;
    }
    this->diag_reporter_->report(Diag_Octal_Literal_May_Not_Have_Exponent{
        Source_Code_Span(garbage_begin, input)});
    input = this->parse_octal_digits(input);
  }
  bool is_bigint = *input == 'n';
  if (is_bigint) {
    input += 1;
    this->diag_reporter_->report(Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int{
        Source_Code_Span(garbage_begin, input)});
    input = this->parse_octal_digits(input);
  }

  this->input_ = check_garbage_in_number_literal<
      Diag_Unexpected_Characters_In_Octal_Number>(input);
}

void Lexer::parse_modern_octal_number() {
  // TODO(strager): Why does this look different from parse_binary_number and
  // parse_hexadecimal_number? We should probably make them look the same and
  // factor the common structure.

  const Char8* input = this->input_;
  input = this->parse_digits_and_underscores(
      [](Char8 character) -> bool { return is_octal_digit(character); }, input);
  if (input == this->input_) {
    this->diag_reporter_->report(Diag_No_Digits_In_Octal_Number{
        Source_Code_Span(this->last_token_.begin, input)});
    return;
  }
  if (*input == u8'n') {
    ++input;
  }
  this->input_ = check_garbage_in_number_literal<
      Diag_Unexpected_Characters_In_Octal_Number>(input);
}

void Lexer::check_integer_precision_loss(String8_View number_literal) {
  // Any integer which is 15 or fewer digits is guaranteed to be able to be
  // represented accurately without precision loss. This is because Numbers have
  // 53 bits of precision, which is equal to 53 log10(2) ≈ 15.955 decimal digits
  // of precision.
  const size_t GUARANTEED_ACC_LENGTH = 15;
  // There is no integer which can be represented accurately that is greater
  // than 309 digits long. This is because the largest representable Number is
  // equal to 2^1023 × (1 + (1 − 2^−52)) ≈ 1.7976931348623157 × 10^308, which is
  // 309 digits long.
  const size_t MAX_ACC_LENGTH = 309;
  if (number_literal.size() <= GUARANTEED_ACC_LENGTH) {
    return;
  }
  std::string cleaned_string = "";
  for (Char8 c : number_literal) {
    if (c != '_') {
      cleaned_string.push_back(static_cast<char>(c));
    }
  }
  if (cleaned_string.size() <= GUARANTEED_ACC_LENGTH) {
    return;
  }
  if (cleaned_string.size() > MAX_ACC_LENGTH) {
    this->diag_reporter_->report(Diag_Integer_Literal_Will_Lose_Precision{
        .characters =
            Source_Code_Span(number_literal.data(),
                             number_literal.data() + number_literal.size()),
        .rounded_val = u8"inf"_sv,
    });
    return;
  }
  double num = strtod(cleaned_string.c_str(), nullptr);
  std::array<char, MAX_ACC_LENGTH + 1> result_string;
  int rc =
      std::snprintf(result_string.data(), result_string.size(), "%.0f", num);
  QLJS_ALWAYS_ASSERT(rc >= 0);
  QLJS_ALWAYS_ASSERT(static_cast<size_t>(rc) < result_string.size());
  std::string_view result_string_view(result_string.data(),
                                      static_cast<size_t>(rc));
  if (cleaned_string != result_string_view) {
    // TODO(strager): Use Linked_Bump_Allocator::new_objects_copy
    Span<Char8> rounded_val =
        this->allocator_.allocate_uninitialized_span<Char8>(
            result_string_view.size());
    std::uninitialized_copy(result_string_view.begin(),
                            result_string_view.end(), rounded_val.data());
    String8_View rounded_val_string_view = String8_View(
        rounded_val.data(), narrow_cast<std::size_t>(rounded_val.size()));
    this->diag_reporter_->report(Diag_Integer_Literal_Will_Lose_Precision{
        .characters =
            Source_Code_Span(number_literal.data(),
                             number_literal.data() + number_literal.size()),
        .rounded_val = rounded_val_string_view,
    });
  }
}

void Lexer::parse_number() {
  QLJS_SLOW_ASSERT(this->is_digit(this->input_[0]) || this->input_[0] == '.');
  const Char8* input = this->input_;
  const Char8* number_begin = input;

  auto consume_garbage = [this, &input]() {
    const Char8* garbage_begin = input;
    const Char8* garbage_end =
        this->parse_identifier(garbage_begin, Identifier_Kind::javascript)
            .after;
    this->diag_reporter_->report(Diag_Unexpected_Characters_In_Number{
        Source_Code_Span(garbage_begin, garbage_end)});
    input = garbage_end;
  };

  input = this->parse_decimal_digits_and_underscores(input);
  bool has_decimal_point = *input == '.';
  if (has_decimal_point) {
    input += 1;
    input = this->parse_decimal_digits_and_underscores(input);
  }
  bool has_exponent = *input == 'e' || *input == 'E';
  if (has_exponent) {
    const Char8* e = input;
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
  bool is_bigint = *input == 'n';
  if (is_bigint) {
    input += 1;
    if (has_decimal_point) {
      this->diag_reporter_->report(Diag_Big_Int_Literal_Contains_Decimal_Point{
          Source_Code_Span(number_begin, input)});
    }
    if (has_exponent) {
      this->diag_reporter_->report(Diag_Big_Int_Literal_Contains_Exponent{
          Source_Code_Span(number_begin, input)});
    }
    QLJS_SLOW_ASSERT(
        !(number_begin[0] == u8'0' && this->is_digit(number_begin[1])));
  }
  if (!has_decimal_point && !has_exponent && !is_bigint) {
    check_integer_precision_loss(make_string_view(number_begin, input));
  }

  switch (*input) {
  QLJS_CASE_IDENTIFIER_START:
    consume_garbage();
    break;
  }
  this->input_ = input;
}

void Lexer::parse_hexadecimal_number() {
  const Char8* input = this->input_;

  input = parse_hex_digits_and_underscores(input);
  bool found_digits = input != this->input_;
  bool is_bigint = *input == u8'n';
  if (is_bigint) {
    ++input;
  }

  if (found_digits) {
    this->input_ = check_garbage_in_number_literal<
        Diag_Unexpected_Characters_In_Hex_Number>(input);
  } else {
    this->diag_reporter_->report(Diag_No_Digits_In_Hex_Number{
        Source_Code_Span(this->last_token_.begin, input)});
    this->input_ = input;
  }
}

template <class Func>
const Char8* Lexer::parse_digits_and_underscores(Func&& is_valid_digit,
                                                 const Char8* input) {
  bool has_trailing_underscore = false;
  const Char8* garbage_begin = nullptr;
  while (is_valid_digit(*input)) {
    has_trailing_underscore = false;
    input += 1;
    if (*input == '_') {
      garbage_begin = input;
      has_trailing_underscore = true;
      input += 1;
      if (*input == '_') {
        has_trailing_underscore = false;

        while (*input == '_') {
          input += 1;
        }

        if (is_valid_digit(*input)) {
          this->diag_reporter_->report(
              Diag_Number_Literal_Contains_Consecutive_Underscores{
                  Source_Code_Span(garbage_begin, input)});
        } else {
          this->diag_reporter_->report(
              Diag_Number_Literal_Contains_Trailing_Underscores{
                  Source_Code_Span(garbage_begin, input)});
        }
      }
    }
  }
  if (garbage_begin != nullptr && has_trailing_underscore == true) {
    this->diag_reporter_->report(
        Diag_Number_Literal_Contains_Trailing_Underscores{
            Source_Code_Span(garbage_begin, input)});
  }
  return input;
}

const Char8* Lexer::parse_octal_digits(const Char8* input) {
  while (this->is_octal_digit(*input)) {
    ++input;
  }
  return input;
}

const Char8* Lexer::parse_decimal_digits_and_underscores(const Char8* input) {
  return this->parse_digits_and_underscores(
      [](Char8 character) -> bool { return is_digit(character); }, input);
}

const Char8* Lexer::parse_hex_digits_and_underscores(const Char8* input) {
  return this->parse_digits_and_underscores(
      [](Char8 character) -> bool { return is_hex_digit(character); }, input);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
Lexer::Parsed_Unicode_Escape Lexer::parse_unicode_escape(const Char8* input,
                                                         Diag_List* out_diags) {
  const Char8* escape_sequence_begin = input;
  auto get_escape_span = [escape_sequence_begin, &input]() {
    return Source_Code_Span(escape_sequence_begin, input);
  };

  const Char8* code_point_hex_begin;
  const Char8* code_point_hex_end;
  if (input[2] == u8'{') {
    code_point_hex_begin = &input[3];
    input += 3;  // Skip "\u{".
    bool found_non_hex_digit = false;
    while (*input != u8'}') {
      if (!this->is_identifier_byte(*input)) {
        // TODO: Add an enum to Diag_Unclosed_Identifier_Escape_Sequence to
        // indicate whether the token is a template literal, a string literal
        // or an identifier.
        out_diags->add(Diag_Unclosed_Identifier_Escape_Sequence{
            .escape_sequence = get_escape_span()});
        return Parsed_Unicode_Escape{.end = input, .code_point = std::nullopt};
      }
      if (!is_hex_digit(*input)) {
        found_non_hex_digit = true;
      }
      ++input;
    }
    code_point_hex_end = input;
    ++input;  // Skip "}".
    if (found_non_hex_digit || code_point_hex_begin == code_point_hex_end) {
      out_diags->add(Diag_Expected_Hex_Digits_In_Unicode_Escape{
          .escape_sequence = get_escape_span()});
      return Parsed_Unicode_Escape{.end = input, .code_point = std::nullopt};
    }
  } else {
    input += 2;  // Skip "\u".
    code_point_hex_begin = input;
    for (int i = 0; i < 4; ++i) {
      if (*input == '\0' && this->is_eof(input)) {
        // TODO: Add an enum to Diag_Expected_Hex_Digits_In_Unicode_Escape to
        // indicate whether the token is a template literal, a string literal
        // or an identifier.
        out_diags->add(Diag_Expected_Hex_Digits_In_Unicode_Escape{
            .escape_sequence = get_escape_span()});
        return Parsed_Unicode_Escape{.end = input, .code_point = std::nullopt};
      }
      if (!is_hex_digit(*input)) {
        out_diags->add(Diag_Expected_Hex_Digits_In_Unicode_Escape{
            .escape_sequence =
                Source_Code_Span(escape_sequence_begin, input + 1)});
        return Parsed_Unicode_Escape{.end = input, .code_point = std::nullopt};
      }
      ++input;
    }
    code_point_hex_end = input;
  }
  char32_t code_point;
  switch (parse_integer_exact_hex(
      make_string_view(code_point_hex_begin, code_point_hex_end), code_point)) {
  case Parse_Integer_Exact_Error::ok:
    break;
  case Parse_Integer_Exact_Error::out_of_range:
    code_point = 0x110000;
    break;
  case Parse_Integer_Exact_Error::invalid:
    QLJS_UNREACHABLE();
    break;
  }
  if (code_point >= 0x110000) {
    out_diags->add(Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range{
        .escape_sequence = get_escape_span()});
  }
  return Parsed_Unicode_Escape{.end = input, .code_point = code_point};
}
QLJS_WARNING_POP

Lexer::Parsed_Identifier Lexer::parse_identifier(const Char8* input,
                                                 Identifier_Kind kind) {
  const Char8* begin = input;
  const Char8* end = this->parse_identifier_fast_only(input);
  if (*end == u8'\\' || (kind == Identifier_Kind::jsx && *end == u8'-') ||
      !this->is_ascii_character(*end)) {
    return this->parse_identifier_slow(end,
                                       /*identifier_begin=*/begin, kind);
  } else {
    return Parsed_Identifier{
        .after = end,
        .normalized = make_string_view(begin, end),
        .escape_sequences = {},
    };
  }
}

const Char8* Lexer::parse_identifier_fast_only(const Char8* input) {
  // TODO(strager): Is the check for '\\' correct?
  QLJS_SLOW_ASSERT(this->is_identifier_byte(*input) || *input == u8'\\');

#if QLJS_HAVE_ARM_NEON
  using Char_Vector = Char_Vector_16_NEON;
#elif QLJS_HAVE_WEB_ASSEMBLY_SIMD128
  using Char_Vector = Char_Vector_16_WASM_SIMD128;
#elif QLJS_HAVE_X86_SSE2
  using Char_Vector = Char_Vector_16_SSE2;
#else
  using Char_Vector = Char_Vector_1;
#endif

  auto count_identifier_characters = [](Char_Vector chars) -> int {
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
#if QLJS_HAVE_ARM_NEON
    using Bool_Vector = Bool_Vector_16_NEON;
#elif QLJS_HAVE_WEB_ASSEMBLY_SIMD128
    using Bool_Vector = Bool_Vector_16_WASM_SIMD128;
#elif QLJS_HAVE_X86_SSE2
    using Bool_Vector = Bool_Vector_16_SSE2;
#else
    using Bool_Vector = Bool_Vector_1;
#endif

    constexpr std::uint8_t upper_to_lower_mask = u8'a' - u8'A';
    static_assert((u8'A' | upper_to_lower_mask) == u8'a');

    Char_Vector lower_cased_characters =
        chars | Char_Vector::repeated(upper_to_lower_mask);
    Bool_Vector is_alpha =
        (lower_cased_characters > Char_Vector::repeated(u8'a' - 1)) &
        (lower_cased_characters < Char_Vector::repeated(u8'z' + 1));
    Bool_Vector is_digit = (chars > Char_Vector::repeated(u8'0' - 1)) &
                           (chars < Char_Vector::repeated(u8'9' + 1));
    Bool_Vector is_identifier = is_alpha | is_digit |  //
                                (chars == Char_Vector::repeated(u8'$')) |
                                (chars == Char_Vector::repeated(u8'_'));
    return is_identifier.find_first_false();
#endif
  };

  bool is_all_identifier_characters;
  do {
    Char_Vector chars = Char_Vector::load(input);
    int identifier_character_count = count_identifier_characters(chars);

    for (int i = 0; i < identifier_character_count; ++i) {
      QLJS_SLOW_ASSERT(input[i] >= 0);
      QLJS_SLOW_ASSERT(this->is_ascii_character(input[i]));
      QLJS_SLOW_ASSERT(is_identifier_character(static_cast<char32_t>(input[i]),
                                               Identifier_Kind::javascript));
    }
    input += identifier_character_count;

    is_all_identifier_characters = identifier_character_count == chars.size;
  } while (is_all_identifier_characters);

  return input;
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
Lexer::Parsed_Identifier Lexer::parse_identifier_slow(
    const Char8* input, const Char8* identifier_begin, Identifier_Kind kind) {
  bool is_private_identifier =
      identifier_begin != this->original_input_.data() &&
      identifier_begin[-1] == u8'#';
  const Char8* private_identifier_begin =
      is_private_identifier ? &identifier_begin[-1] : identifier_begin;

  Vector<Char8> normalized("parse_identifier_slow normalized",
                           &this->allocator_);
  normalized.append(private_identifier_begin, input);

  Escape_Sequence_List* escape_sequences =
      this->allocator_.new_object<Escape_Sequence_List>(
          "parse_identifier_slow escape_sequences", &this->allocator_);

  auto parse_unicode_escape = [&]() {
    const Char8* escape_begin = input;
    // TODO(#1154): Remove this temporary Diag_List.
    Diag_List diags(&this->allocator_);
    Parsed_Unicode_Escape escape =
        this->parse_unicode_escape(escape_begin, &diags);
    this->diag_reporter_->report(diags);

    if (escape.code_point.has_value()) {
      bool is_initial_identifier_character = escape_begin == identifier_begin;
      char32_t code_point = *escape.code_point;
      if (code_point >= 0x110000) {
        // parse_unicode_escape reported
        // Diag_Escaped_Code_Point_In_Identifier_Out_Of_Range already.
        normalized.append(escape_begin, escape.end);
      } else if (!is_initial_identifier_character &&
                 kind == Identifier_Kind::jsx && code_point == u'-') {
        this->diag_reporter_->report(Diag_Escaped_Hyphen_Not_Allowed_In_JSX_Tag{
            .escape_sequence = Source_Code_Span(escape_begin, escape.end)});
        normalized.append(escape_begin, escape.end);
      } else if (!(is_initial_identifier_character
                       ? this->is_initial_identifier_character(code_point)
                       : this->is_identifier_character(
                             code_point, Identifier_Kind::javascript))) {
        this->diag_reporter_->report(
            Diag_Escaped_Character_Disallowed_In_Identifiers{
                .escape_sequence = Source_Code_Span(escape_begin, escape.end)});
        normalized.append(escape_begin, escape.end);
      } else {
        normalized.append(4, u8'\0');
        const Char8* end =
            encode_utf_8(code_point, &normalized.data()[normalized.size() - 4]);
        normalized.resize(narrow_cast<Vector_Size>(end - normalized.data()));
        escape_sequences->emplace_back(escape_begin, escape.end);
      }
    } else {
      normalized.append(escape_begin, escape.end);
    }

    QLJS_ASSERT(input != escape.end);
    input = escape.end;
  };

  for (;;) {
    Decode_UTF8_Result decode_result = decode_utf_8(
        Padded_String_View(input, this->original_input_.null_terminator()));
    if (decode_result.size == 0) {
      QLJS_ASSERT(this->is_eof(input));
      break;
    }
    if (!decode_result.ok) {
      const Char8* errors_begin = input;
      input += decode_result.size;
      for (;;) {
        decode_result = decode_utf_8(
            Padded_String_View(input, this->original_input_.null_terminator()));
        if (decode_result.ok || decode_result.size == 0) {
          break;
        }
        input += decode_result.size;
      }
      this->diag_reporter_->report(Diag_Invalid_Utf_8_Sequence{
          .sequence = Source_Code_Span(errors_begin, input)});
      normalized.append(errors_begin, input);
      continue;
    }

    if (*input == u8'\\') {
      if (input[1] == u8'u') {
        parse_unicode_escape();
      } else {
        const Char8* backslash_begin = input;
        input += 1;
        const Char8* backslash_end = input;
        this->diag_reporter_->report(Diag_Unexpected_Backslash_In_Identifier{
            .backslash = Source_Code_Span(backslash_begin, backslash_end)});
        normalized.append(backslash_begin, backslash_end);
      }
    } else {
      QLJS_ASSERT(decode_result.size >= 1);
      const Char8* character_begin = input;
      const Char8* character_end = input + decode_result.size;
      char32_t code_point = decode_result.code_point;

      bool is_initial_identifier_character =
          character_begin == identifier_begin;
      bool is_legal_character =
          is_initial_identifier_character
              ? this->is_initial_identifier_character(code_point)
              : this->is_identifier_character(code_point, kind);
      if (!is_legal_character) {
        if (this->is_ascii_character(code_point) ||
            this->is_non_ascii_whitespace_character(code_point)) {
          break;
        } else {
          this->diag_reporter_->report(Diag_Character_Disallowed_In_Identifiers{
              .character = Source_Code_Span(character_begin, character_end)});
          // Allow non-ASCII characters in the identifier. Otherwise, we'd try
          // parsing the invalid character as an identifier character again,
          // causing an infinite loop.
        }
      }

      normalized.append(character_begin, character_end);
      input = character_end;
    }
  }

  return Parsed_Identifier{
      .after = input,
      .normalized = normalized.release_to_string_view(),
      .escape_sequences = escape_sequences,
  };
}
QLJS_WARNING_POP

void Lexer::parse_non_ascii() {
  Decode_UTF8_Result character = decode_utf_8(Padded_String_View(
      this->input_, this->original_input_.null_terminator()));
  if (character.code_point == left_single_quote ||
      character.code_point == right_single_quote ||
      character.code_point == left_double_quote ||
      character.code_point == right_double_quote) {
    this->input_ = this->parse_smart_quote_string_literal(character);
    this->last_token_.type = Token_Type::string;
    this->last_token_.end = this->input_;
  } else {
    Parsed_Identifier ident = this->parse_identifier_slow(
        this->input_, this->input_, Identifier_Kind::javascript);
    this->input_ = ident.after;
    this->last_token_.normalized_identifier = ident.normalized;
    this->last_token_.end = ident.after;
    this->last_token_.type = Token_Type::identifier;
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wunknown-attributes")
QLJS_WARNING_IGNORE_CLANG("-Wunreachable-code")
QLJS_WARNING_IGNORE_GCC("-Wattributes")
void Lexer::skip_whitespace() {
  const Char8* input = this->input_;

next:
  Char8 c = input[0];
  unsigned char c0 = static_cast<unsigned char>(input[0]);
  unsigned char c1 = static_cast<unsigned char>(input[1]);
  unsigned char c2 = static_cast<unsigned char>(input[2]);
  if (c == ' ' || c == '\t' || c == '\f' || c == '\v') {
    input += 1;
    goto next;
  } else if (c == '\n' || c == '\r') {
    this->last_token_.has_leading_newline = true;
    input += 1;
    goto next;
  } else if (c0 >= 0xc2) {
    [[unlikely]] switch (c0) {
    case 0xe1:
      if (c1 == 0x9a && c2 == 0x80) {
        // U+1680 Ogham Space Mark
        input += 3;
        goto next;
      } else {
        goto done;
      }

    case 0xe2:
      if (c1 == 0x80) {
        switch (c2) {
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
      } else if (c1 == 0x81) {
        if (c2 == 0x9f) {
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
      if (c1 == 0x80 && c2 == 0x80) {
        // U+3000 Ideographic Space
        input += 3;
        goto next;
      } else {
        goto done;
      }

    case 0xef:
      if (c1 == 0xbb && c2 == 0xbf) {
        // U+FEFF Zero Width No-Break Space (BOM, ZWNBSP)
        input += 3;
        goto next;
      } else {
        goto done;
      }

    case 0xc2:
      if (c1 == 0xa0) {
        // U+00A0 No-Break Space (NBSP)
        input += 2;
        goto next;
      } else if (c1 == 0x85) {
        // U+0085 Next Line (NEL)
        if (this->options_.typescript) {
          input += 2;
          goto next;
        } else {
          goto done;
        }
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

void Lexer::skip_block_comment() {
  QLJS_SLOW_ASSERT(this->input_[0] == '/' && this->input_[1] == '*');
  const Char8* c = this->input_ + 2;
  this->last_token_.has_leading_comment = true;
#if QLJS_HAVE_X86_SSE2
  using Bool_Vector = Bool_Vector_16_SSE2;
  using Char_Vector = Char_Vector_16_SSE2;
#else
  using Bool_Vector = Bool_Vector_1;
  using Char_Vector = Char_Vector_1;
#endif

  auto is_comment_end = [](const Char8* string) -> bool {
    return string[0] == '*' && string[1] == '/';
  };

  for (;;) {
    Char_Vector chars = Char_Vector::load(c);
    Bool_Vector matches =
        (chars == Char_Vector::repeated(u8'*')) |
        (chars == Char_Vector::repeated(u8'\0')) |
        (chars == Char_Vector::repeated(u8'\n')) |
        (chars == Char_Vector::repeated(u8'\r')) |
        (chars == Char_Vector::repeated(static_cast<std::uint8_t>(
                      line_separator_paragraph_separator_first_byte)));
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
    Char_Vector chars = Char_Vector::load(c);
    Bool_Vector matches = (chars == Char_Vector::repeated(u8'\0')) |
                          (chars == Char_Vector::repeated(u8'*'));
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
  this->diag_reporter_->report(Diag_Unclosed_Block_Comment{
      .comment_open = Source_Code_Span(&this->input_[0], &this->input_[2])});
  this->input_ = this->original_input_.null_terminator();
}

void Lexer::skip_line_comment_body() {
#if QLJS_HAVE_ARM_NEON
  using Bool_Vector = Bool_Vector_16_NEON;
  using Char_Vector = Char_Vector_16_NEON;
#elif QLJS_HAVE_WEB_ASSEMBLY_SIMD128
  using Bool_Vector = Bool_Vector_16_WASM_SIMD128;
  using Char_Vector = Char_Vector_16_WASM_SIMD128;
#elif QLJS_HAVE_X86_SSE2
  using Bool_Vector = Bool_Vector_16_SSE2;
  using Char_Vector = Char_Vector_16_SSE2;
#else
  using Bool_Vector = Bool_Vector_1;
  using Char_Vector = Char_Vector_1;
#endif

  this->last_token_.has_leading_comment = true;
  auto found_comment_end = [&]() {
    int n = newline_character_size(this->input_);

    if (n == 1) {
      this->input_ += 1;
      this->skip_whitespace();
      return true;
    }
    // U+2028 Line Separator
    // U+2029 Paragraph Separator
    if (n == 3) {
      this->input_ += 3;
      this->skip_whitespace();
      return true;
    }
    if (*this->input_ == u8'\0' && this->is_eof(this->input_)) {
      return true;
    }

    this->input_ += 1;
    return false;
  };

  Char_Vector newLine = Char_Vector::repeated('\n');
  Char_Vector carriageReturn = Char_Vector::repeated('\r');
  Char_Vector unicodeFirstByte = Char_Vector::repeated(0xe2);  // U+2028 U+2029
  Char_Vector zero = Char_Vector::repeated(0);

  while (true) {
    Char_Vector chars = Char_Vector::load(this->input_);

    Bool_Vector matches = (chars == newLine) | (chars == carriageReturn) |
                          (chars == unicodeFirstByte) | (chars == zero);

    std::uint32_t mask = matches.mask();
    if (mask == 0) {
      // nothing found, go to the next chunk
      this->input_ += Char_Vector::size;
    } else {
      // found an interesting char
      this->input_ += countr_zero(mask);
      if (found_comment_end()) {
        break;
      }
    }
  }

  this->last_token_.has_leading_newline = true;
}

void Lexer::skip_jsx_text() {
  const Char8* c;
  for (c = this->input_;; ++c) {
    switch (*c) {
    case u8'{':
    case u8'<':
      goto done;

    case u8'>':
      this->diag_reporter_->report(Diag_Unexpected_Greater_In_JSX_Text{
          .greater = Source_Code_Span(c, c + 1),
      });
      break;

    case u8'}':
      this->diag_reporter_->report(Diag_Unexpected_Right_Curly_In_JSX_Text{
          .right_curly = Source_Code_Span(c, c + 1),
      });
      break;

    case u8'\0':
      if (this->is_eof(c)) {
        goto done;
      }
      break;

    default:
      break;
    }
  }
done:
  // TODO(strager): Should we set has_leading_newline?
  this->input_ = c;
}

bool Lexer::is_eof(const Char8* input) const {
  QLJS_ASSERT(*input == u8'\0');
  return input == this->original_input_.null_terminator();
}

bool Lexer::is_first_token_on_line() const {
  return this->last_token_.has_leading_newline ||
         this->last_last_token_end_ == this->original_input_.data();
}

bool Lexer::is_binary_digit(Char8 c) { return c == u8'0' || c == u8'1'; }

bool Lexer::is_octal_digit(Char8 c) {
  switch (c) {
  QLJS_CASE_OCTAL_DIGIT:
    return true;
  default:
    return false;
  }
}

bool Lexer::is_digit(Char8 c) {
  switch (c) {
  QLJS_CASE_DECIMAL_DIGIT:
    return true;
  default:
    return false;
  }
}

bool Lexer::is_hex_digit(Char8 c) {
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

bool Lexer::is_initial_identifier_byte(Char8 byte) {
  // TODO(strager): Reuse Lex_Tables::initial_character_class?
  switch (static_cast<std::uint8_t>(byte)) {
  QLJS_CASE_IDENTIFIER_START:
    // clang-format off
  /* 0xc0 */ /* 0xc1 */ case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
  case 0xc8: case 0xc9: case 0xca: case 0xcb: /* 0xcc */ case 0xcd: case 0xce: case 0xcf:
  case 0xd0: case 0xd1: case 0xd2: case 0xd3: case 0xd4: case 0xd5: case 0xd6: case 0xd7:
  case 0xd8: case 0xd9: case 0xda: case 0xdb: case 0xdc: case 0xdd: case 0xde: case 0xdf:
  case 0xe0: case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
  case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: /* 0xee */ case 0xef:
  case 0xf0:
    // clang-format on
    return true;
  default:
    return false;
  }
}

bool Lexer::is_identifier_byte(Char8 byte) {
  switch (static_cast<std::uint8_t>(byte)) {
  QLJS_CASE_DECIMAL_DIGIT:
  QLJS_CASE_IDENTIFIER_START:
  // clang-format off
  /* 0xc0 */ /* 0xc1 */ case 0xc2: case 0xc3: case 0xc4: case 0xc5: case 0xc6: case 0xc7:
  case 0xc8: case 0xc9: case 0xca: case 0xcb: case 0xcc: case 0xcd: case 0xce: case 0xcf:
  case 0xd0: case 0xd1: case 0xd2: case 0xd3: case 0xd4: case 0xd5: case 0xd6: case 0xd7:
  case 0xd8: case 0xd9: case 0xda: case 0xdb: case 0xdc: case 0xdd: case 0xde: case 0xdf:
  case 0xe0: case 0xe1: case 0xe2: case 0xe3: case 0xe4: case 0xe5: case 0xe6: case 0xe7:
  case 0xe8: case 0xe9: case 0xea: case 0xeb: case 0xec: case 0xed: /* 0xee */ case 0xef:
  case 0xf0: /* 0xf1 */ /* 0xf2 */ case 0xf3:
    // clang-format on
    return true;
  default:
    return false;
  }
}

bool Lexer::is_initial_identifier_character(char32_t code_point) {
  return look_up_in_unicode_table(identifier_start_chunk_indexes,
                                  identifier_start_chunk_indexes_size,
                                  code_point);
}

bool Lexer::is_identifier_character(char32_t code_point, Identifier_Kind kind) {
  if (kind == Identifier_Kind::jsx && code_point == u'-') {
    return true;
  }
  return look_up_in_unicode_table(identifier_part_chunk_indexes,
                                  identifier_part_chunk_indexes_size,
                                  code_point);
}

bool Lexer::is_non_ascii_whitespace_character(char32_t code_point) {
  QLJS_ASSERT(code_point >= 0x80);
  static constexpr char16_t non_ascii_whitespace_code_points[] = {
      u'\u00a0',  // 0xc2 0xa0      No-Break Space (NBSP)
      u'\u1680',  // 0xe1 0x9a 0x80 Ogham Space Mark
      u'\u2000',  // 0xe2 0x80 0x80 En Quad
      u'\u2001',  // 0xe2 0x80 0x81 Em Quad
      u'\u2002',  // 0xe2 0x80 0x82 En Space
      u'\u2003',  // 0xe2 0x80 0x83 Em Space
      u'\u2004',  // 0xe2 0x80 0x84 Three-Per-Em Space
      u'\u2005',  // 0xe2 0x80 0x85 Four-Per-Em Space
      u'\u2006',  // 0xe2 0x80 0x86 Six-Per-Em Space
      u'\u2007',  // 0xe2 0x80 0x87 Figure Space
      u'\u2008',  // 0xe2 0x80 0x88 Punctuation Space
      u'\u2009',  // 0xe2 0x80 0x89 Thin Space
      u'\u200a',  // 0xe2 0x80 0x8a Hair Space
      u'\u2028',  // 0xe2 0x80 0xa8 Line Separator
      u'\u2029',  // 0xe2 0x80 0xa9 Paragraph Separator
      u'\u202f',  // 0xe2 0x80 0xaf Narrow No-Break Space (NNBSP)
      u'\u205f',  // 0xe2 0x81 0x9f Medium Mathematical Space (MMSP)
      u'\u3000',  // 0xe3 0x80 0x80 Ideographic Space
      u'\ufeff',  // 0xef 0xbb 0xbf Zero Width No-Break Space (BOM, ZWNBSP)
  };
  if (code_point >= 0x10000) {
    return false;
  } else {
    if (this->options_.typescript && code_point == 0x0085) {
      // U+0085: 0xc2 0x85 Next Line (NEL)
      return true;
    }
    return std::binary_search(std::begin(non_ascii_whitespace_code_points),
                              std::end(non_ascii_whitespace_code_points),
                              narrow_cast<char16_t>(code_point));
  }
}

bool Lexer::is_ascii_character(Char8 code_unit) {
  return static_cast<unsigned char>(code_unit) < 0x80;
}

bool Lexer::is_ascii_character(char32_t code_point) {
  return code_point < 0x80;
}

int Lexer::newline_character_size(const Char8* input) {
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

bool Lexer::is_newline_character(char32_t code_point) {
  return code_point == U'\n' || code_point == U'\r' ||
         code_point == U'\u2028' ||  // Line Separator
         code_point == U'\u2029';    // Paragraph Separator
}

bool is_plain_horizontal_whitespace(Source_Code_Span span) {
  String8_View sv = span.string_view();
  for (Char8 c : sv) {
    if (!(c == u8' ' || c == u8'\t')) {
      return false;
    }
  }
  return true;
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
