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

#include <cstddef>
#include <cstdint>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD \
  case ::quick_lint_js::token_type::kw_break:                          \
  case ::quick_lint_js::token_type::kw_case:                           \
  case ::quick_lint_js::token_type::kw_catch:                          \
  case ::quick_lint_js::token_type::kw_class:                          \
  case ::quick_lint_js::token_type::kw_const:                          \
  case ::quick_lint_js::token_type::kw_continue:                       \
  case ::quick_lint_js::token_type::kw_debugger:                       \
  case ::quick_lint_js::token_type::kw_default:                        \
  case ::quick_lint_js::token_type::kw_delete:                         \
  case ::quick_lint_js::token_type::kw_do:                             \
  case ::quick_lint_js::token_type::kw_else:                           \
  case ::quick_lint_js::token_type::kw_enum:                           \
  case ::quick_lint_js::token_type::kw_export:                         \
  case ::quick_lint_js::token_type::kw_extends:                        \
  case ::quick_lint_js::token_type::kw_false:                          \
  case ::quick_lint_js::token_type::kw_finally:                        \
  case ::quick_lint_js::token_type::kw_for:                            \
  case ::quick_lint_js::token_type::kw_if:                             \
  case ::quick_lint_js::token_type::kw_import:                         \
  case ::quick_lint_js::token_type::kw_in:                             \
  case ::quick_lint_js::token_type::kw_instanceof:                     \
  case ::quick_lint_js::token_type::kw_new:                            \
  case ::quick_lint_js::token_type::kw_null:                           \
  case ::quick_lint_js::token_type::kw_return:                         \
  case ::quick_lint_js::token_type::kw_super:                          \
  case ::quick_lint_js::token_type::kw_switch:                         \
  case ::quick_lint_js::token_type::kw_this:                           \
  case ::quick_lint_js::token_type::kw_throw:                          \
  case ::quick_lint_js::token_type::kw_true:                           \
  case ::quick_lint_js::token_type::kw_try:                            \
  case ::quick_lint_js::token_type::kw_typeof:                         \
  case ::quick_lint_js::token_type::kw_var:                            \
  case ::quick_lint_js::token_type::kw_void:                           \
  case ::quick_lint_js::token_type::kw_while:                          \
  case ::quick_lint_js::token_type::kw_with

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION                \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD: \
  case ::quick_lint_js::token_type::kw_await:                     \
  case ::quick_lint_js::token_type::kw_yield

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD         \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD: \
  case ::quick_lint_js::token_type::kw_function

// Non-contextual keywords, including future reserved words.
// TODO(strager): private, public, and protected should be in this list.
#define QLJS_CASE_RESERVED_KEYWORD                   \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD: \
  case ::quick_lint_js::token_type::kw_await:        \
  case ::quick_lint_js::token_type::kw_yield

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC \
  case ::quick_lint_js::token_type::kw_as:                                   \
  case ::quick_lint_js::token_type::kw_from:                                 \
  case ::quick_lint_js::token_type::kw_let:                                  \
  case ::quick_lint_js::token_type::kw_of

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET       \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC: \
  case ::quick_lint_js::token_type::kw_static

// Keywords which are sometimes treated as identifiers; i.e. identifiers which
// are sometimes treated as keywords.
#define QLJS_CASE_CONTEXTUAL_KEYWORD                         \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET: \
  case ::quick_lint_js::token_type::kw_async:                \
  case ::quick_lint_js::token_type::kw_get:                  \
  case ::quick_lint_js::token_type::kw_set

#define QLJS_CASE_KEYWORD       \
  QLJS_CASE_CONTEXTUAL_KEYWORD: \
  QLJS_CASE_RESERVED_KEYWORD

namespace quick_lint_js {
class error_reporter;
struct lexer_transaction;

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

  complete_template,  // `text` or }text`
  end_of_file,
  identifier,
  incomplete_template,  // `text${
  number,
  regexp,
  string,

  // Reserved words and contextual keywords ('kw' stands for 'KeyWord'):
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
  kw_enum,
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
  kw_set,
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
  ampersand_ampersand,            // &&
  ampersand_equal,                // &=
  bang_equal,                     // !=
  bang_equal_equal,               // !==
  circumflex_equal,               // ^=
  dot_dot_dot,                    // ...
  equal_equal,                    // ==
  equal_equal_equal,              // ===
  equal_greater,                  // =>
  greater_equal,                  // >=
  greater_greater,                // >>
  greater_greater_equal,          // >>=
  greater_greater_greater,        // >>>
  greater_greater_greater_equal,  // >>>=
  less_equal,                     // <=
  less_less,                      // <<
  less_less_equal,                // <<=
  minus_equal,                    // -=
  minus_minus,                    // --
  percent_equal,                  // %=
  pipe_equal,                     // |=
  pipe_pipe,                      // ||
  plus_equal,                     // +=
  plus_plus,                      // ++
  slash_equal,                    // /=
  star_equal,                     // *=
  star_star,                      // **
  star_star_equal,                // **=
};

const char* to_string(token_type);
std::ostream& operator<<(std::ostream&, token_type);

class identifier {
 public:
  // For tests only.
  explicit identifier(source_code_span span) noexcept
      : span_begin_(span.begin()),
        normalized_begin_(this->span_begin_),
        span_size_(narrow_cast<int>(span.end() - span.begin())),
        normalized_size_(this->span_size_) {}

  explicit identifier(source_code_span span, string8_view normalized) noexcept
      : span_begin_(span.begin()),
        normalized_begin_(normalized.data()),
        span_size_(narrow_cast<int>(span.end() - span.begin())),
        normalized_size_(narrow_cast<int>(normalized.size())) {}

  explicit identifier(source_code_span span,
                      const char8* normalized) noexcept = delete;

  source_code_span span() const noexcept {
    return source_code_span(this->span_begin_,
                            this->span_begin_ + this->span_size_);
  }

  // normalized_name returns the variable's name with escape sequences resolved.
  //
  // For example, a variable named \u{61} in the source code will have
  // normalized_name refer to u8"a".
  //
  // The returned pointers might not reside within the source code string. In
  // other words, the normalized name might be heap-allocated. Call span()
  // instead if you want pointers within the source code input.
  string8_view normalized_name() const noexcept {
    return string8_view(this->normalized_begin_,
                        narrow_cast<std::size_t>(this->normalized_size_));
  }

 private:
  const char8* span_begin_;
  const char8* normalized_begin_;
  int span_size_;
  int normalized_size_;
};

struct token {
  identifier identifier_name() const noexcept;
  source_code_span span() const noexcept;

  token_type type;

  const char8* begin;
  const char8* end;

  bool has_leading_newline;

  // Used only if this is a keyword token or an identifier token.
  // If the token contains no escape sequences, .normalized_identifier is
  // equivalent to string8_view(.begin, .end).
  string8_view normalized_identifier;
};

// A lexer reads JavaScript source code one token at a time.
//
// A token is (roughly) either a keyword (if, function, let, etc.), an operator
// (+, !==, *=, etc.), an identifier (variable name), or a literal (number,
// string, boolean, etc.).
//
// Whitespace and comments are not interpreted as tokens.
//
// lexer can modify the input string in some cases. For example, the identifier
// w\u0061t is rewritten to wat (followed by padding spaces).
class lexer {
 public:
  explicit lexer(padded_string_view input, error_reporter*) noexcept;

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

  // Reparse a '/' or '/=' token as a regular expression literal.
  //
  // Precondition: this->peek().type == token_type::slash or
  //               token_type::slash_equal.
  // Postcondition: this->peek().type == token_type::regexp.
  void reparse_as_regexp();

  // Save lexer state.
  //
  // After calling begin_transaction, you must call either commit_transaction or
  // roll_back_transaction with the returned transaction.
  //
  // You cannot call begin_transaction again before calling commit_transaction
  // or roll_back_transaction. In other words, transactions may not be nested.
  //
  // Inside a transaction, errors are not reported until commit_transaction is
  // called.
  lexer_transaction begin_transaction();

  // After calling commit_transaction, it's almost as if you never called
  // begin_transaction in the first place.
  //
  // commit_transaction does not restore the state of the lexer when
  // begin_transaction was called.
  void commit_transaction(lexer_transaction&&);

  // Restore lexer state to a prior version.
  //
  // After calling roll_back_transaction, it's as if you never called
  // begin_transaction or subsequently called skip, insert_semicolon, or
  // other functions.
  //
  // roll_back_transaction effectively undoes calls to skip, insert_semicolon,
  // etc.
  //
  // Calling roll_back_transaction will not report lexer errors which might have
  // been reported if it weren't for begin_transaction.
  void roll_back_transaction(lexer_transaction&&);

  void insert_semicolon();

  // Do not call this after calling insert_semicolon, unless skip has been
  // called after.
  const char8* end_of_previous_token() const noexcept;

  static constexpr std::size_t unicode_table_chunk_size = 256;
  static constexpr std::size_t unicode_tables_chunks_size = 46848;
  static const std::uint8_t unicode_tables_chunks[];

  using unicode_table_chunk_index_type = std::uint8_t;
  static constexpr std::size_t identifier_start_chunk_indexes_size = 788;
  static const unicode_table_chunk_index_type identifier_start_chunk_indexes[];

  static constexpr std::size_t identifier_part_chunk_indexes_size = 3586;
  static const unicode_table_chunk_index_type identifier_part_chunk_indexes[];

 private:
  struct parsed_template_body {
    token_type type;
    const char8* end;
  };

  // The result of parsing an identifier.
  //
  // Typically, .normalized is default-constructed. However, if an identifier
  // contains escape squences, then .normalized points to a heap-allocated
  // null-terminated string of the unescaped identifier.
  //
  // Say we are parsing the identifier starting with 'w' in the following
  // example:
  //
  // Input: log(w\u{61}t)
  //                    ^
  //                    .end
  //
  // In this case, .end points to the ')' character which follows the
  // identifier, and .normalized points to a heap-allocated string u8"wat".
  //
  // Invariant:
  // if (escape_sequences.empty()) normalized.data() == nullptr;
  struct parsed_identifier {
    const char8* after;  // Where to continue parsing.
    string8_view normalized;

    std::vector<source_code_span> escape_sequences;
  };

  void parse_current_token();

  const char8* parse_string_literal() noexcept;

  parsed_template_body parse_template_body(const char8* input,
                                           const char8* template_begin,
                                           error_reporter*);

  void parse_binary_number();

  enum class octal_kind {
    sloppy,
    // strict_0,
    strict_0o,
  };
  void parse_octal_number(octal_kind);
  void parse_hexadecimal_number();
  template <class Error>
  const char8* check_garbage_in_number_literal(const char8* input);
  void parse_number();

  template <class Func>
  const char8* parse_digits_and_underscores(Func&& is_valid_digit,
                                            const char8* input) noexcept;
  const char8* parse_octal_digits(const char8* input) noexcept;
  const char8* parse_decimal_digits_and_underscores(
      const char8* input) noexcept;
  const char8* parse_hex_digits_and_underscores(const char8* input) noexcept;

  parsed_identifier parse_identifier(const char8*);
  parsed_identifier parse_identifier_slow(const char8* input,
                                          const char8* identifier_begin);

  void skip_whitespace();
  void skip_block_comment();
  void skip_line_comment_body();

  bool is_eof(const char8*) noexcept;

  bool is_first_token_on_line() const noexcept;

  static bool is_binary_digit(char8);
  static bool is_octal_digit(char8);
  static bool is_digit(char8);
  static bool is_hex_digit(char8);

 public:
  static bool is_initial_identifier_character(char32_t code_point);
  static bool is_identifier_character(char32_t code_point);

 private:
  static bool is_non_ascii_whitespace_character(char32_t code_point);
  static bool is_ascii_character(char8 code_unit);
  static bool is_ascii_character(char32_t code_point);

  static int newline_character_size(const char8*);

  static token_type identifier_token_type(string8_view) noexcept;

  token last_token_;
  const char8* last_last_token_end_;
  const char8* input_;
  error_reporter* error_reporter_;
  padded_string_view original_input_;

  monotonic_allocator allocator_;
};

struct lexer_transaction {
  // Private to lexer. Do not read or modify.
  token old_last_token;
  const char8* old_last_last_token_end;
  const char8* old_input;
  error_reporter* old_error_reporter;
};
}

#endif
