// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-8.h>
#include <vector>

namespace quick_lint_js {
class Diag_Reporter;
struct Lex_Tables;
struct Lexer_Transaction;

struct Lexer_Options {
  bool typescript = false;
};

// A Lexer reads JavaScript source code one token at a time.
//
// A token is (roughly) either a keyword (if, function, let, etc.), an operator
// (+, !==, *=, etc.), an identifier (variable name), or a literal (number,
// string, boolean, etc.).
//
// Whitespace and comments are not interpreted as tokens.
class Lexer {
 public:
  enum class Identifier_Kind {
    javascript,
    jsx,  // Allows '-'.
  };

  explicit Lexer(Padded_String_View input, Diag_Reporter*);
  explicit Lexer(Padded_String_View input, Diag_Reporter*, Lexer_Options);

  // Return information about the current token.
  const Token& peek() const { return this->last_token_; }

  // Advance to the next token. Use this->peek() after to observe the next
  // token.
  //
  // This function ignores leading and trailing whitespace and comments.
  //
  // Precondition: this->peek().type != Token_Type::end_of_file.
  void skip() { this->parse_current_token(); }

  // After parsing a '}' (right_curly) token, call this function to interpret
  // '}' as ending an expression inside a template literal.
  //
  // For example:
  //
  //   `senior ${language} engineer`
  //
  // After seeing the '}' (right_curly) token, the caller should use
  // this->skip_in_template() so '} engineer`' is interpreted as part of the
  // template literal (instead of a '}' token, an 'engineer' token, and the
  // beginning of another template literal).
  //
  // The given template_begin is used for diagnostic reporting.
  //
  // Precondition: this->peek().type == Token_Type::right_curly
  void skip_in_template(const Char8* template_begin);

  // Like this->skip(), except:
  //
  // * interpret identifiers as JSX identifiers (JSX identifiers may contain
  //   '-')
  // * interpret strings as JSX strings (JSX strings do not support '\' escapes)
  // * interpret '>>', '>=', etc. as a '>' token followed by another token
  void skip_in_jsx();

  // After parsing a '}' (right_curly) token, call this function to interpret
  // '}' as ending an expression inside a JSX element.
  //
  // After parsing a '>' (greater) token, call this function to interpret '>' as
  // the beginning of children for a JSX element.
  //
  // For example:
  //
  //   <div>Hello, {name}!!!</div>
  //
  // After seeing the '>' (greater) token, the caller should use
  // this->skip_in_jsx_children() so 'Hello, {' is interpreted as text (instead
  // of a 'Hello' identifier token, a ',' token, and a '{' token). After seeing
  // the '}' (right_curly) token, the caller should use
  // this->skip_in_jsx_children() so '!!!' is interpreted as text (instead of
  // three '!' tokens).
  //
  // Precondition: this->peek().type == Token_Type::right_curly ||
  //               this->peek().type == Token_Type::greater
  void skip_in_jsx_children();

  // After the current token, look for for first occurrence of any one of the
  // following:
  //
  // * '=>' (invalid in JSX children)
  // * '>' (invalid in JSX children)
  // * '}' (invalid in JSX children)
  // * '{'
  // * '<'
  // * end of file
  //
  // If '=>' was found, this function returns a pointer to the '='. Otherwise,
  // it returns nullptr.
  const Char8* find_equal_greater_in_jsx_children() const;

  // After parsing a '<<' (less_less) token, call this function to reinterpret
  // the token as two '<' (less) tokens, then skip the first token.
  //
  // Precondition:  this->peek().type == Token_Type::less_less
  // Postcondition: this->peek().type == Token_Type::less
  void skip_less_less_as_less();

  // After parsing a '>>', or '>>>' token, call this function to
  // reinterpret the token as a '>' (greater) token followed by another token,
  // then skip the first token.
  //
  // Precondition:  this->peek().type == Token_Type::greater_greater ||
  //                this->peek().type == Token_Type::greater_greater_greater
  // Postcondition: this->peek().type == Token_Type::greater
  void skip_as_greater();

  // Reparse a '/' or '/=' token as a regular expression literal.
  //
  // Precondition: this->peek().type == Token_Type::slash or
  //               Token_Type::slash_equal.
  // Postcondition: this->peek().type == Token_Type::regexp.
  void reparse_as_regexp();

  // Returns true if a valid regexp literal is found
  // Precondition: *regexp_begin == '/'
  bool test_for_regexp(const Char8* regexp_begin);

  // Save lexer state.
  //
  // After calling begin_transaction, you must call either commit_transaction or
  // roll_back_transaction with the returned transaction.
  //
  // You can call begin_transaction again before calling commit_transaction
  // or roll_back_transaction. Doing so begins a nested transaction.
  //
  // Inside a transaction, diagnostics are not reported until commit_transaction
  // is called for the outer-most nested transaction.
  Lexer_Transaction begin_transaction();

  // After calling commit_transaction, it's almost as if you never called
  // begin_transaction in the first place.
  //
  // commit_transaction does not restore the state of the lexer when
  // begin_transaction was called.
  void commit_transaction(Lexer_Transaction&&);

  // Restore lexer state to a prior version.
  //
  // After calling roll_back_transaction, it's as if you never called
  // begin_transaction or subsequently called skip, insert_semicolon, or
  // other functions.
  //
  // roll_back_transaction effectively undoes calls to skip, insert_semicolon,
  // etc.
  //
  // Calling roll_back_transaction will not report lexer diagnostics which might
  // have been reported if it weren't for begin_transaction.
  void roll_back_transaction(Lexer_Transaction&&);

  // transaction_has_lex_diagnostics can only be called while the given
  // transaction is the most recent active transaction.
  bool transaction_has_lex_diagnostics(const Lexer_Transaction&) const;

  void insert_semicolon();

  // Do not call this after calling insert_semicolon, unless skip has been
  // called after.
  const Char8* end_of_previous_token() const;

  Padded_String_View original_input() const;

  void debug_dump_location() const;
  void debug_dump_location(const Char8*) const;

  static constexpr std::size_t unicode_table_chunk_size = 256;
  static constexpr std::size_t unicode_tables_chunks_size = 49152;
  static const std::uint8_t unicode_tables_chunks[];

  using Unicode_Table_Chunk_Index_Type = std::uint8_t;
  static constexpr std::size_t identifier_start_chunk_indexes_size = 804;
  static const Unicode_Table_Chunk_Index_Type identifier_start_chunk_indexes[];

  static constexpr std::size_t identifier_part_chunk_indexes_size = 3586;
  static const Unicode_Table_Chunk_Index_Type identifier_part_chunk_indexes[];

 private:
  struct Parsed_Template_Body {
    Token_Type type;
    const Char8* end;
    // Might be null.
    Diag_List* escape_sequence_diagnostics;
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
  //                    .after
  //
  // In this case, .after points to the ')' character which follows the
  // identifier, and .normalized points to a heap-allocated string u8"wat".
  //
  // If any escape sequences were parsed, .escape_sequences points to a list of
  // escape squence spans.
  //
  // Invariant:
  //   (escape_sequences == nullptr) == (normalized.data() == nullptr)
  struct Parsed_Identifier {
    const Char8* after;  // Where to continue parsing.
    String8_View normalized;

    Escape_Sequence_List* escape_sequences;
  };

  void parse_bom_before_shebang();

  // Skips leading whitespace and comments. Initializes this->last_token_ and
  // this->last_last_token_end_.
  void parse_current_token();

  // Does not skip whitespace.
  //
  // Returns false if a comment was found. Returns true if a token or EOF was
  // found.
  //
  // Does not update this->last_last_token_end_. Assumes
  // this->last_token_.has_leading_newline was previously initialized. Updates
  // this->last_token_.begin and other members of this->last_token_.
  bool try_parse_current_token();

  const Char8* parse_string_literal();
  const Char8* parse_jsx_string_literal();
  const Char8* parse_smart_quote_string_literal(
      const Decode_UTF8_Result& opening_quote);

  Parsed_Template_Body parse_template_body(const Char8* input,
                                           const Char8* template_begin,
                                           Diag_Reporter*);

  void parse_binary_number();
  void parse_legacy_octal_number();  // 0775, 09999, 08.24
  void parse_modern_octal_number();  // 0o775, 0o111_555
  void parse_hexadecimal_number();
  template <class Error>
  const Char8* check_garbage_in_number_literal(const Char8* input);
  void check_integer_precision_loss(String8_View number_literal);
  void parse_number();

  template <class Func>
  const Char8* parse_digits_and_underscores(Func&& is_valid_digit,
                                            const Char8* input);
  const Char8* parse_octal_digits(const Char8* input);
  const Char8* parse_decimal_digits_and_underscores(const Char8* input);
  const Char8* parse_hex_digits_and_underscores(const Char8* input);

  struct Parsed_Unicode_Escape {
    const Char8* end;
    std::optional<char32_t> code_point;
  };

  Parsed_Unicode_Escape parse_unicode_escape(const Char8* input,
                                             Diag_List* out_diags);

  Parsed_Identifier parse_identifier(const Char8*, Identifier_Kind);
  const Char8* parse_identifier_fast_only(const Char8*);
  Parsed_Identifier parse_identifier_slow(const Char8* input,
                                          const Char8* identifier_begin,
                                          Identifier_Kind);

  void parse_non_ascii();

  void skip_whitespace();
  void skip_block_comment();
  void skip_line_comment_body();
  void skip_jsx_text();

  bool is_eof(const Char8*) const;

  bool is_first_token_on_line() const;

  static bool is_binary_digit(Char8);
  static bool is_octal_digit(Char8);
  static bool is_digit(Char8);
  static bool is_hex_digit(Char8);

 public:
  static bool is_initial_identifier_byte(Char8 byte);

  // Returns true if the given byte is the legal anywhere within an identifier,
  // except for the following cases:
  //
  // * '{' or '}'
  // * a byte which cannot be the first byte of a UTF-8 sequence
  static bool is_identifier_byte(Char8 byte);

  static bool is_initial_identifier_character(char32_t code_point);
  static bool is_identifier_character(char32_t code_point, Identifier_Kind);

 private:
  bool is_non_ascii_whitespace_character(char32_t code_point);
  static bool is_ascii_character(Char8 code_unit);
  static bool is_ascii_character(char32_t code_point);

  static int newline_character_size(const Char8*);
  static bool is_newline_character(char32_t code_point);

  static Token_Type identifier_token_type(String8_View);

  Token last_token_;
  const Char8* last_last_token_end_;
  const Char8* input_;
  Diag_Reporter* diag_reporter_;
  Padded_String_View original_input_;
  Lexer_Options options_;

  Monotonic_Allocator allocator_{"lexer::allocator_"};
  Linked_Bump_Allocator transaction_allocator_{"lexer::transaction_allocator_"};

  friend struct Lex_Tables;
};

struct Lexer_Transaction {
  // Private to lexer. Do not construct, read, or modify.

  using Allocator_Type = Linked_Bump_Allocator;

  explicit Lexer_Transaction(Token old_last_token,
                             const Char8* old_last_last_token_end,
                             const Char8* old_input,
                             Diag_Reporter** diag_reporter_pointer,
                             Allocator_Type* allocator)
      : allocator_rewind(allocator->prepare_for_rewind()),
        old_last_token(old_last_token),
        old_last_last_token_end(old_last_last_token_end),
        old_input(old_input),
        reporter(std::in_place, allocator),
        old_diag_reporter(
            std::exchange(*diag_reporter_pointer, get(this->reporter))) {}

  // Don't allow copying a transaction. Lexer::diag_reporter_ might point to
  // Lexer_Transaction::diag_reporter.
  Lexer_Transaction(const Lexer_Transaction&) = delete;
  Lexer_Transaction& operator=(const Lexer_Transaction&) = delete;

  // Rewinds memory allocated by 'reporter'. Must be constructed before
  // 'reporter' is constructed. 'Allocator_Type::rewind' must be called before
  // 'reporter' is destructed.
  Allocator_Type::Rewind_State allocator_rewind;

  Token old_last_token;
  const Char8* old_last_last_token_end;
  const Char8* old_input;
  std::optional<Buffering_Diag_Reporter> reporter;
  Diag_Reporter* old_diag_reporter;
};

bool is_plain_horizontal_whitespace(Source_Code_Span span);
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
