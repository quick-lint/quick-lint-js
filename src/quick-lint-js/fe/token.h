// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iosfwd>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/fe/keyword-list.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD \
  case ::quick_lint_js::Token_Type::kw_break:                          \
  case ::quick_lint_js::Token_Type::kw_case:                           \
  case ::quick_lint_js::Token_Type::kw_catch:                          \
  case ::quick_lint_js::Token_Type::kw_class:                          \
  case ::quick_lint_js::Token_Type::kw_const:                          \
  case ::quick_lint_js::Token_Type::kw_continue:                       \
  case ::quick_lint_js::Token_Type::kw_debugger:                       \
  case ::quick_lint_js::Token_Type::kw_default:                        \
  case ::quick_lint_js::Token_Type::kw_delete:                         \
  case ::quick_lint_js::Token_Type::kw_do:                             \
  case ::quick_lint_js::Token_Type::kw_else:                           \
  case ::quick_lint_js::Token_Type::kw_enum:                           \
  case ::quick_lint_js::Token_Type::kw_export:                         \
  case ::quick_lint_js::Token_Type::kw_extends:                        \
  case ::quick_lint_js::Token_Type::kw_false:                          \
  case ::quick_lint_js::Token_Type::kw_finally:                        \
  case ::quick_lint_js::Token_Type::kw_for:                            \
  case ::quick_lint_js::Token_Type::kw_if:                             \
  case ::quick_lint_js::Token_Type::kw_import:                         \
  case ::quick_lint_js::Token_Type::kw_in:                             \
  case ::quick_lint_js::Token_Type::kw_instanceof:                     \
  case ::quick_lint_js::Token_Type::kw_new:                            \
  case ::quick_lint_js::Token_Type::kw_null:                           \
  case ::quick_lint_js::Token_Type::kw_return:                         \
  case ::quick_lint_js::Token_Type::kw_super:                          \
  case ::quick_lint_js::Token_Type::kw_switch:                         \
  case ::quick_lint_js::Token_Type::kw_this:                           \
  case ::quick_lint_js::Token_Type::kw_throw:                          \
  case ::quick_lint_js::Token_Type::kw_true:                           \
  case ::quick_lint_js::Token_Type::kw_try:                            \
  case ::quick_lint_js::Token_Type::kw_typeof:                         \
  case ::quick_lint_js::Token_Type::kw_var:                            \
  case ::quick_lint_js::Token_Type::kw_void:                           \
  case ::quick_lint_js::Token_Type::kw_while:                          \
  case ::quick_lint_js::Token_Type::kw_with

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION                \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD: \
  case ::quick_lint_js::Token_Type::kw_await:                     \
  case ::quick_lint_js::Token_Type::kw_yield

#define QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD         \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_FUNCTION_AND_YIELD: \
  case ::quick_lint_js::Token_Type::kw_function

// Non-contextual keywords, including future reserved words, for non-strict
// mode.
#define QLJS_CASE_RESERVED_KEYWORD                   \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD: \
  case ::quick_lint_js::Token_Type::kw_await:        \
  case ::quick_lint_js::Token_Type::kw_yield

// Non-contextual keywords, including future reserved words, for strict mode.
// Includes everything from QLJS_CASE_RESERVED_KEYWORD.
#define QLJS_CASE_STRICT_RESERVED_KEYWORD \
  QLJS_CASE_RESERVED_KEYWORD:             \
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD

// Everything in QLJS_CASE_STRICT_RESERVED_KEYWORD except everything in
// QLJS_CASE_RESERVED_KEYWORD.
#define QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD     \
  case ::quick_lint_js::Token_Type::kw_implements: \
  case ::quick_lint_js::Token_Type::kw_interface:  \
  case ::quick_lint_js::Token_Type::kw_package:    \
  case ::quick_lint_js::Token_Type::kw_private:    \
  case ::quick_lint_js::Token_Type::kw_protected:  \
  case ::quick_lint_js::Token_Type::kw_public

#define QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE \
  case ::quick_lint_js::Token_Type::kw_abstract:                 \
  case ::quick_lint_js::Token_Type::kw_any:                      \
  case ::quick_lint_js::Token_Type::kw_assert:                   \
  case ::quick_lint_js::Token_Type::kw_asserts:                  \
  case ::quick_lint_js::Token_Type::kw_bigint:                   \
  case ::quick_lint_js::Token_Type::kw_boolean:                  \
  case ::quick_lint_js::Token_Type::kw_constructor:              \
  case ::quick_lint_js::Token_Type::kw_declare:                  \
  case ::quick_lint_js::Token_Type::kw_global:                   \
  case ::quick_lint_js::Token_Type::kw_infer:                    \
  case ::quick_lint_js::Token_Type::kw_intrinsic:                \
  case ::quick_lint_js::Token_Type::kw_is:                       \
  case ::quick_lint_js::Token_Type::kw_keyof:                    \
  case ::quick_lint_js::Token_Type::kw_module:                   \
  case ::quick_lint_js::Token_Type::kw_namespace:                \
  case ::quick_lint_js::Token_Type::kw_never:                    \
  case ::quick_lint_js::Token_Type::kw_number:                   \
  case ::quick_lint_js::Token_Type::kw_object:                   \
  case ::quick_lint_js::Token_Type::kw_out:                      \
  case ::quick_lint_js::Token_Type::kw_override:                 \
  case ::quick_lint_js::Token_Type::kw_readonly:                 \
  case ::quick_lint_js::Token_Type::kw_require:                  \
  case ::quick_lint_js::Token_Type::kw_string:                   \
  case ::quick_lint_js::Token_Type::kw_symbol:                   \
  case ::quick_lint_js::Token_Type::kw_undefined:                \
  case ::quick_lint_js::Token_Type::kw_unique:                   \
  case ::quick_lint_js::Token_Type::kw_unknown

#define QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD        \
  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE: \
  case ::quick_lint_js::Token_Type::kw_type

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE \
  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE:                           \
  case ::quick_lint_js::Token_Type::kw_accessor:                                      \
  case ::quick_lint_js::Token_Type::kw_as:                                            \
  case ::quick_lint_js::Token_Type::kw_from:                                          \
  case ::quick_lint_js::Token_Type::kw_let:                                           \
  case ::quick_lint_js::Token_Type::kw_of:                                            \
  case ::quick_lint_js::Token_Type::kw_satisfies

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET               \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE \
      :                                                                         \
  case ::quick_lint_js::Token_Type::kw_static:                                  \
  case ::quick_lint_js::Token_Type::kw_type

// Keywords which are sometimes treated as identifiers; i.e. identifiers which
// are sometimes treated as keywords.
#define QLJS_CASE_CONTEXTUAL_KEYWORD                         \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET: \
  case ::quick_lint_js::Token_Type::kw_async:                \
  case ::quick_lint_js::Token_Type::kw_get:                  \
  case ::quick_lint_js::Token_Type::kw_set

// Any kind of keyword in strict or non-strict mode.
#define QLJS_CASE_KEYWORD       \
  QLJS_CASE_CONTEXTUAL_KEYWORD: \
  QLJS_CASE_STRICT_RESERVED_KEYWORD

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_LESS_AND_STAR \
  case ::quick_lint_js::Token_Type::ampersand:                          \
  case ::quick_lint_js::Token_Type::ampersand_ampersand:                \
  case ::quick_lint_js::Token_Type::bang_equal:                         \
  case ::quick_lint_js::Token_Type::bang_equal_equal:                   \
  case ::quick_lint_js::Token_Type::circumflex:                         \
  case ::quick_lint_js::Token_Type::equal_equal:                        \
  case ::quick_lint_js::Token_Type::equal_equal_equal:                  \
  case ::quick_lint_js::Token_Type::greater:                            \
  case ::quick_lint_js::Token_Type::greater_equal:                      \
  case ::quick_lint_js::Token_Type::greater_greater:                    \
  case ::quick_lint_js::Token_Type::greater_greater_greater:            \
  case ::quick_lint_js::Token_Type::less_equal:                         \
  case ::quick_lint_js::Token_Type::percent:                            \
  case ::quick_lint_js::Token_Type::pipe:                               \
  case ::quick_lint_js::Token_Type::pipe_pipe:                          \
  case ::quick_lint_js::Token_Type::question_question:                  \
  case ::quick_lint_js::Token_Type::star_star

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR          \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_LESS_AND_STAR: \
  case ::quick_lint_js::Token_Type::less_less

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL        \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR: \
  case ::quick_lint_js::Token_Type::star

#define QLJS_CASE_BINARY_ONLY_OPERATOR   \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL: \
  case ::quick_lint_js::Token_Type::kw_instanceof

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL  \
  case ::quick_lint_js::Token_Type::ampersand_equal:               \
  case ::quick_lint_js::Token_Type::circumflex_equal:              \
  case ::quick_lint_js::Token_Type::greater_greater_equal:         \
  case ::quick_lint_js::Token_Type::greater_greater_greater_equal: \
  case ::quick_lint_js::Token_Type::less_less_equal:               \
  case ::quick_lint_js::Token_Type::minus_equal:                   \
  case ::quick_lint_js::Token_Type::percent_equal:                 \
  case ::quick_lint_js::Token_Type::pipe_equal:                    \
  case ::quick_lint_js::Token_Type::plus_equal:                    \
  case ::quick_lint_js::Token_Type::star_equal:                    \
  case ::quick_lint_js::Token_Type::star_star_equal

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR   \
  case ::quick_lint_js::Token_Type::slash_equal: \
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL

#define QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR              \
  case ::quick_lint_js::Token_Type::ampersand_ampersand_equal: \
  case ::quick_lint_js::Token_Type::pipe_pipe_equal:           \
  case ::quick_lint_js::Token_Type::question_question_equal

namespace quick_lint_js {
class Diag_List;
class Diag_Reporter;
class Identifier;
class Source_Code_Span;

enum class Token_Type {
  // Single-character symbols:
  ampersand = '&',
  at = '@',
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
  private_identifier,  // #name
  regexp,
  string,

  // An identifier which contains escape sequences and which, if unescaped,
  // matches a reserved keyword. For example, the token `\u{69}\u{66}` unescaped
  // is `if`.
  //
  // Such identifiers are sometimes legal and sometimes illegal depending on the
  // parser's context, hence we distinguish them from Token_Type::identifier.
  reserved_keyword_with_escape_sequence,

  // Symbols:
  ampersand_ampersand,            // &&
  ampersand_ampersand_equal,      // &&=
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
  pipe_pipe_equal,                // ||=
  plus_equal,                     // +=
  plus_plus,                      // ++
  question_dot,                   // ?.
  question_question,              // ??
  question_question_equal,        // ??=
  slash_equal,                    // /=
  star_equal,                     // *=
  star_star,                      // **
  star_star_equal,                // **=

// Reserved words, future reserved words, conditionally reserved words, and
// contextual keywords ('kw' stands for 'KeyWord'):
#define QLJS_KEYWORD(k) kw_##k,
  QLJS_X_KEYWORDS
#undef QLJS_KEYWORD
};

const char* to_string(Token_Type);
std::ostream& operator<<(std::ostream&, Token_Type);

using Escape_Sequence_List = Vector<Source_Code_Span>;

struct Token {
  Identifier identifier_name() const;
  Source_Code_Span span() const;

  // Report Diag_Keywords_Cannot_Contain_Escape_Sequences for each escape
  // sequence in the most recently parsed keyword-looking identifier.
  //
  // Precondition:
  //   this->type == Token_Type::reserved_keyword_with_escape_sequence
  // Precondition: This function was not previously called for the same token.
  void report_errors_for_escape_sequences_in_keyword(Diag_Reporter*) const;

  // Report errors for each invalid escape sequence in the most recently parsed
  // template.
  //
  // Precondition:
  //   this->type == Token_Type::complete_template ||
  //   this->type == Token_Type::incomplete_template
  // Precondition: This function was not previously called for the same token.
  void report_errors_for_escape_sequences_in_template(Diag_Reporter*) const;

  const Char8* begin;
  const Char8* end;

  Token_Type type;

  bool has_leading_newline;
  bool has_leading_comment;

  // Used only if this is a keyword token or an identifier token.
  // If the token contains no escape sequences, .normalized_identifier is
  // equivalent to String8_View(.begin, .end).
  String8_View normalized_identifier;
  bool contains_escape_sequence() const;

  union {
    // Used only if this is a reserved_keyword_with_escape_sequence token.
    Escape_Sequence_List* identifier_escape_sequences;
    // Used only if this is a complete_template or incomplete_template token.
    Diag_List* template_escape_sequence_diagnostics;
  };
};
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
