// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_TOKEN_H
#define QUICK_LINT_JS_FE_TOKEN_H

#include <iosfwd>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/fe/keyword-list.h>
#include <quick-lint-js/port/char8.h>
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

// Non-contextual keywords, including future reserved words, for non-strict
// mode.
#define QLJS_CASE_RESERVED_KEYWORD                   \
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD: \
  case ::quick_lint_js::token_type::kw_await:        \
  case ::quick_lint_js::token_type::kw_yield

// Non-contextual keywords, including future reserved words, for strict mode.
// Includes everything from QLJS_CASE_RESERVED_KEYWORD.
#define QLJS_CASE_STRICT_RESERVED_KEYWORD \
  QLJS_CASE_RESERVED_KEYWORD:             \
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD

// Everything in QLJS_CASE_STRICT_RESERVED_KEYWORD except everything in
// QLJS_CASE_RESERVED_KEYWORD.
#define QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD     \
  case ::quick_lint_js::token_type::kw_implements: \
  case ::quick_lint_js::token_type::kw_interface:  \
  case ::quick_lint_js::token_type::kw_package:    \
  case ::quick_lint_js::token_type::kw_private:    \
  case ::quick_lint_js::token_type::kw_protected:  \
  case ::quick_lint_js::token_type::kw_public

#define QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE \
  case ::quick_lint_js::token_type::kw_abstract:                 \
  case ::quick_lint_js::token_type::kw_any:                      \
  case ::quick_lint_js::token_type::kw_assert:                   \
  case ::quick_lint_js::token_type::kw_asserts:                  \
  case ::quick_lint_js::token_type::kw_bigint:                   \
  case ::quick_lint_js::token_type::kw_boolean:                  \
  case ::quick_lint_js::token_type::kw_constructor:              \
  case ::quick_lint_js::token_type::kw_declare:                  \
  case ::quick_lint_js::token_type::kw_global:                   \
  case ::quick_lint_js::token_type::kw_infer:                    \
  case ::quick_lint_js::token_type::kw_intrinsic:                \
  case ::quick_lint_js::token_type::kw_is:                       \
  case ::quick_lint_js::token_type::kw_keyof:                    \
  case ::quick_lint_js::token_type::kw_module:                   \
  case ::quick_lint_js::token_type::kw_namespace:                \
  case ::quick_lint_js::token_type::kw_never:                    \
  case ::quick_lint_js::token_type::kw_number:                   \
  case ::quick_lint_js::token_type::kw_object:                   \
  case ::quick_lint_js::token_type::kw_out:                      \
  case ::quick_lint_js::token_type::kw_override:                 \
  case ::quick_lint_js::token_type::kw_readonly:                 \
  case ::quick_lint_js::token_type::kw_require:                  \
  case ::quick_lint_js::token_type::kw_string:                   \
  case ::quick_lint_js::token_type::kw_symbol:                   \
  case ::quick_lint_js::token_type::kw_undefined:                \
  case ::quick_lint_js::token_type::kw_unique:                   \
  case ::quick_lint_js::token_type::kw_unknown

#define QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD        \
  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE: \
  case ::quick_lint_js::token_type::kw_type

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE \
  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE:                           \
  case ::quick_lint_js::token_type::kw_as:                                            \
  case ::quick_lint_js::token_type::kw_from:                                          \
  case ::quick_lint_js::token_type::kw_let:                                           \
  case ::quick_lint_js::token_type::kw_of

#define QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET               \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE \
      :                                                                         \
  case ::quick_lint_js::token_type::kw_static:                                  \
  case ::quick_lint_js::token_type::kw_type

// Keywords which are sometimes treated as identifiers; i.e. identifiers which
// are sometimes treated as keywords.
#define QLJS_CASE_CONTEXTUAL_KEYWORD                         \
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET: \
  case ::quick_lint_js::token_type::kw_async:                \
  case ::quick_lint_js::token_type::kw_get:                  \
  case ::quick_lint_js::token_type::kw_set

// Any kind of keyword in strict or non-strict mode.
#define QLJS_CASE_KEYWORD       \
  QLJS_CASE_CONTEXTUAL_KEYWORD: \
  QLJS_CASE_STRICT_RESERVED_KEYWORD

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_LESS_AND_STAR \
  case ::quick_lint_js::token_type::ampersand:                          \
  case ::quick_lint_js::token_type::ampersand_ampersand:                \
  case ::quick_lint_js::token_type::bang_equal:                         \
  case ::quick_lint_js::token_type::bang_equal_equal:                   \
  case ::quick_lint_js::token_type::circumflex:                         \
  case ::quick_lint_js::token_type::equal_equal:                        \
  case ::quick_lint_js::token_type::equal_equal_equal:                  \
  case ::quick_lint_js::token_type::greater:                            \
  case ::quick_lint_js::token_type::greater_equal:                      \
  case ::quick_lint_js::token_type::greater_greater:                    \
  case ::quick_lint_js::token_type::greater_greater_greater:            \
  case ::quick_lint_js::token_type::less_equal:                         \
  case ::quick_lint_js::token_type::percent:                            \
  case ::quick_lint_js::token_type::pipe:                               \
  case ::quick_lint_js::token_type::pipe_pipe:                          \
  case ::quick_lint_js::token_type::question_question:                  \
  case ::quick_lint_js::token_type::star_star

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR          \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_LESS_AND_STAR: \
  case ::quick_lint_js::token_type::less_less

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL        \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR: \
  case ::quick_lint_js::token_type::star

#define QLJS_CASE_BINARY_ONLY_OPERATOR   \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL: \
  case ::quick_lint_js::token_type::kw_instanceof

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL  \
  case ::quick_lint_js::token_type::ampersand_equal:               \
  case ::quick_lint_js::token_type::circumflex_equal:              \
  case ::quick_lint_js::token_type::greater_greater_equal:         \
  case ::quick_lint_js::token_type::greater_greater_greater_equal: \
  case ::quick_lint_js::token_type::less_less_equal:               \
  case ::quick_lint_js::token_type::minus_equal:                   \
  case ::quick_lint_js::token_type::percent_equal:                 \
  case ::quick_lint_js::token_type::pipe_equal:                    \
  case ::quick_lint_js::token_type::plus_equal:                    \
  case ::quick_lint_js::token_type::star_equal:                    \
  case ::quick_lint_js::token_type::star_star_equal

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR   \
  case ::quick_lint_js::token_type::slash_equal: \
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL

#define QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR              \
  case ::quick_lint_js::token_type::ampersand_ampersand_equal: \
  case ::quick_lint_js::token_type::pipe_pipe_equal:           \
  case ::quick_lint_js::token_type::question_question_equal

namespace quick_lint_js {
class buffering_diag_reporter;
class diag_reporter;
class identifier;
class source_code_span;

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
  private_identifier,  // #name
  regexp,
  string,

  // An identifier which contains escape sequences and which, if unescaped,
  // matches a reserved keyword. For example, the token `\u{69}\u{66}` unescaped
  // is `if`.
  //
  // Such identifiers are sometimes legal and sometimes illegal depending on the
  // parser's context, hence we distinguish them from token_type::identifier.
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

const char* to_string(token_type);
std::ostream& operator<<(std::ostream&, token_type);

using escape_sequence_list = bump_vector<source_code_span, monotonic_allocator>;

struct token {
  identifier identifier_name() const noexcept;
  source_code_span span() const noexcept;

  // Report diag_keywords_cannot_contain_escape_sequences for each escape
  // sequence in the most recently parsed keyword-looking identifier.
  //
  // Precondition:
  //   this->type == token_type::reserved_keyword_with_escape_sequence
  // Precondition: This function was not previously called for the same token.
  void report_errors_for_escape_sequences_in_keyword(diag_reporter*) const;

  // Report errors for each invalid escape sequence in the most recently parsed
  // template.
  //
  // Precondition:
  //   this->type == token_type::complete_template ||
  //   this->type == token_type::incomplete_template
  // Precondition: This function was not previously called for the same token.
  void report_errors_for_escape_sequences_in_template(diag_reporter*) const;

  token_type type;

  const char8* begin;
  const char8* end;

  bool has_leading_newline;

  // Used only if this is a keyword token or an identifier token.
  // If the token contains no escape sequences, .normalized_identifier is
  // equivalent to string8_view(.begin, .end).
  string8_view normalized_identifier;

  union {
    // Used only if this is a reserved_keyword_with_escape_sequence token.
    escape_sequence_list* identifier_escape_sequences;
    // Used only if this is a complete_template or incomplete_template token.
    buffering_diag_reporter* template_escape_sequence_diagnostics;
  };
};
}

#endif

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
