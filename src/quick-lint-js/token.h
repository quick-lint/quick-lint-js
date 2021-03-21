// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TOKEN_H
#define QUICK_LINT_JS_TOKEN_H

#include <iosfwd>
#include <quick-lint-js/char8.h>

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
  regexp,
  string,

  // An identifier which contains escape sequences and which, if unescaped,
  // matches a reserved keyword. For example, the token `\u{69}\u{66}` unescaped
  // is `if`.
  //
  // Such identifiers are sometimes legal and sometimes illegal depending on the
  // parser's context, hence we distinguish them from token_type::identifier.
  reserved_keyword_with_escape_sequence,

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
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
