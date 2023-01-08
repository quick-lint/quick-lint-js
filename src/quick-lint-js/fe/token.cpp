// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/buffering-diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
identifier token::identifier_name() const noexcept {
  switch (this->type) {
  QLJS_CASE_KEYWORD:
  case token_type::identifier:
  case token_type::private_identifier:
  case token_type::reserved_keyword_with_escape_sequence:
    break;
  default:
    QLJS_ASSERT(false);
    break;
  }
  return identifier(this->span(),
                    /*normalized=*/this->normalized_identifier);
}

source_code_span token::span() const noexcept {
  return source_code_span(this->begin, this->end);
}

void token::report_errors_for_escape_sequences_in_keyword(
    diag_reporter* reporter) const {
  QLJS_ASSERT(this->type == token_type::reserved_keyword_with_escape_sequence);
  QLJS_ASSERT(this->identifier_escape_sequences);
  QLJS_ASSERT(!this->identifier_escape_sequences->empty());
  for (const source_code_span& escape_sequence :
       *this->identifier_escape_sequences) {
    reporter->report(diag_keywords_cannot_contain_escape_sequences{
        .escape_sequence = escape_sequence});
  }
}

void token::report_errors_for_escape_sequences_in_template(
    diag_reporter* reporter) const {
  QLJS_ASSERT(this->type == token_type::complete_template ||
              this->type == token_type::incomplete_template);
  if (this->template_escape_sequence_diagnostics) {
    this->template_escape_sequence_diagnostics->move_into(reporter);
  }
}

const char* to_string(token_type type) {
#define QLJS_CASE(t)  \
  case token_type::t: \
    return #t;
  switch (type) {
    QLJS_CASE(ampersand)
    QLJS_CASE(ampersand_ampersand)
    QLJS_CASE(ampersand_ampersand_equal)
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
    QLJS_CASE(kw_abstract)
    QLJS_CASE(kw_any)
    QLJS_CASE(kw_as)
    QLJS_CASE(kw_assert)
    QLJS_CASE(kw_asserts)
    QLJS_CASE(kw_async)
    QLJS_CASE(kw_await)
    QLJS_CASE(kw_bigint)
    QLJS_CASE(kw_boolean)
    QLJS_CASE(kw_break)
    QLJS_CASE(kw_case)
    QLJS_CASE(kw_catch)
    QLJS_CASE(kw_class)
    QLJS_CASE(kw_const)
    QLJS_CASE(kw_constructor)
    QLJS_CASE(kw_continue)
    QLJS_CASE(kw_debugger)
    QLJS_CASE(kw_declare)
    QLJS_CASE(kw_default)
    QLJS_CASE(kw_delete)
    QLJS_CASE(kw_do)
    QLJS_CASE(kw_else)
    QLJS_CASE(kw_enum)
    QLJS_CASE(kw_export)
    QLJS_CASE(kw_extends)
    QLJS_CASE(kw_false)
    QLJS_CASE(kw_finally)
    QLJS_CASE(kw_for)
    QLJS_CASE(kw_from)
    QLJS_CASE(kw_function)
    QLJS_CASE(kw_get)
    QLJS_CASE(kw_global)
    QLJS_CASE(kw_if)
    QLJS_CASE(kw_implements)
    QLJS_CASE(kw_import)
    QLJS_CASE(kw_in)
    QLJS_CASE(kw_infer)
    QLJS_CASE(kw_instanceof)
    QLJS_CASE(kw_interface)
    QLJS_CASE(kw_intrinsic)
    QLJS_CASE(kw_is)
    QLJS_CASE(kw_keyof)
    QLJS_CASE(kw_let)
    QLJS_CASE(kw_module)
    QLJS_CASE(kw_namespace)
    QLJS_CASE(kw_never)
    QLJS_CASE(kw_new)
    QLJS_CASE(kw_null)
    QLJS_CASE(kw_number)
    QLJS_CASE(kw_object)
    QLJS_CASE(kw_of)
    QLJS_CASE(kw_out)
    QLJS_CASE(kw_override)
    QLJS_CASE(kw_package)
    QLJS_CASE(kw_private)
    QLJS_CASE(kw_protected)
    QLJS_CASE(kw_public)
    QLJS_CASE(kw_readonly)
    QLJS_CASE(kw_require)
    QLJS_CASE(kw_return)
    QLJS_CASE(kw_set)
    QLJS_CASE(kw_static)
    QLJS_CASE(kw_string)
    QLJS_CASE(kw_super)
    QLJS_CASE(kw_switch)
    QLJS_CASE(kw_symbol)
    QLJS_CASE(kw_this)
    QLJS_CASE(kw_throw)
    QLJS_CASE(kw_true)
    QLJS_CASE(kw_try)
    QLJS_CASE(kw_type)
    QLJS_CASE(kw_typeof)
    QLJS_CASE(kw_undefined)
    QLJS_CASE(kw_unique)
    QLJS_CASE(kw_unknown)
    QLJS_CASE(kw_var)
    QLJS_CASE(kw_void)
    QLJS_CASE(kw_while)
    QLJS_CASE(kw_with)
    QLJS_CASE(kw_yield)
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
    QLJS_CASE(pipe_pipe_equal)
    QLJS_CASE(plus)
    QLJS_CASE(plus_equal)
    QLJS_CASE(plus_plus)
    QLJS_CASE(private_identifier)
    QLJS_CASE(question)
    QLJS_CASE(question_dot)
    QLJS_CASE(question_question)
    QLJS_CASE(question_question_equal)
    QLJS_CASE(regexp)
    QLJS_CASE(reserved_keyword_with_escape_sequence)
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
