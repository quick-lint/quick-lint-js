// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
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

bool token::contains_escape_sequence() {
  return (this->normalized_identifier == identifier_name().span().string_view())
             ? false
             : true;
}

[[gnu::noinline]] const char* to_string(token_type type) {
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

#define QLJS_KEYWORD(k) QLJS_CASE(kw_##k)
    QLJS_X_KEYWORDS
#undef QLJS_KEYWORD
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
