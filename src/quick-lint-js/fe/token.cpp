// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
Identifier Token::identifier_name() const {
  switch (this->type) {
  QLJS_CASE_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::private_identifier:
  case Token_Type::reserved_keyword_with_escape_sequence:
    break;
  default:
    QLJS_ASSERT(false);
    break;
  }
  return Identifier(this->span(),
                    /*normalized=*/this->normalized_identifier);
}

Source_Code_Span Token::span() const {
  return Source_Code_Span(this->begin, this->end);
}

void Token::report_errors_for_escape_sequences_in_keyword(
    Diag_Reporter* reporter) const {
  QLJS_ASSERT(this->type == Token_Type::reserved_keyword_with_escape_sequence);
  QLJS_ASSERT(this->identifier_escape_sequences);
  QLJS_ASSERT(!this->identifier_escape_sequences->empty());
  for (const Source_Code_Span& escape_sequence :
       *this->identifier_escape_sequences) {
    reporter->report(Diag_Keywords_Cannot_Contain_Escape_Sequences{
        .escape_sequence = escape_sequence});
  }
}

void Token::report_errors_for_escape_sequences_in_template(
    Diag_Reporter* reporter) const {
  QLJS_ASSERT(this->type == Token_Type::complete_template ||
              this->type == Token_Type::incomplete_template);
  if (this->template_escape_sequence_diagnostics) {
    reporter->report(*this->template_escape_sequence_diagnostics);
  }
}

bool Token::contains_escape_sequence() const {
  return this->normalized_identifier.data() != this->begin;
}

[[gnu::noinline]] const char* to_string(Token_Type type) {
#define QLJS_CASE(t)  \
  case Token_Type::t: \
    return #t;
  switch (type) {
    QLJS_CASE(ampersand)
    QLJS_CASE(ampersand_ampersand)
    QLJS_CASE(ampersand_ampersand_equal)
    QLJS_CASE(ampersand_equal)
    QLJS_CASE(at)
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
