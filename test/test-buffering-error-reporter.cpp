// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/padded-string.h>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
source_code_span span_of(const padded_string& code) {
  return source_code_span(code.data(), code.null_terminator());
}

TEST(test_buffering_error_reporter, buffers_all_visits) {
  padded_string let_code(u8"let"_sv);
  padded_string expression_code(u8"2+2==5"_sv);

  buffering_error_reporter error_reporter;
  error_reporter.report(error_let_with_no_bindings{.where = span_of(let_code)});
  error_reporter.report(error_expected_parenthesis_around_if_condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  error_collector collector;
  error_reporter.move_into(&collector);
  EXPECT_THAT(
      collector.errors,
      ElementsAre(ERROR_TYPE_FIELD(error_let_with_no_bindings, where,
                                   span_of(let_code)),
                  ERROR_TYPE_2_FIELDS(
                      error_expected_parenthesis_around_if_condition, where,
                      span_of(expression_code),  //
                      token, u8'(')));
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
