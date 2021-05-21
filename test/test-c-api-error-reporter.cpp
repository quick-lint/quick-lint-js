// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/c-api-error-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
namespace {
using vscode_error_reporter =
    c_api_error_reporter<qljs_vscode_diagnostic, lsp_locator>;

TEST(test_vscode_error_reporter, big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n"_sv);
  source_code_span number_span(&input[0], &input[6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  lsp_locator locator(&input);
  vscode_error_reporter reporter;
  reporter.set_input(&input, &locator);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});

  const qljs_vscode_diagnostic *errors = reporter.get_diagnostics();
  EXPECT_EQ(errors[0].start_line, 0);
  EXPECT_EQ(errors[0].start_character, 0);
  EXPECT_EQ(errors[0].end_line, 0);
  EXPECT_EQ(errors[0].end_character, 6);
  EXPECT_STREQ(errors[0].message, "BigInt literal contains decimal point");
  EXPECT_STREQ(errors[0].code, "E005");
  EXPECT_EQ(errors[1].message, nullptr);
}

TEST(test_vscode_error_reporter, diagnostic_notes_are_ignored) {
  padded_string input(u8"myvar; let myvar;"_sv);
  source_code_span use_span(&input[0], &input[5]);
  ASSERT_EQ(use_span.string_view(), u8"myvar");
  source_code_span declaration_span(&input[11], &input[16]);
  ASSERT_EQ(declaration_span.string_view(), u8"myvar");

  lsp_locator locator(&input);
  vscode_error_reporter reporter;
  reporter.set_input(&input, &locator);
  reporter.report(error_variable_used_before_declaration{
      identifier(use_span), identifier(declaration_span)});

  const qljs_vscode_diagnostic *errors = reporter.get_diagnostics();
  EXPECT_NE(errors[0].message, nullptr);
  EXPECT_EQ(errors[1].message, nullptr);
}

TEST(test_vscode_error_reporter, error_after_non_ascii_characters) {
  padded_string input(u8"\u2603 12.34n"_sv);
  source_code_span number_span(&input[4], &input[4 + 6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  lsp_locator locator(&input);
  vscode_error_reporter reporter;
  reporter.set_input(&input, &locator);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});

  const qljs_vscode_diagnostic *errors = reporter.get_diagnostics();
  EXPECT_EQ(errors[0].start_line, 0);
  EXPECT_EQ(errors[0].start_character, 2);
  EXPECT_EQ(errors[0].end_line, 0);
  EXPECT_EQ(errors[0].end_character, 2 + 6);
}

TEST(test_vscode_error_reporter, multiple_errors) {
  padded_string input(u8"abc"_sv);
  source_code_span a_span(&input[0], &input[1]);
  source_code_span b_span(&input[1], &input[2]);
  source_code_span c_span(&input[2], &input[3]);

  lsp_locator locator(&input);
  vscode_error_reporter reporter;
  reporter.set_input(&input, &locator);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(a_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(b_span)});
  reporter.report(error_use_of_undeclared_variable{identifier(c_span)});

  const qljs_vscode_diagnostic *errors = reporter.get_diagnostics();

  EXPECT_EQ(errors[0].start_character, 0);
  EXPECT_EQ(errors[0].end_character, 1);
  EXPECT_NE(errors[0].message, nullptr);
  EXPECT_EQ(errors[0].severity, qljs_severity_error);

  EXPECT_EQ(errors[1].start_character, 1);
  EXPECT_EQ(errors[1].end_character, 2);
  EXPECT_NE(errors[1].message, nullptr);
  EXPECT_EQ(errors[1].severity, qljs_severity_error);

  EXPECT_EQ(errors[2].start_character, 2);
  EXPECT_EQ(errors[2].end_character, 3);
  EXPECT_NE(errors[2].message, nullptr);
  EXPECT_EQ(errors[2].severity, qljs_severity_warning);

  EXPECT_EQ(errors[3].message, nullptr);
}

TEST(test_vscode_error_reporter, error_after_non_ascii) {
  padded_string input(u8"left \u2603 right"_sv);
  source_code_span right_span(
      &input[narrow_cast<int>(strlen(u8"left \u2603 "))],
      input.null_terminator());

  lsp_locator locator(&input);
  vscode_error_reporter reporter;
  reporter.set_input(&input, &locator);
  reporter.report(error_use_of_undeclared_variable{identifier(right_span)});

  const qljs_vscode_diagnostic *errors = reporter.get_diagnostics();
  EXPECT_EQ(errors[0].start_character, std::strlen("left _ "));
  EXPECT_EQ(errors[0].end_character, std::strlen("left _ right"));
}
}
}

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
