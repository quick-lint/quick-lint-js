// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/wasm-demo-error-reporter.h>

namespace quick_lint_js {
namespace {
TEST(test_wasm_demo_error_reporter, big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n"_sv);
  source_code_span number_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  wasm_demo_error_reporter reporter(&input);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});

  const wasm_demo_error_reporter::error *errors = reporter.get_errors();
  EXPECT_EQ(errors[0].begin_offset, 0);
  EXPECT_EQ(errors[0].end_offset, 6);
  EXPECT_EQ(errors[0].message,
            string8_view(u8"BigInt literal contains decimal point"));
  EXPECT_EQ(errors[1].message, nullptr);
}

TEST(test_wasm_demo_error_reporter, error_after_non_ascii_characters) {
  padded_string input(u8"\u2603 12.34n"_sv);
  source_code_span number_span(&input[4], &input[4 + 6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  wasm_demo_error_reporter reporter(&input);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});

  const wasm_demo_error_reporter::error *errors = reporter.get_errors();
  EXPECT_EQ(errors[0].begin_offset, 2);
  EXPECT_EQ(errors[0].end_offset, 2 + 6);
}

TEST(test_wasm_demo_error_reporter, multiple_errors) {
  padded_string input(u8"abc"_sv);
  source_code_span a_span(&input[0], &input[1]);
  source_code_span b_span(&input[1], &input[2]);
  source_code_span c_span(&input[2], &input[3]);

  wasm_demo_error_reporter reporter(&input);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(a_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(b_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(c_span)});

  const wasm_demo_error_reporter::error *errors = reporter.get_errors();

  EXPECT_EQ(errors[0].begin_offset, 0);
  EXPECT_EQ(errors[0].end_offset, 1);
  EXPECT_NE(errors[0].message, nullptr);

  EXPECT_EQ(errors[1].begin_offset, 1);
  EXPECT_EQ(errors[1].end_offset, 2);
  EXPECT_NE(errors[1].message, nullptr);

  EXPECT_EQ(errors[2].begin_offset, 2);
  EXPECT_EQ(errors[2].end_offset, 3);
  EXPECT_NE(errors[2].message, nullptr);

  EXPECT_EQ(errors[3].message, nullptr);
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
