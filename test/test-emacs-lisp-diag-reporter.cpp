// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
namespace {
class test_emacs_lisp_diag_reporter : public ::testing::Test {
 protected:
  emacs_lisp_diag_reporter make_reporter(padded_string_view input) {
    emacs_lisp_diag_reporter reporter(&this->stream_);
    reporter.set_source(input);
    return reporter;
  }

  string8 get_output() {
    this->stream_.flush();
    return this->stream_.get_flushed_string8();
  }

 private:
  memory_output_stream stream_;
};

TEST_F(test_emacs_lisp_diag_reporter, assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;"_sv);
  source_code_span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x");
  source_code_span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_assignment_before_variable_declaration{
      .assignment = identifier(assignment_span),
      .declaration = identifier(declaration_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((1 . 2) 0 "E0001" "variable assigned before its declaration")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, assignment_to_const_global_variable) {
  padded_string input(u8"to Infinity and beyond"_sv);
  source_code_span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);

  reporter.report(
      diag_assignment_to_const_global_variable{identifier(infinity_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((4 . 12) 0 "E0002" "assignment to const global variable")))--");
}

TEST_F(test_emacs_lisp_diag_reporter,
       expected_parenthesis_around_if_condition) {
  padded_string input(u8"if cond) {}"_sv);
  source_code_span parenthesis_span(&input[4 - 1], &input[4 - 1]);
  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_expected_parenthesis_around_if_condition{
      .where = parenthesis_span,
      .token = '(',
  });
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((4 . 4) 0 "E0018" "if statement is missing '(' around condition")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, redeclaration_of_variable) {
  padded_string input(u8"let myvar; let myvar;"_sv);
  source_code_span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((16 . 21) 0 "E0034" "redeclaration of variable: myvar")))--");
}

TEST_F(test_emacs_lisp_diag_reporter,
       redeclaration_of_variable_after_multi_byte) {
  padded_string input(u8"/*\u263b*/let myvar; let myvar;"_sv);
  source_code_span original_declaration_span(&input[11], &input[16]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[22], &input[27]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((21 . 26) 0 "E0034" "redeclaration of variable: myvar")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, unexpected_hash_character) {
  padded_string input(u8"#"_sv);
  source_code_span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_unexpected_hash_character{hash_span});
  reporter.finish();
  EXPECT_EQ(this->get_output(),
            u8R"--((((1 . 2) 0 "E0052" "unexpected '#'")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, use_of_undeclared_variable) {
  padded_string input(u8"myvar;"_sv);
  source_code_span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_use_of_undeclared_variable{identifier(myvar_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((1 . 6) 2 "E0057" "use of undeclared variable: myvar")))--");
}

TEST_F(test_emacs_lisp_diag_reporter,
       use_of_undeclared_variable_after_multibyte) {
  padded_string input(u8"/*\u2639*/myvar;"_sv);
  source_code_span myvar_span(&input[7], &input[12]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_use_of_undeclared_variable{identifier(myvar_span)});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((6 . 11) 2 "E0057" "use of undeclared variable: myvar")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, blackslash_is_escaped) {
  padded_string input(u8"hello\backslash"_sv);
  source_code_span span(&input[5], &input[6]);

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_unexpected_backslash_in_identifier{span});
  reporter.finish();
  EXPECT_EQ(this->get_output(),
            u8R"--((((6 . 7) 0 "E0043" "unexpected '\\' in identifier")))--");
}

TEST_F(test_emacs_lisp_diag_reporter, double_quote_is_escaped) {
  padded_string input(u8"import { x } ;"_sv);
  source_code_span span(&input[12], &input[12]);

  emacs_lisp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_expected_from_and_module_specifier{span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((13 . 13) 0 "E0129" "expected 'from \"name_of_module.mjs\"'")))--");
}
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
