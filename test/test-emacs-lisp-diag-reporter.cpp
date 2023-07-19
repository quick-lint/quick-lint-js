// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/cli/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/cli/emacs-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
class Test_Emacs_Lisp_Diag_Reporter : public ::testing::Test {
 protected:
  Emacs_Lisp_Diag_Reporter make_reporter(Padded_String_View input) {
    Emacs_Lisp_Diag_Reporter reporter(Translator(), &this->stream_);
    reporter.set_source(input);
    return reporter;
  }

  String8 get_output() {
    this->stream_.flush();
    return this->stream_.get_flushed_string8();
  }

 private:
  Memory_Output_Stream stream_;
};

TEST_F(Test_Emacs_Lisp_Diag_Reporter, assignment_before_variable_declaration) {
  Padded_String input(u8"x=0;let x;"_sv);
  Source_Code_Span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);
  Source_Code_Span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Assignment_Before_Variable_Declaration{
      .assignment = assignment_span, .declaration = declaration_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((1 . 2) 0 "E0001" "variable assigned before its declaration")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, assignment_to_const_global_variable) {
  Padded_String input(u8"to Infinity and beyond"_sv);
  Source_Code_Span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);

  reporter.report(Diag_Assignment_To_Const_Global_Variable{infinity_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((4 . 12) 0 "E0002" "assignment to const global variable")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter,
       expected_parenthesis_around_if_condition) {
  Padded_String input(u8"if cond) {}"_sv);
  Source_Code_Span parenthesis_span(&input[4 - 1], &input[4 - 1]);
  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Expected_Parenthesis_Around_If_Condition{
      .where = parenthesis_span,
      .token = '(',
  });
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((4 . 4) 0 "E0018" "if statement is missing '(' around condition")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, redeclaration_of_variable) {
  Padded_String input(u8"let myvar; let myvar;"_sv);
  Source_Code_Span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar"_sv);
  Source_Code_Span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Redeclaration_Of_Variable{redeclaration_span,
                                                 original_declaration_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((16 . 21) 0 "E0034" "redeclaration of variable: myvar")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter,
       redeclaration_of_variable_after_multi_byte) {
  Padded_String input(u8"/*\u263b*/let myvar; let myvar;"_sv);
  Source_Code_Span original_declaration_span(&input[11], &input[16]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar"_sv);
  Source_Code_Span redeclaration_span(&input[22], &input[27]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Redeclaration_Of_Variable{redeclaration_span,
                                                 original_declaration_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((21 . 26) 0 "E0034" "redeclaration of variable: myvar")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, unexpected_hash_character) {
  Padded_String input(u8"#"_sv);
  Source_Code_Span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Unexpected_Hash_Character{hash_span});
  reporter.finish();
  EXPECT_EQ(this->get_output(),
            u8R"--((((1 . 2) 0 "E0052" "unexpected '#'")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, use_of_undeclared_variable) {
  Padded_String input(u8"myvar;"_sv);
  Source_Code_Span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Use_Of_Undeclared_Variable{myvar_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((1 . 6) 2 "E0057" "use of undeclared variable: myvar")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter,
       use_of_undeclared_variable_after_multibyte) {
  Padded_String input(u8"/*\u2639*/myvar;"_sv);
  Source_Code_Span myvar_span(&input[7], &input[12]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar"_sv);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Use_Of_Undeclared_Variable{myvar_span});
  reporter.finish();
  EXPECT_EQ(
      this->get_output(),
      u8R"--((((6 . 11) 2 "E0057" "use of undeclared variable: myvar")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, blackslash_is_escaped) {
  Padded_String input(u8"hello\backslash"_sv);
  Source_Code_Span span(&input[5], &input[6]);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Unexpected_Backslash_In_Identifier{span});
  reporter.finish();
  EXPECT_EQ(this->get_output(),
            u8R"--((((6 . 7) 0 "E0043" "unexpected '\\' in identifier")))--");
}

TEST_F(Test_Emacs_Lisp_Diag_Reporter, double_quote_is_escaped) {
  Padded_String input(u8"import { x } ;"_sv);
  Source_Code_Span span(&input[12], &input[12]);

  Emacs_Lisp_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Expected_From_And_Module_Specifier{span});
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
