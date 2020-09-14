// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>
#include <sstream>

namespace quick_lint_js {
namespace {
class test_text_error_reporter : public ::testing::Test {
 protected:
  text_error_reporter make_reporter() {
    return text_error_reporter(this->stream_);
  }

  text_error_reporter make_reporter(padded_string_view input) {
    text_error_reporter reporter(this->stream_);
    reporter.set_source(input, this->file_path_);
    return reporter;
  }

  std::string get_output() { return this->stream_.str(); }

 private:
  std::ostringstream stream_;
  static constexpr const char *file_path_ = "FILE";
};

TEST_F(test_text_error_reporter, change_source) {
  text_error_reporter reporter = this->make_reporter();

  padded_string input_1(u8"aaaaaaaa");
  reporter.set_source(&input_1, /*file_name=*/"hello.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_1[4 - 1], &input_1[4 - 1]))});

  padded_string input_2(u8"bbbbbbbb");
  reporter.set_source(&input_2, /*file_name=*/"world.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_2[5 - 1], &input_2[5 - 1]))});

  EXPECT_EQ(this->get_output(),
            "hello.js:1:4: error: assignment to const global variable\n"
            "world.js:1:5: error: assignment to const global variable\n");
}

TEST_F(test_text_error_reporter, assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;");
  source_code_span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x");
  source_code_span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x");

  this->make_reporter(&input).report(
      error_assignment_before_variable_declaration{
          .assignment = identifier(assignment_span),
          .declaration = identifier(declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: variable assigned before its declaration\n"
            "FILE:1:9: note: variable declared here\n");
}

TEST_F(test_text_error_reporter, assignment_to_const_global_variable) {
  padded_string input(u8"to Infinity and beyond");
  source_code_span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity");

  this->make_reporter(&input).report(
      error_assignment_to_const_global_variable{identifier(infinity_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:4: error: assignment to const global variable\n");
}

TEST_F(test_text_error_reporter, assignment_to_const_variable) {
  padded_string input(u8"const x=0;x=1;");
  source_code_span x_declaration_span(&input[7 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(x_declaration_span.string_view(), u8"x");
  source_code_span x_assignment_span(&input[11 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(x_assignment_span.string_view(), u8"x");

  this->make_reporter(&input).report(error_assignment_to_const_variable{
      identifier(x_declaration_span), identifier(x_assignment_span),
      variable_kind::_const});
  EXPECT_EQ(this->get_output(),
            "FILE:1:11: error: assignment to const variable\n"
            "FILE:1:7: note: const variable declared here\n");
}

TEST_F(test_text_error_reporter, assignment_to_undeclared_variable) {
  padded_string input(u8"uhoh=true;");
  source_code_span uhoh_span(&input[1 - 1], &input[4 + 1 - 1]);
  ASSERT_EQ(uhoh_span.string_view(), u8"uhoh");

  this->make_reporter(&input).report(
      error_assignment_to_undeclared_variable{identifier(uhoh_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: assignment to undeclared variable\n");
}

TEST_F(test_text_error_reporter, big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n");
  source_code_span number_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  this->make_reporter(&input).report(
      error_big_int_literal_contains_decimal_point{number_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: BigInt literal contains decimal point\n");
}

TEST_F(test_text_error_reporter, big_int_literal_contains_exponent) {
  padded_string input(u8"9e9n");
  source_code_span number_span(&input[1 - 1], &input[4 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"9e9n");

  this->make_reporter(&input).report(
      error_big_int_literal_contains_exponent{number_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: BigInt literal contains exponent\n");
}

TEST_F(test_text_error_reporter, big_int_literal_contains_leading_zero) {
  padded_string input(u8"080085n");
  source_code_span number_span(&input[1 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"080085n");

  this->make_reporter(&input).report(
      error_big_int_literal_contains_leading_zero{number_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: BigInt literal has a leading 0 digit\n");
}

TEST_F(test_text_error_reporter, invalid_binding_in_let_statement) {
  padded_string input(u8"let 2 = 3;");
  source_code_span two_span(&input[5 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(two_span.string_view(), u8"2");

  this->make_reporter(&input).report(
      error_invalid_binding_in_let_statement{two_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:5: error: invalid binding in let statement\n");
}

TEST_F(test_text_error_reporter, invalid_expression_left_of_assignment) {
  padded_string input(u8"2 = 3;");
  source_code_span two_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(two_span.string_view(), u8"2");

  this->make_reporter(&input).report(
      error_invalid_expression_left_of_assignment{two_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: invalid expression left of assignment\n");
}

TEST_F(test_text_error_reporter, let_with_no_bindings) {
  padded_string input(u8"let;");
  source_code_span let_span(&input[1 - 1], &input[3 + 1 - 1]);
  ASSERT_EQ(let_span.string_view(), u8"let");

  this->make_reporter(&input).report(error_let_with_no_bindings{let_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: let with no bindings\n");
}

TEST_F(test_text_error_reporter, missing_comma_between_object_literal_entries) {
  padded_string input(u8"{k v}");
  source_code_span plus_span(&input[3 - 1], &input[3 - 1]);
  ASSERT_EQ(plus_span.string_view(), u8"")
      << "span should be empty because the inserted comma does not exist "
         "in the source code";

  this->make_reporter(&input).report(
      error_missing_comma_between_object_literal_entries{plus_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:3: error: missing comma between object literal entries\n");
}

TEST_F(test_text_error_reporter, missing_operand_for_operator) {
  padded_string input(u8"2 + ");
  source_code_span plus_span(&input[3 - 1], &input[3 + 1 - 1]);
  ASSERT_EQ(plus_span.string_view(), u8"+");

  this->make_reporter(&input).report(
      error_missing_operand_for_operator{plus_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:3: error: missing operand for operator\n");
}

TEST_F(test_text_error_reporter, missing_semicolon_after_expression) {
  padded_string input(u8"a() b()");
  source_code_span inserted_semicolon_span(&input[4 - 1], &input[4 - 1]);
  ASSERT_EQ(inserted_semicolon_span.string_view(), u8"")
      << "span should be empty because the inserted semicolon does not exist "
         "in the source code";

  this->make_reporter(&input).report(
      error_missing_semicolon_after_expression{inserted_semicolon_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:4: error: missing semicolon after expression\n");
}

TEST_F(test_text_error_reporter, redeclaration_of_global_variable) {
  padded_string input(u8"let require");
  source_code_span declaration_span(&input[5 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"require");

  this->make_reporter(&input).report(
      error_redeclaration_of_global_variable{identifier(declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:5: error: redeclaration of global variable\n");
}

TEST_F(test_text_error_reporter, redeclaration_of_variable) {
  padded_string input(u8"let myvar; let myvar;");
  source_code_span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  this->make_reporter(&input).report(error_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:16: error: redeclaration of variable: myvar\n"
            "FILE:1:5: note: variable already declared here\n");
}

TEST_F(test_text_error_reporter, stray_comma_in_let_statement) {
  padded_string input(u8"let x , , y");
  source_code_span comma_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(comma_span.string_view(), u8",");

  this->make_reporter(&input).report(
      error_stray_comma_in_let_statement{comma_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:9: error: stray comma in let statement\n");
}

TEST_F(test_text_error_reporter, unclosed_block_comment) {
  padded_string input(u8"/* hello");
  source_code_span comment_span(&input[1 - 1], &input[8 + 1 - 1]);
  ASSERT_EQ(comment_span.string_view(), input);

  this->make_reporter(&input).report(
      error_unclosed_block_comment{.comment_open = comment_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unclosed block comment\n");
}

TEST_F(test_text_error_reporter, unclosed_regexp_literal) {
  padded_string input(u8"/hello");
  source_code_span regexp_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(regexp_span.string_view(), input);

  this->make_reporter(&input).report(
      error_unclosed_regexp_literal{regexp_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unclosed regexp literal\n");
}

TEST_F(test_text_error_reporter, unclosed_string_literal) {
  padded_string input(u8"'hello");
  source_code_span string_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(string_span.string_view(), input);

  this->make_reporter(&input).report(
      error_unclosed_string_literal{string_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unclosed string literal\n");
}

TEST_F(test_text_error_reporter, unclosed_template) {
  padded_string input(u8"`hello");
  source_code_span string_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(string_span.string_view(), input);

  this->make_reporter(&input).report(error_unclosed_template{string_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unclosed template\n");
}

TEST_F(test_text_error_reporter, unexpected_characters_in_number) {
  padded_string input(u8"123loveme");
  source_code_span garbage_span(&input[4 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(garbage_span.string_view(), u8"loveme");

  this->make_reporter(&input).report(
      error_unexpected_characters_in_number{garbage_span});
  EXPECT_EQ(this->get_output(),
            "FILE:1:4: error: unexpected characters in number literal\n");
}

TEST_F(test_text_error_reporter, unexpected_hash_character) {
  padded_string input(u8"#");
  source_code_span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#");

  this->make_reporter(&input).report(
      error_unexpected_hash_character{hash_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unexpected '#'\n");
}

TEST_F(test_text_error_reporter, unexpected_identifier) {
  padded_string input(u8"let x y");
  source_code_span y_span(&input[7 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(y_span.string_view(), u8"y");

  this->make_reporter(&input).report(error_unexpected_identifier{y_span});
  EXPECT_EQ(this->get_output(), "FILE:1:7: error: unexpected identifier\n");
}

TEST_F(test_text_error_reporter, unmatched_parenthesis) {
  padded_string input(u8"x)");
  source_code_span paren_span(&input[2 - 1], &input[2 + 1 - 1]);
  ASSERT_EQ(paren_span.string_view(), u8")");

  this->make_reporter(&input).report(error_unmatched_parenthesis{paren_span});
  EXPECT_EQ(this->get_output(), "FILE:1:2: error: unmatched parenthesis\n");
}

TEST_F(test_text_error_reporter, use_of_undeclared_variable) {
  padded_string input(u8"myvar;");
  source_code_span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  this->make_reporter(&input).report(
      error_use_of_undeclared_variable{identifier(myvar_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: use of undeclared variable: myvar\n");
}

TEST_F(test_text_error_reporter, variable_used_before_declaration) {
  padded_string input(u8"myvar; let myvar;");
  source_code_span use_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(use_span.string_view(), u8"myvar");
  source_code_span declaration_span(&input[12 - 1], &input[16 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"myvar");

  this->make_reporter(&input).report(error_variable_used_before_declaration{
      identifier(use_span), identifier(declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: variable used before declaration: myvar\n"
            "FILE:1:12: note: variable declared here\n");
}

TEST(test_text_error_formatter, single_span_simple_message) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator)
      .error(u8"something happened", source_code_span(&code[0], &code[5]))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: something happened\n");
}

TEST(test_text_error_formatter, message_with_note) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator)
      .error(u8"something happened", source_code_span(&code[0], &code[5]))
      .note(u8"see here", source_code_span(&code[6], &code[11]))
      .end();

  EXPECT_EQ(stream.str(),
            "FILE:1:1: error: something happened\nFILE:1:7: note: see here\n");
}

TEST(test_text_error_formatter, message_with_zero_placeholder) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator)
      .error(u8"this {0} looks fishy", source_code_span(&code[0], &code[5]))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: this hello looks fishy\n");
}

TEST(test_text_error_formatter, message_with_extra_identifier_placeholder) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator)
      .error(u8"this {1} looks fishy", source_code_span(&code[0], &code[5]),
             identifier(source_code_span(&code[6], &code[11])))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: this world looks fishy\n");
}

TEST(test_text_error_formatter, message_with_multiple_span_placeholders) {
  padded_string code(u8"let me = be(free);");
  quick_lint_js::locator locator(&code);
  source_code_span let_span(&code[0], &code[3]);
  ASSERT_EQ(let_span.string_view(), u8"let");
  source_code_span me_span(&code[4], &code[6]);
  ASSERT_EQ(me_span.string_view(), u8"me");
  source_code_span be_span(&code[9], &code[11]);
  ASSERT_EQ(be_span.string_view(), u8"be");

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator)
      .error(u8"free {1} and {0} {1} {2}", let_span, me_span, be_span)
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: free me and let me be\n");
}
}
}
