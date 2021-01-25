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
#include <quick-lint-js/cli-location.h>
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

  padded_string input_1(u8"aaaaaaaa"_sv);
  reporter.set_source(&input_1, /*file_name=*/"hello.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_1[4 - 1], &input_1[4 - 1]))});

  padded_string input_2(u8"bbbbbbbb"_sv);
  reporter.set_source(&input_2, /*file_name=*/"world.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_2[5 - 1], &input_2[5 - 1]))});

  EXPECT_EQ(
      this->get_output(),
      "hello.js:1:4: error: assignment to const global variable [E002]\n"
      "world.js:1:5: error: assignment to const global variable [E002]\n");
}

TEST_F(test_text_error_reporter, assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;"_sv);
  source_code_span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x");
  source_code_span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x");

  this->make_reporter(&input).report(
      error_assignment_before_variable_declaration{
          .assignment = identifier(assignment_span),
          .declaration = identifier(declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: error: variable assigned before its declaration [E001]\n"
            "FILE:1:9: note: variable declared here [E001]\n");
}

TEST_F(test_text_error_reporter, assignment_to_const_global_variable) {
  padded_string input(u8"to Infinity and beyond"_sv);
  source_code_span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity");

  this->make_reporter(&input).report(
      error_assignment_to_const_global_variable{identifier(infinity_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:4: error: assignment to const global variable [E002]\n");
}

TEST_F(test_text_error_reporter, expected_parenthesis_around_if_condition) {
  padded_string input(u8"if cond) {}"_sv);
  source_code_span parenthesis_span(&input[4 - 1], &input[4 - 1]);

  this->make_reporter(&input).report(
      error_expected_parenthesis_around_if_condition{
          .where = parenthesis_span,
          .token = '(',
      });
  EXPECT_EQ(
      this->get_output(),
      "FILE:1:4: error: if statement is missing '(' around condition [E018]\n");
}

TEST_F(test_text_error_reporter, redeclaration_of_variable) {
  padded_string input(u8"let myvar; let myvar;"_sv);
  source_code_span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  this->make_reporter(&input).report(error_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:16: error: redeclaration of variable: myvar [E034]\n"
            "FILE:1:5: note: variable already declared here [E034]\n");
}

TEST_F(test_text_error_reporter, unexpected_hash_character) {
  padded_string input(u8"#"_sv);
  source_code_span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#");

  this->make_reporter(&input).report(
      error_unexpected_hash_character{hash_span});
  EXPECT_EQ(this->get_output(), "FILE:1:1: error: unexpected '#' [E052]\n");
}

TEST_F(test_text_error_reporter, use_of_undeclared_variable) {
  padded_string input(u8"myvar;"_sv);
  source_code_span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  this->make_reporter(&input).report(
      error_use_of_undeclared_variable{identifier(myvar_span)});
  EXPECT_EQ(this->get_output(),
            "FILE:1:1: warning: use of undeclared variable: myvar [E057]\n");
}

TEST(test_text_error_formatter, single_span_simple_message) {
  padded_string code(u8"hello world"_sv);
  cli_locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator, "E999")
      .error("something happened"_gmo_message,
             source_code_span(&code[0], &code[5]))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: something happened [E999]\n");
}

TEST(test_text_error_formatter, message_with_note) {
  padded_string code(u8"hello world"_sv);
  cli_locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator, "E999")
      .error("something happened"_gmo_message,
             source_code_span(&code[0], &code[5]))
      .note("see here"_gmo_message, source_code_span(&code[6], &code[11]))
      .end();

  EXPECT_EQ(stream.str(),
            "FILE:1:1: error: something happened [E999]\nFILE:1:7: note: see "
            "here [E999]\n");
}

TEST(test_text_error_formatter, message_with_zero_placeholder) {
  padded_string code(u8"hello world"_sv);
  cli_locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator, "E888")
      .error("this {0} looks fishy"_gmo_message,
             source_code_span(&code[0], &code[5]))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: this hello looks fishy [E888]\n");
}

TEST(test_text_error_formatter, message_with_extra_identifier_placeholder) {
  padded_string code(u8"hello world"_sv);
  cli_locator locator(&code);

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator, "E888")
      .error("this {1} looks fishy"_gmo_message,
             source_code_span(&code[0], &code[5]),
             identifier(source_code_span(&code[6], &code[11])))
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: this world looks fishy [E888]\n");
}

TEST(test_text_error_formatter, message_with_multiple_span_placeholders) {
  padded_string code(u8"let me = be(free);"_sv);
  cli_locator locator(&code);
  source_code_span let_span(&code[0], &code[3]);
  ASSERT_EQ(let_span.string_view(), u8"let");
  source_code_span me_span(&code[4], &code[6]);
  ASSERT_EQ(me_span.string_view(), u8"me");
  source_code_span be_span(&code[9], &code[11]);
  ASSERT_EQ(be_span.string_view(), u8"be");

  std::ostringstream stream;
  text_error_formatter(stream, "FILE", locator, "E999")
      .error("free {1} and {0} {1} {2}"_gmo_message, let_span, me_span, be_span)
      .end();

  EXPECT_EQ(stream.str(), "FILE:1:1: error: free me and let me be [E999]\n");
}
}
}
