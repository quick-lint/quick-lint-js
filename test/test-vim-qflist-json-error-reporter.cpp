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

#include <gtest/gtest.h>
#include <json/reader.h>
#include <json/value.h>
#include <json/writer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <sstream>

namespace quick_lint_js {
namespace {
::Json::Value parse_json(std::stringstream &stream) {
  SCOPED_TRACE(stream.str());
  stream.seekg(0);
  ::Json::Value root;
  ::Json::CharReaderBuilder builder;
  builder.strictMode(&builder.settings_);
  ::Json::String errors;
  bool ok = ::Json::parseFromStream(builder, stream, &root, &errors);
  EXPECT_TRUE(ok) << errors;
  return root;
}

class test_vim_qflist_json_error_reporter : public ::testing::Test {
 protected:
  vim_qflist_json_error_reporter make_reporter() {
    return vim_qflist_json_error_reporter(this->stream_);
  }

  vim_qflist_json_error_reporter make_reporter(padded_string_view input,
                                               int vim_bufnr) {
    vim_qflist_json_error_reporter reporter(this->stream_);
    reporter.set_source(input, /*vim_bufnr=*/vim_bufnr);
    return reporter;
  }

  vim_qflist_json_error_reporter make_reporter(padded_string_view input,
                                               const char *file_name) {
    vim_qflist_json_error_reporter reporter(this->stream_);
    reporter.set_source(input, /*file_name=*/file_name);
    return reporter;
  }

  ::Json::Value parse_json() {
    ::Json::Value root = quick_lint_js::parse_json(this->stream_);
    this->stream_ = std::stringstream();
    return root;
  }

  std::stringstream stream_;
};

TEST_F(test_vim_qflist_json_error_reporter,
       big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n");
  source_code_span number_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 6);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "BigInt literal contains decimal point");
}

TEST_F(test_vim_qflist_json_error_reporter,
       big_int_literal_contains_leading_zero) {
  padded_string input(u8"080085n");
  source_code_span number_span(&input[1 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"080085n");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_big_int_literal_contains_leading_zero{number_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 7);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "BigInt literal has a leading 0 digit");
}

TEST_F(test_vim_qflist_json_error_reporter,
       assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;");
  source_code_span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x");
  source_code_span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_assignment_before_variable_declaration{
      .assignment = identifier(assignment_span),
      .declaration = identifier(declaration_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 1);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "variable assigned before its declaration");
}

TEST_F(test_vim_qflist_json_error_reporter, big_int_literal_contains_exponent) {
  padded_string input(u8"9e9n");
  source_code_span number_span(&input[1 - 1], &input[4 + 1 - 1]);
  ASSERT_EQ(number_span.string_view(), u8"9e9n");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_big_int_literal_contains_exponent{number_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 4);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "BigInt literal contains exponent");
}

TEST_F(test_vim_qflist_json_error_reporter, multiple_errors) {
  padded_string input(u8"abc");
  source_code_span a_span(&input[0], &input[1]);
  source_code_span b_span(&input[1], &input[2]);
  source_code_span c_span(&input[2], &input[3]);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(a_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(b_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(c_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 3);
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_buffer_number_if_requested) {
  padded_string input(u8"");
  source_code_span span(&input[0], &input[0]);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(error_assignment_to_const_global_variable{identifier(span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["bufnr"], 42);
  EXPECT_FALSE(qflist[0].isMember("filename"));
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_file_name_if_requested) {
  padded_string input(u8"");
  source_code_span span(&input[0], &input[0]);

  for (const char *file_name : {"hello.js", "file\\name\\with\\backslashes.js",
                                "file\"name\'with\nfunky\tcharacters"}) {
    SCOPED_TRACE(file_name);

    vim_qflist_json_error_reporter reporter =
        this->make_reporter(&input, /*file_name=*/file_name);
    reporter.report(
        error_assignment_to_const_global_variable{identifier(span)});
    reporter.finish();

    ::Json::Value qflist = this->parse_json()["qflist"];
    ASSERT_EQ(qflist.size(), 1);
    EXPECT_EQ(qflist[0]["filename"], file_name);
    EXPECT_FALSE(qflist[0].isMember("bufnr"));
  }
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_file_name_and_buffer_number_if_requested) {
  padded_string input(u8"");
  source_code_span span(&input[0], &input[0]);

  vim_qflist_json_error_reporter reporter = this->make_reporter();
  reporter.set_source(&input, /*file_name=*/"hello.js", /*vim_bufnr=*/1337);
  reporter.report(error_assignment_to_const_global_variable{identifier(span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["bufnr"], 1337);
  EXPECT_EQ(qflist[0]["filename"], "hello.js");
}

TEST_F(test_vim_qflist_json_error_reporter, change_source) {
  vim_qflist_json_error_reporter reporter = this->make_reporter();

  padded_string input_1(u8"aaaaaaaa");
  reporter.set_source(&input_1, /*file_name=*/"hello.js", /*vim_bufnr=*/1);
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_1[4 - 1], &input_1[4 - 1]))});

  padded_string input_2(u8"bbbbbbbb");
  reporter.set_source(&input_2, /*file_name=*/"world.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_2[5 - 1], &input_2[5 - 1]))});

  padded_string input_3(u8"cccccccc");
  reporter.set_source(&input_3, /*vim_bufnr=*/2);
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_3[6 - 1], &input_3[6 - 1]))});

  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 3);

  EXPECT_EQ(qflist[0]["bufnr"], 1);
  EXPECT_EQ(qflist[0]["col"], 4);
  EXPECT_EQ(qflist[0]["filename"], "hello.js");

  EXPECT_FALSE(qflist[1].isMember("bufnr"));
  EXPECT_EQ(qflist[1]["col"], 5);
  EXPECT_EQ(qflist[1]["filename"], "world.js");

  EXPECT_EQ(qflist[2]["bufnr"], 2);
  EXPECT_EQ(qflist[2]["col"], 6);
  EXPECT_FALSE(qflist[2].isMember("filename"));
}

TEST_F(test_vim_qflist_json_error_reporter,
       assignment_to_const_global_variable) {
  padded_string input(u8"to Infinity and beyond");
  source_code_span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(infinity_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 4);
  EXPECT_EQ(qflist[0]["end_col"], 11);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "assignment to const global variable");
  EXPECT_EQ(qflist[0]["vcol"], 0);
}

TEST_F(test_vim_qflist_json_error_reporter, assignment_to_const_variable) {
  padded_string input(u8"const x=0;x=1;");
  source_code_span x_declaration_span(&input[7 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(x_declaration_span.string_view(), u8"x");
  source_code_span x_assignment_span(&input[11 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(x_assignment_span.string_view(), u8"x");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/69);
  reporter.report(error_assignment_to_const_variable{
      identifier(x_declaration_span), identifier(x_assignment_span),
      variable_kind::_const});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 11);
  EXPECT_EQ(qflist[0]["end_col"], 11);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "assignment to const variable");
  EXPECT_EQ(qflist[0]["vcol"], 0);
}

TEST_F(test_vim_qflist_json_error_reporter, assignment_to_undeclared_variable) {
  padded_string input(u8"uhoh=true;");
  source_code_span uhoh_span(&input[1 - 1], &input[4 + 1 - 1]);
  ASSERT_EQ(uhoh_span.string_view(), u8"uhoh");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(
      error_assignment_to_undeclared_variable{identifier(uhoh_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 4);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "assignment to undeclared variable");
}

TEST_F(test_vim_qflist_json_error_reporter, invalid_binding_in_let_statement) {
  padded_string input(u8"let 2 = 3;");
  source_code_span two_span(&input[5 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(two_span.string_view(), u8"2");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_invalid_binding_in_let_statement{two_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 5);
  EXPECT_EQ(qflist[0]["end_col"], 5);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "invalid binding in let statement");
}

TEST_F(test_vim_qflist_json_error_reporter,
       invalid_expression_left_of_assignment) {
  padded_string input(u8"2 = 3;");
  source_code_span two_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(two_span.string_view(), u8"2");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_invalid_expression_left_of_assignment{two_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 1);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "invalid expression left of assignment");
}

TEST_F(test_vim_qflist_json_error_reporter, let_with_no_bindings) {
  padded_string input(u8"let;");
  source_code_span let_span(&input[1 - 1], &input[3 + 1 - 1]);
  ASSERT_EQ(let_span.string_view(), u8"let");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_let_with_no_bindings{let_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 3);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "let with no bindings");
}

TEST_F(test_vim_qflist_json_error_reporter,
       missing_comma_between_object_literal_entries) {
  padded_string input(u8"{k v}");
  source_code_span plus_span(&input[3 - 1], &input[3 - 1]);
  ASSERT_EQ(plus_span.string_view(), u8"")
      << "span should be empty because the inserted comma does not exist "
         "in the source code";

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(
      error_missing_comma_between_object_literal_entries{plus_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 3);
  EXPECT_EQ(qflist[0]["end_col"], 3);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "missing comma between object literal entries");
}

TEST_F(test_vim_qflist_json_error_reporter, missing_operand_for_operator) {
  padded_string input(u8"2 + ");
  source_code_span plus_span(&input[3 - 1], &input[3 + 1 - 1]);
  ASSERT_EQ(plus_span.string_view(), u8"+");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_missing_operand_for_operator{plus_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 3);
  EXPECT_EQ(qflist[0]["end_col"], 3);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "missing operand for operator");
}

TEST_F(test_vim_qflist_json_error_reporter,
       missing_semicolon_after_expression) {
  padded_string input(u8"a() b()");
  source_code_span inserted_semicolon_span(&input[4 - 1], &input[4 - 1]);
  ASSERT_EQ(inserted_semicolon_span.string_view(), u8"")
      << "span should be empty because the inserted semicolon does not exist "
         "in the source code";

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(
      error_missing_semicolon_after_expression{inserted_semicolon_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 4);
  EXPECT_EQ(qflist[0]["end_col"], 4);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "missing semicolon after expression");
}

TEST_F(test_vim_qflist_json_error_reporter, redeclaration_of_global_variable) {
  padded_string input(u8"let require");
  source_code_span declaration_span(&input[5 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"require");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(
      error_redeclaration_of_global_variable{identifier(declaration_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 5);
  EXPECT_EQ(qflist[0]["end_col"], 11);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "redeclaration of global variable");
}

TEST_F(test_vim_qflist_json_error_reporter, redeclaration_of_variable) {
  padded_string input(u8"let myvar; let myvar;");
  source_code_span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 16);
  EXPECT_EQ(qflist[0]["end_col"], 20);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "redeclaration of variable: myvar");
}

TEST_F(test_vim_qflist_json_error_reporter, stray_comma_in_let_statement) {
  padded_string input(u8"let x , , y");
  source_code_span comma_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(comma_span.string_view(), u8",");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_stray_comma_in_let_statement{comma_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 9);
  EXPECT_EQ(qflist[0]["end_col"], 9);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "stray comma in let statement");
}

TEST_F(test_vim_qflist_json_error_reporter, unclosed_block_comment) {
  padded_string input(u8"/* hello");
  source_code_span comment_span(&input[1 - 1], &input[8 + 1 - 1]);
  ASSERT_EQ(comment_span.string_view(), input);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unclosed_block_comment{.comment_open = comment_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 8);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unclosed block comment");
}

TEST_F(test_vim_qflist_json_error_reporter, unclosed_regexp_literal) {
  padded_string input(u8"/hello");
  source_code_span regexp_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(regexp_span.string_view(), input);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unclosed_regexp_literal{regexp_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 6);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unclosed regexp literal");
}

TEST_F(test_vim_qflist_json_error_reporter, unclosed_string_literal) {
  padded_string input(u8"'hello");
  source_code_span string_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(string_span.string_view(), input);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unclosed_string_literal{string_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 6);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unclosed string literal");
}

TEST_F(test_vim_qflist_json_error_reporter, unclosed_template) {
  padded_string input(u8"`hello");
  source_code_span string_span(&input[1 - 1], &input[6 + 1 - 1]);
  ASSERT_EQ(string_span.string_view(), input);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unclosed_template{string_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 6);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unclosed template");
}

TEST_F(test_vim_qflist_json_error_reporter, unexpected_characters_in_number) {
  padded_string input(u8"123loveme");
  source_code_span garbage_span(&input[4 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(garbage_span.string_view(), u8"loveme");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unexpected_characters_in_number{garbage_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 4);
  EXPECT_EQ(qflist[0]["end_col"], 9);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unexpected characters in number literal");
}

TEST_F(test_vim_qflist_json_error_reporter, unexpected_hash_character) {
  padded_string input(u8"#");
  source_code_span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unexpected_hash_character{hash_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 1);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unexpected '#'");
}

TEST_F(test_vim_qflist_json_error_reporter, unexpected_identifier) {
  padded_string input(u8"let x y");
  source_code_span y_span(&input[7 - 1], &input[7 + 1 - 1]);
  ASSERT_EQ(y_span.string_view(), u8"y");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unexpected_identifier{y_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 7);
  EXPECT_EQ(qflist[0]["end_col"], 7);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unexpected identifier");
}

TEST_F(test_vim_qflist_json_error_reporter, unmatched_parenthesis) {
  padded_string input(u8"x)");
  source_code_span paren_span(&input[2 - 1], &input[2 + 1 - 1]);
  ASSERT_EQ(paren_span.string_view(), u8")");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unmatched_parenthesis{paren_span});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 2);
  EXPECT_EQ(qflist[0]["end_col"], 2);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "unmatched parenthesis");
}

TEST_F(test_vim_qflist_json_error_reporter, use_of_undeclared_variable) {
  padded_string input(u8"myvar;");
  source_code_span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_use_of_undeclared_variable{identifier(myvar_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 5);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "use of undeclared variable: myvar");
}

TEST_F(test_vim_qflist_json_error_reporter, variable_used_before_declaration) {
  padded_string input(u8"myvar; let myvar;");
  source_code_span use_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(use_span.string_view(), u8"myvar");
  source_code_span declaration_span(&input[12 - 1], &input[16 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"myvar");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_variable_used_before_declaration{
      identifier(use_span), identifier(declaration_span)});
  reporter.finish();

  ::Json::Value qflist = this->parse_json()["qflist"];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0]["col"], 1);
  EXPECT_EQ(qflist[0]["end_col"], 5);
  EXPECT_EQ(qflist[0]["end_lnum"], 1);
  EXPECT_EQ(qflist[0]["lnum"], 1);
  EXPECT_EQ(qflist[0]["text"], "variable used before declaration: myvar");
}

TEST(test_vim_qflist_json_error_formatter, single_span_simple_message) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::stringstream stream;
  vim_qflist_json_error_formatter(stream, locator, "FILE",
                                  /*bufnr=*/std::string_view())
      .error(u8"something happened", source_code_span(&code[0], &code[5]))
      .end();

  ::Json::Value object = parse_json(stream);
  EXPECT_EQ(object["col"], 1);
  EXPECT_EQ(object["end_col"], 5);
  EXPECT_EQ(object["end_lnum"], 1);
  EXPECT_EQ(object["lnum"], 1);
  EXPECT_EQ(object["text"], "something happened");
}

TEST(test_vim_qflist_json_error_formatter, message_with_note_ignores_note) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::stringstream stream;
  vim_qflist_json_error_formatter(stream, locator, "FILE",
                                  /*bufnr=*/std::string_view())
      .error(u8"something happened", source_code_span(&code[0], &code[5]))
      .note(u8"see here", source_code_span(&code[6], &code[11]))
      .end();

  ::Json::Value object = parse_json(stream);
  EXPECT_EQ(object["col"], 1);
  EXPECT_EQ(object["end_col"], 5);
  EXPECT_EQ(object["end_lnum"], 1);
  EXPECT_EQ(object["lnum"], 1);
  EXPECT_EQ(object["text"], "something happened");
}

TEST(test_vim_qflist_json_error_formatter, message_with_zero_placeholder) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::stringstream stream;
  vim_qflist_json_error_formatter(stream, locator, "FILE",
                                  /*bufnr=*/std::string_view())
      .error(u8"this {0} looks fishy", source_code_span(&code[0], &code[5]))
      .end();

  ::Json::Value object = parse_json(stream);
  EXPECT_EQ(object["text"], "this hello looks fishy");
}

TEST(test_vim_qflist_json_error_formatter,
     message_with_extra_identifier_placeholder) {
  padded_string code(u8"hello world");
  quick_lint_js::locator locator(&code);

  std::stringstream stream;
  vim_qflist_json_error_formatter(stream, locator, "FILE",
                                  /*bufnr=*/std::string_view())
      .error(u8"this {1} looks fishy", source_code_span(&code[0], &code[5]),
             identifier(source_code_span(&code[6], &code[11])))
      .end();

  ::Json::Value object = parse_json(stream);
  EXPECT_EQ(object["text"], "this world looks fishy");
}

TEST(test_vim_qflist_json_error_formatter,
     message_with_multiple_span_placeholders) {
  padded_string code(u8"let me = be(free);");
  quick_lint_js::locator locator(&code);
  source_code_span let_span(&code[0], &code[3]);
  ASSERT_EQ(let_span.string_view(), u8"let");
  source_code_span me_span(&code[4], &code[6]);
  ASSERT_EQ(me_span.string_view(), u8"me");
  source_code_span be_span(&code[9], &code[11]);
  ASSERT_EQ(be_span.string_view(), u8"be");

  std::stringstream stream;
  vim_qflist_json_error_formatter(stream, locator, "FILE",
                                  /*bufnr=*/std::string_view())
      .error(u8"free {1} and {0} {1} {2}", let_span, me_span, be_span)
      .end();

  ::Json::Value object = parse_json(stream);
  EXPECT_EQ(object["text"], "free me and let me be");
}
}
}
