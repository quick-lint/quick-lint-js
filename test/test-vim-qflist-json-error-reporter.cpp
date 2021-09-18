// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <json/reader.h>
#include <json/value.h>
#include <json/writer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-json.h>
#include <quick-lint-js/vim-location.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <sstream>

namespace quick_lint_js {
namespace {
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

  ::boost::json::value parse_json() {
    ::boost::json::value root = parse_boost_json(this->stream_.str());
    this->stream_ = std::stringstream();
    return root;
  }

  std::stringstream stream_;
};

TEST_F(test_vim_qflist_json_error_reporter,
       assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;"_sv);
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

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 1);
  EXPECT_EQ(look_up(qflist, 0, "end_col"), 1);
  EXPECT_EQ(look_up(qflist, 0, "end_lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "nr"), "E001");
  EXPECT_EQ(look_up(qflist, 0, "type"), "E");
  EXPECT_EQ(look_up(qflist, 0, "text"),
            "variable assigned before its declaration");
}

TEST_F(test_vim_qflist_json_error_reporter, multiple_errors) {
  padded_string input(u8"abc"_sv);
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

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 3);
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_buffer_number_if_requested) {
  padded_string input(u8""_sv);
  source_code_span span(&input[0], &input[0]);

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(error_assignment_to_const_global_variable{identifier(span)});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "bufnr"), 42);
  EXPECT_FALSE(qflist[0].as_object().contains("filename"));
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_file_name_if_requested) {
  padded_string input(u8""_sv);
  source_code_span span(&input[0], &input[0]);

  for (const char *file_name : {"hello.js", "file\\name\\with\\backslashes.js",
                                "file\"name\'with\nfunky\tcharacters"}) {
    SCOPED_TRACE(file_name);

    vim_qflist_json_error_reporter reporter =
        this->make_reporter(&input, /*file_name=*/file_name);
    reporter.report(
        error_assignment_to_const_global_variable{identifier(span)});
    reporter.finish();

    ::boost::json::array qflist =
        look_up(this->parse_json(), "qflist").as_array();
    ASSERT_EQ(qflist.size(), 1);
    EXPECT_EQ(look_up(qflist, 0, "filename"), file_name);
    EXPECT_FALSE(qflist[0].as_object().contains("bufnr"));
  }
}

TEST_F(test_vim_qflist_json_error_reporter,
       errors_have_file_name_and_buffer_number_if_requested) {
  padded_string input(u8""_sv);
  source_code_span span(&input[0], &input[0]);

  vim_qflist_json_error_reporter reporter = this->make_reporter();
  reporter.set_source(&input, /*file_name=*/"hello.js", /*vim_bufnr=*/1337);
  reporter.report(error_assignment_to_const_global_variable{identifier(span)});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "bufnr"), 1337);
  EXPECT_EQ(look_up(qflist, 0, "filename"), "hello.js");
}

TEST_F(test_vim_qflist_json_error_reporter, change_source) {
  vim_qflist_json_error_reporter reporter = this->make_reporter();

  padded_string input_1(u8"aaaaaaaa"_sv);
  reporter.set_source(&input_1, /*file_name=*/"hello.js", /*vim_bufnr=*/1);
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_1[4 - 1], &input_1[4 - 1]))});

  padded_string input_2(u8"bbbbbbbb"_sv);
  reporter.set_source(&input_2, /*file_name=*/"world.js");
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_2[5 - 1], &input_2[5 - 1]))});

  padded_string input_3(u8"cccccccc"_sv);
  reporter.set_source(&input_3, /*vim_bufnr=*/2);
  reporter.report(error_assignment_to_const_global_variable{
      identifier(source_code_span(&input_3[6 - 1], &input_3[6 - 1]))});

  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 3);

  EXPECT_EQ(look_up(qflist, 0, "bufnr"), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 4);
  EXPECT_EQ(look_up(qflist, 0, "filename"), "hello.js");

  EXPECT_FALSE(qflist[1].as_object().contains("bufnr"));
  EXPECT_EQ(look_up(qflist, 1, "col"), 5);
  EXPECT_EQ(look_up(qflist, 1, "filename"), "world.js");

  EXPECT_EQ(look_up(qflist, 2, "bufnr"), 2);
  EXPECT_EQ(look_up(qflist, 2, "col"), 6);
  EXPECT_FALSE(qflist[2].as_object().contains("filename"));
}

TEST_F(test_vim_qflist_json_error_reporter,
       assignment_to_const_global_variable) {
  padded_string input(u8"to Infinity and beyond"_sv);
  source_code_span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(infinity_span)});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 4);
  EXPECT_EQ(look_up(qflist, 0, "end_col"), 11);
  EXPECT_EQ(look_up(qflist, 0, "end_lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "nr"), "E002");
  EXPECT_EQ(look_up(qflist, 0, "type"), "E");
  EXPECT_EQ(look_up(qflist, 0, "text"), "assignment to const global variable");
  EXPECT_EQ(look_up(qflist, 0, "vcol"), 0);
}

TEST_F(test_vim_qflist_json_error_reporter, redeclaration_of_variable) {
  padded_string input(u8"let myvar; let myvar;"_sv);
  source_code_span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar");
  source_code_span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_redeclaration_of_variable{
      identifier(redeclaration_span), identifier(original_declaration_span)});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 16);
  EXPECT_EQ(look_up(qflist, 0, "end_col"), 20);
  EXPECT_EQ(look_up(qflist, 0, "end_lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "nr"), "E034");
  EXPECT_EQ(look_up(qflist, 0, "type"), "E");
  EXPECT_EQ(look_up(qflist, 0, "text"), "redeclaration of variable: myvar");
}

TEST_F(test_vim_qflist_json_error_reporter, unexpected_hash_character) {
  padded_string input(u8"#"_sv);
  source_code_span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_unexpected_hash_character{hash_span});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 1);
  EXPECT_EQ(look_up(qflist, 0, "end_col"), 1);
  EXPECT_EQ(look_up(qflist, 0, "end_lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "nr"), "E052");
  EXPECT_EQ(look_up(qflist, 0, "type"), "E");
  EXPECT_EQ(look_up(qflist, 0, "text"), "unexpected '#'");
}

TEST_F(test_vim_qflist_json_error_reporter, use_of_undeclared_variable) {
  padded_string input(u8"myvar;"_sv);
  source_code_span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar");

  vim_qflist_json_error_reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(error_use_of_undeclared_variable{identifier(myvar_span)});
  reporter.finish();

  ::boost::json::array qflist =
      look_up(this->parse_json(), "qflist").as_array();
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(look_up(qflist, 0, "col"), 1);
  EXPECT_EQ(look_up(qflist, 0, "end_col"), 5);
  EXPECT_EQ(look_up(qflist, 0, "end_lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "lnum"), 1);
  EXPECT_EQ(look_up(qflist, 0, "nr"), "E057");
  EXPECT_EQ(look_up(qflist, 0, "text"), "use of undeclared variable: myvar");
  EXPECT_EQ(look_up(qflist, 0, "type"), "W");
}

TEST(test_vim_qflist_json_error_formatter, single_span_simple_message) {
  constexpr diagnostic_info diag_info = {
      .code = "E999",
      .messages =
          {
              diagnostic_message_info{
                  .format = "something happened"_gmo_message,
                  .severity = diagnostic_severity::error,
                  .args =
                      {
                          {0, diagnostic_arg_type::source_code_span},
                      },
              },
          },
  };

  padded_string code(u8"hello world"_sv);
  source_code_span hello_span(&code[0], &code[5]);
  vim_locator locator(&code);

  std::stringstream stream;
  vim_qflist_json_error_formatter formatter(stream, locator, "FILE",
                                            /*bufnr=*/std::string_view());
  formatter.format(diag_info, &hello_span);

  ::boost::json::object object = parse_boost_json(stream.str()).as_object();
  EXPECT_EQ(object["col"], 1);
  EXPECT_EQ(object["end_col"], 5);
  EXPECT_EQ(object["end_lnum"], 1);
  EXPECT_EQ(object["lnum"], 1);
  EXPECT_EQ(object["text"], "something happened");
}

TEST(test_vim_qflist_json_error_formatter, message_with_note_ignores_note) {
  struct test_diag {
    source_code_span hello_span;
    source_code_span world_span;
  };
  constexpr diagnostic_info diag_info = {
      .code = "E999",
      .messages =
          {
              diagnostic_message_info{
                  .format = "something happened"_gmo_message,
                  .severity = diagnostic_severity::error,
                  .args =
                      {
                          {offsetof(test_diag, hello_span),
                           diagnostic_arg_type::source_code_span},
                      },
              },
              diagnostic_message_info{
                  .format = "here"_gmo_message,
                  .severity = diagnostic_severity::note,
                  .args =
                      {
                          {offsetof(test_diag, world_span),
                           diagnostic_arg_type::source_code_span},
                      },
              },
          },
  };

  padded_string code(u8"hello world"_sv);
  vim_locator locator(&code);

  std::stringstream stream;
  test_diag diag = {
      .hello_span = source_code_span(&code[0], &code[5]),
      .world_span = source_code_span(&code[6], &code[11]),
  };
  vim_qflist_json_error_formatter formatter(stream, locator, "FILE",
                                            /*bufnr=*/std::string_view());
  formatter.format(diag_info, &diag);

  ::boost::json::object object = parse_boost_json(stream.str()).as_object();
  EXPECT_EQ(object["col"], 1);
  EXPECT_EQ(object["end_col"], 5);
  EXPECT_EQ(object["end_lnum"], 1);
  EXPECT_EQ(object["lnum"], 1);
  EXPECT_EQ(object["text"], "something happened");
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
