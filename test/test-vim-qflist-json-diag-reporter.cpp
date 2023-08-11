// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/cli/vim-location.h>
#include <quick-lint-js/cli/vim-qflist-json-diag-reporter.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/tjson.h>

namespace quick_lint_js {
namespace {
class Test_Vim_QFList_JSON_Diag_Reporter : public ::testing::Test {
 protected:
  Vim_QFList_JSON_Diag_Reporter make_reporter() {
    return Vim_QFList_JSON_Diag_Reporter(Translator(), &this->stream_);
  }

  Vim_QFList_JSON_Diag_Reporter make_reporter(Padded_String_View input,
                                              int vim_bufnr) {
    Vim_QFList_JSON_Diag_Reporter reporter(Translator(), &this->stream_);
    reporter.set_source(input, /*vim_bufnr=*/vim_bufnr);
    return reporter;
  }

  Vim_QFList_JSON_Diag_Reporter make_reporter(Padded_String_View input,
                                              const char *file_name) {
    Vim_QFList_JSON_Diag_Reporter reporter(Translator(), &this->stream_);
    reporter.set_source(input, /*file_name=*/file_name);
    return reporter;
  }

  TJSON parse_json() {
    this->stream_.flush();
    TJSON document(this->stream_.get_flushed_string8());
    this->stream_.clear();
    return document;
  }

  Memory_Output_Stream stream_;
};

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter,
       assignment_before_variable_declaration) {
  Padded_String input(u8"x=0;let x;"_sv);
  Source_Code_Span assignment_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);
  Source_Code_Span declaration_span(&input[9 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(declaration_span.string_view(), u8"x"_sv);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(Diag_Assignment_Before_Variable_Declaration{
      .assignment = assignment_span, .declaration = declaration_span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 1);
  EXPECT_EQ(qflist[0][u8"end_col"_sv], 1);
  EXPECT_EQ(qflist[0][u8"end_lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"nr"_sv], u8"E0001"_sv);
  EXPECT_EQ(qflist[0][u8"type"_sv], u8"E"_sv);
  EXPECT_EQ(qflist[0][u8"text"_sv],
            u8"variable assigned before its declaration"_sv);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, multiple_errors) {
  Padded_String input(u8"abc"_sv);
  Source_Code_Span a_span(&input[0], &input[1]);
  Source_Code_Span b_span(&input[1], &input[2]);
  Source_Code_Span c_span(&input[2], &input[3]);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{a_span});
  reporter.report(Diag_Assignment_To_Const_Global_Variable{b_span});
  reporter.report(Diag_Assignment_To_Const_Global_Variable{c_span});
  reporter.finish();

  TJSON document = this->parse_json();
  ASSERT_EQ(document[u8"qflist"_sv].size(), 3);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter,
       errors_have_buffer_number_if_requested) {
  Padded_String input(u8""_sv);
  Source_Code_Span span(&input[0], &input[0]);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"bufnr"_sv], 42);
  EXPECT_FALSE(qflist[0][u8"filename"_sv].exists());
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, errors_have_file_name_if_requested) {
  Padded_String input(u8""_sv);
  Source_Code_Span span(&input[0], &input[0]);

  for (const char *file_name : {"hello.js", "file\\name\\with\\backslashes.js",
                                "file\"name\'with\nfunky\tcharacters"}) {
    SCOPED_TRACE(file_name);

    Vim_QFList_JSON_Diag_Reporter reporter =
        this->make_reporter(&input, /*file_name=*/file_name);
    reporter.report(Diag_Assignment_To_Const_Global_Variable{span});
    reporter.finish();

    TJSON document = this->parse_json();
    TJSON_Value qflist = document[u8"qflist"_sv];
    ASSERT_EQ(qflist.size(), 1);
    EXPECT_EQ(qflist[0][u8"filename"_sv], to_string8_view(file_name));
    EXPECT_FALSE(qflist[0][u8"bufnr"_sv].exists());
  }
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter,
       errors_have_file_name_and_buffer_number_if_requested) {
  Padded_String input(u8""_sv);
  Source_Code_Span span(&input[0], &input[0]);

  Vim_QFList_JSON_Diag_Reporter reporter = this->make_reporter();
  reporter.set_source(&input, /*file_name=*/"hello.js", /*vim_bufnr=*/1337);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"bufnr"_sv], 1337);
  EXPECT_EQ(qflist[0][u8"filename"_sv], u8"hello.js"_sv);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, change_source) {
  Vim_QFList_JSON_Diag_Reporter reporter = this->make_reporter();

  Padded_String input_1(u8"aaaaaaaa"_sv);
  reporter.set_source(&input_1, /*file_name=*/"hello.js", /*vim_bufnr=*/1);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{
      Source_Code_Span::unit(&input_1[4 - 1])});

  Padded_String input_2(u8"bbbbbbbb"_sv);
  reporter.set_source(&input_2, /*file_name=*/"world.js");
  reporter.report(Diag_Assignment_To_Const_Global_Variable{
      Source_Code_Span::unit(&input_2[5 - 1])});

  Padded_String input_3(u8"cccccccc"_sv);
  reporter.set_source(&input_3, /*vim_bufnr=*/2);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{
      Source_Code_Span::unit(&input_3[6 - 1])});

  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 3);

  EXPECT_EQ(qflist[0][u8"bufnr"_sv], 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 4);
  EXPECT_EQ(qflist[0][u8"filename"_sv], u8"hello.js"_sv);

  EXPECT_FALSE(qflist[1][u8"bufnr"_sv].exists());
  EXPECT_EQ(qflist[1][u8"col"_sv], 5);
  EXPECT_EQ(qflist[1][u8"filename"_sv], u8"world.js"_sv);

  EXPECT_EQ(qflist[2][u8"bufnr"_sv], 2);
  EXPECT_EQ(qflist[2][u8"col"_sv], 6);
  EXPECT_FALSE(qflist[2][u8"filename"_sv].exists());
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter,
       assignment_to_const_global_variable) {
  Padded_String input(u8"to Infinity and beyond"_sv);
  Source_Code_Span infinity_span(&input[4 - 1], &input[11 + 1 - 1]);
  ASSERT_EQ(infinity_span.string_view(), u8"Infinity"_sv);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/42);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{infinity_span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 4);
  EXPECT_EQ(qflist[0][u8"end_col"_sv], 11);
  EXPECT_EQ(qflist[0][u8"end_lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"nr"_sv], u8"E0002"_sv);
  EXPECT_EQ(qflist[0][u8"type"_sv], u8"E"_sv);
  EXPECT_EQ(qflist[0][u8"text"_sv], u8"assignment to const global variable"_sv);
  EXPECT_EQ(qflist[0][u8"vcol"_sv], 0);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, redeclaration_of_variable) {
  Padded_String input(u8"let myvar; let myvar;"_sv);
  Source_Code_Span original_declaration_span(&input[5 - 1], &input[9 + 1 - 1]);
  ASSERT_EQ(original_declaration_span.string_view(), u8"myvar"_sv);
  Source_Code_Span redeclaration_span(&input[16 - 1], &input[20 + 1 - 1]);
  ASSERT_EQ(redeclaration_span.string_view(), u8"myvar"_sv);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(Diag_Redeclaration_Of_Variable{redeclaration_span,
                                                 original_declaration_span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 16);
  EXPECT_EQ(qflist[0][u8"end_col"_sv], 20);
  EXPECT_EQ(qflist[0][u8"end_lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"nr"_sv], u8"E0034"_sv);
  EXPECT_EQ(qflist[0][u8"type"_sv], u8"E"_sv);
  EXPECT_EQ(qflist[0][u8"text"_sv], u8"redeclaration of variable: myvar"_sv);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, unexpected_hash_character) {
  Padded_String input(u8"#"_sv);
  Source_Code_Span hash_span(&input[1 - 1], &input[1 + 1 - 1]);
  ASSERT_EQ(hash_span.string_view(), u8"#"_sv);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(Diag_Unexpected_Hash_Character{hash_span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 1);
  EXPECT_EQ(qflist[0][u8"end_col"_sv], 1);
  EXPECT_EQ(qflist[0][u8"end_lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"nr"_sv], u8"E0052"_sv);
  EXPECT_EQ(qflist[0][u8"type"_sv], u8"E"_sv);
  EXPECT_EQ(qflist[0][u8"text"_sv], u8"unexpected '#'"_sv);
}

TEST_F(Test_Vim_QFList_JSON_Diag_Reporter, use_of_undeclared_variable) {
  Padded_String input(u8"myvar;"_sv);
  Source_Code_Span myvar_span(&input[1 - 1], &input[5 + 1 - 1]);
  ASSERT_EQ(myvar_span.string_view(), u8"myvar"_sv);

  Vim_QFList_JSON_Diag_Reporter reporter =
      this->make_reporter(&input, /*vim_bufnr=*/0);
  reporter.report(Diag_Use_Of_Undeclared_Variable{myvar_span});
  reporter.finish();

  TJSON document = this->parse_json();
  TJSON_Value qflist = document[u8"qflist"_sv];
  ASSERT_EQ(qflist.size(), 1);
  EXPECT_EQ(qflist[0][u8"col"_sv], 1);
  EXPECT_EQ(qflist[0][u8"end_col"_sv], 5);
  EXPECT_EQ(qflist[0][u8"end_lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"lnum"_sv], 1);
  EXPECT_EQ(qflist[0][u8"nr"_sv], u8"E0057"_sv);
  EXPECT_EQ(qflist[0][u8"text"_sv], u8"use of undeclared variable: myvar"_sv);
  EXPECT_EQ(qflist[0][u8"type"_sv], u8"W"_sv);
}

TEST(Test_Vim_Qflist_JSON_Diag_Formatter, single_span_simple_message) {
  constexpr Diagnostic_Info diag_info = {
      .code = 9999,
      .severity = Diagnostic_Severity::error,
      .message_formats = {QLJS_TRANSLATABLE("something happened")},
      .message_args =
          {
              Diagnostic_Message_Args{{
                  {0, Diagnostic_Arg_Type::source_code_span},
              }},
          },
  };

  Padded_String code(u8"hello world"_sv);
  Source_Code_Span hello_span(&code[0], &code[5]);
  Vim_Locator locator(&code);

  Memory_Output_Stream stream;
  Vim_QFList_JSON_Diag_Formatter formatter(Translator(), &stream, locator,
                                           "FILE",
                                           /*bufnr=*/std::string_view());
  formatter.format(diag_info, &hello_span);
  stream.flush();

  TJSON document(stream.get_flushed_string8());
  EXPECT_EQ(document[u8"col"_sv], 1);
  EXPECT_EQ(document[u8"end_col"_sv], 5);
  EXPECT_EQ(document[u8"end_lnum"_sv], 1);
  EXPECT_EQ(document[u8"lnum"_sv], 1);
  EXPECT_EQ(document[u8"text"_sv], u8"something happened"_sv);
}

TEST(Test_Vim_Qflist_JSON_Diag_Formatter, message_with_note_ignores_note) {
  struct Test_Diag {
    Source_Code_Span hello_span;
    Source_Code_Span world_span;
  };
  constexpr Diagnostic_Info diag_info = {
      .code = 9999,
      .severity = Diagnostic_Severity::error,
      .message_formats = {QLJS_TRANSLATABLE("something happened"),
                          QLJS_TRANSLATABLE("here")},
      .message_args =
          {
              Diagnostic_Message_Args{{
                  {offsetof(Test_Diag, hello_span),
                   Diagnostic_Arg_Type::source_code_span},
              }},
              Diagnostic_Message_Args{{
                  {offsetof(Test_Diag, world_span),
                   Diagnostic_Arg_Type::source_code_span},
              }},
          },
  };

  Padded_String code(u8"hello world"_sv);
  Vim_Locator locator(&code);

  Memory_Output_Stream stream;
  Test_Diag diag = {
      .hello_span = Source_Code_Span(&code[0], &code[5]),
      .world_span = Source_Code_Span(&code[6], &code[11]),
  };
  Vim_QFList_JSON_Diag_Formatter formatter(Translator(), &stream, locator,
                                           "FILE",
                                           /*bufnr=*/std::string_view());
  formatter.format(diag_info, &diag);
  stream.flush();

  TJSON document(stream.get_flushed_string8());
  EXPECT_EQ(document[u8"col"_sv], 1);
  EXPECT_EQ(document[u8"end_col"_sv], 5);
  EXPECT_EQ(document[u8"end_lnum"_sv], 1);
  EXPECT_EQ(document[u8"lnum"_sv], 1);
  EXPECT_EQ(document[u8"text"_sv], u8"something happened"_sv);
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
