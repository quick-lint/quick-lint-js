// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/tjson.h>
#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/lsp/lsp-diag-reporter.h>
#include <quick-lint-js/port/char8.h>
#include <sstream>
#include <system_error>

namespace quick_lint_js {
namespace {
constexpr int lsp_error_severity = 1;
constexpr int lsp_warning_severity = 2;

class Test_LSP_Diag_Reporter : public ::testing::Test {
 protected:
  LSP_Diag_Reporter make_reporter(Padded_String_View input) {
    return LSP_Diag_Reporter(Translator(), this->buffer_, input);
  }

  TJSON parse_json() {
    TJSON document(this->buffer_);
    this->buffer_.clear();
    return document;
  }

  Byte_Buffer buffer_;

  Monotonic_Allocator memory_ = Monotonic_Allocator("Test_LSP_Diag_Reporter");
};

TEST_F(Test_LSP_Diag_Reporter, big_int_literal_contains_decimal_point) {
  Padded_String input(u8"12.34n"_sv);
  Source_Code_Span number_span(&input[0], &input[6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n"_sv);

  Diag_List diags(&this->memory_);
  diags.add(Diag_Big_Int_Literal_Contains_Decimal_Point{number_span});

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(diags);
  reporter.finish();

  TJSON diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"character"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"character"_sv], 6);
  EXPECT_EQ(diagnostics[0][u8"severity"_sv], lsp_error_severity);
  EXPECT_EQ(diagnostics[0][u8"message"_sv],
            u8"BigInt literal contains decimal point"_sv);
  EXPECT_EQ(diagnostics[0][u8"code"_sv], u8"E0005"_sv);
  EXPECT_EQ(diagnostics[0][u8"source"_sv], u8"quick-lint-js"_sv);
  EXPECT_EQ(diagnostics[0][u8"codeDescription"_sv][u8"href"_sv],
            u8"https://quick-lint-js.com/errors/E0005/"_sv);
}

TEST_F(Test_LSP_Diag_Reporter, assignment_before_variable_declaration) {
  Padded_String input(u8"x=0;let x;"_sv);
  Source_Code_Span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);
  Source_Code_Span declaration_span(&input[8], &input[9]);
  ASSERT_EQ(declaration_span.string_view(), u8"x"_sv);

  Diag_List diags(&this->memory_);
  diags.add(Diag_Assignment_Before_Variable_Declaration{
      .assignment = assignment_span, .declaration = declaration_span});

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(diags);
  reporter.finish();

  TJSON diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"character"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"character"_sv], 1);
  EXPECT_EQ(diagnostics[0][u8"severity"_sv], lsp_error_severity);
  EXPECT_EQ(diagnostics[0][u8"message"_sv],
            u8"variable assigned before its declaration"_sv);
  EXPECT_EQ(diagnostics[0][u8"code"_sv], u8"E0001"_sv);
  EXPECT_EQ(diagnostics[0][u8"source"_sv], u8"quick-lint-js"_sv);
  EXPECT_EQ(diagnostics[0][u8"codeDescription"_sv][u8"href"_sv],
            u8"https://quick-lint-js.com/errors/E0001/"_sv);
  // TODO(#200): Show the declaration as relatedInformation.
}

TEST_F(Test_LSP_Diag_Reporter, assignment_to_undeclared_variable) {
  Padded_String input(u8"x=5;"_sv);
  Source_Code_Span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);

  Diag_List diags(&this->memory_);
  diags.add(
      Diag_Assignment_To_Undeclared_Variable{.assignment = assignment_span});

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(diags);
  reporter.finish();

  TJSON diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"start"_sv][u8"character"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"line"_sv], 0);
  EXPECT_EQ(diagnostics[0][u8"range"_sv][u8"end"_sv][u8"character"_sv], 1);
  EXPECT_EQ(diagnostics[0][u8"severity"_sv], lsp_warning_severity);
  EXPECT_EQ(diagnostics[0][u8"message"_sv],
            u8"assignment to undeclared variable"_sv);
  EXPECT_EQ(diagnostics[0][u8"code"_sv], u8"E0059"_sv);
  EXPECT_EQ(diagnostics[0][u8"codeDescription"_sv][u8"href"_sv],
            u8"https://quick-lint-js.com/errors/E0059/"_sv);
}

TEST_F(Test_LSP_Diag_Reporter, multiple_errors) {
  Padded_String input(u8"abc"_sv);
  Source_Code_Span a_span(&input[0], &input[1]);
  Source_Code_Span b_span(&input[1], &input[2]);
  Source_Code_Span c_span(&input[2], &input[3]);

  Diag_List diags(&this->memory_);
  diags.add(Diag_Assignment_To_Const_Global_Variable{a_span});
  diags.add(Diag_Assignment_To_Const_Global_Variable{b_span});
  diags.add(Diag_Assignment_To_Const_Global_Variable{c_span});

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(diags);
  reporter.finish();

  TJSON diagnostics = this->parse_json();
  EXPECT_EQ(diagnostics.size(), 3);
}

TEST_F(Test_LSP_Diag_Reporter, messages_use_translator) {
  Translator t;
  t.use_messages_from_locale("en_US@snarky");

  Padded_String input(u8"0e1n"_sv);

  Diag_List diags(&this->memory_);
  diags.add(Diag_Big_Int_Literal_Contains_Exponent{
      .where = Source_Code_Span(&input[1], &input[2]),
  });

  LSP_Diag_Reporter reporter(t, this->buffer_, &input);
  reporter.report(diags);
  reporter.finish();

  TJSON diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0][u8"message"_sv],
            u8"BigExponInt is an ES2069 feature"_sv);
}
}
}

#endif

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
