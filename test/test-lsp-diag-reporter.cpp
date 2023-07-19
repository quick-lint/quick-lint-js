// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/lsp/lsp-diag-reporter.h>
#include <quick-lint-js/parse-json.h>
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

  ::boost::json::value parse_json() {
    String8 json;
    json.resize(this->buffer_.size());
    this->buffer_.copy_to(json.data());
    SCOPED_TRACE(out_string8(json));

    ::boost::json::value root = parse_boost_json(json);

    this->buffer_.clear();
    return root;
  }

  Byte_Buffer buffer_;
};

TEST_F(Test_LSP_Diag_Reporter, big_int_literal_contains_decimal_point) {
  Padded_String input(u8"12.34n"_sv);
  Source_Code_Span number_span(&input[0], &input[6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n"_sv);

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Big_Int_Literal_Contains_Decimal_Point{number_span});
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.as_array().size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "character"), 6);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "BigInt literal contains decimal point");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0005");
  EXPECT_EQ(look_up(diagnostics, 0, "source"), "quick-lint-js");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0005/");
}

TEST_F(Test_LSP_Diag_Reporter, assignment_before_variable_declaration) {
  Padded_String input(u8"x=0;let x;"_sv);
  Source_Code_Span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);
  Source_Code_Span declaration_span(&input[8], &input[9]);
  ASSERT_EQ(declaration_span.string_view(), u8"x"_sv);

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Assignment_Before_Variable_Declaration{
      .assignment = assignment_span, .declaration = declaration_span});
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.as_array().size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "character"), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "variable assigned before its declaration");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0001");
  EXPECT_EQ(look_up(diagnostics, 0, "source"), "quick-lint-js");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0001/");
  // TODO(#200): Show the declaration as relatedInformation.
}

TEST_F(Test_LSP_Diag_Reporter, assignment_to_undeclared_variable) {
  Padded_String input(u8"x=5;"_sv);
  Source_Code_Span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(
      Diag_Assignment_To_Undeclared_Variable{.assignment = assignment_span});
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.as_array().size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "character"), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_warning_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "assignment to undeclared variable");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0059");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0059/");
}

TEST_F(Test_LSP_Diag_Reporter, multiple_errors) {
  Padded_String input(u8"abc"_sv);
  Source_Code_Span a_span(&input[0], &input[1]);
  Source_Code_Span b_span(&input[1], &input[2]);
  Source_Code_Span c_span(&input[2], &input[3]);

  LSP_Diag_Reporter reporter = this->make_reporter(&input);
  reporter.report(Diag_Assignment_To_Const_Global_Variable{a_span});
  reporter.report(Diag_Assignment_To_Const_Global_Variable{b_span});
  reporter.report(Diag_Assignment_To_Const_Global_Variable{c_span});
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  EXPECT_EQ(diagnostics.as_array().size(), 3);
}

TEST_F(Test_LSP_Diag_Reporter, messages_use_translator) {
  Translator t;
  t.use_messages_from_locale("en_US@snarky");

  Padded_String input(u8"0e1n"_sv);

  LSP_Diag_Reporter reporter(t, this->buffer_, &input);
  reporter.report(Diag_Big_Int_Literal_Contains_Exponent{
      .where = Source_Code_Span(&input[1], &input[2]),
  });
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.as_array().size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "BigExponInt is an ES2069 feature");
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
