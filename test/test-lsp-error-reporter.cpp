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
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/padded-string.h>
#include <sstream>

namespace quick_lint_js {
namespace {
constexpr int lsp_error_severity = 1;

class test_lsp_error_reporter : public ::testing::Test {
 protected:
  lsp_error_reporter make_reporter(padded_string_view input) {
    return lsp_error_reporter(this->buffer_, input);
  }

  ::Json::Value parse_json() {
    string8 json;
    json.resize(this->buffer_.size());
    this->buffer_.copy_to(json.data());
    SCOPED_TRACE(out_string8(json));

    ::Json::Value root;
    ::Json::String errors;
    bool ok = quick_lint_js::parse_json(json, &root, &errors);
    EXPECT_TRUE(ok) << errors;

    this->buffer_ = byte_buffer();
    return root;
  }

  byte_buffer buffer_;
};

TEST_F(test_lsp_error_reporter, big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n");
  source_code_span number_span(&input[0], &input[6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n");

  lsp_error_reporter reporter = this->make_reporter(&input);
  reporter.report(error_big_int_literal_contains_decimal_point{number_span});
  reporter.finish();

  ::Json::Value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["character"], 6);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"], "BigInt literal contains decimal point");
}

TEST_F(test_lsp_error_reporter, assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;");
  source_code_span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x");
  source_code_span declaration_span(&input[8], &input[9]);
  ASSERT_EQ(declaration_span.string_view(), u8"x");

  lsp_error_reporter reporter = this->make_reporter(&input);
  reporter.report(error_assignment_before_variable_declaration{
      .assignment = identifier(assignment_span),
      .declaration = identifier(declaration_span)});
  reporter.finish();

  ::Json::Value diagnostics = this->parse_json();
  ASSERT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["character"], 1);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"],
            "variable assigned before its declaration");
  // TODO(strager): Show the declaration as relatedInformation.
}

TEST_F(test_lsp_error_reporter, multiple_errors) {
  padded_string input(u8"abc");
  source_code_span a_span(&input[0], &input[1]);
  source_code_span b_span(&input[1], &input[2]);
  source_code_span c_span(&input[2], &input[3]);

  lsp_error_reporter reporter = this->make_reporter(&input);
  reporter.report(
      error_assignment_to_const_global_variable{identifier(a_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(b_span)});
  reporter.report(
      error_assignment_to_const_global_variable{identifier(c_span)});
  reporter.finish();

  ::Json::Value diagnostics = this->parse_json();
  EXPECT_EQ(diagnostics.size(), 3);
}

// TODO(strager): LSP has particular and peculiar rules for source locations.
// Ensure we implement them properly. (We don't.)
}
}
