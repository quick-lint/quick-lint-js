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

class test_lsp_diag_reporter : public ::testing::Test {
 protected:
  lsp_diag_reporter make_reporter(padded_string_view input) {
    return lsp_diag_reporter(translator(), this->buffer_, input);
  }

  ::boost::json::value parse_json() {
    string8 json;
    json.resize(this->buffer_.size());
    this->buffer_.copy_to(json.data());
    SCOPED_TRACE(out_string8(json));

    ::boost::json::value root = parse_boost_json(json);

    this->buffer_.clear();
    return root;
  }

  byte_buffer buffer_;
};

TEST_F(test_lsp_diag_reporter, big_int_literal_contains_decimal_point) {
  padded_string input(u8"12.34n"_sv);
  source_code_span number_span(&input[0], &input[6]);
  ASSERT_EQ(number_span.string_view(), u8"12.34n"_sv);

  lsp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_big_int_literal_contains_decimal_point{number_span});
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

TEST_F(test_lsp_diag_reporter, assignment_before_variable_declaration) {
  padded_string input(u8"x=0;let x;"_sv);
  source_code_span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);
  source_code_span declaration_span(&input[8], &input[9]);
  ASSERT_EQ(declaration_span.string_view(), u8"x"_sv);

  lsp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_assignment_before_variable_declaration{
      .assignment = identifier(assignment_span),
      .declaration = identifier(declaration_span)});
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

TEST_F(test_lsp_diag_reporter, assignment_to_undeclared_variable) {
  padded_string input(u8"x=5;"_sv);
  source_code_span assignment_span(&input[0], &input[1]);
  ASSERT_EQ(assignment_span.string_view(), u8"x"_sv);

  lsp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_assignment_to_undeclared_variable{
      .assignment = identifier(assignment_span)});
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

TEST_F(test_lsp_diag_reporter, multiple_errors) {
  padded_string input(u8"abc"_sv);
  source_code_span a_span(&input[0], &input[1]);
  source_code_span b_span(&input[1], &input[2]);
  source_code_span c_span(&input[2], &input[3]);

  lsp_diag_reporter reporter = this->make_reporter(&input);
  reporter.report(diag_assignment_to_const_global_variable{identifier(a_span)});
  reporter.report(diag_assignment_to_const_global_variable{identifier(b_span)});
  reporter.report(diag_assignment_to_const_global_variable{identifier(c_span)});
  reporter.finish();

  ::boost::json::value diagnostics = this->parse_json();
  EXPECT_EQ(diagnostics.as_array().size(), 3);
}

TEST_F(test_lsp_diag_reporter, messages_use_translator) {
  translator t;
  t.use_messages_from_locale("en_US@snarky");

  padded_string input(u8"0e1n"_sv);

  lsp_diag_reporter reporter(t, this->buffer_, &input);
  reporter.report(diag_big_int_literal_contains_exponent{
      .where = source_code_span(&input[1], &input[2]),
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
