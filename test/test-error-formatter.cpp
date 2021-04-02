// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/location.h>

namespace quick_lint_js {
namespace {
class string_error_formatter : public error_formatter<string_error_formatter> {
 public:
  void write_before_message(severity, const source_code_span&) {}

  void write_message_part(severity, string8_view message_part) {
    this->message += message_part;
  }

  void write_after_message(severity, const source_code_span&) {
    this->message += u8'\n';
  }

  string8 message;
};

TEST(test_error_formatter, single_span_simple_message) {
  const char8* code = u8"hello world";
  string_error_formatter formatter;

  formatter.error("something happened"_gmo_message,
                  source_code_span(&code[0], &code[5]));

  EXPECT_EQ(formatter.message, u8"something happened\n");
}

TEST(test_error_formatter, message_with_note) {
  const char8* code = u8"hello world";
  string_error_formatter formatter;

  formatter
      .error("something happened"_gmo_message,
             source_code_span(&code[0], &code[5]))
      .note("see here"_gmo_message, source_code_span(&code[6], &code[11]));

  EXPECT_EQ(formatter.message,
            u8"something happened\n"
            u8"see here\n");
}

TEST(test_error_formatter, message_with_zero_placeholder) {
  const char8* code = u8"hello world";
  string_error_formatter formatter;

  formatter.error("this {0} looks fishy"_gmo_message,
                  source_code_span(&code[0], &code[5]));

  EXPECT_EQ(formatter.message, u8"this hello looks fishy\n");
}

TEST(test_error_formatter, message_with_extra_identifier_placeholder) {
  const char8* code = u8"hello world";
  string_error_formatter formatter;

  formatter.error("this {1} looks fishy"_gmo_message,
                  source_code_span(&code[0], &code[5]),
                  identifier(source_code_span(&code[6], &code[11])));

  EXPECT_EQ(formatter.message, u8"this world looks fishy\n");
}

TEST(test_error_formatter, message_with_multiple_span_placeholders) {
  const char8* code = u8"let me = be(free);";
  source_code_span let_span(&code[0], &code[3]);
  ASSERT_EQ(let_span.string_view(), u8"let");
  source_code_span me_span(&code[4], &code[6]);
  ASSERT_EQ(me_span.string_view(), u8"me");
  source_code_span be_span(&code[9], &code[11]);
  ASSERT_EQ(be_span.string_view(), u8"be");

  string_error_formatter formatter;
  formatter.error("free {1} and {0} {1} {2}"_gmo_message, let_span, me_span,
                  be_span);

  EXPECT_EQ(formatter.message, u8"free me and let me be\n");
}

TEST(test_error_formatter, message_with_escaped_curlies) {
  const char8* code = u8"hello world";
  source_code_span code_span(&code[0], &code[3]);
  string_error_formatter formatter;

  formatter.error("a {{0} b }} c"_gmo_message, code_span);

  EXPECT_EQ(formatter.message, u8"a {0} b }} c\n");
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
