// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
source_code_span empty_span(nullptr, nullptr);

class string_diagnostic_formatter
    : public diagnostic_formatter<string_diagnostic_formatter> {
 public:
  void write_before_message(std::string_view, diagnostic_severity,
                            const source_code_span&) {}

  void write_message_part(std::string_view, diagnostic_severity,
                          string8_view message_part) {
    this->message += message_part;
  }

  void write_after_message(std::string_view, diagnostic_severity,
                           const source_code_span&) {
    this->message += u8'\n';
  }

  string8 message;
};

TEST(test_diagnostic_formatter, origin_span) {
  static constexpr const char8* code = u8"hello world";
  static const source_code_span span(&code[0], &code[5]);

  struct test_diagnostic_formatter
      : public diagnostic_formatter<test_diagnostic_formatter> {
    void write_before_message(std::string_view, diagnostic_severity,
                              const source_code_span& origin_span) {
      EXPECT_EQ(origin_span, span);
      this->write_before_message_call_count += 1;
    }

    void write_message_part(std::string_view, diagnostic_severity,
                            string8_view) {}

    void write_after_message(std::string_view, diagnostic_severity,
                             const source_code_span& origin_span) {
      EXPECT_EQ(origin_span, span);
      this->write_after_message_call_count += 1;
    }

    int write_before_message_call_count = 0;
    int write_after_message_call_count = 0;
  };

  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("something happened"),
      .args =
          {
              {0, diagnostic_arg_type::source_code_span},
          },
  };

  test_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &span);

  EXPECT_EQ(formatter.write_before_message_call_count, 1);
  EXPECT_EQ(formatter.write_after_message_call_count, 1);
}

TEST(test_diagnostic_formatter, single_span_simple_message) {
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("something happened"),
      .args =
          {
              {0, diagnostic_arg_type::source_code_span},
          },
  };

  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &empty_span);
  EXPECT_EQ(formatter.message, u8"something happened\n");
}

TEST(test_diagnostic_formatter, diagnostic_with_single_message) {
  constexpr diagnostic_info info = {
      .code = "E9999",
      .severity = diagnostic_severity::error,
      .messages =
          {
              diagnostic_message_info{
                  .format = QLJS_TRANSLATABLE("something happened"),
                  .args =
                      {
                          {0, diagnostic_arg_type::source_code_span},
                      },
              },
          },
  };

  string_diagnostic_formatter formatter;
  formatter.format(info, &empty_span);
  EXPECT_EQ(formatter.message, u8"something happened\n");
}

TEST(test_diagnostic_formatter, diagnostic_with_two_messages) {
  constexpr diagnostic_info info = {
      .code = "E9999",
      .severity = diagnostic_severity::error,
      .messages =
          {
              diagnostic_message_info{
                  .format = QLJS_TRANSLATABLE("something happened"),
                  .args =
                      {
                          {0, diagnostic_arg_type::source_code_span},
                      },
              },
              diagnostic_message_info{
                  .format = QLJS_TRANSLATABLE("see here"),
                  .args =
                      {
                          {0, diagnostic_arg_type::source_code_span},
                      },
              },
          },
  };

  string_diagnostic_formatter formatter;
  formatter.format(info, &empty_span);
  EXPECT_EQ(formatter.message,
            u8"something happened\n"
            u8"see here\n");
}

TEST(test_diagnostic_formatter, message_with_zero_placeholder) {
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("this {0} looks fishy"),
      .args =
          {
              {0, diagnostic_arg_type::source_code_span},
          },
  };

  const char8* code = u8"hello world";
  source_code_span hello_span(&code[0], &code[5]);

  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &hello_span);
  EXPECT_EQ(formatter.message, u8"this hello looks fishy\n");
}

TEST(test_diagnostic_formatter, message_with_extra_identifier_placeholder) {
  struct test_diag {
    source_code_span hello;
    identifier world;
  };
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("this {1} looks fishy"),
      .args =
          {
              {offsetof(test_diag, hello),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, world), diagnostic_arg_type::identifier},
          },
  };

  const char8* code = u8"hello world";
  test_diag diag = {
      .hello = source_code_span(&code[0], &code[5]),
      .world = identifier(source_code_span(&code[6], &code[11])),
  };

  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &diag);
  EXPECT_EQ(formatter.message, u8"this world looks fishy\n");
}

TEST(test_diagnostic_formatter, message_with_multiple_span_placeholders) {
  struct test_diag {
    source_code_span let_span;
    source_code_span me_span;
    source_code_span be_span;
  };
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("free {1} and {0} {1} {2}"),
      .args =
          {
              {offsetof(test_diag, let_span),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, me_span),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, be_span),
               diagnostic_arg_type::source_code_span},
          },
  };

  const char8* code = u8"let me = be(free);";
  test_diag diag = {
      .let_span = source_code_span(&code[0], &code[3]),
      .me_span = source_code_span(&code[4], &code[6]),
      .be_span = source_code_span(&code[9], &code[11]),
  };
  ASSERT_EQ(diag.let_span.string_view(), u8"let");
  ASSERT_EQ(diag.me_span.string_view(), u8"me");
  ASSERT_EQ(diag.be_span.string_view(), u8"be");

  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &diag);
  EXPECT_EQ(formatter.message, u8"free me and let me be\n");
}

TEST(test_diagnostic_formatter, message_with_char_placeholder) {
  struct test_diag {
    source_code_span span;
    char8 c;
  };
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("what is this '{1}' nonsense?"),
      .args =
          {
              {offsetof(test_diag, span),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, c), diagnostic_arg_type::char8},
          },
  };

  test_diag diag = {
      .span = empty_span,
      .c = u8'Q',
  };
  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &diag);
  EXPECT_EQ(formatter.message, u8"what is this 'Q' nonsense?\n");
}

TEST(test_diagnostic_formatter, message_with_escaped_curlies) {
  constexpr diagnostic_message_info message_info = {
      .format = QLJS_TRANSLATABLE("a {{0} b }} c"),
      .args =
          {
              {0, diagnostic_arg_type::source_code_span},
          },
  };

  const char8* code = u8"hello world";
  source_code_span code_span(&code[0], &code[3]);

  string_diagnostic_formatter formatter;
  formatter.format_message("E9999"sv, diagnostic_severity::error, message_info,
                           &code_span);
  EXPECT_EQ(formatter.message, u8"a {0} b }} c\n");
}

TEST(test_diagnostic_formatter, statement_kind_placeholder) {
  struct test_diag {
    source_code_span empty_span;
    statement_kind statement;
  };
  constexpr diagnostic_message_info headlinese_message_info = {
      .format = QLJS_TRANSLATABLE("expected {1:headlinese}"),
      .args =
          {
              {offsetof(test_diag, empty_span),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, statement),
               diagnostic_arg_type::statement_kind},
          },
  };
  constexpr diagnostic_message_info singular_message_info = {
      .format = QLJS_TRANSLATABLE("expected {1:singular}"),
      .args =
          {
              {offsetof(test_diag, empty_span),
               diagnostic_arg_type::source_code_span},
              {offsetof(test_diag, statement),
               diagnostic_arg_type::statement_kind},
          },
  };

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::do_while_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             headlinese_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'do-while' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::do_while_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             singular_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'do-while' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::for_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             headlinese_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'for' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::for_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             singular_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'for' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::if_statement,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             headlinese_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'if' statement\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::if_statement,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             singular_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected an 'if' statement\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::while_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             headlinese_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'while' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::while_loop,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             singular_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'while' loop\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::with_statement,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             headlinese_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'with' statement\n");
  }

  {
    test_diag diag = {
        .empty_span = empty_span,
        .statement = statement_kind::with_statement,
    };
    string_diagnostic_formatter formatter;
    formatter.format_message("E9999"sv, diagnostic_severity::error,
                             singular_message_info, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'with' statement\n");
  }
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
