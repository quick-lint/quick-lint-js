// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
Source_Code_Span empty_span(nullptr, nullptr);

class String_Diagnostic_Formatter
    : public Diagnostic_Formatter<String_Diagnostic_Formatter> {
 public:
  explicit String_Diagnostic_Formatter()
      : Diagnostic_Formatter<String_Diagnostic_Formatter>(Translator()) {}

  void write_before_message(std::string_view, Diagnostic_Severity,
                            const Source_Code_Span&) {}

  void write_message_part(std::string_view, Diagnostic_Severity,
                          String8_View message_part) {
    this->message += message_part;
  }

  void write_after_message(std::string_view, Diagnostic_Severity,
                           const Source_Code_Span&) {
    this->message += u8'\n';
  }

  String8 message;
};

TEST(Test_Diagnostic_Formatter, origin_span) {
  static constexpr const Char8* code = u8"hello world";
  static const Source_Code_Span span(&code[0], &code[5]);

  struct Test_Diagnostic_Formatter
      : public Diagnostic_Formatter<Test_Diagnostic_Formatter> {
    using Diagnostic_Formatter<Test_Diagnostic_Formatter>::Diagnostic_Formatter;

    void write_before_message(std::string_view, Diagnostic_Severity,
                              const Source_Code_Span& origin_span) {
      EXPECT_TRUE(same_pointers(origin_span, span));
      this->write_before_message_call_count += 1;
    }

    void write_message_part(std::string_view, Diagnostic_Severity,
                            String8_View) {}

    void write_after_message(std::string_view, Diagnostic_Severity,
                             const Source_Code_Span& origin_span) {
      EXPECT_TRUE(same_pointers(origin_span, span));
      this->write_after_message_call_count += 1;
    }

    int write_before_message_call_count = 0;
    int write_after_message_call_count = 0;
  };

  Translator t;
  t.use_messages_from_source_code();

  Test_Diagnostic_Formatter formatter(t);
  formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                           QLJS_TRANSLATABLE("something happened"),
                           Diagnostic_Message_Args{{
                               {0, Diagnostic_Arg_Type::source_code_span},
                           }},
                           &span);

  EXPECT_EQ(formatter.write_before_message_call_count, 1);
  EXPECT_EQ(formatter.write_after_message_call_count, 1);
}

TEST(Test_Diagnostic_Formatter, single_span_simple_message) {
  String_Diagnostic_Formatter formatter;
  formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                           QLJS_TRANSLATABLE("something happened"),
                           Diagnostic_Message_Args{{
                               {0, Diagnostic_Arg_Type::source_code_span},
                           }},
                           &empty_span);
  EXPECT_EQ(formatter.message, u8"something happened\n");
}

TEST(Test_Diagnostic_Formatter, diagnostic_with_single_message) {
  constexpr Diagnostic_Info info = {
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

  String_Diagnostic_Formatter formatter;
  formatter.format(info, &empty_span);
  EXPECT_EQ(formatter.message, u8"something happened\n");
}

TEST(Test_Diagnostic_Formatter, diagnostic_with_two_messages) {
  constexpr Diagnostic_Info info = {
      .code = 9999,
      .severity = Diagnostic_Severity::error,
      .message_formats =
          {
              QLJS_TRANSLATABLE("something happened"),
              QLJS_TRANSLATABLE("see here"),
          },
      .message_args =
          {
              Diagnostic_Message_Args{{
                  {0, Diagnostic_Arg_Type::source_code_span},
              }},
              Diagnostic_Message_Args{
                  {
                      {0, Diagnostic_Arg_Type::source_code_span},
                  },
              },
          },
  };

  String_Diagnostic_Formatter formatter;
  formatter.format(info, &empty_span);
  EXPECT_EQ(formatter.message,
            u8"something happened\n"
            u8"see here\n");
}

TEST(Test_Diagnostic_Formatter, message_with_zero_placeholder) {
  const Char8* code = u8"hello world";
  Source_Code_Span hello_span(&code[0], &code[5]);

  String_Diagnostic_Formatter formatter;
  formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                           QLJS_TRANSLATABLE("this {0} looks fishy"),
                           Diagnostic_Message_Args{{
                               {0, Diagnostic_Arg_Type::source_code_span},
                           }},
                           &hello_span);
  EXPECT_EQ(formatter.message, u8"this hello looks fishy\n");
}

TEST(Test_Diagnostic_Formatter, message_with_multiple_span_placeholders) {
  const Char8* code = u8"let me = be(free);";
  struct Test_Diag {
    Source_Code_Span let_span;
    Source_Code_Span me_span;
    Source_Code_Span be_span;
  };
  Test_Diag diag = {
      .let_span = Source_Code_Span(&code[0], &code[3]),
      .me_span = Source_Code_Span(&code[4], &code[6]),
      .be_span = Source_Code_Span(&code[9], &code[11]),
  };
  ASSERT_EQ(diag.let_span.string_view(), u8"let"_sv);
  ASSERT_EQ(diag.me_span.string_view(), u8"me"_sv);
  ASSERT_EQ(diag.be_span.string_view(), u8"be"_sv);

  String_Diagnostic_Formatter formatter;
  formatter.format_message(
      "E9999"sv, Diagnostic_Severity::error,
      QLJS_TRANSLATABLE("free {1} and {0} {1} {2}"),
      Diagnostic_Message_Args{{
          {offsetof(Test_Diag, let_span),
           Diagnostic_Arg_Type::source_code_span},
          {offsetof(Test_Diag, me_span), Diagnostic_Arg_Type::source_code_span},
          {offsetof(Test_Diag, be_span), Diagnostic_Arg_Type::source_code_span},
      }},
      &diag);
  EXPECT_EQ(formatter.message, u8"free me and let me be\n");
}

TEST(Test_Diagnostic_Formatter, message_with_char_placeholder) {
  struct Test_Diag {
    Source_Code_Span span;
    Char8 c;
  };
  Test_Diag diag = {
      .span = empty_span,
      .c = u8'Q',
  };
  String_Diagnostic_Formatter formatter;
  formatter.format_message(
      "E9999"sv, Diagnostic_Severity::error,
      QLJS_TRANSLATABLE("what is this '{1}' nonsense?"),
      Diagnostic_Message_Args{{
          {offsetof(Test_Diag, span), Diagnostic_Arg_Type::source_code_span},
          {offsetof(Test_Diag, c), Diagnostic_Arg_Type::char8},
      }},
      &diag);
  EXPECT_EQ(formatter.message, u8"what is this 'Q' nonsense?\n");
}

TEST(Test_Diagnostic_Formatter, message_with_escaped_curlies) {
  const Char8* code = u8"hello world";
  Source_Code_Span code_span(&code[0], &code[3]);

  String_Diagnostic_Formatter formatter;
  formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                           QLJS_TRANSLATABLE("a {{0} b }} c"),
                           Diagnostic_Message_Args{{
                               {0, Diagnostic_Arg_Type::source_code_span},
                           }},
                           &code_span);
  EXPECT_EQ(formatter.message, u8"a {0} b }} c\n");
}

TEST(Test_Diagnostic_Formatter, enum_kind_placeholder) {
  struct Test_Diag {
    Source_Code_Span empty_span;
    Enum_Kind kind;
  };
  constexpr Diagnostic_Message_Args message_args = {
      Diagnostic_Message_Args{{
          {offsetof(Test_Diag, empty_span),
           Diagnostic_Arg_Type::source_code_span},
          {offsetof(Test_Diag, kind), Diagnostic_Arg_Type::enum_kind},
      }},
  };

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .kind = Enum_Kind::normal,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected enum\n");
  }
}

TEST(Test_Diagnostic_Formatter, statement_kind_placeholder) {
  struct Test_Diag {
    Source_Code_Span empty_span;
    Statement_Kind statement;
  };
  constexpr Diagnostic_Message_Args message_args = {
      Diagnostic_Message_Args{{
          {offsetof(Test_Diag, empty_span),
           Diagnostic_Arg_Type::source_code_span},
          {offsetof(Test_Diag, statement), Diagnostic_Arg_Type::statement_kind},
      }},
  };

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::do_while_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'do-while' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::do_while_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'do-while' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::for_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'for' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::for_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'for' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::if_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'if' statement\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::if_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected an 'if' statement\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::while_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'while' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::while_loop,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'while' loop\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::with_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected 'with' statement\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::with_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected a 'with' statement\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::labelled_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:headlinese}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected labelled statement\n");
  }

  {
    Test_Diag diag = {
        .empty_span = empty_span,
        .statement = Statement_Kind::labelled_statement,
    };
    String_Diagnostic_Formatter formatter;
    formatter.format_message("E9999"sv, Diagnostic_Severity::error,
                             QLJS_TRANSLATABLE("expected {1:singular}"),
                             message_args, &diag);
    EXPECT_EQ(formatter.message, u8"expected a labelled statement\n");
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
