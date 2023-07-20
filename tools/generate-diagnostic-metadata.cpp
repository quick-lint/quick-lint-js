// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
enum class CXX_Token_Type {
  end_of_file,
  identifier,
  string_literal,

  colon_colon,   // ::
  comma,         // ,
  left_curly,    // {
  left_paren,    // (
  left_square,   // [
  right_curly,   // }
  right_paren,   // )
  right_square,  // ]
  semicolon,     // ;
};

std::string_view to_string(CXX_Token_Type token_type) {
#define QLJS_CASE(tt)      \
  case CXX_Token_Type::tt: \
    return #tt##sv;
  switch (token_type) {
    QLJS_CASE(colon_colon)
    QLJS_CASE(comma)
    QLJS_CASE(end_of_file)
    QLJS_CASE(identifier)
    QLJS_CASE(left_curly)
    QLJS_CASE(left_paren)
    QLJS_CASE(left_square)
    QLJS_CASE(right_curly)
    QLJS_CASE(right_paren)
    QLJS_CASE(right_square)
    QLJS_CASE(semicolon)
    QLJS_CASE(string_literal)
  }
#undef QLJS_CASE
  QLJS_UNREACHABLE();
}

struct CXX_Token {
  CXX_Token_Type type;

  // If type == CXX_Token_Type::identifier:
  String8_View identifier;
  // If type == CXX_Token_Type::string_literal:
  String8_View string;
};

class CXX_Lexer {
 public:
  explicit CXX_Lexer(const char* file_path, Padded_String_View input)
      : file_path_(file_path), input_(input.data()), original_input_(input) {
    this->parse_token();
  }

  const CXX_Token& peek() { return this->token_; }

  void skip() { this->parse_token(); }

 private:
  void parse_token() {
  parse_again:
    switch (*this->input_) {
    case u8'\0':
      this->token_.type = CXX_Token_Type::end_of_file;
      break;

    case u8'/':
      this->skip_line_comment();
      goto parse_again;

    case u8'#':
      this->skip_preprocessor_directive();
      goto parse_again;

    case u8'"':
      this->token_.string = this->lex_string_literal();
      this->token_.type = CXX_Token_Type::string_literal;
      break;

#define SINGLE_CHARACTER_TOKEN(c, token_type) \
  case (c):                                   \
    this->input_ += 1;                        \
    this->token_.type = (token_type);         \
    break;

      SINGLE_CHARACTER_TOKEN(u8'(', CXX_Token_Type::left_paren)
      SINGLE_CHARACTER_TOKEN(u8')', CXX_Token_Type::right_paren)
      SINGLE_CHARACTER_TOKEN(u8',', CXX_Token_Type::comma)
      SINGLE_CHARACTER_TOKEN(u8';', CXX_Token_Type::semicolon)
      SINGLE_CHARACTER_TOKEN(u8'[', CXX_Token_Type::left_square)
      SINGLE_CHARACTER_TOKEN(u8']', CXX_Token_Type::right_square)
      SINGLE_CHARACTER_TOKEN(u8'{', CXX_Token_Type::left_curly)
      SINGLE_CHARACTER_TOKEN(u8'}', CXX_Token_Type::right_curly)

#undef SINGLE_CHARACTER_TOKEN

    case u8':':
      if (this->input_[1] != u8':') {
        this->fatal();
      }
      this->input_ += 2;
      this->token_.type = CXX_Token_Type::colon_colon;
      break;

    case u8' ':
    case u8'\t':
    case u8'\r':
    case u8'\n':
      // Skip whitespace.
      this->input_ += 1;
      goto parse_again;

    default:
      if (this->is_identifier_start(*this->input_)) {
        this->token_.identifier = this->lex_identifier();
        this->token_.type = CXX_Token_Type::identifier;
      } else {
        this->fatal();
      }
      break;
    }
  }

  bool is_identifier_start(Char8 c) {
    return (u8'a' <= c && c <= u8'z') || (u8'A' <= c && c <= u8'Z') ||
           c == u8'_';
  }

  bool is_identifier_continue(Char8 c) {
    return this->is_identifier_start(c) || this->is_digit(c);
  }

  bool is_digit(Char8 c) { return u8'0' <= c && c <= u8'9'; }

  String8_View lex_identifier() {
    QLJS_ASSERT(this->is_identifier_start(*this->input_));
    const Char8* begin = this->input_;
    this->input_ += 1;
    while (this->is_identifier_continue(*this->input_)) {
      this->input_ += 1;
    }
    const Char8* end = this->input_;
    return make_string_view(begin, end);
  }

  String8_View lex_string_literal() {
    QLJS_ASSERT(*this->input_ == u8'"');
    const Char8* begin = this->input_;
    this->input_ += 1;
    for (;;) {
      switch (*this->input_) {
      case u8'"': {
        this->input_ += 1;
        const Char8* end = this->input_;
        return make_string_view(begin, end);
      }

      case u8'\\':
        this->input_ += 2;
        break;

      default:
        this->input_ += 1;
        break;

      case u8'\0':
        this->fatal();
        break;
      }
    }
  }

  void skip_line_comment() {
    QLJS_ASSERT(*this->input_ == u8'/');
    this->input_ += 1;
    if (*this->input_ != u8'/') {
      this->fatal();
    }
    this->input_ += 1;
    this->skip_to_end_of_line();
  }

  void skip_preprocessor_directive() {
    QLJS_ASSERT(*this->input_ == u8'#');
    this->input_ += 1;
    this->skip_to_end_of_line();
  }

  void skip_to_end_of_line() {
    while (u8"\0\r\n"_sv.find(*this->input_) == String8_View::npos) {
      this->input_ += 1;
    }
  }

  [[noreturn]] void fatal() {
    CLI_Locator locator(this->original_input_);
    CLI_Source_Position p = locator.position(this->input_);
    std::fprintf(stderr, "%s:%d:%d: error: failed to lex\n", this->file_path_,
                 p.line_number, p.column_number);
    std::exit(1);
  }

  CXX_Token token_;
  const char* file_path_;
  const Char8* input_;
  Padded_String_View original_input_;

  friend class CXX_Parser;
};

struct Diagnostic_Message {
  // Each message_strings item is concatenated together by the C++ preprocessor.
  std::vector<String8_View> message_strings;
  std::vector<String8_View> argument_variables;
};

struct Diagnostic_Variable {
  String8_View type;
  String8_View name;
};

struct Diagnostic_Type {
  String8_View name;
  String8_View code_string;
  String8_View severity;
  std::vector<Diagnostic_Message> messages;
  std::vector<Diagnostic_Variable> variables;
};

class CXX_Parser {
 public:
  explicit CXX_Parser(const char* file_path, Padded_String_View input)
      : lexer_(file_path, input) {}

  void parse_file() {
    this->skip_preprocessor_directives();
    this->expect_skip(u8"namespace");
    this->expect_skip(u8"quick_lint_js");
    this->expect_skip(CXX_Token_Type::left_curly);

    while (this->peek().type != CXX_Token_Type::right_curly) {
      if (!(this->peek().type == CXX_Token_Type::identifier &&
            this->peek().identifier == u8"struct"_sv)) {
        this->fatal("expected 'struct' or '}'");
      }
      this->skip();

      // struct Diag_Name { ... };
      this->expect(CXX_Token_Type::identifier);
      String8_View diagnostic_struct_name = this->peek().identifier;
      this->skip();

      this->expect_skip(CXX_Token_Type::left_curly);
      this->parse_diagnostic_struct_body(diagnostic_struct_name);
      this->expect_skip(CXX_Token_Type::right_curly);
      this->expect_skip(CXX_Token_Type::semicolon);
    }
  }

  void parse_diagnostic_struct_body(String8_View diagnostic_struct_name) {
    Diagnostic_Type& type = this->parsed_types.emplace_back();
    type.name = diagnostic_struct_name;

    for (;;) {
      switch (this->peek().type) {
      case CXX_Token_Type::left_square:
        // [[qljs:: ... ]]
        this->skip();
        this->expect_skip(CXX_Token_Type::left_square);

        this->expect_skip(u8"qljs"_sv);
        this->expect_skip(CXX_Token_Type::colon_colon);
        this->expect(CXX_Token_Type::identifier);
        if (this->peek().identifier == u8"diag"_sv) {
          // [[qljs::diag("E0666", Diagnostic_Severity::warning)]]
          this->skip();
          this->expect_skip(CXX_Token_Type::left_paren);

          this->expect(CXX_Token_Type::string_literal);
          type.code_string = this->peek().string;
          this->skip();

          this->expect_skip(CXX_Token_Type::comma);
          this->expect_skip(u8"Diagnostic_Severity"_sv);
          this->expect_skip(CXX_Token_Type::colon_colon);

          this->expect(CXX_Token_Type::identifier);
          type.severity = this->peek().identifier;
          this->skip();

          this->expect_skip(CXX_Token_Type::right_paren);
        } else if (this->peek().identifier == u8"message"_sv) {
          // [[qljs::message("string", ARG(a))]]
          // [[qljs::message("string", ARG(a), ARG(b), ARG(c))]]
          this->skip();
          this->expect_skip(CXX_Token_Type::left_paren);

          Diagnostic_Message& message = type.messages.emplace_back();

          this->expect(CXX_Token_Type::string_literal);
          message.message_strings.push_back(this->peek().string);
          this->skip();
          while (this->peek().type == CXX_Token_Type::string_literal) {
            // Adjacent string literals: [[qljs::message("a" "b", ...)]]
            message.message_strings.push_back(this->peek().string);
            this->skip();
          }

          this->expect_skip(CXX_Token_Type::comma);

        another_argument:
          this->expect_skip(u8"ARG"_sv);
          this->expect_skip(CXX_Token_Type::left_paren);
          this->expect(CXX_Token_Type::identifier);
          message.argument_variables.push_back(this->peek().identifier);
          this->skip();
          this->expect_skip(CXX_Token_Type::right_paren);
          if (this->peek().type == CXX_Token_Type::comma) {
            this->skip();
            goto another_argument;
          }

          this->expect_skip(CXX_Token_Type::right_paren);
        } else {
          this->fatal("expected qljs::diag or qljs::message");
        }
        this->expect_skip(CXX_Token_Type::right_square);
        this->expect_skip(CXX_Token_Type::right_square);
        break;

      case CXX_Token_Type::identifier: {
        // Source_Code_Span where;
        // Statement_Kind kind_of_statement;
        Diagnostic_Variable& var = type.variables.emplace_back();

        var.type = this->peek().identifier;
        this->skip();

        this->expect(CXX_Token_Type::identifier);
        var.name = this->peek().identifier;
        this->skip();

        this->expect_skip(CXX_Token_Type::semicolon);
        break;
      }

      case CXX_Token_Type::right_curly:
        return;

      default:
        this->fatal("expected metadata or member variable or '}'");
        break;
      }
    }
  }

  std::vector<Diagnostic_Type> parsed_types;

 private:
  void skip_preprocessor_directives() {
  again:
    if (this->peek().type == CXX_Token_Type::identifier) {
      if (this->peek().identifier == u8"QLJS_WARNING_PUSH") {
        this->skip();
        goto again;
      } else if (this->peek().identifier == u8"QLJS_WARNING_IGNORE_CLANG" ||
                 this->peek().identifier == u8"QLJS_WARNING_IGNORE_GCC") {
        this->skip();
        this->expect_skip(CXX_Token_Type::left_paren);
        this->expect_skip(CXX_Token_Type::string_literal);
        this->expect_skip(CXX_Token_Type::right_paren);
        goto again;
      }
    }
  }

  const CXX_Token& peek() { return this->lexer_.peek(); }
  void skip() { this->lexer_.skip(); }

  void expect_skip(CXX_Token_Type expected_token_type) {
    this->expect(expected_token_type);
    this->skip();
  }

  void expect(CXX_Token_Type expected_token_type) {
    if (this->peek().type != expected_token_type) {
      this->fatal(
          concat("expected "sv, to_string(expected_token_type)).c_str());
    }
  }

  void expect_skip(String8_View expected_identifier) {
    std::string message = concat("expected identifier '"sv,
                                 to_string_view(expected_identifier), "'"sv);
    if (this->peek().type != CXX_Token_Type::identifier) {
      this->fatal(message.c_str());
    }
    if (this->peek().identifier != expected_identifier) {
      this->fatal(message.c_str());
    }
    this->skip();
  }

  [[noreturn]] void fatal(const char* message) {
    CLI_Locator locator(this->lexer_.original_input_);
    CLI_Source_Position p = locator.position(this->lexer_.input_);
    std::fprintf(stderr, "%s:%d:%d: error: failed to parse before: %s\n",
                 this->lexer_.file_path_, p.line_number, p.column_number,
                 message);
    std::exit(1);
  }

  CXX_Lexer lexer_;
};
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  const char* diagnostic_types_file_path = nullptr;
  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      if (diagnostic_types_file_path != nullptr) {
        std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
        std::exit(2);
      }
      diagnostic_types_file_path = argument;
    }

    QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }
  if (diagnostic_types_file_path == nullptr) {
    std::fprintf(stderr, "error: missing path to diagnostic types file\n");
    std::exit(2);
  }

  Result<Padded_String, Read_File_IO_Error> diagnostic_types_source =
      read_file(diagnostic_types_file_path);
  if (!diagnostic_types_source.ok()) {
    std::fprintf(stderr, "error: %s\n",
                 diagnostic_types_source.error_to_string().c_str());
    std::exit(1);
  }

  CXX_Parser cxx_parser(diagnostic_types_file_path, &*diagnostic_types_source);
  cxx_parser.parse_file();

  File_Output_Stream* out = File_Output_Stream::get_stdout();
  out->append_literal(
      u8R"(// Code generated by tools/generate-diagnostic-metadata.cpp. DO NOT EDIT.
// source: src/quick-lint-js/diag/diagnostic-types-2.h

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// clang-format off

)");

  out->append_literal(u8"#define QLJS_X_DIAG_TYPES_GENERATED \\\n");
  for (Diagnostic_Type& type : cxx_parser.parsed_types) {
    out->append_literal(u8"  QLJS_DIAG_TYPE( \\\n"_sv);

    out->append_literal(u8"    "_sv);
    out->append_copy(type.name);
    out->append_literal(u8", "_sv);
    out->append_copy(type.code_string);
    out->append_literal(u8", \\\n"_sv);

    out->append_literal(u8"    Diagnostic_Severity::"_sv);
    out->append_copy(type.severity);
    out->append_literal(u8", \\\n"_sv);

    out->append_literal(u8"    { \\\n"_sv);

    for (Diagnostic_Variable& var : type.variables) {
      out->append_literal(u8"      "_sv);
      out->append_copy(var.type);
      out->append_literal(u8" "_sv);
      out->append_copy(var.name);
      out->append_literal(u8"; \\\n"_sv);
    }

    out->append_literal(u8"    }, \\\n"_sv);

    for (Diagnostic_Message& message : type.messages) {
      out->append_literal(u8"      MESSAGE(QLJS_TRANSLATABLE("_sv);
      for (String8_View string : message.message_strings) {
        out->append_copy(string);
      }
      out->append_literal(u8")"_sv);
      for (String8_View arg : message.argument_variables) {
        out->append_literal(u8", "_sv);
        out->append_copy(arg);
      }
      out->append_literal(u8") \\\n"_sv);
    }

    out->append_literal(u8"  ) \\\n"_sv);

    out->append_literal(u8"  \\\n"_sv);
  }

  out->append_literal(
      u8R"(  /* END */

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
)");
  out->flush();

  return 0;
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
