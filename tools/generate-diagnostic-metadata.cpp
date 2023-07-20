// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cpp.h>
#include <string_view>

QLJS_WARNING_IGNORE_GCC("-Wshadow=compatible-local")

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
    this->skip_newline();
  }

  void skip_preprocessor_directive() {
    QLJS_ASSERT(*this->input_ == u8'#');
    this->input_ += 1;
    this->skip_to_end_of_line();
    while (this->input_[-1] == u8'\\') {
      this->skip_newline();
      this->skip_to_end_of_line();
    }
  }

  void skip_to_end_of_line() {
    while (u8"\0\r\n"_sv.find(*this->input_) == String8_View::npos) {
      this->input_ += 1;
    }
  }

  void skip_newline() {
    if (*this->input_ == u8'\r') {
      this->input_ += 1;
      if (*this->input_ == u8'\n') {
        this->input_ += 1;
      }
    } else if (*this->input_ == u8'\n') {
      this->input_ += 1;
    } else {
      this->fatal();
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

  std::uint16_t code_number() const {
    std::uint16_t code_number;
    Parse_Integer_Exact_Error error =
        parse_integer_exact(this->code_string.substr(2, 4), code_number);
    if (error != Parse_Integer_Exact_Error::ok) {
      QLJS_UNREACHABLE();  // check_diag_codes should have validated already.
    }
    return code_number;
  }

  // Returns nullptr on failure.
  const Diagnostic_Variable* variable_from_name(
      String8_View variable_name) const {
    auto it =
        find_unique_if(this->variables,
                       [variable_name](const Diagnostic_Variable& var) -> bool {
                         return var.name == variable_name;
                       });
    if (it == this->variables.end()) {
      return nullptr;
    }
    return &*it;
  }
};

// Returns the Diagnostic_Arg_Type enum value name for the given C++ type name.
//
// Returns an empty string on failure.
String8_View diagnostic_arg_type_code_from_type(String8_View type) {
#define QLJS_CASE(type_name, arg_type)             \
  do {                                             \
    if (type == QLJS_CPP_QUOTE_U8_SV(type_name)) { \
      return QLJS_CPP_QUOTE_U8_SV(arg_type);       \
    }                                              \
  } while (false)

  QLJS_CASE(Char8, char8);
  QLJS_CASE(Enum_Kind, enum_kind);
  QLJS_CASE(Source_Code_Span, source_code_span);
  QLJS_CASE(Statement_Kind, statement_kind);
  QLJS_CASE(String8_View, string8_view);
  QLJS_CASE(Variable_Kind, variable_kind);

#undef QLJS_CASE

  return u8""_sv;
}

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
      if (this->peek().type == CXX_Token_Type::identifier &&
          this->peek().identifier == u8"struct"_sv) {
        // struct Diag_Name { ... };
        this->skip();

        this->expect(CXX_Token_Type::identifier);
        String8_View diagnostic_struct_name = this->peek().identifier;
        this->skip();

        this->expect_skip(CXX_Token_Type::left_curly);
        this->parse_diagnostic_struct_body(diagnostic_struct_name);
        this->expect_skip(CXX_Token_Type::right_curly);
        this->expect_skip(CXX_Token_Type::semicolon);
      } else if (this->peek().type == CXX_Token_Type::identifier &&
                 this->peek().identifier == u8"QLJS_RESERVED_DIAG"_sv) {
        // QLJS_RESERVED_DIAG("E0242")
        this->skip();
        this->expect_skip(CXX_Token_Type::left_paren);

        this->expect(CXX_Token_Type::string_literal);
        this->reserved_code_strings.push_back(this->peek().string);
        this->skip();

        this->expect_skip(CXX_Token_Type::right_paren);
      } else {
        this->fatal("expected 'struct' or '}' or 'QLJS_RESERVED_DIAG'");
      }
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

  bool check_diag_codes() {
    bool ok = true;
    Hash_Map<String8_View, String8_View> code_to_diag_name;
    for (String8_View reserved_code_string : this->reserved_code_strings) {
      code_to_diag_name[reserved_code_string] = u8"(reserved)"_sv;
    }

    for (const Diagnostic_Type& type : this->parsed_types) {
      auto existing_it = code_to_diag_name.find(type.code_string);
      if (existing_it == code_to_diag_name.end()) {
        code_to_diag_name.emplace(type.code_string, type.name);
      } else {
        CLI_Locator locator(this->lexer_.original_input_);
        CLI_Source_Position p = locator.position(type.code_string.data());
        std::fprintf(
            stderr,
            "%s:%d:%d: error: diag code %s already in use; try this "
            "unused diag code: %s\n",
            this->lexer_.file_path_, p.line_number, p.column_number,
            quick_lint_js::to_string(type.code_string).c_str(),
            quick_lint_js::to_string(this->next_unused_diag_code_string())
                .c_str());
        p = locator.position(existing_it->first.data());
        std::fprintf(stderr, "%s:%d:%d: note: %s used code %s here\n",
                     this->lexer_.file_path_, p.line_number, p.column_number,
                     quick_lint_js::to_string(existing_it->second).c_str(),
                     quick_lint_js::to_string(existing_it->first).c_str());
        ok = false;
      }

      if (!this->is_valid_code_string(type.code_string)) {
        CLI_Locator locator(this->lexer_.original_input_);
        CLI_Source_Position p = locator.position(type.code_string.data());
        std::fprintf(
            stderr,
            "%s:%d:%d: error: diag code %s is malformed; expected a code like "
            "\"E1234\"; try this diag code instead: %s\n",
            this->lexer_.file_path_, p.line_number, p.column_number,
            quick_lint_js::to_string(type.code_string).c_str(),
            quick_lint_js::to_string(this->next_unused_diag_code_string())
                .c_str());
        ok = false;
      }
    }
    return ok;
  }

  bool is_valid_code_string(String8_View code_string) {
    return code_string.size() == 7 && code_string[0] == u8'"' &&
           code_string[1] == u8'E' && this->lexer_.is_digit(code_string[2]) &&
           this->lexer_.is_digit(code_string[3]) &&
           this->lexer_.is_digit(code_string[4]) &&
           this->lexer_.is_digit(code_string[5]) && code_string[6] == u8'"';
  }

  String8 next_unused_diag_code_string() {
    for (int i = 1; i <= 9999; ++i) {
      char code_string_raw[8];
      std::snprintf(code_string_raw, sizeof(code_string_raw), "\"E%04d\"", i);
      String8_View code_string = to_string8_view(code_string_raw);
      bool in_use = any_of(this->parsed_types,
                           [&](const Diagnostic_Type& type) {
                             return code_string == type.code_string;
                           }) ||
                    any_of(this->reserved_code_strings,
                           [&](const String8_View& reserved_code_string) {
                             return code_string == reserved_code_string;
                           });
      if (!in_use) {
        return String8(code_string);
      }
    }
    QLJS_UNIMPLEMENTED();
  }

  std::vector<Diagnostic_Type> parsed_types;
  std::vector<String8_View> reserved_code_strings;

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

void write_file_begin(Output_Stream& out) {
  out.append_literal(
      u8R"(// Code generated by tools/generate-diagnostic-metadata.cpp. DO NOT EDIT.
// source: src/quick-lint-js/diag/diagnostic-types-2.h

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

)");
}

void write_file_end(Output_Stream& out) {
  out.append_literal(
      u8R"(
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
}

void write_type_list_h(Span<const Diagnostic_Type> types, Output_Stream& out) {
  write_file_begin(out);

  out.append_literal(
      u8R"(#include <quick-lint-js/diag/diagnostic.h>

namespace quick_lint_js {
// clang-format off
)");

  out.append_literal(u8"#define QLJS_X_DIAG_TYPES_GENERATED \\\n");
  for (const Diagnostic_Type& type : types) {
    out.append_literal(u8"  QLJS_DIAG_TYPE( \\\n"_sv);

    out.append_literal(u8"    "_sv);
    out.append_copy(type.name);
    out.append_literal(u8", "_sv);
    out.append_copy(type.code_string);
    out.append_literal(u8", \\\n"_sv);

    out.append_literal(u8"    Diagnostic_Severity::"_sv);
    out.append_copy(type.severity);
    out.append_literal(u8", \\\n"_sv);

    out.append_literal(u8"    { \\\n"_sv);

    for (const Diagnostic_Variable& var : type.variables) {
      out.append_literal(u8"      "_sv);
      out.append_copy(var.type);
      out.append_literal(u8" "_sv);
      out.append_copy(var.name);
      out.append_literal(u8"; \\\n"_sv);
    }

    out.append_literal(u8"    }, \\\n"_sv);

    for (const Diagnostic_Message& message : type.messages) {
      out.append_literal(u8"      MESSAGE(QLJS_TRANSLATABLE("_sv);
      for (String8_View string : message.message_strings) {
        out.append_copy(string);
      }
      out.append_literal(u8")"_sv);
      for (String8_View arg : message.argument_variables) {
        out.append_literal(u8", "_sv);
        out.append_copy(arg);
      }
      out.append_literal(u8") \\\n"_sv);
    }

    out.append_literal(u8"  ) \\\n"_sv);

    out.append_literal(u8"  \\\n"_sv);
  }

  out.append_literal(
      u8R"(  /* END */
// clang-format on
)"_sv);

  out.append_literal(u8"\ninline constexpr int Diag_Type_Count = "_sv);
  out.append_decimal_integer(types.size());
  out.append_literal(u8";\n"_sv);

  out.append_literal(
      u8"\nextern const Diagnostic_Info all_diagnostic_infos[Diag_Type_Count];\n"_sv);

  out.append_literal(u8"}\n"_sv);

  write_file_end(out);
}

void write_info_cpp(Span<const Diagnostic_Type> types, Output_Stream& out) {
  write_file_begin(out);

  out.append_literal(
      u8R"(#include <quick-lint-js/diag/diagnostic-metadata-generated.h>
#include <quick-lint-js/diag/diagnostic-types-2.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/port/constinit.h>

namespace quick_lint_js {
// clang-format off
// If you see an error with the following lines, translation-table-generated.h
// is probably out of date. Run tools/update-translator-sources to rebuild this
// file.
const QLJS_CONSTINIT Diagnostic_Info all_diagnostic_infos[] = {
)");
  bool is_first = true;
  for (const Diagnostic_Type& type : types) {
    if (!is_first) {
      out.append_literal(u8"\n"_sv);
    }

    out.append_literal(u8"    // "_sv);
    out.append_copy(type.name);
    out.append_literal(u8"\n"_sv);

    out.append_literal(u8"    {\n"_sv);

    out.append_literal(u8"      .code = "_sv);
    out.append_decimal_integer(type.code_number());
    out.append_literal(u8",\n"_sv);

    out.append_literal(u8"      .severity = Diagnostic_Severity::"_sv);
    out.append_copy(type.severity);
    out.append_literal(u8",\n"_sv);

    out.append_literal(u8"      .message_formats = {\n"_sv);
    for (const Diagnostic_Message& message : type.messages) {
      out.append_literal(u8"        QLJS_TRANSLATABLE("_sv);
      for (String8_View string : message.message_strings) {
        out.append_copy(string);
      }
      out.append_literal(u8"),\n"_sv);
    }
    out.append_literal(u8"      },\n"_sv);

    out.append_literal(u8"      .message_args = {\n"_sv);
    for (const Diagnostic_Message& message : type.messages) {
      out.append_literal(u8"        {\n"_sv);
      for (String8_View arg : message.argument_variables) {
        out.append_literal(
            u8"          Diagnostic_Message_Arg_Info(offsetof("_sv);
        out.append_copy(type.name);
        out.append_literal(u8", "_sv);
        out.append_copy(arg);
        out.append_literal(u8"), Diagnostic_Arg_Type::"_sv);
        const Diagnostic_Variable* var = type.variable_from_name(arg);
        if (var == nullptr) {
          out.append_literal(u8"(error: type not found)"_sv);
        } else {
          out.append_copy(diagnostic_arg_type_code_from_type(var->type));
        }
        out.append_literal(u8"),\n"_sv);
      }
      out.append_literal(u8"        },\n"_sv);
    }
    out.append_literal(u8"      },\n"_sv);

    out.append_literal(u8"    },\n"_sv);

    if (is_first) {
      is_first = false;
    }
  }

  out.append_literal(
      u8R"(};
}
)"_sv);

  write_file_end(out);
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  const char* diagnostic_types_file_path = nullptr;
  const char* output_info_cpp_path = nullptr;
  const char* output_type_list_h_path = nullptr;
  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      if (diagnostic_types_file_path != nullptr) {
        std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
        std::exit(2);
      }
      diagnostic_types_file_path = argument;
    }

    QLJS_OPTION(const char* arg_value, "--output-info-cpp"sv) {
      output_info_cpp_path = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--output-type-list-h"sv) {
      output_type_list_h_path = arg_value;
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

  if (!cxx_parser.check_diag_codes()) {
    std::exit(1);
  }

  {
    Result<Platform_File, Write_File_IO_Error> type_list_h =
        open_file_for_writing(output_type_list_h_path);
    if (!type_list_h.ok()) {
      std::fprintf(stderr, "error: %s\n",
                   type_list_h.error_to_string().c_str());
      std::exit(1);
    }
    File_Output_Stream out(type_list_h->ref());
    write_type_list_h(Span<const Diagnostic_Type>(cxx_parser.parsed_types),
                      out);
    out.flush();
  }

  {
    Result<Platform_File, Write_File_IO_Error> info_cpp =
        open_file_for_writing(output_info_cpp_path);
    if (!info_cpp.ok()) {
      std::fprintf(stderr, "error: %s\n", info_cpp.error_to_string().c_str());
      std::exit(1);
    }
    File_Output_Stream out(info_cpp->ref());
    write_info_cpp(Span<const Diagnostic_Type>(cxx_parser.parsed_types), out);
    out.flush();
  }

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
