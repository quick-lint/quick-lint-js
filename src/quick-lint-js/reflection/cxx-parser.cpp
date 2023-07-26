// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <cstdio>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/reflection/cxx-parser.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cpp.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/pointer.h>
#include <string_view>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
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

CXX_Lexer::CXX_Lexer(const char* file_path, Padded_String_View input)
    : file_path_(file_path), input_(input.data()), original_input_(input) {
  this->parse_token();
}

void CXX_Lexer::parse_token() {
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
    this->token_.decoded_string = this->lex_string_literal();
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

bool CXX_Lexer::is_identifier_start(Char8 c) {
  return (u8'a' <= c && c <= u8'z') || (u8'A' <= c && c <= u8'Z') || c == u8'_';
}

bool CXX_Lexer::is_identifier_continue(Char8 c) {
  return this->is_identifier_start(c) || this->is_digit(c);
}

bool CXX_Lexer::is_digit(Char8 c) { return u8'0' <= c && c <= u8'9'; }

String8_View CXX_Lexer::lex_identifier() {
  QLJS_ASSERT(this->is_identifier_start(*this->input_));
  const Char8* begin = this->input_;
  this->input_ += 1;
  while (this->is_identifier_continue(*this->input_)) {
    this->input_ += 1;
  }
  const Char8* end = this->input_;
  return make_string_view(begin, end);
}

String8_View CXX_Lexer::lex_string_literal() {
  QLJS_ASSERT(*this->input_ == u8'"');
  this->input_ += 1;

  Bump_Vector<Char8, Monotonic_Allocator> decoded(
      "CXX_Lexer::lex_string_literal", &this->decoded_string_allocator_);
  for (;;) {
    switch (*this->input_) {
    case u8'"': {
      this->input_ += 1;
      this->skip_whitespace_and_comments();

      if (*this->input_ == u8'"') {
        // Concatenated strings: "abc" "def"
        this->input_ += 1;
        continue;
      }

      String8_View decoded_view(decoded);
      decoded.release();
      return decoded_view;
    }

    case u8'\\':
      switch (this->input_[1]) {
      case u8'\\':
        decoded += u8'\\';
        this->input_ += 2;
        break;
      case u8'n':
        decoded += u8'\n';
        this->input_ += 2;
        break;
      case u8'\'':
      case u8'"':
        decoded += this->input_[1];
        this->input_ += 2;
        break;
      default:
        this->fatal();
        break;
      }
      break;

    default:
      decoded += *this->input_;
      this->input_ += 1;
      break;

    case u8'\0':
      this->fatal();
      break;
    }
  }
}

void CXX_Lexer::skip_whitespace_and_comments() {
  this->skip_whitespace();
  while (this->input_[0] == u8'/' && this->input_[1] == u8'/') {
    this->skip_line_comment();
    this->skip_whitespace();
  }
}

void CXX_Lexer::skip_whitespace() {
  while (u8"\r\n\t "_sv.find(*this->input_) != String8_View::npos) {
    this->input_ += 1;
  }
}

void CXX_Lexer::skip_line_comment() {
  QLJS_ASSERT(*this->input_ == u8'/');
  this->input_ += 1;
  if (*this->input_ != u8'/') {
    this->fatal();
  }
  this->input_ += 1;
  this->skip_to_end_of_line();
  this->skip_newline();
}

void CXX_Lexer::skip_preprocessor_directive() {
  QLJS_ASSERT(*this->input_ == u8'#');
  this->input_ += 1;
  this->skip_to_end_of_line();
  while (this->input_[-1] == u8'\\') {
    this->skip_newline();
    this->skip_to_end_of_line();
  }
}

void CXX_Lexer::skip_to_end_of_line() {
  while (u8"\0\r\n"_sv.find(*this->input_) == String8_View::npos) {
    this->input_ += 1;
  }
}

void CXX_Lexer::skip_newline() {
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

[[noreturn]] void CXX_Lexer::fatal() {
  CLI_Locator locator(this->original_input_);
  CLI_Source_Position p = locator.position(this->input_);
  std::fprintf(stderr, "%s:%d:%d: error: failed to lex\n", this->file_path_,
               p.line_number, p.column_number);
  std::exit(1);
}

std::uint16_t CXX_Diagnostic_Type::code_number() const {
  std::uint16_t code_number;
  Parse_Integer_Exact_Error error =
      parse_integer_exact(this->code_string.substr(2, 4), code_number);
  if (error != Parse_Integer_Exact_Error::ok) {
    QLJS_UNREACHABLE();  // check_diag_codes should have validated already.
  }
  return code_number;
}

const CXX_Diagnostic_Variable* CXX_Diagnostic_Type::variable_from_name(
    String8_View variable_name) const {
  auto it = find_unique_if(
      this->variables,
      [variable_name](const CXX_Diagnostic_Variable& var) -> bool {
        return var.name == variable_name;
      });
  if (it == this->variables.end()) {
    return nullptr;
  }
  return &*it;
}

CXX_Parser::CXX_Parser(const char* file_path, Padded_String_View input)
    : lexer_(file_path, input) {}

void CXX_Parser::parse_file() {
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
      this->reserved_code_strings.push_back(this->peek().decoded_string);
      this->skip();

      this->expect_skip(CXX_Token_Type::right_paren);
    } else {
      this->fatal("expected 'struct' or '}' or 'QLJS_RESERVED_DIAG'");
    }
  }
}

void CXX_Parser::parse_diagnostic_struct_body(
    String8_View diagnostic_struct_name) {
  CXX_Diagnostic_Type& type = this->parsed_types.emplace_back();
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
        type.code_string = this->peek().decoded_string;
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

        CXX_Diagnostic_Message& message = type.messages.emplace_back();

        this->expect(CXX_Token_Type::string_literal);
        message.message = this->peek().decoded_string;
        this->skip();

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
      CXX_Diagnostic_Variable& var = type.variables.emplace_back();

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

bool CXX_Parser::check_diag_codes() {
  bool ok = true;
  Hash_Map<String8_View, String8_View> code_to_diag_name;
  for (String8_View reserved_code_string : this->reserved_code_strings) {
    code_to_diag_name[reserved_code_string] = u8"(reserved)"_sv;
  }

  for (const CXX_Diagnostic_Type& type : this->parsed_types) {
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

bool CXX_Parser::is_valid_code_string(String8_View code_string) {
  return code_string.size() == 5 && code_string[0] == u8'E' &&
         this->lexer_.is_digit(code_string[1]) &&
         this->lexer_.is_digit(code_string[2]) &&
         this->lexer_.is_digit(code_string[3]) &&
         this->lexer_.is_digit(code_string[4]);
}

String8 CXX_Parser::next_unused_diag_code_string() {
  for (int i = 1; i <= 9999; ++i) {
    char code_string_raw[8];
    std::snprintf(code_string_raw, sizeof(code_string_raw), "E%04d", i);
    String8_View code_string = to_string8_view(code_string_raw);
    bool in_use = any_of(this->parsed_types,
                         [&](const CXX_Diagnostic_Type& type) {
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

void CXX_Parser::skip_preprocessor_directives() {
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

void CXX_Parser::expect_skip(CXX_Token_Type expected_token_type) {
  this->expect(expected_token_type);
  this->skip();
}

void CXX_Parser::expect(CXX_Token_Type expected_token_type) {
  if (this->peek().type != expected_token_type) {
    this->fatal(concat("expected "sv, to_string(expected_token_type)).c_str());
  }
}

void CXX_Parser::expect_skip(String8_View expected_identifier) {
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

[[noreturn]] void CXX_Parser::fatal(const char* message) {
  CLI_Locator locator(this->lexer_.original_input_);
  CLI_Source_Position p = locator.position(this->lexer_.input_);
  std::fprintf(stderr, "%s:%d:%d: error: failed to parse before: %s\n",
               this->lexer_.file_path_, p.line_number, p.column_number,
               message);
  std::exit(1);
}

Fixed_Vector<std::size_t, 4> layout_offsets(
    Span<const CXX_Diagnostic_Variable> variables) {
  struct Type_Info {
    String8_View name;
    std::size_t size;
    std::size_t alignment;
  };
  static constexpr Type_Info type_infos[] = {
#define TYPE_INFO(type)                   \
  {                                       \
      .name = QLJS_CPP_QUOTE_U8_SV(type), \
      .size = sizeof(type),               \
      .alignment = alignof(type),         \
  }

      TYPE_INFO(Char8),
      TYPE_INFO(Enum_Kind),
      TYPE_INFO(Source_Code_Span),
      TYPE_INFO(Statement_Kind),
      TYPE_INFO(String8_View),
      TYPE_INFO(Variable_Kind),

#undef TYPE_INFO
  };

  Fixed_Vector<std::size_t, 4> offsets;
  QLJS_ASSERT(variables.size() <= offsets.capacity());
  std::size_t offset = 0;
  for (const CXX_Diagnostic_Variable& var : variables) {
    const Type_Info& var_type_info = *find_unique_existing_if(
        type_infos, [&](const Type_Info& type_info) -> bool {
          return type_info.name == var.type;
        });
    offset = align_up(offset, var_type_info.alignment);
    offsets.push_back(offset);
    offset += var_type_info.size;
  }
  return offsets;
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
