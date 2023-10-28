// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <string_view>

namespace quick_lint_js {
class CLI_Locator;
class CXX_Diagnostic_Types_Parser;
class CXX_Parser_Base;

enum class CXX_Token_Type {
  end_of_file,
  identifier,
  number_literal,
  string_literal,

  ampersand,            // &
  ampersand_ampersand,  // &&
  bang,                 // !
  bang_equal,           // !=
  colon,                // :
  colon_colon,          // ::
  comma,                // ,
  dot,                  // .
  equal,                // =
  equal_equal,          // ==
  greater,              // >
  left_curly,           // {
  left_paren,           // (
  left_square,          // [
  less,                 // <
  right_curly,          // }
  right_paren,          // )
  right_square,         // ]
  semicolon,            // ;
};

std::string_view to_string(CXX_Token_Type);

struct CXX_Token {
  CXX_Token_Type type;

  // If type == CXX_Token_Type::identifier:
  String8_View identifier;
  // If type == CXX_Token_Type::string_literal:
  String8_View decoded_string;
  // If type == CXX_Token_Type::number_literal:
  std::uint64_t decoded_number;
};

class CXX_Lexer {
 public:
  explicit CXX_Lexer(Padded_String_View input, const char* file_path,
                     CLI_Locator* locator);

  const CXX_Token& peek() { return this->token_; }

  void skip() { this->parse_token(); }

  const Char8* remaining() { return this->input_; }

  void error_at(const Char8* location, const char* message, ...);
  void error_at_v(const Char8* location, const char* message, std::va_list);

  [[noreturn]] void fatal();
  [[noreturn]] void fatal(const char* message, ...);
  [[noreturn]] void fatal_at(const Char8* location, const char* message, ...);
  [[noreturn]] void fatal_at_v(const Char8* location, const char* message,
                               std::va_list);

 private:
  void parse_token();

  bool is_identifier_start(Char8 c);
  bool is_identifier_continue(Char8 c);
  bool is_digit(Char8 c);

  String8_View lex_identifier();
  String8_View lex_string_literal();

  void skip_whitespace_and_comments();
  void skip_whitespace();
  void skip_line_comment();
  void skip_preprocessor_directive();
  void skip_to_end_of_line();
  void skip_newline();

  CXX_Token token_;
  const Char8* input_;

  const char* file_path_;
  CLI_Locator* locator_;

  Monotonic_Allocator decoded_string_allocator_{
      "CXX_Lexer::decoded_string_allocator_"};

  friend CXX_Diagnostic_Types_Parser;
  friend CXX_Parser_Base;
};

struct CXX_Diagnostic_Message {
  String8_View message;
  Fixed_Vector<String8_View, 4> argument_variables;
};

struct CXX_Diagnostic_Variable {
  String8_View type;
  String8_View name;
};

struct CXX_Diagnostic_Type {
  String8_View name;
  String8_View code_string;
  const Char8* code_string_location;
  String8_View severity;
  Fixed_Vector<CXX_Diagnostic_Message, 4> messages;
  Fixed_Vector<CXX_Diagnostic_Variable, 4> variables;

  std::uint16_t code_number() const;

  // Returns nullptr on failure.
  const CXX_Diagnostic_Variable* variable_from_name(
      String8_View variable_name) const;
};

class CXX_Parser_Base {
 public:
  explicit CXX_Parser_Base(Padded_String_View input, const char* file_path,
                           CLI_Locator* locator);

  const char* file_path() const { return this->lexer_.file_path_; }
  CLI_Locator& locator() const { return *this->lexer_.locator_; }

 protected:
  void skip_preprocessor_directives();

  const CXX_Token& peek() { return this->lexer_.peek(); }
  void skip() { this->lexer_.skip(); }

  bool peek_is(CXX_Token_Type);
  bool peek_is(String8_View identifier);

  void expect_skip(CXX_Token_Type expected_token_type);
  void expect(CXX_Token_Type expected_token_type);
  void expect_skip(String8_View expected_identifier);

  String8_View expect_skip_identifier();

 public:
  void error_at(const Char8* location, const char* message, ...);
  void error_at_v(const Char8* location, const char* message, std::va_list);

  [[noreturn]] void fatal(const char* message, ...);
  [[noreturn]] void fatal_at(const Char8* location, const char* message, ...);
  [[noreturn]] void fatal_at_v(const Char8* location, const char* message,
                               std::va_list);

 protected:
  CXX_Lexer lexer_;
};

// Parses <quick-lint-js/diag/diagnostic-types-2.h>.
class CXX_Diagnostic_Types_Parser : private CXX_Parser_Base {
 private:
  using Base = CXX_Parser_Base;

 public:
  using Base::Base;

  struct Diag_Code_Definition {
    const Char8* location;
    // Name of the class starting with "Diag_", or "(reserved)".
    String8_View diag_type;
    String8_View code;
  };

  void parse_file();

  void parse_diagnostic_struct_body(String8_View diagnostic_struct_name);

  bool check_diag_codes();

  bool is_valid_code_string(String8_View code_string);

  String8_View next_unused_diag_code_string();

  using Base::fatal_at;

  Monotonic_Allocator allocator_{"CXX_Diagnostic_Types_Parser"};
  Bump_Vector<CXX_Diagnostic_Type, Monotonic_Allocator> parsed_types{
      "parsed_types", &this->allocator_};
  Bump_Vector<Diag_Code_Definition, Monotonic_Allocator> reserved_codes{
      "reserved_codes", &this->allocator_};
};

// Precondition: variables.size() <= 4
//
// Returns a corresponding array of the offset of each variable in a class. In
// other words, return the result of offsetof() of each variable.
Fixed_Vector<std::size_t, 4> layout_offsets(
    Span<const CXX_Diagnostic_Variable> variables);
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
