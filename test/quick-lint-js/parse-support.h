// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/failing-diag-reporter.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/source-location.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class Expression;

// Escape the first character in the given keyword with a JavaScript identifier
// escape sequence (\u{..}).
//
// Example: break -> \u{62}reak
//
// The returned string will always be 5 bytes longer: +6 bytes for \u{??} and -1
// byte for the replaced character.
String8 escape_first_character_in_keyword(String8_View keyword);

void summarize(const Expression&, std::string& out);
void summarize(Expression*, std::string& out);
std::string summarize(Expression*);
std::string summarize(std::optional<Expression*>);

inline constexpr Parser_Options javascript_options = [] {
  Parser_Options options;
  options.jsx = false;
  options.typescript = false;
  return options;
}();

inline constexpr Parser_Options jsx_options = [] {
  Parser_Options options;
  options.jsx = true;
  return options;
}();

inline constexpr Parser_Options typescript_options = [] {
  Parser_Options options;
  options.typescript = true;
  return options;
}();

inline constexpr Parser_Options typescript_jsx_options = [] {
  Parser_Options options;
  options.jsx = true;
  options.typescript = true;
  return options;
}();

struct Capture_Diags_Tag {};
constexpr Capture_Diags_Tag capture_diags;

class Test_Parser {
 public:
  explicit Test_Parser(String8_View input, Capture_Diags_Tag)
      : Test_Parser(input, Parser_Options(), capture_diags) {}

  explicit Test_Parser(String8_View input, const Parser_Options& options,
                       Capture_Diags_Tag)
      : code_(input), parser_(&this->code_, &this->errors_, options) {}

  // Fails the test if there are any diagnostics during parsing.
  explicit Test_Parser(String8_View input)
      : Test_Parser(input, Parser_Options()) {}

  // Fails the test if there are any diagnostics during parsing.
  explicit Test_Parser(String8_View input, const Parser_Options& options)
      : code_(input),
        parser_(&this->code_, &this->failing_reporter_, options) {}

  Expression* parse_expression() {
    return this->parser_.parse_expression(this->errors_);
  }

  void parse_and_visit_expression() {
    this->parser_.parse_and_visit_expression(this->errors_);
  }

  void parse_and_visit_statement() {
    EXPECT_TRUE(this->parser_.parse_and_visit_statement(this->errors_));
  }

  void parse_and_visit_statement(Parser::Parse_Statement_Type statement_type) {
    EXPECT_TRUE(
        this->parser_.parse_and_visit_statement(this->errors_, statement_type));
  }

  void parse_and_visit_module() {
    this->parser_.parse_and_visit_module(this->errors_);
  }

  bool parse_and_visit_module_catching_fatal_parse_errors() {
    return this->parser_.parse_and_visit_module_catching_fatal_parse_errors(
        this->errors_);
  }

  void parse_and_visit_typescript_type_expression() {
    this->parser_.parse_and_visit_typescript_type_expression(this->errors_);
  }

  void parse_and_visit_typescript_generic_parameters() {
    this->parser_.parse_and_visit_typescript_generic_parameters(this->errors_);
  }

  [[nodiscard]] quick_lint_js::Parser::Class_Guard enter_class() {
    return this->parser_.enter_class();
  }

  [[nodiscard]] quick_lint_js::Parser::Loop_Guard enter_loop() {
    return this->parser_.enter_loop();
  }

  [[nodiscard]] quick_lint_js::Parser::Function_Guard enter_function(
      Function_Attributes attributes) {
    return this->parser_.enter_function(attributes);
  }

  // See offsets_matcher's constructor.
  Offsets_Matcher matches_offsets(CLI_Source_Position::Offset_Type begin_offset,
                                  String8_View text) {
    return Offsets_Matcher(&this->code_, begin_offset, text);
  }

  // See offsets_matcher's constructor.
  Offsets_Matcher matches_offsets(CLI_Source_Position::Offset_Type begin_offset,
                                  CLI_Source_Position::Offset_Type end_offset) {
    return Offsets_Matcher(&this->code_, begin_offset, end_offset);
  }

  Spy_Visitor& spy_visitor() { return this->errors_; }

  void assert_diagnostics(Span<const Diagnostic_Assertion> diags,
                          Source_Location caller);

 private:
  Padded_String code_;
  Spy_Visitor errors_;
  Failing_Diag_Reporter failing_reporter_;
  quick_lint_js::Parser parser_;

 public:
  // Aliases for convenience.
  std::vector<std::string_view>& visits = this->errors_.visits;
  std::vector<String8>& enter_named_function_scopes =
      this->errors_.enter_named_function_scopes;
  std::vector<std::optional<String8>>& property_declarations =
      this->errors_.property_declarations;
  std::vector<String8>& variable_assignments =
      this->errors_.variable_assignments;
  std::vector<Visited_Variable_Declaration>& variable_declarations =
      this->errors_.variable_declarations;
  std::vector<String8>& variable_uses = this->errors_.variable_uses;
  std::vector<Diag_Collector::Diag>& errors = this->errors_.errors;
  Padded_String_View code = Padded_String_View(&this->code_);
};

struct No_Diags_Tag {};
constexpr inline No_Diags_Tag no_diags;

// Create a Parser and call Parser::parse_and_visit_statement. Assert that
// exactly the given diagnostics were emitted. See NOTE[_diag-syntax] for
// examples.
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, No_Diags_Tag,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Span<const Diagnostic_Assertion>, Parser_Options,
    Source_Location caller = Source_Location::current());

// Create a Parser and call Parser::parse_and_visit_module. Assert that
// exactly the given diagnostics were emitted. See NOTE[_diag-syntax] for
// examples.
Spy_Visitor test_parse_and_visit_module(
    String8_View input, No_Diags_Tag,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_module(
    String8_View input, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_module(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_module(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_module(
    String8_View input, Span<const Diagnostic_Assertion>, Parser_Options,
    Source_Location caller = Source_Location::current());

// Create a Parser and call Parser::parse_and_visit_expression. Assert that
// exactly the given diagnostics were emitted. See NOTE[_diag-syntax] for
// examples.
Spy_Visitor test_parse_and_visit_expression(
    String8_View input, No_Diags_Tag,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_expression(
    String8_View input, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_expression(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_expression(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion,
    Parser_Options = javascript_options,  // TODO(strager): Make explicit.
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_expression(
    String8_View input, Span<const Diagnostic_Assertion>, Parser_Options,
    Source_Location caller = Source_Location::current());

// Create a Parser and call Parser::parse_and_visit_typescript_type_expression.
// Assert that exactly the given diagnostics were emitted. See
// NOTE[_diag-syntax] for examples.
Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, No_Diags_Tag, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Parser_Options, Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Span<const Diagnostic_Assertion>, Parser_Options,
    Source_Location caller = Source_Location::current());

// Create a Parser and call
// Parser::parse_and_visit_typescript_generic_parameters. Assert that exactly
// the given diagnostics were emitted. See NOTE[_diag-syntax] for examples.
Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, No_Diags_Tag, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Parser_Options, Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    Diagnostic_Assertion, Parser_Options,
    Source_Location caller = Source_Location::current());
Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Span<const Diagnostic_Assertion>, Parser_Options,
    Source_Location caller = Source_Location::current());

// TODO(strager): Delete.
class Test_Parse_Expression : public ::testing::Test {};

namespace {
// Identifiers which are ReservedWord-s only in strict mode.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
const inline Dirty_Set<String8> strict_only_reserved_keywords = {
    u8"implements", u8"interface", u8"package",
    u8"private",    u8"protected", u8"public",
};

// Exclusions from BindingIdentifier (ReservedWord except 'await' and 'yield')
// https://262.ecma-international.org/11.0/#prod-ReservedWord
// https://262.ecma-international.org/11.0/#prod-BindingIdentifier
const inline Dirty_Set<String8> disallowed_binding_identifier_keywords = {
    u8"break",    u8"case",       u8"catch",    u8"class",   u8"const",
    u8"continue", u8"debugger",   u8"default",  u8"delete",  u8"do",
    u8"else",     u8"enum",       u8"export",   u8"extends", u8"false",
    u8"finally",  u8"for",        u8"function", u8"if",      u8"import",
    u8"in",       u8"instanceof", u8"new",      u8"null",    u8"return",
    u8"super",    u8"switch",     u8"this",     u8"throw",   u8"true",
    u8"try",      u8"typeof",     u8"var",      u8"void",    u8"while",
    u8"with",
};
const inline Dirty_Set<String8> strict_disallowed_binding_identifier_keywords =
    disallowed_binding_identifier_keywords | strict_only_reserved_keywords;

// ReservedWord in non-strict mode.
// https://262.ecma-international.org/11.0/#prod-ReservedWord
const inline Dirty_Set<String8> reserved_keywords =
    disallowed_binding_identifier_keywords |
    Dirty_Set<String8>{u8"await", u8"yield"};
// ReservedWord in strict mode. Includes all of reserved_keywords.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
const inline Dirty_Set<String8> strict_reserved_keywords =
    strict_disallowed_binding_identifier_keywords |
    Dirty_Set<String8>{u8"await", u8"yield"};

// TODO(strager): Add 'await' and 'yield'.
const inline Dirty_Set<String8> contextual_keywords = {
    u8"abstract",  u8"any",       u8"as",       u8"assert",      u8"asserts",
    u8"async",     u8"bigint",    u8"boolean",  u8"constructor", u8"declare",
    u8"from",      u8"get",       u8"global",   u8"infer",       u8"intrinsic",
    u8"is",        u8"keyof",     u8"let",      u8"meta",        u8"module",
    u8"namespace", u8"never",     u8"number",   u8"object",      u8"of",
    u8"out",       u8"override",  u8"readonly", u8"require",     u8"satisfies",
    u8"set",       u8"static",    u8"string",   u8"symbol",      u8"target",
    u8"type",      u8"undefined", u8"unique",   u8"unknown",
};

// ReservedWord or contextual keyword in strict mode or non-strict mode.
const inline Dirty_Set<String8> keywords =
    strict_reserved_keywords | contextual_keywords;

const inline Dirty_Set<String8> typescript_builtin_type_keywords = {
    u8"bigint", u8"boolean", u8"null",      u8"number", u8"object",
    u8"string", u8"symbol",  u8"undefined", u8"void",
};

const inline Dirty_Set<String8> typescript_special_type_keywords = {
    u8"any",
    u8"never",
    u8"unknown",
};

// Identifiers which are always keywords inside TypeScript types but never
// keywords outside TypeScript types.
const inline Dirty_Set<String8> typescript_type_only_keywords = {
    u8"infer",
};
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
