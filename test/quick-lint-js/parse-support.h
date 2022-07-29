// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_SUPPORT_H
#define QUICK_LINT_JS_PARSE_SUPPORT_H

#include <array>
#include <boost/container/pmr/global_resource.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/dirty-set.h>
#include <quick-lint-js/failing-diag-reporter.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class expression;

// Escape the first character in the given keyword with a JavaScript identifier
// escape sequence (\u{..}).
//
// Example: break -> \u{62}reak
//
// The returned string will always be 5 bytes longer: +6 bytes for \u{??} and -1
// byte for the replaced character.
string8 escape_first_character_in_keyword(string8_view keyword);

std::string summarize(const expression&);
std::string summarize(expression*);
std::string summarize(std::optional<expression*>);

inline constexpr parser_options javascript_options = [] {
  parser_options options;
  options.jsx = false;
  options.typescript = false;
  return options;
}();

inline constexpr parser_options jsx_options = [] {
  parser_options options;
  options.jsx = true;
  return options;
}();

inline constexpr parser_options typescript_options = [] {
  parser_options options;
  options.typescript = true;
  return options;
}();

inline constexpr parser_options typescript_jsx_options = [] {
  parser_options options;
  options.jsx = true;
  options.typescript = true;
  return options;
}();

class test_parser {
 public:
  struct fail_test_on_diag {};

  explicit test_parser(padded_string_view input)
      : test_parser(input.string_view()) {}

  explicit test_parser(string8_view input)
      : test_parser(input, parser_options()) {}

  explicit test_parser(string8_view input, const parser_options& options)
      : code_(input), parser_(&this->code_, &this->errors_, options) {}

  explicit test_parser(string8_view input, const parser_options& options,
                       fail_test_on_diag)
      : code_(input),
        parser_(&this->code_, &this->failing_reporter_, options) {}

  expression* parse_expression() {
    expression* ast = this->parser_.parse_expression(this->errors_);
    this->expressions_needing_cleanup_.push_back(ast);
    return ast;
  }

  void parse_and_visit_statement() {
    EXPECT_TRUE(this->parser_.parse_and_visit_statement(this->errors_));
  }

  const std::vector<diag_collector::diag>& errors() const noexcept {
    return this->errors_.errors;
  }

  spy_visitor& v() noexcept { return this->errors_; }

  padded_string_view code() const noexcept { return &this->code_; }

  quick_lint_js::parser& parser() noexcept { return this->parser_; }

  // See offsets_matcher's constructor.
  offsets_matcher matches_offsets(cli_source_position::offset_type begin_offset,
                                  string8_view text) {
    return offsets_matcher(&this->code_, begin_offset, text);
  }

  // See offsets_matcher's constructor.
  offsets_matcher matches_offsets(cli_source_position::offset_type begin_offset,
                                  cli_source_position::offset_type end_offset) {
    return offsets_matcher(&this->code_, begin_offset, end_offset);
  }

 private:
  padded_string code_;
  spy_visitor errors_;
  failing_diag_reporter failing_reporter_;
  quick_lint_js::parser parser_;
  std::vector<expression*> expressions_needing_cleanup_;

 public:
  // Aliases for convenience.
  std::vector<std::string_view>& visits = this->errors_.visits;
  std::vector<string8>& enter_named_function_scopes =
      this->errors_.enter_named_function_scopes;
  std::vector<std::optional<string8>>& property_declarations =
      this->errors_.property_declarations;
  std::vector<string8>& variable_assignments =
      this->errors_.variable_assignments;
  std::vector<visited_variable_declaration>& variable_declarations =
      this->errors_.variable_declarations;
  std::vector<string8>& variable_uses = this->errors_.variable_uses;
};

class test_parse_expression : public ::testing::Test {
 protected:
  expression* parse_expression(string8_view input) {
    return this->parse_expression(input, parser_options());
  }

  expression* parse_expression(string8_view input,
                               const parser_options& options) {
    test_parser& p = this->make_parser(input, options);

    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(), ::testing::IsEmpty()) << out_string8(input);
    return ast;
  }

  test_parser& make_parser(string8_view input) {
    return this->parsers_.emplace_back(input);
  }

  test_parser& make_javascript_parser(string8_view input) {
    return this->make_parser(input, javascript_options);
  }

  test_parser& make_typescript_parser(string8_view input) {
    return this->make_parser(input, typescript_options);
  }

  test_parser& make_parser(string8_view input, const parser_options& options) {
    return this->parsers_.emplace_back(input, options);
  }

  // Fails the test if there are any diagnostics during parsing.
  test_parser& errorless_parser(string8_view input) {
    return this->errorless_parser(input, parser_options());
  }

  // Fails the test if there are any diagnostics during parsing.
  test_parser& errorless_parser(string8_view input,
                                const parser_options& options) {
    return this->parsers_.emplace_back(input, options,
                                       test_parser::fail_test_on_diag{});
  }

 private:
  linked_vector<test_parser> parsers_ = linked_vector<test_parser>(
      ::boost::container::pmr::new_delete_resource());
};

namespace {
inline parse_visit_collector parse_and_visit_module(string8_view raw_code) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter);
  parse_visit_collector v;
  p.parse_and_visit_module(v);
  return v;
}

inline parse_visit_collector parse_and_visit_statement(string8_view raw_code,
                                                       parser_options options) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter, options);
  parse_visit_collector v;
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  return v;
}

inline parse_visit_collector parse_and_visit_statement(string8_view raw_code) {
  return parse_and_visit_statement(raw_code, parser_options());
}

inline parse_visit_collector parse_and_visit_statement(
    padded_string_view raw_code) {
  return parse_and_visit_statement(raw_code.string_view());
}

inline parse_visit_collector parse_and_visit_statement(
    string8_view raw_code, function_attributes attributes) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter);
  parse_visit_collector v;
  auto guard = p.enter_function(attributes);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  return v;
}

inline parse_visit_collector parse_and_visit_typescript_module(
    string8_view raw_code) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter, typescript_options);
  parse_visit_collector v;
  p.parse_and_visit_module(v);
  return v;
}

inline parse_visit_collector parse_and_visit_typescript_statement(
    string8_view raw_code) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter, typescript_options);
  parse_visit_collector v;
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  return v;
}

inline parse_visit_collector parse_and_visit_typescript_type(
    string8_view raw_code) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter, typescript_options);
  parse_visit_collector v;
  p.parse_and_visit_typescript_type_expression(v);
  return v;
}

inline parse_visit_collector parse_and_visit_expression(string8_view raw_code) {
  padded_string code(raw_code);
  failing_diag_reporter reporter;
  parser p(&code, &reporter);
  parse_visit_collector v;
  p.parse_and_visit_expression(v);
  return v;
}

// Identifiers which are ReservedWord-s only in strict mode.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
const inline dirty_set<string8> strict_only_reserved_keywords = {
    u8"implements", u8"interface", u8"package",
    u8"private",    u8"protected", u8"public",
};

// Exclusions from BindingIdentifier (ReservedWord except 'await' and 'yield')
// https://262.ecma-international.org/11.0/#prod-ReservedWord
// https://262.ecma-international.org/11.0/#prod-BindingIdentifier
const inline dirty_set<string8> disallowed_binding_identifier_keywords = {
    u8"break",    u8"case",       u8"catch",    u8"class",   u8"const",
    u8"continue", u8"debugger",   u8"default",  u8"delete",  u8"do",
    u8"else",     u8"enum",       u8"export",   u8"extends", u8"false",
    u8"finally",  u8"for",        u8"function", u8"if",      u8"import",
    u8"in",       u8"instanceof", u8"new",      u8"null",    u8"return",
    u8"super",    u8"switch",     u8"this",     u8"throw",   u8"true",
    u8"try",      u8"typeof",     u8"var",      u8"void",    u8"while",
    u8"with",
};
const inline dirty_set<string8> strict_disallowed_binding_identifier_keywords =
    disallowed_binding_identifier_keywords | strict_only_reserved_keywords;

// ReservedWord in non-strict mode.
// https://262.ecma-international.org/11.0/#prod-ReservedWord
const inline dirty_set<string8> reserved_keywords =
    disallowed_binding_identifier_keywords |
    dirty_set<string8>{u8"await", u8"yield"};
// ReservedWord in strict mode. Includes all of reserved_keywords.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
const inline dirty_set<string8> strict_reserved_keywords =
    strict_disallowed_binding_identifier_keywords |
    dirty_set<string8>{u8"await", u8"yield"};

// TODO(strager): Add 'await' and 'yield'.
const inline dirty_set<string8> contextual_keywords = {
    u8"abstract",  u8"any",      u8"as",       u8"assert",      u8"asserts",
    u8"async",     u8"bigint",   u8"boolean",  u8"constructor", u8"declare",
    u8"from",      u8"get",      u8"global",   u8"infer",       u8"intrinsic",
    u8"is",        u8"keyof",    u8"let",      u8"meta",        u8"module",
    u8"namespace", u8"never",    u8"number",   u8"object",      u8"of",
    u8"out",       u8"override", u8"readonly", u8"require",     u8"set",
    u8"static",    u8"string",   u8"symbol",   u8"target",      u8"type",
    u8"undefined", u8"unique",   u8"unknown",
};

// ReservedWord or contextual keyword in strict mode or non-strict mode.
const inline dirty_set<string8> keywords =
    strict_reserved_keywords | contextual_keywords;

const inline dirty_set<string8> typescript_builtin_type_keywords = {
    u8"bigint", u8"boolean", u8"null",      u8"number", u8"object",
    u8"string", u8"symbol",  u8"undefined", u8"void",
};

const inline dirty_set<string8> typescript_special_type_keywords = {
    u8"any",
    u8"never",
    u8"unknown",
};
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
