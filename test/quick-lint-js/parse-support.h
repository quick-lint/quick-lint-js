// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_SUPPORT_H
#define QUICK_LINT_JS_PARSE_SUPPORT_H

#include <array>
#include <deque>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/null-visitor.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
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

class test_parser {
 public:
  explicit test_parser(padded_string_view input)
      : test_parser(input.string_view()) {}

  explicit test_parser(string8_view input)
      : test_parser(input, parser_options()) {}

  explicit test_parser(string8_view input, const parser_options& options)
      : code_(input),
        locator(&this->code_),
        parser_(&this->code_, &this->errors_, options) {}

  expression* parse_expression() {
    null_visitor v;
    expression* ast = this->parser_.parse_expression(v);
    this->expressions_needing_cleanup_.push_back(ast);
    return ast;
  }

  const std::vector<diag_collector::diag>& errors() const noexcept {
    return this->errors_.errors;
  }

  cli_source_range range(expression* ast) { return this->range(ast->span()); }

  cli_source_range range(source_code_span span) {
    return this->locator.range(span);
  }

  padded_string_view code() const noexcept { return &this->code_; }

  quick_lint_js::parser& parser() noexcept { return this->parser_; }

 private:
  padded_string code_;

 public:
  cli_locator locator;

 private:
  diag_collector errors_;
  quick_lint_js::parser parser_;
  std::vector<expression*> expressions_needing_cleanup_;
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

  test_parser& make_parser(string8_view input, const parser_options& options) {
    return this->parsers_.emplace_back(input, options);
  }

 private:
  std::deque<test_parser> parsers_;
};

namespace {
constexpr parser_options jsx_options = [] {
  parser_options options;
  options.jsx = true;
  return options;
}();

inline spy_visitor parse_and_visit_module(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, ::testing::IsEmpty());
  return v;
}

inline spy_visitor parse_and_visit_statement(string8_view raw_code,
                                             parser_options options) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v, options);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.errors, ::testing::IsEmpty());
  return v;
}

inline spy_visitor parse_and_visit_statement(string8_view raw_code) {
  return parse_and_visit_statement(raw_code, parser_options());
}

inline spy_visitor parse_and_visit_statement(padded_string_view raw_code) {
  return parse_and_visit_statement(raw_code.string_view());
}

inline spy_visitor parse_and_visit_statement(string8_view raw_code,
                                             function_attributes attributes) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  auto guard = p.enter_function(attributes);
  EXPECT_TRUE(p.parse_and_visit_statement(v));
  EXPECT_THAT(v.errors, ::testing::IsEmpty());
  return v;
}

inline spy_visitor parse_and_visit_expression(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_expression(v);
  EXPECT_THAT(v.errors, ::testing::IsEmpty());
  return v;
}

// Identifiers which are ReservedWord-s only in strict mode.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
constexpr inline std::array strict_only_reserved_keywords =
    make_array(u8"implements", u8"interface", u8"package", u8"private",
               u8"protected", u8"public");

// Exclusions from BindingIdentifier (ReservedWord except 'await' and 'yield')
// https://262.ecma-international.org/11.0/#prod-ReservedWord
// https://262.ecma-international.org/11.0/#prod-BindingIdentifier
constexpr inline std::array disallowed_binding_identifier_keywords = make_array(
    u8"break", u8"case", u8"catch", u8"class", u8"const", u8"continue",
    u8"debugger", u8"default", u8"delete", u8"do", u8"else", u8"enum",
    u8"export", u8"extends", u8"false", u8"finally", u8"for", u8"function",
    u8"if", u8"import", u8"in", u8"instanceof", u8"new", u8"null", u8"return",
    u8"super", u8"switch", u8"this", u8"throw", u8"true", u8"try", u8"typeof",
    u8"var", u8"void", u8"while", u8"with");
constexpr inline std::array strict_disallowed_binding_identifier_keywords =
    concat(disallowed_binding_identifier_keywords,
           strict_only_reserved_keywords);

// ReservedWord in non-strict mode.
// https://262.ecma-international.org/11.0/#prod-ReservedWord
constexpr inline std::array reserved_keywords = concat(
    disallowed_binding_identifier_keywords, make_array(u8"await", u8"yield"));
// ReservedWord in strict mode. Includes all of reserved_keywords.
// https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
constexpr inline std::array strict_reserved_keywords =
    concat(strict_disallowed_binding_identifier_keywords,
           make_array(u8"await", u8"yield"));

constexpr inline std::array contextual_keywords =
    make_array(u8"as", u8"async", u8"from", u8"get", u8"let", u8"meta", u8"of",
               u8"set", u8"static", u8"target");

// ReservedWord or contextual keyword in strict mode or non-strict mode.
constexpr inline std::array keywords =
    concat(reserved_keywords, contextual_keywords);
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
