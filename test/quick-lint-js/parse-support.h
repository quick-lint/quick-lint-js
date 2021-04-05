// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_SUPPORT_H
#define QUICK_LINT_JS_PARSE_SUPPORT_H

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
extern template void parser::parse_and_visit_module<spy_visitor>(
    spy_visitor &v);
extern template bool parser::parse_and_visit_statement<spy_visitor>(
    spy_visitor &v, bool allow_declarations);

// Escape the first character in the given keyword with a JavaScript identifier
// escape sequence (\u{..}).
//
// Example: break -> \u{62}reak
//
// The returned string will always be 5 bytes longer: +6 bytes for \u{??} and -1
// byte for the replaced character.
string8 escape_first_character_in_keyword(string8_view keyword);

namespace {
inline spy_visitor parse_and_visit_module(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, ::testing::IsEmpty());
  return v;
}

inline spy_visitor parse_and_visit_statement(string8_view raw_code) {
  padded_string code(raw_code);
  spy_visitor v;
  parser p(&code, &v);
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

// ReservedWord https://262.ecma-international.org/11.0/#prod-ReservedWord
constexpr inline std::array reserved_keywords = make_array(
    u8"await", u8"break", u8"case", u8"catch", u8"class", u8"const",
    u8"continue", u8"debugger", u8"default", u8"delete", u8"do", u8"else",
    u8"enum", u8"export", u8"extends", u8"false", u8"finally", u8"for",
    u8"function", u8"if", u8"import", u8"in", u8"instanceof", u8"new", u8"null",
    u8"return", u8"super", u8"switch", u8"this", u8"throw", u8"true", u8"try",
    u8"typeof", u8"var", u8"void", u8"while", u8"with", u8"yield");

// Exclusions from BindingIdentifier (ReservedWord except 'await' and 'yield')
// https://262.ecma-international.org/11.0/#prod-ReservedWord
// https://262.ecma-international.org/11.0/#prod-BindingIdentifier
std::vector<const char8 *> inline disallowed_binding_identifier_keywords =
    []() {
      std::vector<const char8 *> result;
      for (const char8 *keyword : reserved_keywords) {
        if (!(keyword == u8"await"_sv || keyword == u8"yield"_sv)) {
          result.push_back(keyword);
        }
      }
      return result;
    }();

constexpr std::array inline contextual_keywords =
    make_array(u8"as", u8"async", u8"from", u8"get", u8"let", u8"meta", u8"of",
               u8"set", u8"static", u8"target");

constexpr std::array inline keywords =
    concat(reserved_keywords, contextual_keywords);
}
}

#endif

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
