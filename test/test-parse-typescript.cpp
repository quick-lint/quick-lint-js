// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript, type_annotation_in_expression_is_an_error) {
  {
    padded_string code(u8"x = myVar: Type;"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",          // myVar
                                      "visit_variable_assignment"))  // x
        << "visit_variable_type_use for Type should not happen because it "
           "might produce spurious warnings about undeclared types";
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"myVar"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code,
                              diag_typescript_type_annotation_in_expression,  //
                              type_colon, strlen(u8"x = myVar"), u8":")));
  }
}

TEST(test_parse_typescript, type_alias) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"type T = U;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",     // T
                                      "visit_enter_type_alias_scope",   // T
                                      "visit_variable_type_use",        // U
                                      "visit_exit_type_alias_scope"));  // T
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"U"));
    EXPECT_THAT(v.variable_declarations, ElementsAre(type_alias_decl(u8"T")));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"type MyAlias<T> = U;"_sv);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",     // MyAlias
                            "visit_enter_type_alias_scope",   // MyAlias
                            "visit_variable_declaration",     // T
                            "visit_variable_type_use",        // U
                            "visit_exit_type_alias_scope"));  // MyAlias
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"U"));
    EXPECT_THAT(
        v.variable_declarations,
        ElementsAre(type_alias_decl(u8"MyAlias"), generic_param_decl(u8"T")));
  }
}

TEST(test_parse_typescript, type_alias_requires_semicolon_or_asi) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"type T = U"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",     // T
                                      "visit_enter_type_alias_scope",   // T
                                      "visit_variable_type_use",        // U
                                      "visit_exit_type_alias_scope"));  // T
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_module(u8"type T = U\ntype V = W;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T
                                      "visit_enter_type_alias_scope",  // T
                                      "visit_variable_type_use",       // U
                                      "visit_exit_type_alias_scope",   // T
                                      "visit_variable_declaration",    // V
                                      "visit_enter_type_alias_scope",  // V
                                      "visit_variable_type_use",       // W
                                      "visit_exit_type_alias_scope",   // V
                                      "visit_end_of_module"));
  }

  {
    padded_string code(u8"type T = U type V = W;"_sv);
    spy_visitor v;
    parser p(&code, &v, typescript_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_declaration",    // T
                                      "visit_enter_type_alias_scope",  // T
                                      "visit_variable_type_use",       // U
                                      "visit_exit_type_alias_scope",   // T
                                      "visit_variable_declaration",    // V
                                      "visit_enter_type_alias_scope",  // V
                                      "visit_variable_type_use",       // W
                                      "visit_exit_type_alias_scope",   // V
                                      "visit_end_of_module"));
    EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_OFFSETS(
                              &code,
                              diag_missing_semicolon_after_statement,  //
                              where, strlen(u8"type T = U"), u8"")));
  }
}

TEST(test_parse_typescript,
     type_alias_can_be_named_certain_contextual_keywords) {
  for (string8 name :
       dirty_set<string8>{u8"await"} |
           (contextual_keywords - typescript_builtin_type_keywords -
            typescript_special_type_keywords -
            dirty_set<string8>{
                u8"let",
                u8"static",
                u8"yield",
            })) {
    string8 code = u8"type " + name + u8" = T;";
    SCOPED_TRACE(out_string8(code));
    spy_visitor v = parse_and_visit_typescript_statement(code);
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",     // (name)
                            "visit_enter_type_alias_scope",   // (name)
                            "visit_variable_type_use",        // T
                            "visit_exit_type_alias_scope"));  // (name)
    EXPECT_THAT(v.variable_declarations, ElementsAre(type_alias_decl(name)));
  }
}

TEST(test_parse_typescript, type_alias_cannot_have_newline_after_type_keyword) {
  {
    spy_visitor v = parse_and_visit_typescript_module(u8"type\nT = U;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // type
                                      "visit_variable_use",         // U
                                      "visit_variable_assignment",  // T
                                      "visit_end_of_module"));
    EXPECT_THAT(v.variable_uses, ElementsAre(u8"type", u8"U"));
  }
}

TEST(test_parse_typescript, type_alias_not_allowed_in_javascript) {
  {
    padded_string code(u8"type T = U;"_sv);
    spy_visitor v;
    parser p(&code, &v, javascript_options);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.visits,
                ElementsAre("visit_variable_declaration",     // T
                            "visit_enter_type_alias_scope",   // (name)
                            "visit_variable_type_use",        // U
                            "visit_exit_type_alias_scope"));  // (name)
    EXPECT_THAT(v.errors,
                ElementsAre(DIAG_TYPE_OFFSETS(
                    &code,
                    diag_typescript_type_alias_not_allowed_in_javascript,  //
                    type_keyword, 0, u8"type")));
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
