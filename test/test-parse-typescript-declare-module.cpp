// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Module : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Declare_Module, export_as_namespace) {
  Spy_Visitor p =
      test_parse_and_visit_module(u8"export as namespace mynamespace;"_sv,
                                  no_diags, typescript_definition_options);
  EXPECT_THAT(p.visits, ElementsAre("visit_end_of_module"));
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_allows_newlines) {
  Spy_Visitor p =
      test_parse_and_visit_module(u8"export\nas\nnamespace\nmynamespace\n;"_sv,
                                  no_diags, typescript_definition_options);
  EXPECT_THAT(p.visits, ElementsAre("visit_end_of_module"));
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_requires_semicolon_or_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export as namespace mynamespace\ndeclare let x;"_sv, no_diags,
        typescript_definition_options);
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_declare_scope",   //
                                      "visit_variable_declaration",  // x
                                      "visit_exit_declare_scope",    //
                                      "visit_end_of_module"));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"export as namespace mynamespace   declare let x;"_sv,  //
        u8"                               ` Diag_Missing_Semicolon_After_Statement"_diag,
        typescript_definition_options);
    EXPECT_THAT(p.visits, ElementsAre("visit_enter_declare_scope",   //
                                      "visit_variable_declaration",  // x
                                      "visit_exit_declare_scope",    //
                                      "visit_end_of_module"));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_allows_contextual_keywords) {
  for (String8_View keyword : contextual_keywords) {
    test_parse_and_visit_module(
        concat(u8"export as namespace "_sv, keyword, u8";"_sv), no_diags,
        typescript_definition_options);
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_not_allowed_in_javascript) {
  test_parse_and_visit_module(
      u8"export as namespace mynamespace;"_sv,  //
      u8"^^^^^^ Diag_TypeScript_Export_As_Namespace_Is_Only_Allowed_In_TypeScript_Definition_File"_diag,
      javascript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_not_allowed_in_typescript_non_definition) {
  test_parse_and_visit_module(
      u8"export as namespace mynamespace;"_sv,  //
      u8"^^^^^^ Diag_TypeScript_Export_As_Namespace_Is_Only_Allowed_In_TypeScript_Definition_File"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Declare_Module,
       export_as_namespace_not_allowed_in_namespace) {
  test_parse_and_visit_module(
      u8"declare namespace ns { export as namespace mynamespace; }"_sv,  //
      u8"                       ^^^^^^ Diag_TypeScript_Export_As_Namespace_Is_Not_Allowed_In_Namespace_Or_Module.export_keyword\n"_diag
      u8"        ^^^^^^^^^ .namespace_or_module_keyword",
      typescript_definition_options);

  test_parse_and_visit_module(
      u8"declare module 'mod' { export as namespace mynamespace; }"_sv,  //
      u8"                       ^^^^^^ Diag_TypeScript_Export_As_Namespace_Is_Not_Allowed_In_Namespace_Or_Module.export_keyword\n"_diag
      u8"        ^^^^^^ .namespace_or_module_keyword",
      typescript_definition_options);
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
