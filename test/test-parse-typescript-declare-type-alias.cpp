// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag-collector.h>
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

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Declare_Type_Alias : public Test_Parse_Expression {
};

TEST_F(Test_Parse_TypeScript_Declare_Type_Alias, declare_type_acts_like_type) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"declare type MyType = OtherType;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // MyType
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // OtherType
                              "visit_exit_type_scope",       //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OtherType"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"MyType"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Declare_Type_Alias,
       declare_before_type_keyword_triggers_asi) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"declare\ntype MyType = OtherType;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // declare
                              "visit_variable_declaration",  // MyType
                              "visit_enter_type_scope",      //
                              "visit_variable_type_use",     // OtherType
                              "visit_exit_type_scope",       //
                              "visit_end_of_module",         //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"declare"_sv, u8"OtherType"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"MyType"_sv)}));
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
