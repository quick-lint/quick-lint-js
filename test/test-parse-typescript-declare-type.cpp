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
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class test_parse_typescript_declare_type : public test_parse_expression {};

TEST_F(test_parse_typescript_declare_type, declare_type_acts_like_type) {
  {
    test_parser p(u8"declare type MyType = OtherType;"_sv, typescript_options);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // MyType
                              "visit_enter_type_alias_scope",  //
                              "visit_variable_type_use",       // OtherType
                              "visit_exit_type_alias_scope",   //
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OtherType"_sv}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"MyType"_sv)}));
  }
}

TEST_F(test_parse_typescript_declare_type,
       declare_before_type_keyword_triggers_asi) {
  {
    test_parser p(u8"declare\ntype MyType = OtherType;"_sv, typescript_options);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",            // declare
                              "visit_variable_declaration",    // MyType
                              "visit_enter_type_alias_scope",  //
                              "visit_variable_type_use",       // OtherType
                              "visit_exit_type_alias_scope",   //
                              "visit_end_of_module",           //
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
