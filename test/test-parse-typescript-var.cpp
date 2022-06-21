// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
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
#include <quick-lint-js/string-view.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_typescript_var, let_can_have_type_annotation) {
  {
    spy_visitor v = parse_and_visit_typescript_statement(u8"let x: C;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // C
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.variable_declarations,
                ElementsAre(spy_visitor::visited_variable_declaration{
                    u8"x", variable_kind::_let, variable_init_kind::normal}));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"C"}));
  }

  {
    spy_visitor v =
        parse_and_visit_typescript_statement(u8"let x: C = init;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // C
                                      "visit_variable_use",            // init
                                      "visit_variable_declaration"));  // x
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"C"},
                            spy_visitor::visited_variable_use{u8"init"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"let [x, y, z]: Array = init;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",            // init
                                      "visit_variable_type_use",       // Array
                                      "visit_variable_declaration",    // x
                                      "visit_variable_declaration",    // y
                                      "visit_variable_declaration"));  // z
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"init"},
                            spy_visitor::visited_variable_use{u8"Array"}));
  }

  {
    spy_visitor v = parse_and_visit_typescript_statement(
        u8"let {p1, p2: x, p3 = y}: T;"_sv);
    EXPECT_THAT(v.visits, ElementsAre("visit_variable_type_use",       // T
                                      "visit_variable_declaration",    // p1
                                      "visit_variable_declaration",    // p2
                                      "visit_variable_use",            // y
                                      "visit_variable_declaration"));  // p3
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"T"},
                            spy_visitor::visited_variable_use{u8"y"}));
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
