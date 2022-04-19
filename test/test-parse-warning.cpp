// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
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
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
TEST(test_parse, condition_with_assignment_from_literal) {
  {
    padded_string code(u8"if (x = 42) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_assignment_makes_condition_constant,  //
                    assignment_operator, strlen(u8"if (x "), u8"=")));
  }

  {
    padded_string code(u8"if (o.prop = 'hello') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_assignment_makes_condition_constant,  //
                    assignment_operator, strlen(u8"if (o.prop "), u8"=")));
  }

  for (string8_view code_view : {
           u8"while (x = 'hello') {}"_sv,
           u8"for (; x = 'hello'; ) {}"_sv,
           u8"do {} while (x = 'hello');"_sv,
       }) {
    padded_string code(code_view);
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE(diag_assignment_makes_condition_constant)));
  }
}

TEST(test_parse, non_condition_with_assignment_from_literal) {
  for (string8_view code_view : {
           u8"with (x = 'hello') {}"_sv,
           u8"for (x = 'hello'; ; ) {}"_sv,
           u8"for (; ; x = 'hello') {}"_sv,
           u8"switch (x = 'hello') {}"_sv,
       }) {
    padded_string code(code_view);
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, condition_with_assignment_from_literal_with_parentheses) {
  {
    padded_string code(u8"if ((x = 42)) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, condition_with_updating_assignment_from_literal) {
  {
    padded_string code(u8"if (x += 42) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse, condition_with_assignment_from_non_literal) {
  {
    padded_string code(u8"if (x = y) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_assignments,
                ElementsAre(spy_visitor::visited_variable_assignment{u8"x"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_error_equals_does_not_distribute_over_or, examples) {
  {
    padded_string code(u8"if (x === 'A' || 'B') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"x"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_OFFSETS(
                    &code, diag_equals_does_not_distribute_over_or,   //
                    or_operator, strlen(u8"if (x === 'A' "), u8"||",  //
                    equals_operator, strlen(u8"if (x "), u8"===")));
  }

  {
    padded_string code(u8"if (x === 10 || 0) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_OFFSETS(
                    &code, diag_equals_does_not_distribute_over_or,  //
                    or_operator, strlen(u8"if (x === 10 "), u8"||",  //
                    equals_operator, strlen(u8"if (x "), u8"===")));
  }

  {
    padded_string code(u8"if (x == 'A' || 'B') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_OFFSETS(
                    &code, diag_equals_does_not_distribute_over_or,  //
                    or_operator, strlen(u8"if (x == 'A' "), u8"||",  //
                    equals_operator, strlen(u8"if (x "), u8"==")));
  }
}

TEST(test_error_equals_does_not_distribute_over_or, not_equals) {
  {
    padded_string code(u8"if (x != 'A' || 'B') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"if (x !== 'A' || 'B') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_error_equals_does_not_distribute_over_or, logical_and) {
  {
    padded_string code(u8"if (x == 'A' && 'B') {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_error_equals_does_not_distribute_over_or, non_constant) {
  {
    padded_string code(u8"if (x === 'A' || y) {}"_sv);
    spy_visitor v;
    parser p(&code, &v);
    EXPECT_TRUE(p.parse_and_visit_statement(v));
    EXPECT_THAT(v.errors, IsEmpty());
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
