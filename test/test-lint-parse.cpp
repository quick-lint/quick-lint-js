// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
global_declared_variable_set default_globals = configuration().globals();

TEST(test_lint, let_variable_use_before_declaration_with_parsing) {
  padded_string input(u8"let x = y, y = x;"_sv);
  error_collector v;
  linter l(&v, &default_globals);
  parser p(&input, &v);
  EXPECT_TRUE(p.parse_and_visit_statement(l));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                            error_variable_used_before_declaration,  //
                            use, offsets_matcher(&input, 8, 9),      //
                            declaration, offsets_matcher(&input, 11, 12))));
}

TEST(
    test_lint,
    variables_with_different_escape_sequences_are_equivalent_after_normalization) {
  padded_string input(u8"let \\u{69} = 0; i += 1; \\u0069;"_sv);
  error_collector v;

  linter l(&v, &default_globals);
  parser p(&input, &v);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
     errors_for_variables_with_escape_sequences_cover_entire_variable_name) {
  padded_string input(u8R"(const immut\u{61}ble = 0; immut\u{61}ble = 1;)"_sv);
  error_collector v;

  linter l(&v, &default_globals);
  parser p(&input, &v);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors,
              ElementsAre(ERROR_TYPE_2_FIELDS(
                  error_assignment_to_const_variable,                //
                  assignment, offsets_matcher(&input, 26, 26 + 14),  //
                  declaration, offsets_matcher(&input, 6, 6 + 14))));
}

TEST(test_lint, escape_sequences_are_allowed_for_arguments_variable) {
  padded_string input(u8R"(function f() { return \u{61}rgument\u{73}; })"_sv);
  error_collector v;

  linter l(&v, &default_globals);
  parser p(&input, &v);
  p.parse_and_visit_module(l);

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint,
     function_statement_inside_if_does_not_conflict_with_let_variable) {
  padded_string input(u8"let f;\nif (true)\n  function f() {}"_sv);

  error_collector v;
  linter l(&v, &default_globals);
  parser p(&input, &v);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_lint, typeof_with_conditional_operator) {
  {
    padded_string input(u8"typeof x ? 10 : 20;"_sv);
    error_collector v;
    linter l(&v, &default_globals);
    parser p(&input, &v);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, prefix_plusplus_on_const_variable) {
  {
    padded_string input(u8"const x = 42; ++x;"_sv);
    error_collector v;
    linter l(&v, &default_globals);
    parser p(&input, &v);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_assignment_to_const_variable, assignment,
                              offsets_matcher(&input, 16, 16 + 1), declaration,
                              offsets_matcher(&input, 6, 6 + 1))));
  }

  {
    padded_string input(u8"const x = {y : 10};\n ++x.y;"_sv);
    error_collector v;
    linter l(&v, &default_globals);
    parser p(&input, &v);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_lint, prefix_plusplus_plus_operand) {
  {
    padded_string input(u8"const x = [42]; ++x[0];"_sv);
    error_collector v;
    linter l(&v, &default_globals);
    parser p(&input, &v);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string input(u8"const x = 42;\n const y =10;\n ++x + y;"_sv);
    error_collector v;
    linter l(&v, &default_globals);
    parser p(&input, &v);
    p.parse_and_visit_module(l);
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_2_FIELDS(
                              error_assignment_to_const_variable, assignment,
                              offsets_matcher(&input, 31, 31 + 1), declaration,
                              offsets_matcher(&input, 6, 6 + 1))));
  }
}


TEST(test_lint, use_await_label_in_non_async_function) {
  padded_string input(u8"function f() {await: for(;;){break await;}}"_sv);
  error_collector v;
  linter l(&v, &default_globals);
  parser p(&input, &v);
  p.parse_and_visit_module(l);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
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
