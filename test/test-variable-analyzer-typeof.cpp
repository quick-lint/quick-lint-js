// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_variable_analyzer_typeof,
     using_undeclared_variable_in_typeof_is_not_an_error) {
  const char8 use[] = u8"v";

  // typeof v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_typeof, typeof_declares_variable_automagically) {
  const char8 typeof_use[] = u8"v";
  const char8 other_use[] = u8"v";

  // typeof v;
  // v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_variable_use(identifier_of(other_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_typeof,
     typeof_declares_variable_automagically_in_parent_function) {
  const char8 use_before[] = u8"v";
  const char8 typeof_use[] = u8"v";
  const char8 use_after[] = u8"v";

  // v;
  // (() => {
  //   typeof v;
  // });
  // v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_typeof,
     typeof_refers_to_already_declared_variable) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // let v;
  // typeof v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_variable_analyzer_typeof,
     typeof_variable_declared_later_is_an_error) {
  const char8 declaration[] = u8"v";
  const char8 use[] = u8"v";

  // typeof v;  // ERROR
  // let v;
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_typeof_use(identifier_of(use));
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, ElementsAre(DIAG_TYPE_2_SPANS(
                            diag_variable_used_before_declaration,  //
                            use, span_of(use),                      //
                            declaration, span_of(declaration))));
}

TEST(
    test_variable_analyzer_typeof,
    typeof_already_declared_variable_does_not_declare_variable_in_parent_function) {
  const char8 use_before[] = u8"v";
  const char8 declaration[] = u8"v";
  const char8 typeof_use[] = u8"v";
  const char8 use_after[] = u8"v";

  // v;           // ERROR
  // (() => {
  //   let v;
  //   typeof v;
  // });
  // v;           // ERROR
  diag_collector v;
  variable_analyzer l(&v, &default_globals);
  l.visit_variable_use(identifier_of(use_before));
  l.visit_enter_function_scope();
  l.visit_enter_function_scope_body();
  l.visit_variable_declaration(identifier_of(declaration), variable_kind::_let,
                               variable_init_kind::normal);
  l.visit_variable_typeof_use(identifier_of(typeof_use));
  l.visit_exit_function_scope();
  l.visit_variable_use(identifier_of(use_after));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors,
              ElementsAre(DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                         span_of(use_before)),
                          DIAG_TYPE_SPAN(diag_use_of_undeclared_variable, name,
                                         span_of(use_after))));
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
